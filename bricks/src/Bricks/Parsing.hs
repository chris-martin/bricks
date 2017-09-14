{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Parsec 'Parser's for the Bricks language.

Most parsers consume trailing whitespace, except ones that operate within
quoted string environments where whitespace is significant.

-}
module Bricks.Parsing where

-- Bricks
import Bricks.Expression
import Bricks.IndentedString
import Bricks.Keyword
import Bricks.UnquotedString

-- Bricks internal
import           Bricks.Internal.Functor (fmap, void, ($>), (<$>), (<&>))
import           Bricks.Internal.Prelude
import           Bricks.Internal.Seq     (Seq, (|>))
import qualified Bricks.Internal.Seq     as Seq
import qualified Bricks.Internal.Text    as Text

-- Parsec
import           Text.Parsec      ((<?>))
import qualified Text.Parsec      as P
import           Text.Parsec.Text (Parser)

-- Base
import Prelude (succ)

parse'spaces :: Parser ()
parse'spaces =
  void $ P.many (void P.space <|> parse'comment)

parse'comment :: Parser ()
parse'comment =
  parse'comment'inline <|> parse'comment'block

parse'comment'inline :: Parser ()
parse'comment'inline =
  void $ P.try (P.string "--") *> P.manyTill P.anyChar (P.char '\n')

parse'comment'block :: Parser ()
parse'comment'block =
  start <* P.manyTill middle end
  where
    start  = void $ P.try (P.string "{-")
    middle = parse'comment'block <|> void P.anyChar
    end    = P.try (P.string "-}")

-- | Backtracking parser for a particular keyword.
parse'keyword :: Keyword -> Parser ()
parse'keyword k =
  P.try $ do
    -- Consume the keyword
    _ <- P.string (keywordString k)

    -- Do /not/ consume any subsequent character that are allowed to be part
    -- of a valid identifier. For example, this prevents this parser from
    -- interpreting the beginning of an identifier named "letter" as the
    -- keyword "let".
    _ <- P.notFollowedBy (P.satisfy canBeBare'char)

    -- As usual, consume trailing spaces.
    _ <- parse'spaces

    pure ()

{- | Parser for a bare (unquoted) string. Bare strings are restricted to a
conservative set of characters, and they may not be any of the keywords. -}
parse'bare :: Parser Bare
parse'bare =
  do
    -- Consume at least one character
    a <- Text.pack <$> P.many1 (P.satisfy canBeBare'char)

    -- Fail if what we just parsed isn't a valid bare string
    case bareMaybe a of
      Nothing -> P.parserZero
      Just b  -> parse'spaces $> b

{- | Parser for a static string which may be either bare or a quoted.
By "static," we mean that the string may /not/ contain antiquotation. -}
parse'strStatic :: Parser Str'Static
parse'strStatic =
  parse'strStatic'quoted <|> parse'strStatic'bare

-- | Parser for a static string that is quoted.
parse'strStatic'quoted :: Parser Str'Static
parse'strStatic'quoted =
  parse'strDynamic'quoted <&> str'dynamicToStatic >>= \case
    Nothing -> P.parserZero
    Just x  -> pure x

-- | Parser for a static string that is bare (unquoted).
parse'strStatic'bare :: Parser Str'Static
parse'strStatic'bare =
  parse'bare <&> bare'str

{- | Parser for a dynamic string that is quoted. It may be a "normal" quoted
string delimited by one double-quote @"@...@"@ ('parse'strDynamic'normalQ') or
an "indented" string delimited by two single-quotes @''@...@''@
('parse'strDynamic'indentedQ'). -}
parse'strDynamic'quoted :: Parser Str'Dynamic
parse'strDynamic'quoted =
  parse'strDynamic'normalQ <|> parse'strDynamic'indentedQ

-- | Parser for a dynamic string enclosed in "normal" quotes (@"@...@"@).
parse'strDynamic'normalQ :: Parser Str'Dynamic
parse'strDynamic'normalQ =
  P.char '"' *> go Seq.empty
  where
    go :: Seq Str'1 -> Parser Str'Dynamic
    go previousParts =
      asum
        [ end $> previousParts
        , (chars <|> anti) >>= \x -> go $ previousParts |> x
        ]

    -- Read the closing " character
    end = P.char '"' *> parse'spaces

    -- Read an antiquote
    anti = fmap Str'1'Antiquote $
      P.try (P.string "${") *> parse'spaces *> parse'expression <* P.char '}'

    -- Read some normal characters in the string
    chars = do
      xs <- P.many1 $ asum
        [ do
            c <- P.satisfy (\c -> c /= '$' && c /= '"' && c /= '\\')
            pure $ Text.singleton c
        , P.try $ do
            c <- P.char '$'
            _ <- P.notFollowedBy (P.char '{')
            pure $ Text.singleton c
        , do
            _ <- P.char '\\'
            asum
              [ P.char '\\'   $> "\\"
              , P.char '"'    $> "\""
              , P.char 'n'    $> "\n"
              , P.char 'r'    $> "\r"
              , P.char 't'    $> "\t"
              , P.string "${" $> "${"
              ]
        ]
      pure $ Str'1'Literal (Text.concat xs)

{- | Parser for a dynamic string enclosed in "indented string" format,
delimited by two single-quotes @''@...@''@. This form of string does not have
any escape sequences. -}
parse'strDynamic'indentedQ :: Parser Str'Dynamic
parse'strDynamic'indentedQ =
  inStr'join . inStr'dedent . inStr'trim <$> parse'inStr

{- | Parser for an indented string. This parser produces a representation of
the lines from the source as-is, before the whitespace is cleaned up. -}
parse'inStr :: Parser InStr
parse'inStr =
  P.string "''" *> go Seq.empty
  where
    go :: Seq InStr'1 -> Parser InStr
    go previousLines =
      do
        line <- parse'inStr'1
        let newLines = previousLines |> line
        asum
          [ P.string "''" *> parse'spaces $> newLines
          , P.char '\n'   *> go newLines
          ]

-- | Parser for a single line of an 'InStr'.
parse'inStr'1 :: Parser InStr'1
parse'inStr'1 =
  do
    a <- parse'count (P.char ' ')
    b <- go Seq.empty
    pure $ InStr'1 a b
  where
    go :: Seq Str'1 -> Parser Str'Dynamic
    go previousParts =
      asum
        [ end              $> previousParts
        , chars           >>= \x  -> go (previousParts |> x)
        , parse'antiquote >>= \xs -> go (previousParts <> xs)
        ]

    end = P.lookAhead $ asum
      [ void $ P.char '\n'
      , void $ P.try (P.string "''")
      ]

    chars = fmap (Str'1'Literal . Text.pack) $ P.many1 $ asum
      [ P.satisfy (\c -> c /= '$' && c /= '\'' && c /= '\n')
      , P.try $ P.char '$'  <* P.notFollowedBy (P.char '{')
      , P.try $ P.char '\'' <* P.notFollowedBy (P.char '\'')
      ]

parse'antiquote :: Parser (Seq Str'1)
parse'antiquote =
  (P.try (P.string "${") *> parse'spaces *> parse'expression <* P.char '}')
  <&> \case
    Expr'Str x -> x
    x -> Seq.singleton (Str'1'Antiquote x)

{- | Parser for a function parameter (the beginning of a 'Lambda'), including
the colon. This forms part of 'parse'expression', so it backtracks in places
where it has overlap with other types of expressions. -}
parse'param :: Parser Param
parse'param =
  asum
    [ startBare
    , pattern <&> Param'DictPattern
    ]
  where

    -- A parameter that starts with a bare string. This could be a simple
    -- param that consists only of the bare string, or it could be followed by
    -- a dict pattern.
    startBare = do
      -- This part backtracks because until we get to the : or @, we don't
      -- know whether the variable name we're reading is a lambda parameter
      -- or just the name by itself (and not part of a lambda).
      (a, b) <- P.try $ do
        a <- parse'bare <* parse'spaces
        b <- ((P.char ':' $> False) <|> (P.char '@' $> True)) <* parse'spaces
        pure (a, b)
      if b
        -- If we read an @, then the next thing is a pattern.
        then Param'Both a <$> pattern
        -- Otherwise it's just the bare identifier and we're done.
        else pure $ Param'Bare a

    -- A dict pattern. This branch backtracks because the beginning of a
    -- dict pattern looks like the beginning of a dict expression.
    pattern = do
      -- First we look ahead to determine whether it looks like a lambda.
      _ <- P.try . P.lookAhead $ parse'dictPattern'start

      -- And if so, then we go on and parse the dict pattern with no
      -- further backtracking.
      parse'dictPattern <* P.char ':' <* parse'spaces

{- | Parser for a dict pattern (the type of lambda parameter that does dict
destructuring. This parser does not backtrack. -}
parse'dictPattern :: Parser DictPattern
parse'dictPattern =
  P.char '{' *> parse'spaces *> go Seq.empty
  where
    go :: Seq DictPattern'1 -> Parser DictPattern
    go previousItems =
      asum
        [ end $> DictPattern previousItems False
        , ellipsis $> DictPattern previousItems True
        , do
            newItems <- item <&> \x -> previousItems |> x
            asum
              [ P.char ',' *> parse'spaces *> go newItems
              , end $> DictPattern newItems False
              ]
        ]

    item = DictPattern'1 <$> parse'bare <*> P.optionMaybe def

    ellipsis = P.string "..." *> parse'spaces *> end

    def = P.char '?' *> parse'spaces *> parse'expression

    end = P.char '}' *> parse'spaces

{- | This is used in a lookahead by 'parse'param' to determine whether we're
about to start parsing a 'DictPattern'. -}
parse'dictPattern'start :: Parser ()
parse'dictPattern'start =
  P.char '{' *> parse'spaces *> asum
    [ void $ P.string "..."
    , void $ P.char '}' *> parse'spaces *> P.char ':'
    , void $ parse'bare *> (P.char ',' <|> P.char '?' <|> P.char '}')
    ]

applyArgs :: Expression   -- ^ Function
          -> [Expression] -- ^ Args
          -> Expression   -- ^ Function application
applyArgs =
  foldl (\acc b -> Expr'Apply (Apply acc b))

-- | Parser for a lambda expression (@x: y@).
parse'lambda :: Parser Lambda
parse'lambda =
  Lambda <$> parse'param <*> parse'expression

-- | Parser for a list expression (@[ ... ]@).
parse'list :: Parser List
parse'list =
  (P.char '[' *> parse'spaces *> parse'expressionList <* P.char ']' <* parse'spaces)
  <&> Seq.fromList

-- | Parser for a dict expression, either recursive (@rec@ keyword) or not.
parse'dict :: Parser Dict
parse'dict =
  asum
    [ parse'dict'noRec <&> Dict False
    , parse'dict'rec   <&> Dict True
    ]

-- | Parser for a recursive (@rec@ keyword) dict.
parse'dict'rec :: Parser (Seq DictBinding)
parse'dict'rec =
  parse'keyword keyword'rec *> parse'dict'noRec

-- | Parser for a non-recursive (no @rec@ keyword) dict.
parse'dict'noRec :: Parser (Seq DictBinding)
parse'dict'noRec =
  P.char '{' *> parse'spaces *> go Seq.empty
  where
    go :: Seq DictBinding -> Parser (Seq DictBinding)
    go previousBindings = asum
      [ P.char '}' *> parse'spaces $> previousBindings
      , parse'dictBinding >>= \a -> go (previousBindings |> a)
      ]

{- | Parser for a chain of dict lookups (like @.a.b.c@) on the right-hand side
of a 'Dot' expression. -}
parse'dot'rhs'chain :: Parser [Expression]
parse'dot'rhs'chain =
  P.many $
  P.char '.' *> parse'spaces *> parse'expression'dictKey <* parse'spaces

applyDots :: Expression -> [Expression] -> Expression
applyDots =
  foldl (\acc b -> Expr'Dot (Dot acc b))

parse'let :: Parser Let
parse'let =
  parse'keyword keyword'let *> go Seq.empty
  where
    go :: Seq LetBinding -> Parser Let
    go previousBindings =
      asum
        [ end              <&> Let previousBindings
        , parse'letBinding >>= \a -> go (previousBindings |> a)
        ]

    end = parse'keyword keyword'in *> parse'expression

parse'with :: Parser With
parse'with =
  With
    <$> (parse'keyword keyword'with *> parse'expression)
    <*> (P.char ';' *> parse'spaces *> parse'expression)

parse'dictBinding :: Parser DictBinding
parse'dictBinding =
  parse'dictBinding'inherit <|> parse'dictBinding'eq

parse'dictBinding'inherit :: Parser DictBinding
parse'dictBinding'inherit =
  DictBinding'Inherit <$> parse'inherit

parse'dictBinding'eq :: Parser DictBinding
parse'dictBinding'eq =
  DictBinding'Eq
    <$> (parse'expression'dictKey <* parse'spaces <* P.char '=' <* parse'spaces)
    <*> (parse'expression         <* parse'spaces <* P.char ';' <* parse'spaces)

parse'letBinding :: Parser LetBinding
parse'letBinding =
  parse'letBinding'inherit <|> parse'letBinding'eq

parse'letBinding'eq :: Parser LetBinding
parse'letBinding'eq =
  LetBinding'Eq
    <$> (parse'strStatic  <* parse'spaces <* P.char '=' <* parse'spaces)
    <*> (parse'expression <* parse'spaces <* P.char ';' <* parse'spaces)

parse'letBinding'inherit :: Parser LetBinding
parse'letBinding'inherit =
  LetBinding'Inherit <$> parse'inherit

parse'inherit :: Parser Inherit
parse'inherit =
  Inherit
    <$> (parse'keyword keyword'inherit *> P.optionMaybe parse'expression'paren)
    <*> go Seq.empty
  where
    go :: Seq Str'Static -> Parser (Seq Str'Static)
    go previousList =
      asum
        [ P.char ';' *> parse'spaces $> previousList
        , parse'strStatic >>= \x -> go (previousList |> x)
        ]

{- | The primary, top-level expression parser. This is what you use to parse a
@.nix@ file. -}
parse'expression :: Parser Expression
parse'expression =
  p <?> "expression"
  where
    p = asum
      [ parse'let    <&> Expr'Let
      , parse'with   <&> Expr'With
      , parse'lambda <&> Expr'Lambda
      , parse'expressionList >>= \case
          [] -> P.parserZero
          f : args -> pure $ applyArgs f args
      ]

{- | Parser for a list of expressions in a list literal (@[ x y z ]@) or in a
chain of function arguments (@f x y z@). -}
parse'expressionList :: Parser [Expression]
parse'expressionList =
  P.many parse'expressionList'1 <?> "expression list"

{- | Parser for a single item within an expression list ('expressionListP').
This expression is not a lambda, a function application, a @let@-@in@
expression, or a @with@ expression. -}
parse'expressionList'1 :: Parser Expression
parse'expressionList'1 =
  applyDots
    <$> parse'expressionList'1'noDot
    <*> parse'dot'rhs'chain
    <?> "expression list item"

{- | Like 'parse'expressionList'1', but with the further restriction that the
expression may not be a 'Dot'. -}
parse'expressionList'1'noDot :: Parser Expression
parse'expressionList'1'noDot =
  asum
    [ parse'strDynamic'quoted <&> Expr'Str
    , parse'list              <&> Expr'List
    , parse'dict              <&> Expr'Dict
    , parse'bare              <&> Expr'Var
    , parse'expression'paren
    ]
    <?> "expression list item without a dot"

{- | Parser for a parenthesized expression, from opening parenthesis to closing
parenthesis. -}
parse'expression'paren :: Parser Expression
parse'expression'paren =
  P.char '(' *> parse'spaces *> parse'expression <* P.char ')' <* parse'spaces

{- | Parser for an expression in a context that is expecting a dict key.

One of:

- a bare string
- a quoted dynamic string
- an arbitrary expression wrapped in antiquotes (@${@...@}@)
-}
parse'expression'dictKey :: Parser Expression
parse'expression'dictKey =
  asum
    [ parse'strDynamic'quoted <&> Expr'Str
    , P.string "${" *> parse'spaces *> parse'expression
        <* P.char '}' <* parse'spaces
    , parse'bare <&> Expr'Str . str'staticToDynamic . bare'str
    ]

parse'count :: Parser a -> Parser Natural
parse'count p = go 0
  where
    go :: Natural -> Parser Natural
    go n = (p *> go (succ n)) <|> pure n
