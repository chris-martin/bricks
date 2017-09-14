{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Parsec 'Parser's for the Bricks language.

Most parsers consume trailing whitespace, except ones that operate within
quoted string environments where whitespace is significant.

-}
module Bricks.Parsing
  (
  -- * Expressions
    parse'expression
  , parse'expression'paren
  , parse'expression'dictKey

  -- * Expression lists
  , parse'expressionList
  , parse'expressionList'1
  , parse'expressionList'1'noDot

  -- * Strings
  , parse'strUnquoted
  , parse'strStatic
  , parse'strStatic'quoted
  , parse'strStatic'unquoted
  , parse'strDynamic'quoted
  , parse'strDynamic'normalQ
  , parse'strDynamic'indentedQ
  , parse'str'within'normalQ
  , parse'str'escape'normalQ
  , parse'inStr
  , parse'inStr'1

  -- * Lists
  , parse'list

  -- * Dicts
  , parse'dict
  , parse'dict'rec
  , parse'dict'noRec
  , parse'dictBinding
  , parse'dictBinding'inherit
  , parse'dictBinding'eq

  -- * Dict lookup
  , parse'dot'rhs'chain

  -- * Lambdas
  , parse'lambda

  -- * Function parameters
  , parse'param
  , parse'dictPattern
  , parse'dictPattern'start

  -- * @let@
  , parse'let
  , parse'letBinding
  , parse'letBinding'eq
  , parse'letBinding'inherit

  -- * @with@
  , parse'with

  -- * @inherit@
  , parse'inherit

  -- * Comments and whitespace
  , parse'spaces
  , parse'comment
  , parse'comment'inline
  , parse'comment'block

  -- * Keywords
  , parse'keyword

  -- * Antiquotation
  , parse'antiquote

  ) where

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
import           Bricks.Internal.Text    (Text)
import qualified Bricks.Internal.Text    as Text

-- Parsec
import           Text.Parsec      ((<?>))
import qualified Text.Parsec      as P
import           Text.Parsec.Text (Parser)

-- Base
import Control.Monad (fail)
import Prelude       (succ)

{- $setup

>>> import Text.Parsec (parseTest)

-}

parse'spaces :: Parser ()
parse'spaces =
  (void $ P.many (void (P.space <?> "") <|> parse'comment))

parse'comment :: Parser ()
parse'comment =
  parse'comment'inline <|> parse'comment'block

parse'comment'inline :: Parser ()
parse'comment'inline =
  void $ P.try (P.string "--" <?> "") *> P.manyTill P.anyChar (P.char '\n')

parse'comment'block :: Parser ()
parse'comment'block =
  start <* P.manyTill middle end
  where
    start  = void $ P.try (P.string "{-" <?> "")
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
    _ <- P.notFollowedBy (P.satisfy char'canRenderUnquoted)

    -- As usual, consume trailing spaces.
    _ <- parse'spaces

    pure ()

{- | Parser for an unquoted string. Unquoted strings are restricted to a
conservative set of characters, and they may not be any of the keywords.

>>> parseTest parse'strUnquoted "abc"
unquoted "abc"

>>> parseTest parse'strUnquoted "x{y"
unquoted "x"

>>> parseTest parse'strUnquoted "let"
parse error at (line 1, column 4):
unexpected end of input

-}
parse'strUnquoted :: Parser Str'Unquoted
parse'strUnquoted =
  do
    -- Consume at least one character
    a <- Text.pack <$> P.many1 (P.satisfy char'canRenderUnquoted)

    -- Fail if what we just parsed isn't a valid unquoted string
    case str'tryUnquoted a of
      Nothing -> P.parserZero
      Just b  -> parse'spaces $> b

{- | Parser for a static string which may be either quoted or unquoted.

>>> parseTest parse'strStatic "\"hello\""
"hello"

>>> parseTest parse'strStatic "hello"
"hello"

>>> parseTest parse'strStatic "\"a b\""
"a b"

>>> parseTest parse'strStatic "a b"
"a"

By "static," we mean that the string may /not/ contain antiquotation.

>>> parseTest parse'strStatic "\"a${x}b\" xyz"
parse error at (line 1, column 5):
antiquotation is not allowed in this context

-}
parse'strStatic :: Parser Str'Static
parse'strStatic =
  (parse'strStatic'quoted <|> parse'strStatic'unquoted) <?> "static string"

-- | Parser for a static string that is quoted.
parse'strStatic'quoted :: Parser Str'Static
parse'strStatic'quoted =
  P.char '"' *> parse'str'within'normalQ <* asum
    [ P.char '"' *> parse'spaces
    , P.string "${" *> fail "antiquotation is not allowed in this context"
    ]

-- | Parser for an unquoted static string.
parse'strStatic'unquoted :: Parser Str'Static
parse'strStatic'unquoted =
  parse'strUnquoted <&> str'unquotedToStatic

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
        [ end $> Str'Dynamic previousParts
        , asum
            [ parse'str'within'normalQ <&> Str'1'Literal
            , anti
            ]
          >>= \x -> go $ previousParts |> x
        ]

    -- Read the closing " character
    end = P.char '"' *> parse'spaces

    -- Read an antiquote
    anti = fmap Str'1'Antiquote $
      P.try (P.string "${") *> parse'spaces *> parse'expression <* P.char '}'

{- | Parser for at least one normal character, within a normally-quoted string
context, up to but not including the end of the string or the start of an
antiquotation. -}
parse'str'within'normalQ :: Parser Text
parse'str'within'normalQ = do
  xs <- P.many1 $ asum
    [ P.satisfy (\c -> c /= '$' && c /= '"' && c /= '\\') <&> Text.singleton
    , P.try $ P.char '$' <* P.notFollowedBy (P.char '{')  <&> Text.singleton
    , parse'str'escape'normalQ
    ]
  pure $ Text.concat xs

parse'str'escape'normalQ :: Parser Text
parse'str'escape'normalQ =
  P.char '\\' *> asum
    [ P.char '\\'   $> "\\"
    , P.char '"'    $> "\""
    , P.char 'n'    $> "\n"
    , P.char 'r'    $> "\r"
    , P.char 't'    $> "\t"
    , P.string "${" $> "${"
    ]

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
          [ P.string "''" *> parse'spaces $> InStr newLines
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
        [ end              $> Str'Dynamic previousParts
        , chars           >>= \x                -> go (previousParts |> x)
        , parse'antiquote >>= \(Str'Dynamic xs) -> go (previousParts <> xs)
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

parse'antiquote :: Parser Str'Dynamic
parse'antiquote =
  (P.try (P.string "${") *> parse'spaces *> parse'expression <* P.char '}')
  <&> \case
    Expr'Str x -> x
    x -> strDynamic'singleton (Str'1'Antiquote x)

{- | Parser for a function parameter (the beginning of a 'Lambda'), including
the colon. This forms part of 'parse'expression', so it backtracks in places
where it has overlap with other types of expressions. -}
parse'param :: Parser Param
parse'param =
  asum
    [ startWithVar
    , pattern <&> Param'DictPattern
    ]
  where

    -- A parameter that starts with a variable. This could be a simple param
    -- that consists only of only the variable, or the variable may be followed
    -- by a dict pattern.
    startWithVar = do
      -- This part backtracks because until we get to the : or @, we don't
      -- know whether the variable name we're reading is a lambda parameter
      -- or just the name by itself (and not part of a lambda).
      (a, b) <- P.try $ do
        a <- parse'strUnquoted <* parse'spaces
        b <- ((P.char ':' $> False) <|> (P.char '@' $> True)) <* parse'spaces
        pure (a, b)
      if b
        -- If we read an @, then the next thing is a pattern.
        then Param'Both a <$> pattern
        -- Otherwise it's just the variable and we're done.
        else pure $ Param'Name a

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

    item = DictPattern'1 <$> parse'strUnquoted <*> P.optionMaybe def

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
    , void $ parse'strUnquoted *> (P.char ',' <|> P.char '?' <|> P.char '}')
    ]

{- | Parser for a lambda expression (@x: y@).

>>> parseTest parse'lambda "x: [x x \"a\"]"
lambda (param "x") (list [var "x", var "x", str ["a"]])

>>> parseTest parse'lambda "{a,b}:a"
lambda (dict pattern [param "a", param "b"]) (var "a")

>>> parseTest parse'lambda "{ ... }: \"x\""
lambda (dict pattern [], ellipsis) (str ["x"])

>>> parseTest parse'lambda "a@{ f, b ? g x, ... }: f b"
lambda (param "a", dict pattern [param "f", param "b" with default (apply (var "g") (var "x"))], ellipsis) (apply (var "f") (var "b"))

>>> parseTest parse'lambda "a: b: \"x\""
lambda (param "a") (lambda (param "b") (str ["x"]))

-}
parse'lambda :: Parser Lambda
parse'lambda =
  Lambda <$> parse'param <*> parse'expression

{- | Parser for a list expression (@[ ... ]@).

>>> parseTest parse'list "[]"
list []

>>> parseTest parse'list "[x \"one\" (a: b) (c d)]"
list [var "x", str ["one"], lambda (param "a") (var "b"), apply (var "c") (var "d")]
-}
parse'list :: Parser List
parse'list =
  (start *> parse'expressionList <* end) <&> List . Seq.fromList
  where
    start = P.char '[' *> parse'spaces
    end   = P.char ']' <* parse'spaces

{- | Parser for a dict expression, either recursive (@rec@ keyword) or not.

>>> parseTest parse'dict "{}"
dict []

>>> parseTest parse'dict "rec {  }"
recursive dict []

>>> parseTest parse'dict "{ a = b; inherit (x) y z \"s t\"; }"
dict [binding (str ["a"]) (var "b"), inherit from (var "x") ["y", "z", "s t"]]

-}
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
          f : args -> pure $ expression'applyArgs f args
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
  expression'applyDots
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
    , parse'strUnquoted       <&> Expr'Var
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

- an unquoted string
- a quoted dynamic string
- an arbitrary expression wrapped in antiquotes (@${@...@}@)
-}
parse'expression'dictKey :: Parser Expression
parse'expression'dictKey =
  asum
    [ parse'strDynamic'quoted <&> Expr'Str
    , P.string "${" *> parse'spaces *> parse'expression
        <* P.char '}' <* parse'spaces
    , parse'strUnquoted <&> Expr'Str . str'unquotedToDynamic
    ]

parse'count :: Parser a -> Parser Natural
parse'count p = go 0
  where
    go :: Natural -> Parser Natural
    go n = (p *> go (succ n)) <|> pure n
