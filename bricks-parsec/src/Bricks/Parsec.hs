{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{- |

Parsec 'Parser's for the Bricks language.

Most parsers consume trailing whitespace, except ones that operate within quoted
string environments where whitespace is significant.

-}
module Bricks.Parsec
  (
  -- * Expressions
    parse'expression
  , parse'expression'paren
  , parse'expression'antiquote
  , parse'expression'dictKey

  -- * Expression lists
  , parse'expressionList
  , parse'expressionList'1
  , parse'expressionList'1'noDot

  -- * Variables
  , parse'var

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
  , parse'param'var
  , parse'param'noVar
  , parse'dictPattern
  , parse'dictPattern'start

  -- * @let@
  , parse'let
  , parse'letBinding
  , parse'letBinding'eq
  , parse'letBinding'inherit

  -- * Comments and whitespace
  , parse'spaces
  , parse'comment
  , parse'comment'inline
  , parse'comment'block

  -- * Keywords
  , parse'keyword

  ) where

-- Bricks
import Bricks.Expression
import Bricks.IndentedString
import Bricks.Keyword
import Bricks.Source
import Bricks.UnquotedString

-- Bricks internal
import           Bricks.Internal.Prelude
import           Bricks.Internal.Seq     (Seq, (|>))
import qualified Bricks.Internal.Seq     as Seq
import           Bricks.Internal.Text    (Text)
import qualified Bricks.Internal.Text    as Text

-- Parsec
import           Text.Parsec      ((<?>))
import qualified Text.Parsec      as P
import           Text.Parsec.Text (Parser)

-- Containers
import           Data.Set (Set)
import qualified Data.Set as Set

-- Base
import Control.Monad (fail)
import Prelude       (fromIntegral, succ)

-- $setup
--
-- ==== Doctest setup
--
-- >>> import Data.Foldable (length)
-- >>> import Text.Parsec (parseTest)
-- >>> import Prelude (putStrLn)

parse'position :: Parser SourcePosition
parse'position = do
  x <- P.getPosition
  pure SourcePosition
    { sourcePosition'line = fromIntegral $ P.sourceLine x
    , sourcePosition'column = fromIntegral $ P.sourceColumn x
    }

parse'spaces :: Parser ()
parse'spaces =
  void $ P.many (void (P.space <?> "") <|> parse'comment)

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

{- | Backtracking parser for a particular keyword. -}

parse'keyword :: Keyword -> Parser ()
parse'keyword k =
  P.try (void p)
  where
    p =
      -- Consume the keyword
      P.string (keywordString k) *>

      -- Do /not/ consume any subsequent character that are allowed to be part
      -- of a valid identifier. For example, this prevents this parser from
      -- interpreting the beginning of an identifier named "letter" as the
      -- keyword "let".
      P.notFollowedBy (P.satisfy char'canBeUnquoted) *>

      -- As usual, consume trailing spaces.
      parse'spaces

{- | Parser for an unquoted string. Unquoted strings are restricted to a
conservative set of characters, and they may not be any of the keywords. See
'text'canBeUnquoted' for a complete description of the unquoted string rules. -}

-- | ==== Examples
--
-- >>> parseTest parse'strUnquoted "abc"
-- ("abc",1:1-1:4)
--
-- Here the parser consumes letters up to but not including @{@, because that
-- character does not satisfy 'char'canBeUnquoted':
--
-- >>> parseTest parse'strUnquoted "ab{c"
-- ("ab",1:1-1:3)
--
-- \"let\" does not parse as an unquoted string because @let@ is a keyword:
--
-- >>> parseTest parse'strUnquoted "let"
-- parse error at (line 1, column 4):
-- unexpected end of input
--
-- This parser does /not/ parse quoted strings:
--
-- >>> parseTest parse'strUnquoted "\"abc\""
-- parse error at (line 1, column 1):
-- unexpected "\""

parse'strUnquoted :: Parser (UnquotedString, SourceRange)
parse'strUnquoted = do

  pos'1 <- parse'position
  text <- P.many1 (P.satisfy char'canBeUnquoted) <&> Text.pack
  pos'2 <- parse'position

  -- Fail if what we just parsed isn't a valid unquoted string
  case unquotedString'try text of
    Nothing -> P.parserZero
    Just b  -> do
      _ <- parse'spaces
      pure (b, SourceRange pos'1 pos'2)

parse'var :: Parser Var
parse'var = do
  (a, b) <- parse'strUnquoted
  pure $ Var a (Just b)

{- | Parser for a static string which may be either quoted or unquoted. -}

-- | ==== Examples
--
-- >>> parseTest parse'strStatic "\"hello\""
-- {- 1:1-1:8 -} "hello"
--
-- >>> parseTest parse'strStatic "hello"
-- {- 1:1-1:6 -} "hello"
--
-- >>> parseTest parse'strStatic "\"a b\""
-- {- 1:1-1:6 -} "a b"
--
-- >>> parseTest parse'strStatic "a b"
-- {- 1:1-1:2 -} "a"
--
-- By "static," we mean that the string may /not/ contain antiquotation:
--
-- >>> parseTest parse'strStatic "\"a${x}b\" xyz"
-- parse error at (line 1, column 5):
-- antiquotation is not allowed in this context

parse'strStatic :: Parser Str'Static
parse'strStatic =
  (parse'strStatic'quoted <|> parse'strStatic'unquoted) <?> "static string"

{- | Parser for a static string that is quoted. -}

parse'strStatic'quoted :: Parser Str'Static
parse'strStatic'quoted =
  do
    pos'1 <- parse'position
    _ <- P.char '"'
    Str'Static text _ <- parse'str'within'normalQ
    _ <- P.char '"' <|> anti
    pos'2 <- parse'position
    _ <- parse'spaces
    let src = Just (SourceRange pos'1 pos'2)
    pure $ Str'Static text src
  where
    anti = do
      _ <- P.string "${"
      fail "antiquotation is not allowed in this context"

{- | Parser for an unquoted static string. -}

parse'strStatic'unquoted :: Parser Str'Static
parse'strStatic'unquoted = do
  (a, b) <- parse'strUnquoted
  pure $ Str'Static (unquotedString'text a) (Just b)

{- | Parser for a dynamic string that is quoted. It may be a "normal" quoted
string delimited by one double-quote @"@ ... @"@ ('parse'strDynamic'normalQ') or
an "indented" string delimited by two single-quotes @''@ ... @''@
('parse'strDynamic'indentedQ'). -}

parse'strDynamic'quoted :: Parser Str'Dynamic
parse'strDynamic'quoted =
  parse'strDynamic'normalQ <|> parse'strDynamic'indentedQ

{- | Parser for a dynamic string enclosed in "normal" quotes (@"@ ... @"@). -}

parse'strDynamic'normalQ :: Parser Str'Dynamic
parse'strDynamic'normalQ =
  do
    pos'1 <- parse'position
    _ <- P.char '"'
    xs <- go Seq.empty
    pos'2 <- parse'position
    _ <- parse'spaces
    let src = Just (SourceRange pos'1 pos'2)
    pure $ Str'Dynamic xs src
  where
    go :: Seq Str'1 -> Parser (Seq Str'1)
    go previousParts =
      asum
        [ do
            _ <- P.char '"'
            pure previousParts
        , do
            x <- lit <|> (parse'expression'antiquote <&> Str'1'Antiquote)
            go $ previousParts |> x
        ]

    -- Read some literal characters
    lit :: Parser Str'1
    lit = parse'str'within'normalQ <&> Str'1'Literal

{- | Parser for at least one normal character, within a normally-quoted string
context, up to but not including the end of the string or the start of an
antiquotation. -}

parse'str'within'normalQ :: Parser Str'Static
parse'str'within'normalQ =
  do
    pos'1 <- parse'position
    t <- text
    pos'2 <- parse'position
    let src = Just (SourceRange pos'1 pos'2)
    pure $ Str'Static t src

  where
    text :: Parser Text
    text = P.many1 (char <|> parse'str'escape'normalQ) <&> Text.concat

    char :: Parser Text
    char = asum
      [ P.satisfy (\x -> x /= '$' && x /= '"' && x /= '\\')
      , P.try $ P.char '$' <* P.notFollowedBy (P.char '{')
      ] <&> Text.singleton

parse'str'escape'normalQ :: Parser Text
parse'str'escape'normalQ =
  P.char '\\' *> esc
  where
    esc = asum
      [ P.char '\\'   $> "\\"
      , P.char '"'    $> "\""
      , P.char 'n'    $> "\n"
      , P.char 'r'    $> "\r"
      , P.char 't'    $> "\t"
      , P.string "${" $> "${"
      ]

{- | Parser for a dynamic string enclosed in "indented string" format, delimited
by two single-quotes @''@ ... @''@.

This form of string does not have any escape sequences. Therefore the only way
to express @''@ or @${@ within an indented string is to antiquote them. -}

-- | ==== Examples
--
-- >>> x = "''${\"''\"} and ${\"\\${\"}''"
--
-- >>> putStrLn x
-- ''${"''"} and ${"\${"}''
--
-- >>> parseTest (str'dynamic'discardSource <$> parse'strDynamic'indentedQ) x
-- str [antiquote (str ["''"]), " and ", antiquote (str ["${"])]
--
-- >>> parseTest parse'strDynamic'indentedQ x
-- {- 1:1-1:25 -} str [antiquote ({- 1:5-1:9 -} str [{- 1:6-1:8 -} "''"]), {- 1:10-1:15 -} " and ", antiquote ({- 1:17-1:22 -} str [{- 1:18-1:21 -} "${"])]

parse'strDynamic'indentedQ :: Parser Str'Dynamic
parse'strDynamic'indentedQ =
  parse'inStr <&> inStr'to'strDynamic

{- | Parser for an indented string. This parser produces a representation of the
lines from the source as-is, before the whitespace is cleaned up. -}

parse'inStr :: Parser InStr
parse'inStr =
  do
    pos'1 <- parse'position
    _ <- P.string "''"
    lines <- go Seq.empty
    pos'2 <- parse'position
    _ <- parse'spaces
    let src = Just (SourceRange pos'1 pos'2)
    pure $ InStr lines src

  where
    go :: Seq InStr'1 -> Parser (Seq InStr'1)
    go previousLines =
      do
        x <- parse'inStr'1
        let newLines = previousLines |> x

        if isJust (inStr'1'lineBreak x)
          then go newLines
          else P.string "''" $> newLines

{- | Parser for a single line of an 'InStr'. -}

parse'inStr'1 :: Parser InStr'1
parse'inStr'1 =
  do
    pos'1 <- parse'position
    n <- parse'count (P.char ' ')
    pos'2 <- parse'position
    (str, break) <- go Seq.empty
    pure InStr'1
      { inStr'1'level = n
      , inStr'1'indentSource = Just (SourceRange pos'1 pos'2)
      , inStr'1'str = str
      , inStr'1'lineBreak = break
      }
  where
    go :: Seq Str'1 -> Parser (Seq Str'1, Maybe Str'Static)
    go previousParts =
      asum
        [ do
            break <- end
            pure (previousParts, break)
        , do
            x <- chars
            go (previousParts |> x)
        , do
            x <- parse'expression'antiquote
            go (previousParts |> Str'1'Antiquote x)
        ]

    end :: Parser (Maybe Str'Static)
    end = asum
      [ do
          end'pos'1 <- parse'position
          s <- P.char '\n'
          end'pos'2 <- parse'position
          pure $ Just $ Str'Static
            { str'static'text = Text.singleton s
            , str'static'source = Just (SourceRange end'pos'1 end'pos'2)
            }
      , do
          _ <- P.lookAhead . P.try $ P.string "''"
          pure Nothing
      ]

    chars :: Parser Str'1
    chars = do
      chars'pos'1 <- parse'position
      s <- P.many1 $ asum
        [ P.satisfy (\c -> c /= '$' && c /= '\'' && c /= '\n')
        , P.try $ P.char '$'  <* P.notFollowedBy (P.char '{')
        , P.try $ P.char '\'' <* P.notFollowedBy (P.char '\'')
        ]
      chars'pos'2 <- parse'position
      pure $ Str'1'Literal $ Str'Static
        { str'static'text = Text.pack s
        , str'static'source = Just (SourceRange chars'pos'1 chars'pos'2)
        }

{- | Parser for a function parameter (the beginning of a 'Lambda'), including
the colon. This forms part of 'parse'expression', so it backtracks in places
where it has overlap with other types of expressions. -}

parse'param :: Parser Param
parse'param =
  parse'param'var <|> parse'param'noVar

{- | Parser for a parameter that starts with a variable. This could be a simple
param that consists only of /only/ the variable, or the variable may be followed
by a dict pattern. -}

parse'param'var :: Parser Param
parse'param'var = do
  -- This part backtracks because until we get to the : or @, we don't
  -- know whether the variable name we're reading is a lambda parameter
  -- or just the name by itself (and not part of a lambda).
  (a, b) <- P.try $ do
    a <- parse'var <* parse'spaces
    b <- ((P.char ':' $> False) <|> (P.char '@' $> True)) <* parse'spaces
    pure (a, b)
  if b
    -- If we read an @, then the next thing is a pattern.
    then parse'dictPattern <* P.char ':' <* parse'spaces <&> Param'Both a
    -- Otherwise it's just the variable and we're done.
    else pure $ Param'Name a

{- | Parser for a param that has no variable, only a a dict pattern. This parser
backtracks because the beginning of a dict pattern looks like the beginning of a
dict expression. -}

parse'param'noVar :: Parser Param
parse'param'noVar = Param'DictPattern <$> do
  -- First we look ahead to determine whether it looks like a lambda.
  _ <- P.try . P.lookAhead $ parse'dictPattern'start

  -- And if so, then we go on and parse the dict pattern with no
  -- further backtracking.
  parse'dictPattern <* P.char ':' <* parse'spaces

{- | Parser for a dict pattern (the type of lambda parameter that does dict
destructuring. This parser does not backtrack. -}

parse'dictPattern :: Parser DictPattern
parse'dictPattern =
  P.char '{' *> parse'spaces *> go Seq.empty Set.empty
  where

    -- We keep track of what we've parsed so far in two forms:
    go :: Seq DictPattern'1 -- 1. A sequence of items (which will be
                            --    included directly in the result)
       -> Set Text          -- 2. A set of the names of the items (which is
                            --    used to test each new item so we can issue
                            --    an error message if the list contains two
                            --    items having the same name)
       -> Parser DictPattern

    go previousItems previousNames =
      asum
        [ end $> DictPattern previousItems False
        , ellipsis $> DictPattern previousItems True
        , more
        ]
      where
        more :: Parser DictPattern
        more = item >>= \newItem ->
          let
            newName = var'text (dictPattern'1'name newItem)
            newItems = previousItems |> newItem
            newNames = Set.insert newName previousNames

          in
            if newName `Set.member` previousNames
            then fail $ "Name " <> Text.unpack newName <>
                      " appears twice in a dict pattern"
            else asum
              [ P.char ',' *> parse'spaces *> go newItems newNames
              , end $> DictPattern newItems False
              ]

    item :: Parser DictPattern'1
    item = DictPattern'1 <$> parse'var <*> P.optionMaybe def

    def :: Parser Expression
    def = P.char '?' *> parse'spaces *> parse'expression

    ellipsis :: Parser ()
    ellipsis = P.string "..." *> parse'spaces *> end

    end :: Parser ()
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

{- | Parser for a lambda expression (@x: y@). -}

-- | ==== Examples
--
-- >>> test = parseTest (lambda'discardSource <$> parse'lambda)
--
-- >>> test "x: [x x \"a\"]"
-- lambda (param "x") (list [var "x", var "x", str ["a"]])
--
-- >>> test "{a,b}:a"
-- lambda (pattern [dict'param "a", dict'param "b"]) (var "a")
--
-- >>> test "{ ... }: \"x\""
-- lambda (pattern [] <> ellipsis) (str ["x"])
--
-- >>> test "a@{ f, b ? g x, ... }: f b"
-- lambda (param "a" <> pattern [dict'param "f", dict'param "b" & def (apply (var "g") (var "x"))] <> ellipsis) (apply (var "f") (var "b"))
--
-- >>> test "a: b: \"x\""
-- lambda (param "a") (lambda (param "b") (str ["x"]))

parse'lambda :: Parser Lambda
parse'lambda = do
  pos'1 <- parse'position
  head <- parse'param
  body <- parse'expression
  let pos'2 = sourceRange'end <$> expression'source body
  let src = SourceRange pos'1 <$> pos'2
  pure $ Lambda head body src

{- | Parser for a list expression (@[ ... ]@). -}

-- | ==== Examples
--
-- >>> parseTest parse'list "[]"
-- {- 1:1-1:3 -} list []
--
-- >>> :{
-- >>> parseTest (list'discardSource <$> parse'list)
-- >>>   "[x \"one\" (a: b) (c d)]"
-- >>> :}
-- list [var "x", str ["one"], lambda (param "a") (var "b"), apply (var "c") (var "d")]

parse'list :: Parser List
parse'list =
  do
    pos'1 <- parse'position
    _ <- P.char '['
    _ <- parse'spaces
    x <- parse'expressionList
    _ <- P.char ']'
    pos'2 <- parse'position
    _ <- parse'spaces
    let src = (Just (SourceRange pos'1 pos'2))
    pure $ List (Seq.fromList x) src

{- | Parser for a dict expression, either recursive (@rec@ keyword) or not. -}

-- | ==== Examples
--
-- >>> parseTest parse'dict "{}"
-- {- 1:1-1:3 -} dict []
--
-- >>> parseTest parse'dict "rec { }"
-- {- 1:1-1:8 -} rec'dict []
--
-- >>> :{
-- >>> parseTest (dict'discardSource <$> parse'dict)
-- >>>   "{ a = b; inherit (x) y z \"s t\"; }"
-- >>> :}
-- dict [dict'eq (str ["a"]) (var "b"), dict'inherit'from (var "x") ["y", "z", "s t"]]

parse'dict :: Parser Dict
parse'dict =
  parse'dict'noRec <|> parse'dict'rec

{- | Parser for a recursive (@rec@ keyword) dict. -}

-- | ==== Examples
--
-- >>> parseTest parse'dict'rec "rec { }"
-- {- 1:1-1:8 -} rec'dict []
--
-- >>> :{
-- >>> parseTest (dict'discardSource <$> parse'dict'rec)
-- >>>   "rec { a = \"1\"; b = \"${a}2\"; }"
-- >>> :}
-- rec'dict [dict'eq (str ["a"]) (str ["1"]), dict'eq (str ["b"]) (str [antiquote (var "a"), "2"])]

parse'dict'rec :: Parser Dict
parse'dict'rec =
  do
    pos'1 <- parse'position
    _ <- parse'keyword keyword'rec
    Dict _ xs src' <- parse'dict'noRec
    let src = src' <&> \s -> s{ sourceRange'start = pos'1 }
    pure $ Dict True xs src

{- | Parser for a non-recursive (no @rec@ keyword) dict. -}

-- | ==== Examples
--
-- >>> parseTest parse'dict'noRec "{ }"
-- {- 1:1-1:4 -} dict []
--
-- >>> :{
-- >>> parseTest (dict'discardSource <$> parse'dict'noRec)
-- >>>   "{ a = \"1\"; b = \"${a}2\"; }"
-- >>> :}
-- dict [dict'eq (str ["a"]) (str ["1"]), dict'eq (str ["b"]) (str [antiquote (var "a"), "2"])]

parse'dict'noRec :: Parser Dict
parse'dict'noRec =
  do
    pos'1 <- parse'position
    _ <- P.char '{'
    _ <- parse'spaces
    xs <- go Seq.empty
    pos'2 <- parse'position
    _ <- parse'spaces
    let src = Just (SourceRange pos'1 pos'2)
    pure $ Dict False xs src
  where
    go :: Seq DictBinding -> Parser (Seq DictBinding)
    go previousBindings =
      asum
        [ do
            _ <- P.char '}'
            pure previousBindings
        , do
            a <- parse'dictBinding
            go (previousBindings |> a)
        ]

{- | Parser for a chain of dict lookups (like @.a.b.c@) on the right-hand side
of a 'Dot' expression. -}

-- | ==== Examples
--
-- >>> parseTest parse'dot'rhs'chain ""
-- []
--
-- >>> parseTest parse'dot'rhs'chain ".abc"
-- [{- 1:2-1:5 -} str [{- 1:2-1:5 -} "abc"]]
--
-- >>> :{
-- >>> parseTest (fmap expression'discardSource <$> parse'dot'rhs'chain)
-- >>>   ".a.${b}.\"c\".\"d${e}\""
-- >>> :}
-- [str ["a"],var "b",str ["c"],str ["d", antiquote (var "e")]]

parse'dot'rhs'chain :: Parser [Expression]
parse'dot'rhs'chain =
  P.many dot
  where
    dot = do
      _ <- P.char '.'
      _ <- parse'spaces
      x <- parse'expression'dictKey
      _ <- parse'spaces
      pure x

parse'let :: Parser Let
parse'let =
  do
    pos'1 <- parse'position
    _ <- parse'keyword keyword'let
    (xs, e) <- go Seq.empty
    let
      src = do
        pos'2 <- sourceRange'end <$> expression'source e
        pure $ SourceRange pos'1 pos'2
    pure $ Let xs e src
  where
    go :: Seq LetBinding -> Parser (Seq LetBinding, Expression)
    go previousBindings =
      asum
        [ do
            _ <- parse'keyword keyword'in
            e <- parse'expression
            pure (previousBindings, e)
        , do
            a <- parse'letBinding
            go (previousBindings |> a)
        ]

parse'dictBinding :: Parser DictBinding
parse'dictBinding =
  parse'dictBinding'inherit <|> parse'dictBinding'eq

parse'dictBinding'inherit :: Parser DictBinding
parse'dictBinding'inherit =
  do
    _ <- parse'keyword keyword'inherit
    asum
      [ do
          a <- parse'expression'paren
          xs <- go'strs Seq.empty
          pure $ DictBinding'Inherit'Dict a xs
      , do
          xs <- go'vars Seq.empty
          pure $ DictBinding'Inherit'Var xs
      ]
  where
    go'strs :: Seq Str'Static -> Parser (Seq Str'Static)
    go'strs previousList =
      asum
        [ do
            _ <- P.char ';'
            _ <- parse'spaces
            pure previousList
        , do
            x <- parse'strStatic
            go'strs (previousList |> x)
        ]

    go'vars :: Seq Var -> Parser (Seq Var)
    go'vars previousList =
      asum
        [ do
            _ <- P.char ';'
            _ <- parse'spaces
            pure previousList
        , do
            x <- parse'var
            go'vars (previousList |> x)
        ]

parse'dictBinding'eq :: Parser DictBinding
parse'dictBinding'eq =
  do
    key <- parse'expression'dictKey
    _ <- parse'spaces
    _ <- P.char '='
    _ <- parse'spaces
    val <- parse'expression
    _ <- parse'spaces
    _ <- P.char ';'
    _ <- parse'spaces
    pure $ DictBinding'Eq key val

parse'letBinding :: Parser LetBinding
parse'letBinding =
  parse'letBinding'inherit <|> parse'letBinding'eq

parse'letBinding'eq :: Parser LetBinding
parse'letBinding'eq =
  do
    key <- parse'var
    _ <- parse'spaces
    _ <- P.char '='
    _ <- parse'spaces
    val <- parse'expression
    _ <- parse'spaces
    _ <- P.char ';'
    _ <- parse'spaces
    pure $ LetBinding'Eq key val

parse'letBinding'inherit :: Parser LetBinding
parse'letBinding'inherit =
  do
    _ <- parse'keyword keyword'inherit
    a <- parse'expression'paren
    xs <- go Seq.empty
    pure $ LetBinding'Inherit a xs
  where
    go :: Seq Var -> Parser (Seq Var)
    go previousList =
      asum
        [ do
            _ <- P.char ';'
            _ <- parse'spaces
            pure previousList
        , do
            x <- parse'var
            go (previousList |> x)
        ]

{- | The primary, top-level expression parser. This is what you use to parse a
@.nix@ file. -}

-- | ==== Examples
--
-- >>> parseTest parse'expression ""
-- parse error at (line 1, column 1):
-- unexpected end of input
-- expecting expression

parse'expression :: Parser Expression
parse'expression =
  p <?> "expression"
  where
    p = asum
      [ parse'let    <&> Expr'Let
      , parse'lambda <&> Expr'Lambda
      , list
      ]
    list = parse'expressionList >>= \case
      [] -> P.parserZero
      f : args -> pure $ expression'applyArgs f args

{- | Parser for a list of expressions in a list literal (@[ x y z ]@) or in a
chain of function arguments (@f x y z@). -}

-- | ==== Examples
--
-- >>> parseTest parse'expressionList ""
-- []
--
-- >>> parseTest (length <$> parse'expressionList) "x \"one two\" (a: b) (c d)"
-- 4
--
-- >>> parseTest (length <$> parse'expressionList) "(x \"one two\" (a: b) (c d))"
-- 1

parse'expressionList :: Parser [Expression]
parse'expressionList =
  P.many parse'expressionList'1 <?> "expression list"

{- | Parser for a single item within an expression list ('expressionListP').
This expression is not a lambda, a function application, a @let@-@in@
expression, or a @with@ expression. -}

-- | ==== Examples
--
-- >>> parseTest parse'expressionList'1 "ab.xy"
-- {- 1:1-1:6 -} dot ({- 1:1-1:3 -} var "ab") ({- 1:4-1:6 -} str [{- 1:4-1:6 -} "xy"])
--
-- >>> :{
-- >>> parseTest (expression'discardSource <$> parse'expressionList'1)
-- >>>   "(x: f x x) y z"
-- >>> :}
-- lambda (param "x") (apply (apply (var "f") (var "x")) (var "x"))
--
-- >>> :{
-- >>> parseTest (expression'discardSource <$> parse'expressionList'1)
-- >>>   "{ a = b; }.a y"
-- >>> :}
-- dot (dict [dict'eq (str ["a"]) (var "b")]) (str ["a"])

parse'expressionList'1 :: Parser Expression
parse'expressionList'1 =
  expression'applyDots
    <$> parse'expressionList'1'noDot
    <*> parse'dot'rhs'chain
    <?> "expression list item"

{- | Like 'parse'expressionList'1', but with the further restriction that the
expression may not be a 'Dot'. -}

-- | ==== Examples
--
-- >>> parseTest parse'expressionList'1'noDot "ab.xy"
-- {- 1:1-1:3 -} var "ab"
--
-- >>> :{
-- >>> parseTest (expression'discardSource <$> parse'expressionList'1'noDot)
-- >>>   "(x: f x x) y z"
-- >>> :}
-- lambda (param "x") (apply (apply (var "f") (var "x")) (var "x"))
--
-- >>> :{
-- >>> parseTest (expression'discardSource <$> parse'expressionList'1'noDot)
-- >>>   "{ a = b; }.a y"
-- >>> :}
-- dict [dict'eq (str ["a"]) (var "b")]

parse'expressionList'1'noDot :: Parser Expression
parse'expressionList'1'noDot =
  asum
    [ parse'strDynamic'quoted <&> Expr'Str
    , parse'list              <&> Expr'List
    , parse'dict              <&> Expr'Dict
    , parse'var               <&> Expr'Var
    , parse'expression'paren
    ]
    <?> "expression list item without a dot"

{- | Parser for a parenthesized expression, from opening parenthesis to closing
parenthesis. -}

parse'expression'paren :: Parser Expression
parse'expression'paren =
  P.char '(' *> parse'spaces *> parse'expression <* P.char ')' <* parse'spaces

parse'expression'antiquote :: Parser Expression
parse'expression'antiquote =
  P.try (P.string "${") *> parse'spaces *> parse'expression <* P.char '}'

{- | Parser for an expression in a context that is expecting a dict key.

One of:

  - an unquoted string
  - a quoted dynamic string
  - an arbitrary expression wrapped in antiquotes (@${@ ... @}@) -}

parse'expression'dictKey :: Parser Expression
parse'expression'dictKey =
  quoted <|> antiquoted <|> unquoted

  where
    quoted :: Parser Expression
    quoted = parse'strDynamic'quoted <&> Expr'Str

    antiquoted :: Parser Expression
    antiquoted = do
      _ <- P.string "${"
      _ <- parse'spaces
      e <- parse'expression
      _ <- P.char '}'
      _ <- parse'spaces
      pure e

    unquoted :: Parser Expression
    unquoted = do
      (x, src) <- parse'strUnquoted
      pure $ Expr'Str $ str'static'to'dynamic $
        Str'Static (unquotedString'text x) (Just src)

parse'count :: Parser a -> Parser Natural
parse'count p =
  go 0
  where
    go :: Natural -> Parser Natural
    go n = (p *> go (succ n)) <|> pure n
