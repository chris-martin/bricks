{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Parsec parsers for the Bricks language.

All parsers consume trailing whitespace unless otherwise noted.

-}
module Bricks.Parsing where

import Bricks.Identifiers
import Bricks.Keywords
import Bricks.Types

import Control.Applicative (pure, (*>), (<*>), (<|>))
import Control.Monad       (guard)
import Data.Bool           (Bool (..), not, (&&))
import Data.Eq             (Eq (..))
import Data.Foldable       (asum, foldl)
import Data.Function       (id, ($), (.))
import Data.Functor        (void, ($>), (<$>))
import Data.Ord            (Ord (..))
import Numeric.Natural     (Natural)
import Prelude             (Num (..), fromIntegral, succ)
import Text.Parsec         ((<?>))
import Text.Parsec.Text    (Parser)

import qualified Data.List   as List
import qualified Data.Text   as Text
import qualified Text.Parsec as P

-- | Parser for a particular keyword.
keywordP  :: Keyword    -- The keyword you're expecting
          -> Parser ()
keywordP k =
  do
    -- Consume the keyword
    _ <- P.string (keywordString k)

    -- Do /not/ consume any subsequent character that are allowed to be part
    -- of a valid identifier. For example, this prevents this parser from
    -- interpreting the beginning of an identifier named "letter" as the
    -- keyword "let".
    _ <- P.notFollowedBy (P.satisfy isBareIdentifierChar)

    -- As usual, consume trailing spaces.
    _ <- P.spaces

    pure ()

{- | Parser for a bare (unquoted) identifier. Bare identifiers are restricted
to a conservative set of characters (see 'isBareIdentifierChar'), and they may
not be any of the 'keywords'. -}
bareIdP :: Parser BareId
bareIdP =
  p <?> "bare identifier"
  where
    p = do
      -- Consume at least one character
      a <- Text.pack <$> P.many1 (P.satisfy isBareIdentifierChar)

      -- Fail if what we just parsed is a keyword.
      guard $ List.all ((/= a) . keywordText) keywords
      _ <- P.spaces
      pure $ BareId a

idExprP :: Parser StrExpr
idExprP =
  asum
    [ strExprP
    , do
        a <- bareIdP
        pure $ StrExpr [ StrExprPart'Literal (bareIdText a) ]
    ]

{- | Parser for any kind of string literal. This includes "normal" string
literals delimited by one double-quote @"@ ('strExprP'normal') and "indented"
string literals delimited by two single-quotes @''@ ('strExprP'indented'). -}
strExprP :: Parser StrExpr
strExprP =
  (strExprP'normal <|> strExprP'indented) <?> "string"

antiquoteP :: Parser Expression
antiquoteP =
  do
    _ <- P.try (P.string "${")
    _ <- P.spaces
    a <- expressionP
    _ <- P.char '}'
    pure a

{- | Parser for a "normal" string literal, delimited by one double-quote (@"@).
Normal string literals have antiquotation and backslash escape sequences. They
may span multiple lines. -}
strExprP'normal :: Parser StrExpr
strExprP'normal =
  (P.char '"' *> go id) <?> "normal string literal"
  where
    go :: ([StrExprPart] -> [StrExprPart]) -> Parser StrExpr
    go previousParts =
      asum
        [ do
            _ <- P.char '"'
            _ <- P.spaces
            pure $ StrExpr (previousParts [])
        , do
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
                    [ P.char '\\' $> "\\"
                    , P.char '"' $> "\""
                    , P.char 'n' $> "\n"
                    , P.char 'r' $> "\r"
                    , P.char 't' $> "\t"
                    , P.string "${" $> "${"
                    ]
              ]
            go $ previousParts . (StrExprPart'Literal (Text.concat xs) :)
        , do
            a <- antiquoteP
            go $ previousParts . (StrExprPart'Antiquote a :)
        ]

{- | Parser for an "indented string literal," delimited by two single-quotes
@''@ ('strExprP'indented'). Indented string literals have antiquotation but no
backslash escape sequences.

This type of literal is called "indented" because leading whitespace is
intelligently stripped from the string ('stripIndentation'), which makes it
convenient to use these literals for multi-line strings within an indented
expression without the whitespace from indentation ending up as part of the
string. -}
strExprP'indented :: Parser StrExpr
strExprP'indented =
  p <?> "indented string literal"
  where
    p = do
      a <- indentedStringP
      pure $ indentedString'joinLines . stripIndentation . trimEmptyLines $ a

{- | Parser for an indented string. This parser produces unprocessed lines,
/without/ stripping the indentation or removing leading/trailing empty lines.
For a parser that does those things, see 'strExprP'indented'. -}
indentedStringP :: Parser IndentedString
indentedStringP =
  do
    _ <- P.string "''"
    go id
  where
    go :: ([IndentedStringLine] -> [IndentedStringLine])
       -> Parser IndentedString
    go previousLines =
      do
        line <- indentedStringLineP
        asum
          [ do
              _ <- P.string "''"
              _ <- P.spaces
              pure $ IndentedString (previousLines [line])
          , do
              _ <- P.char '\n'
              go $ previousLines . (line :)
          ]

-- | Parser for a single line of an 'IndentedString'.
indentedStringLineP :: Parser IndentedStringLine
indentedStringLineP =
  do
    a <- counterP (P.char ' ')
    b <- go id
    pure $ IndentedStringLine a b
  where
    go :: ([StrExprPart] -> [StrExprPart]) -> Parser StrExpr
    go previousParts =
      asum
        [ do
            _ <- P.lookAhead $ void (P.char '\n') <|> void (P.try (P.string "''"))
            pure $ StrExpr (previousParts [])
        , do
            xs <- P.many1 $ asum
              [ do
                  c <- P.satisfy (\c -> c /= '$' && c /= '\'' && c /= '\n')
                  pure $ Text.singleton c
              , P.try $ do
                  c <- P.char '$'
                  _ <- P.notFollowedBy (P.char '{')
                  pure $ Text.singleton c
              , P.try $ do
                  c <- P.char '\''
                  _ <- P.notFollowedBy (P.char '\'')
                  pure $ Text.singleton c
              ]
            go $ previousParts . (StrExprPart'Literal (Text.concat xs) :)
        , do
            a <- antiquoteP
            go $ previousParts . (StrExprPart'Antiquote a :)
        ]

-- | Join 'IndentedStringLine's with newlines interspersed.
indentedString'joinLines :: IndentedString -> StrExpr
indentedString'joinLines (IndentedString xs) =
  StrExpr $ List.concat $ List.intersperse [newline] (f <$> xs)
  where
    newline = StrExprPart'Literal "\n"
    f :: IndentedStringLine -> [StrExprPart]
    f (IndentedStringLine n (StrExpr parts)) =
      StrExprPart'Literal (Text.replicate (fromIntegral n) " ") : parts

{- | Determines whether an 'IndentedStringLine' contains any non-space
characters. The opposite of 'indentedStringLine'nonEmpty'.

This is used to determine whether this line should be considered when
calculating the number of space characters to strip in 'stripIndentation'. -}
indentedStringLine'nonEmpty :: IndentedStringLine -> Bool
indentedStringLine'nonEmpty =
  \case
    IndentedStringLine{ indentedStringLine'str = StrExpr [] } -> False
    _ -> True

-- | The opposite of 'indentedStringLine'nonEmpty'.
indentedStringLine'empty :: IndentedStringLine -> Bool
indentedStringLine'empty =
  not . indentedStringLine'nonEmpty

{- | Determine how many characters of whitespace to strip from an indented
string. -}
indentedString'indentationSize :: IndentedString -> Natural
indentedString'indentationSize (IndentedString xs) =
  case List.filter indentedStringLine'nonEmpty xs of
    [] -> 0
    ys -> List.minimum (indentedStringLine'leadingSpaces <$> ys)

{- | Modify an 'IndentedStringLine' by applying a function to its number of
leading spaces. -}
indentedStringLine'modifyLeadingSpaces
  :: (Natural -> Natural) -> IndentedStringLine -> IndentedStringLine
indentedStringLine'modifyLeadingSpaces
  f x@IndentedStringLine{indentedStringLine'leadingSpaces = a} =
  x{ indentedStringLine'leadingSpaces = f a }

{- | Determine the minimum indentation of any nonempty line, and remove that
many space characters from the front of every line. -}
stripIndentation :: IndentedString -> IndentedString
stripIndentation is@(IndentedString xs) =
  let
    b = indentedString'indentationSize is
    f a = if a >= b then a - b else 0
  in
    IndentedString (indentedStringLine'modifyLeadingSpaces f <$> xs)

-- | Remove any empty lines from the beginning or end of an indented string.
trimEmptyLines :: IndentedString -> IndentedString
trimEmptyLines (IndentedString xs) =
  IndentedString (trimWhile indentedStringLine'empty xs)
  where
    trimWhile f = List.dropWhileEnd f . List.dropWhile f

paramP :: Parser Param
paramP =
  asum
    [ Param'Id   <$> idParamP
    , Param'Dict <$> dictParamP
    ]

idParamP :: Parser BareId
idParamP = P.try $ do
  a <- bareIdP
  _ <- P.spaces
  _ <- P.char ':'
  _ <- P.spaces
  pure a

dictParamP :: Parser DictParam
dictParamP =
  do
    _ <- P.try . P.lookAhead $ dictParamStartP
    _ <- P.char '{'
    _ <- P.spaces
    asum
      [ do
          _ <- dictParamEndP
          pure $ DictParam [] False
      , go id
      ]
  where
    go :: ([DictParamItem] -> [DictParamItem]) -> Parser DictParam
    go previousItems =
      asum
        [ do
            _ <- P.string "..."
            _ <- P.spaces
            _ <- dictParamEndP
            pure (DictParam (previousItems []) True)
        , do
            a <- bareIdP
            b <- P.optionMaybe paramDefaultP
            let items = previousItems . (DictParamItem a b :)
            asum
              [ do
                  _ <- P.char ','
                  _ <- P.spaces
                  go items
              , do
                  _ <- dictParamEndP
                  pure (DictParam (items []) False)
              ]
        ]

    paramDefaultP :: Parser ParamDefault
    paramDefaultP =
      do
        _ <- P.char '?'
        _ <- P.spaces
        ParamDefault <$> expressionP

{- | This is used in a lookahead by 'dictParamP' to determine whether we're
about to start parsing a 'DictParam'. -}
dictParamStartP :: Parser ()
dictParamStartP =
  P.char '{' *> P.spaces *>
    asum
      [ void $ P.string "..."
      , void $ P.char '}' *> P.spaces *> P.char ':'
      , void $ bareIdP *> (P.char ',' <|> P.char '?' <|> P.char '}')
      ]

dictParamEndP :: Parser ()
dictParamEndP =
  void $ P.char '}' *> P.char ':' *> P.spaces

applyArgs:: Expression -> [Expression] -> Expression
applyArgs =
  foldl (\acc b -> Expr'Call (CallExpr acc b))

funcP :: Parser FuncExpr
funcP =
  FuncExpr <$> paramP <*> expressionP

listLiteralP :: Parser ListLiteral
listLiteralP =
  p <?> "list"
  where
    p = do
      _ <- P.char '['
      _ <- P.spaces
      xs <- expressionListP
      _ <- P.char ']'
      _ <- P.spaces
      pure $ ListLiteral xs

dictLiteralP :: Parser DictLiteral
dictLiteralP =
  p <?> "dict"
  where
    p = asum
      [ do
          a <- dictLiteralP'noRec
          pure $ DictLiteral False a
      , do
          _ <- P.try (keywordP keyword'rec)
          a <- dictLiteralP'noRec
          pure $ DictLiteral True a
      ]

-- | Parser for a non-recursive (no @rec@ keyword) dict literal.
dictLiteralP'noRec :: Parser [Binding]
dictLiteralP'noRec =
  P.char '{' *> P.spaces *> go id
  where
    go :: ([Binding] -> [Binding]) -> Parser [Binding]
    go previousBindings =
      asum
        [ do
            _ <- P.char '}'
            _ <- P.spaces
            pure $ previousBindings []
        , do
            a <- bindingP
            go $ previousBindings . (a :)
        ]

-- | Parser for a chain of dict lookups (like @.a.b.c@).
dotsP :: Parser [StrExpr]
dotsP =
  P.many dotP <?> "dots"

dotP :: Parser StrExpr
dotP =
  do
    _ <- P.char '.'
    _ <- P.spaces
    a <- idExprP
    _ <- P.spaces
    pure a

applyDots :: Expression -> [StrExpr] -> Expression
applyDots =
  foldl (\acc b -> Expr'Dot (Dot acc b))

letExprP :: Parser LetExpr
letExprP =
  do
    _ <- keywordP keyword'let
    go id
  where
    go :: ([Binding] -> [Binding]) -> Parser LetExpr
    go previousBindings =
      asum
        [ do
            _ <- keywordP keyword'in
            x <- expressionP
            pure $ LetExpr (previousBindings []) x
        , do
            a <- bindingP
            go $ previousBindings . (a :)
        ]

withP :: Parser With
withP =
  do
    _ <- keywordP keyword'with
    a <- expressionP
    _ <- P.char ';'
    _ <- P.spaces
    b <- expressionP
    pure $ With a b

bindingP :: Parser Binding
bindingP =
  do
    a <- idExprP
    _ <- P.spaces
    _ <- P.char '='
    _ <- P.spaces
    b <- expressionP
    _ <- P.spaces
    _ <- P.char ';'
    _ <- P.spaces
    pure $ Binding a b

{- | The primary, top-level expression parser. This is what you use to parse a
@.nix@ file. -}
expressionP :: Parser Expression
expressionP =
  p <?> "expression"
  where
    p = asum
      [ Expr'Let  <$> letExprP
      , Expr'With <$> withP
      , Expr'Func <$> funcP
      , do
          a <- expressionListP
          case a of
            [] -> P.parserZero
            f : args -> pure $ applyArgs f args
      ]

{- | Parser for a list of expressions in a list literal (@[ x y z ]@) or in a
chain of function arguments (@f x y z@). -}
expressionListP :: Parser [Expression]
expressionListP =
  P.many expressionP'listItem <?> "expression list"

{- | Parser for a single item within an expression list ('expressionListP').
This expression is not a function, a function application, or a let binding. -}
expressionP'listItem :: Parser Expression
expressionP'listItem =
  p <?> "expression list item"
  where
    p = do
      a <- expressionP'listItem'noDot
      b <- dotsP
      pure $ applyDots a b

{- | Like 'expressionP'listItem', but with the further restriction that the
expression may not be a dot. -}
expressionP'listItem'noDot :: Parser Expression
expressionP'listItem'noDot =
  asum
    [ Expr'Str  <$> strExprP
    , Expr'List <$> listLiteralP
    , Expr'Dict <$> dictLiteralP
    , Expr'Id   <$> bareIdP
    , expressionP'paren
    ]
    <?> "expression list item without a dot"

{- | Parser for a parenthesized expression, from opening parenthesis to closing
parenthesis. -}
expressionP'paren :: Parser Expression
expressionP'paren =
  do
    _ <- P.char '('
    _ <- P.spaces
    a <- expressionP
    _ <- P.char ')'
    _ <- P.spaces
    pure a

counterP :: Parser a -> Parser Natural
counterP p = go 0
  where
    go :: Natural -> Parser Natural
    go n = (p *> go (succ n)) <|> pure n
