{-# LANGUAGE ApplicativeDo, LambdaCase, NamedFieldPuns, NoImplicitPrelude,
             OverloadedStrings, ScopedTypeVariables, ViewPatterns #-}

{- | This module parses and evaluates a Nix-like language. I don't claim that it
/is/ Nix, for two reasons:

1. Nix doesn't actually have a specification.
2. In the interest of laziness, I have only built out enough of it for my
   purpose at hand.

Notable differences from Nix:

- No built-in null, integer, or boolean types
- No @\@@ keyword
- No @builtins@ and no infix operators (@+@, @-@, @//@)
- No URI literals
- The concept of "set" is referred to as "dict" (this is not actually a language
  difference, I just use a different word to talk about the same concept)
- No comments (todo)

-}
module ChrisMartinOrg.NixLike where

import Control.Applicative ((<|>), (<*>), (*>), pure)
import Control.Arrow ((>>>))
import Control.Monad (guard)
import Text.Parsec ((<?>))
import Text.Parsec.Text (Parser)
import Data.Bool (Bool (..), (&&), (||), not)
import Data.Char (Char)
import Data.Eq (Eq (..))
import Data.Foldable (Foldable, asum, foldMap, foldl)
import Data.Function (($), (.), id)
import Data.Functor (Functor (..), (<$>), ($>), void)
import Data.Maybe (Maybe (..))
import Data.Ord (Ord (..))
import Data.Semigroup ((<>))
import Data.Text (Text)
import Numeric.Natural (Natural)
import Prelude (fromIntegral, Num (..), succ)

import qualified Text.Parsec as P
import qualified Data.Char as Char
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Text as Text

{- $setup

>>> :set -XOverloadedStrings

>>> import Data.Either (Either (..), either)
>>> import Data.Function (const)
>>> import Prelude (putStrLn, putStr, print, Show, show, IO, String)

We'll use the @parseTest@ function a lot to test parsers. It's a lot like
'P.parseTest' from the parsec library, but it works on parsers of type 'Text'
rather than @'Show' a => a@. It also prints the unparsed input so we can verify
that our parser consumes the right amount of input.

>>> :{
>>> parseTest :: Parser Text -> Text -> IO ()
>>> parseTest p input =
>>>   do
>>>     case P.parse p "" input of
>>>       Left err -> putStr "parse error at " *> print err
>>>       Right x -> putStrLn (Text.unpack x)
>>>     remainingInputTest p input
>>>
>>> remainingInputTest :: Parser a -> Text -> IO ()
>>> remainingInputTest p input =
>>>   let
>>>     remainderP = P.many P.anyChar :: Parser String
>>>     p' = (("" <$ p) <|> remainderP) *> remainderP
>>>     r = either (const "") id $ P.parse p' "" input
>>>   in
>>>     putStr "remaining input: " *> print r
>>> :}

-}


--------------------------------------------------------------------------------
--  Keywords
--------------------------------------------------------------------------------

keyword'rec :: Text
keyword'rec = "rec"

keyword'let :: Text
keyword'let = "let"

keyword'in :: Text
keyword'in = "in"

keyword'with :: Text
keyword'with = "with"

keywords :: [Text]
keywords =
  [ keyword'rec
  , keyword'let
  , keyword'in
  , keyword'with
  ]

keywordP :: Text -> Parser ()
keywordP k = do
  _ <- P.string (Text.unpack k)
  _ <- P.notFollowedBy (P.satisfy isBareIdentifierChar)
  _ <- P.spaces
  pure ()


--------------------------------------------------------------------------------
--  Identifiers
--------------------------------------------------------------------------------

{- | An identifier which /must/ be unquoted. For example, in a binding @x = y;@,
the @x@ may be quoted, but the @y@ must be a bare identifier. The bare
identifiers are a subset of the identifiers. -}
newtype BareId =
  BareId
    { bareIdText :: Text
    }

{- | An identifier can be /any/ string. In some cases this means we need to
render it in quotes; see 'isUnquotableText'.

>>> test = putStrLn . Text.unpack . renderIdentifier

>>> test "abc"
abc

>>> test "a\"b"
"a\"b"

>>> test "-ab"
-ab

>>> test ""
""

-}
renderIdentifier :: Text -> Text
renderIdentifier x =
  if isBareIdentifierName x then x else renderQuotedString x

renderBareId :: BareId -> Text
renderBareId (BareId x) = x

renderIdExpr :: StrExpr -> Text
renderIdExpr =
  \case
    StrExpr (Foldable.toList -> [StrExprPart'Literal x])
      | isBareIdentifierName x -> x
    x -> renderStrExpr x

{- | Whether an identifier having this name can be rendered without quoting it.
We allow a name to be a bare identifier, and thus to render unquoted, if all
these conditions are met:

- The string is nonempty
- All characters satify 'isBareIdentifierChar'
- The string is not a keyword

>>> isBareIdentifierName "-ab_c"
True

>>> isBareIdentifierName ""
False

>>> isBareIdentifierName "a\"b"
False

>>> isBareIdentifierName "let"
False

-}
isBareIdentifierName :: Text -> Bool
isBareIdentifierName x =
  Text.all isBareIdentifierChar x
  && List.all (/= x) ("" : keywords)

-- | Letters, @-@, and @_@.
isBareIdentifierChar :: Char -> Bool
isBareIdentifierChar c =
  Char.isLetter c || c == '-' || c == '_'

{- |

>>> test = parseTest (bareIdText <$> bareIdP)

>>> test "-ab_c"
-ab_c
remaining input: ""

>>> test ""
parse error at (line 1, column 1):
unexpected end of input
expecting bare identifier
remaining input: ""

>>> test "a\"b"
a
remaining input: "\"b"

>>> test "a b"
a
remaining input: "b"

-}
bareIdP :: Parser BareId
bareIdP =
  p <?> "bare identifier"
  where
    p = do
      a <- Text.pack <$> P.many1 (P.satisfy isBareIdentifierChar)
      guard $ not (a `List.elem` keywords)
      _ <- P.spaces
      pure $ BareId a

{- |

>>> test = parseTest (renderStrExpr <$> idExprP)

>>> test "a"
"a"
remaining input: ""

>>> test "\"a\""
"a"
remaining input: ""

>>> test "a b"
"a"
remaining input: "b"

-}
idExprP :: Parser StrExpr
idExprP =
  asum
    [ strExprP
    , do
        a <- bareIdP
        pure $ strExpr (bareIdText a)
    ]


--------------------------------------------------------------------------------
--  String
--------------------------------------------------------------------------------

{- | A quoted string expression, which may be a simple string like @"hello"@ or
a more complex string containing antiquotation like @"Hello, my name is
${name}!"@. -}
newtype StrExpr = StrExpr [StrExprPart]

data StrExprPart
  = StrExprPart'Literal Text
  | StrExprPart'Antiquote Expression


-- | A simple string literal expression with no antiquotation.
strExpr :: Text -> StrExpr
strExpr =
  StrExpr . (\x -> [x]) . StrExprPart'Literal


--------------------------------------------------------------------------------
--  String rendering
--------------------------------------------------------------------------------

{- |

>>> test = putStrLn . Text.unpack . renderStrExpr . StrExpr

>>> test []
""

>>> test [ StrExprPart'Literal "hello" ]
"hello"

>>> test [ StrExprPart'Literal "escape ${ this and \" this" ]
"escape \${ this and \" this"

>>> :{
>>> test [ StrExprPart'Literal "Hello, my name is "
>>>      , StrExprPart'Antiquote (Expr'Id (BareId "name"))
         , StrExprPart'Literal "!"
>>>      ]
>>> :}
"Hello, my name is ${name}!"

-}
renderStrExpr :: StrExpr -> Text
renderStrExpr (StrExpr xs) =
  "\"" <> renderStrExprParts xs <> "\""

renderStrExprParts :: [StrExprPart] -> Text
renderStrExprParts = foldMap renderStrExprPart

renderStrExprPart :: StrExprPart -> Text
renderStrExprPart =
  \case
    StrExprPart'Literal t -> strEscape t
    StrExprPart'Antiquote e -> renderAntiquote e

renderAntiquote :: Expression -> Text
renderAntiquote e =
  "${" <> renderExpression RenderContext'Normal e <> "}"

renderQuotedString :: Text -> Text
renderQuotedString x =
  "\"" <> strEscape x <> "\""

strEscape :: Text -> Text
strEscape =
  Text.replace "\"" "\\\"" .
  Text.replace "${" "\\${" .
  Text.replace "\n" "\\n" .
  Text.replace "\r" "\\r" .
  Text.replace "\t" "\\t"


--------------------------------------------------------------------------------
--  String parsing
--------------------------------------------------------------------------------

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


--------------------------------------------------------------------------------
--  Normal strings
--------------------------------------------------------------------------------

{- | Parser for a "normal" string literal, delimited by one double-quote (@"@).
Normal string literals have antiquotation and backslash escape sequences. They
may span multiple lines.

>>> test = parseTest (renderStrExpr <$> strExprP)

>>> test "\"a\""
"a"
remaining input: ""

>>> test "\"a\" x"
"a"
remaining input: "x"

>>> test "\"a ${b} c\""
"a ${b} c"
remaining input: ""

>>> test "\"a${ b }c\""
"a${b}c"
remaining input: ""

>>> test "\"$\""
"$"
remaining input: ""

>>> test "\"a$\""
"a$"
remaining input: ""

>>> test "\"\\${\""
"\${"
remaining input: ""

>>> test "\"a\\${\""
"a\${"
remaining input: ""

-}
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


--------------------------------------------------------------------------------
--  Indented strings
--------------------------------------------------------------------------------

{- | An "indented string literal," delimited by two single-quotes @''@. This is
parsed with 'indentedStringP', which is used to implement 'strExprP'indented'.
-}
newtype IndentedString = IndentedString [IndentedStringLine]

-- | One line of an 'IndentedString'. This is parsed with 'indentedStringLineP'.
data IndentedStringLine =
  IndentedStringLine
    { indentedStringLine'leadingSpaces :: Natural
        -- ^ The number of leading space characters. We store this separately
        -- for easier implementation of 'stripIndentation'.
    , indentedStringLine'str :: StrExpr
        -- ^ The rest of the line after any leading spaces.
    }

{- | Parser for an "indented string literal," delimited by two single-quotes
@''@ ('strExprP'indented'). Indented string literals have antiquotation but no
backslash escape sequences.

This type of literal is called "indented" because leading whitespace is
intelligently stripped from the string ('stripIndentation'), which makes it
convenient to use these literals for multi-line strings within an indented
expression without the whitespace from indentation ending up as part of the
string.

>>> :{
>>> test = parseTest
>>>      $ fmap renderStrExpr
>>>      $ P.spaces *> strExprP'indented
>>> :}

>>> test "''hello''x"
"hello"
remaining input: "x"

>>> test "''hello'' x"
"hello"
remaining input: "x"

>>> :{
>>> test "  ''\n\
>>>      \    one\n\
>>>      \    two\n\
>>>      \  ''x"
>>> :}
"one\ntwo"
remaining input: "x"

>>> :{
>>> test "  ''\n\
>>>      \    one\n\
>>>      \\n\
>>>      \    two\n\
>>>      \  ''x"
>>> :}
"one\n\ntwo"
remaining input: "x"

-}
strExprP'indented :: Parser StrExpr
strExprP'indented =
  p <?> "indented string literal"
  where
    p = do
      a <- indentedStringP
      pure $ indentedString'joinLines . stripIndentation . trimEmptyLines $ a

{- | Parse an indented string. This parser produces unprocessed lines, /without/
stripping the indentation or removing leading/trailing empty lines. For a parser
that does those things, see 'strExprP'indented'.

>>> :{
>>> test = parseTest
>>>      $ fmap (Text.pack . show)
>>>      $ fmap (\(IndentedString xs) -> renderIndentedStringLine <$> xs)
>>>      $ P.spaces *> indentedStringP
>>> :}

>>> :{
>>> test "  ''\n\
>>>      \    one\n\
>>>      \    two\n\
>>>      \  ''x"
>>> :}
["","    one","    two","  "]
remaining input: "x"

>>> :{
>>> test "  ''\n\
>>>      \    one\n\
>>>      \\n\
>>>      \    two\n\
>>>      \  ''x"
>>> :}
["","    one","","    two","  "]
remaining input: "x"

>>> test "'''' x"
[""]
remaining input: "x"

>>> test "''abc''"
["abc"]
remaining input: ""

>>> test "''\n''"
["",""]
remaining input: ""

>>> test "''  \n''"
["  ",""]
remaining input: ""

>>> test "''   abc\ndef''"
["   abc","def"]
remaining input: ""

-}
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

{- | Parser for a single line of an 'IndentedString'.

>>> :{
>>> test = parseTest
>>>      $ fmap (Text.pack . show . renderIndentedStringLine)
>>>      $ indentedStringLineP
>>> :}

>>> test "abc"
parse error at (line 1, column 4):
unexpected end of input
expecting "$", "'", "\n", "''" or "${"
remaining input: ""

>>> test "\n"
""
remaining input: "\n"

>>> test "  \n"
"  "
remaining input: "\n"

>>> test "   abc\ndef"
"   abc"
remaining input: "\ndef"

>>> test "   abc''x"
"   abc"
remaining input: "''x"

-}
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

{- |

>>> test n xs = renderIndentedStringLine $ IndentedStringLine n (StrExpr xs)

>>> :{
>>> test 2 [ StrExprPart'Literal "abc"
>>>        , StrExprPart'Antiquote (Expr'Id $ BareId "x")
>>>        ]
>>> :}
"  abc${x}"

-}
renderIndentedStringLine :: IndentedStringLine -> Text
renderIndentedStringLine (IndentedStringLine n (StrExpr xs)) =
  Text.replicate (fromIntegral n) " " <> renderStrExprParts xs

renderIndentedStringLines :: [IndentedStringLine] -> Text
renderIndentedStringLines =
  foldMap renderIndentedStringLine

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


--------------------------------------------------------------------------------
--  Function
--------------------------------------------------------------------------------

-- | A function expression.
data FuncExpr =
  FuncExpr
    { funcExpr'param :: Param
        -- ^ A declaration of the function's parameter
    , funcExpr'expression :: Expression
        -- ^ The body of the function; what it evaluates to
    }

-- | A function call expression.
data CallExpr =
  CallExpr
    { callExpr'function :: Expression
        -- ^ The function being called
    , callExpr'expression :: Expression
        -- ^ The argument to the function
    }

{- | The parameter to a function. All functions have a single parameter, but
it's more complicated than that because it may also include dict destructuring.
-}
data Param
  = Param'Id BareId
      -- ^ A simple single-parameter function
  | Param'Dict DictParam
      -- ^ Dict destructuring, which gives you something resembling multiple
      -- named parameters with default values

-- | A function parameter that does dict destructuring. See 'Param'.
data DictParam =
  DictParam
    { dictParam'items :: [DictParamItem]
        -- ^ The set of destructured identifiers, along with any default value
        -- each may have
    , dictParam'ellipsis :: Bool
        -- ^ Whether to allow additional keys beyond what is listed in the
        -- items, corresponding to the @...@ keyword
    }

data DictParamItem =
  DictParamItem
    { dictParamItem'variable :: BareId
        -- ^ The bound variable
    , dictParamItem'default :: Maybe ParamDefault
        -- ^ The default value to be used if the key is not present in the dict
    }

{- | A default expression to use for a variable bound by a dict destructuring
expression (see 'DictParamItem') if the key is not present in the dict. -}
newtype ParamDefault = ParamDefault Expression

renderParam :: Param -> Text
renderParam =
  \case
    Param'Id x -> renderBareId x <> ":"
    Param'Dict x -> renderDictParam x

{- |

>>> test a b = putStrLn . Text.unpack . renderDictParam $ DictParam a b

>>> test [] False
{ }:

>>> test [] True
{ ... }:

>>> item1 = DictParamItem (BareId "x") Nothing

>>> :{
>>> item2 = DictParamItem (BareId "y")
>>>   (Just . ParamDefault . Expr'Str . strExpr $ "abc")
>>> :}

>>> test [ item1, item2 ] False
{ x, y ? "abc" }:

>>> test [ item1, item2 ] True
{ x, y ? "abc", ... }:

-}
renderDictParam :: DictParam -> Text
renderDictParam (DictParam items ellipsis) =
  case Foldable.toList items of
    [] -> if ellipsis then "{ ... }:" else "{ }:"
    xs -> "{ " <> Text.intercalate ", " (fmap renderDictParamItem xs) <>
          (if ellipsis then ", ... }:" else " }:")

renderDictParamItem :: DictParamItem -> Text
renderDictParamItem =
  \case
    DictParamItem a Nothing  -> renderBareId a
    DictParamItem a (Just b) -> renderBareId a <> " " <>
                                renderParamDefault b

renderParamDefault :: ParamDefault -> Text
renderParamDefault (ParamDefault x) =
  "? " <> renderExpression RenderContext'Normal x

renderFuncExpr :: RenderContext -> FuncExpr -> Text
renderFuncExpr cx (FuncExpr a b) =
  if p then "(" <> x <> ")" else x

  where
    x = renderParam a <> " " <>
        renderExpression RenderContext'Normal b

    p = case cx of
      RenderContext'Normal   -> False
      RenderContext'List     -> True
      RenderContext'Call'lhs -> True
      RenderContext'Call'rhs -> False
      RenderContext'Dot'lhs  -> True

renderCallExpr :: RenderContext -> CallExpr -> Text
renderCallExpr cx (CallExpr a b) =
  if p then "(" <> x <> ")" else x

  where
    x = renderExpression RenderContext'Call'lhs a <> " " <>
        renderExpression RenderContext'Call'rhs b

    p = case cx of
      RenderContext'Normal   -> False
      RenderContext'List     -> True
      RenderContext'Call'lhs -> False
      RenderContext'Call'rhs -> True
      RenderContext'Dot'lhs  -> True

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
about to start parsing a 'DictParam'.

>>> :{
>>> test = parseTest
>>>      $ (P.try dictParamStartP $> "yes") <|> pure "no"
>>> :}

>>> test "{a, b}:"
yes
remaining input: " b}:"

>>> test "{a ? 4, b}:"
yes
remaining input: " 4, b}:"

>>> test "{ }: x"
yes
remaining input: " x"

{ } is not enough to determine whether we're parsing a dict param, because if
it isn't followed by a colon, then it's actually an empty dict literal.

>>> test "{ } x"
no
remaining input: "{ } x"

>>> test "{ ... }:"
yes
remaining input: " }:"

-}
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


--------------------------------------------------------------------------------
--  List
--------------------------------------------------------------------------------

-- | A list literal expression, starting with @[@ and ending with @]@.
data ListLiteral = ListLiteral [Expression]

{- |

>>> test = putStrLn . Text.unpack . renderListLiteral . ListLiteral

>>> test []
[ ]

>>> test [ Expr'Id (BareId "a") ]
[ a ]

>>> :{
>>> test [ Expr'Id (BareId "a")
>>>      , Expr'Id (BareId "b") ]
>>> :}
[ a b ]

>>> :{
>>> call = Expr'Call $ CallExpr (Expr'Id (BareId "f"))
>>>                             (Expr'Id (BareId "x"))
>>> :}

>>> test [ call ]
[ (f x) ]

>>> test [ call, Expr'Id (BareId "a") ]
[ (f x) a ]

-}
renderListLiteral :: ListLiteral -> Text
renderListLiteral =
  \case
    ListLiteral (Foldable.toList -> []) -> renderEmptyList
    ListLiteral (Foldable.toList -> values) ->
      "[ " <>
      foldMap (\v -> renderExpression RenderContext'List v <> " ") values <>
      "]"

renderEmptyList :: Text
renderEmptyList = "[ ]"

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


--------------------------------------------------------------------------------
--  Dict
--------------------------------------------------------------------------------

{- | A dict literal expression, starting with @{@ or @rec {@ and ending with
@}@. -}
data DictLiteral =
  DictLiteral
    { dictLiteral'rec :: Bool
        -- ^ Whether the dict is recursive (denoted by the @rec@ keyword)
    , dictLiteral'bindings :: [Binding]
        -- ^ The bindings (everything between @{@ and @}@)
    }

-- | An expression of the form @person.name@ that looks up a key from a dict.
data Dot = Dot
  { dot'dict :: Expression
  , dot'key :: StrExpr
  }

renderDictLiteral :: DictLiteral -> Text
renderDictLiteral =
  \case
    DictLiteral True  [] -> "rec " <> renderEmptyDict
    DictLiteral False [] -> renderEmptyDict
    DictLiteral True  bs -> "rec { " <> renderBindingList bs <> " }"
    DictLiteral False bs -> "{ " <> renderBindingList bs <> " }"

renderEmptyDict :: Text
renderEmptyDict = "{ }"

renderDot :: Dot -> Text
renderDot (Dot a b) =
  renderExpression RenderContext'Dot'lhs a <> "." <> renderIdExpr b

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

{- | Parser for a chain of dict lookups (like @.a.b.c@).

>>> test = parseTest (Text.intercalate "\n" . fmap renderStrExpr <$> dotsP)

This parser /does/ match the empty string.

>>> test ""
<BLANKLINE>
remaining input: ""

The simplest nonempty dot list.

>>> test ".a"
"a"
remaining input: ""

This parser consumes any trailing whitespace beyond the dot list.

>>> test ".a "
"a"
remaining input: ""

Dot attributes are usually bare identifiers, but they may also be quoted.

>>> test ".\"a\""
"a"
remaining input: ""

Here we throw some extra whitespace into the middle, which makes no difference,
and some extra stuff onto the end, which does not get consumed.

>>> test ".a . b c"
"a"
"b"
remaining input: "c"

Another example of a quoted dot, this time following an unquoted dot.

>>> test ".a.\"b\""
"a"
"b"
remaining input: ""

If quotes or braces are involved, the stuff that follows a dot expression
can directly abut it with no whitespace in between.

>>> test ".a.\"b\"x"
"a"
"b"
remaining input: "x"

>>> test ".a.b\"x\""
"a"
"b"
remaining input: "\"x\""

>>> test ".a.b(x)"
"a"
"b"
remaining input: "(x)"

-}
dotsP :: Parser [StrExpr]
dotsP =
  P.many dotP <?> "dots"

{- |

>>> test = parseTest (renderStrExpr <$> dotP)

>>> test ".a"
"a"
remaining input: ""

>>> test ". a . b"
"a"
remaining input: ". b"

>>> test ". \"a\""
"a"
remaining input: ""

>>> test ". \"a\".b"
"a"
remaining input: ".b"

-}
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


--------------------------------------------------------------------------------
--  Let
--------------------------------------------------------------------------------

-- | A @let@-@in@ expression.
data LetExpr =
  LetExpr
    { letExpr'bindings :: [Binding]
        -- ^ The bindings (everything between the @let@ and @in@ keywords)
    , letExpr'value :: Expression
        -- ^ The value (everything after the @in@ keyword)
    }

renderLetExpr :: LetExpr -> Text
renderLetExpr (LetExpr bs x) =
  if List.null bs
    then "let in " <> body
    else "let " <> renderBindingList bs <> " in " <> body
  where
    body = renderExpression RenderContext'Normal x

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

--------------------------------------------------------------------------------
--  With
--------------------------------------------------------------------------------

data With =
  With
    { with'dict :: Expression
    , with'expr :: Expression
    }

renderWith :: With -> Text
renderWith (With a b) =
  keyword'with <> " " <>
  renderExpression RenderContext'Normal a <> "; " <>
  renderExpression RenderContext'Normal b

withP :: Parser With
withP =
  do
    _ <- keywordP keyword'with
    a <- expressionP
    _ <- P.char ';'
    _ <- P.spaces
    b <- expressionP
    pure $ With a b


--------------------------------------------------------------------------------
--  Binding
--------------------------------------------------------------------------------

-- | A binding of the form @x = y;@ within a 'DictLiteral' or 'LetExpr'.
data Binding = Binding StrExpr Expression

renderBinding :: Binding -> Text
renderBinding (Binding a b) =
  renderIdExpr a <> " = " <> renderExpression RenderContext'Normal b <> ";"

renderBindingList :: Foldable f => f Binding -> Text
renderBindingList =
  Foldable.toList >>> \case
    [] -> ""
    bs -> Text.intercalate " " (fmap renderBinding bs)

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


--------------------------------------------------------------------------------
--  Expression
--------------------------------------------------------------------------------

data Expression
  = Expr'Str  StrExpr
  | Expr'List ListLiteral
  | Expr'Dict DictLiteral
  | Expr'Dot  Dot
  | Expr'Id   BareId
  | Expr'Func FuncExpr
  | Expr'Call CallExpr
  | Expr'Let  LetExpr
  | Expr'With With

renderExpression :: RenderContext -> Expression -> Text
renderExpression c =
  \case
    Expr'Str  x -> renderStrExpr x
    Expr'Dict x -> renderDictLiteral x
    Expr'List x -> renderListLiteral x
    Expr'Id   x -> renderBareId x
    Expr'Dot  x -> renderDot x
    Expr'Func x -> renderFuncExpr c x
    Expr'Call x -> renderCallExpr c x
    Expr'Let  x -> renderLetExpr x
    Expr'With x -> renderWith x

{- | The primary, top-level expression parser. This is what you use to parse a
@.nix@ file.

>>> test = parseTest (renderExpression RenderContext'Normal <$> expressionP)

The empty string is /not/ a valid expression.

>>> test ""
parse error at (line 1, column 1):
unexpected end of input
expecting expression
remaining input: ""

A very simple expression: a one-letter bare identifier.

>>> test "a"
a
remaining input: ""

Parsing an expression consumes any subsequent whitespace.

>>> test "a "
a
remaining input: ""

When there are multiple expressions, that is parsed as a function call.

>>> test "f x"
f x
remaining input: ""

Expressions can directly abut each other, so it's important that the expression
parser is also able to read an expression even when another expression directly
follows it.

>>> test "f[x y]"
f [ x y ]
remaining input: ""

A simple example of parsing a dot expression.

>>> test "a.b"
a.b
remaining input: ""

Dot parsing also consumes trailing whitespace.

>>> test "a.b "
a.b
remaining input: ""

It looks odd when a subsequent expression appears after a dot expression with no
whitespace, but it is permitted.

>>> test "a.b\"c\""
a.b "c"
remaining input: ""

A simple list example.

>>> test "[ a b ]"
[ a b ]
remaining input: ""

A list with trailing whitespace that get consumed.

>>> test "[ a b ] "
[ a b ]
remaining input: ""

A list that is in the left-hand side of a function call. This will fail at
runtime if the call is evaluated, because a list is not a function, but it
should /parse/ successfully.

>>> test "[ a b ] x"
[ a b ] x
remaining input: ""

The same thing with other weird stuff on the left-hand side of a function call.

>>> test "{ a = b; } x"
{ a = b; } x
remaining input: ""

>>> test "{ } x"
{ } x
remaining input: ""

Note that the case where an empty dict is on the left-hand side of a function
call looks very similar to the case where a function expression using dict
deconstruction with no bindings. The only difference is the colon.

>>> test "{ }: x"
{ }: x
remaining input: ""

A list with a function call inside.

>>> test "[ (f x) ]"
[ (f x) ]
remaining input: ""

>>> test "[ a (f x) ]"
[ a (f x) ]
remaining input: ""

A minimal dict literal.

>>> test "{ x = y; }"
{ x = y; }
remaining input: ""

The left-hand side of a binding is allowed to be anything, even something that
would not be valid as a bare identifier, if it's in quotes.

>>> test "{ \"a b\" = y; }"
{ "a b" = y; }
remaining input: ""

It may even be the empty string.

>>> test "{ \"\" = y; }"
{ "" = y; }
remaining input: ""

None of the conventional whitespace within a dict literal is mandatory.

>>> test "{x=y;}"
{ x = y; }
remaining input: ""

A simple dict literal with two bindings.

>>> test "{ x = y; a = b; }"
{ x = y; a = b; }
remaining input: ""

The same thing without any whitespace.

>>> test "{x=y;a=b;}"
{ x = y; a = b; }
remaining input: ""

A simple function.

>>> test "x : y"
x: y
remaining input: ""

Whitespace before the colon is unconventional, but allowed.

>>> test "x : y"
x: y
remaining input: ""

The space after the colon is not mandatory. (In Nix, this example would be
parsed as the string "x:y", but here we do not support URI literals.)

>>> test "x:y"
x: y
remaining input: ""

A slightly bigger example where we're starting to nest more things.

>>> test "[ \"abc\" f { x = y; } ]"
[ "abc" f { x = y; } ]
remaining input: ""

>>> test "[ \"abc\" (f { x = y; }) ]"
[ "abc" (f { x = y; }) ]
remaining input: ""

This is not valid a expression (though the first bit of it is).

>>> test "a b: c"
a b
remaining input: ": c"

This is not a valid expression.

>>> test "(a b: c)"
parse error at (line 1, column 5):
unexpected ":"
expecting expression list item or ")"
remaining input: ""

Here are some functions that use dict deconstruction.

>>> test "{ a, b, c ? x, ... }: g b (f a c)"
{ a, b, c ? x, ... }: g b (f a c)
remaining input: ""

>>> test "{ x, ... }: f x"
{ x, ... }: f x
remaining input: ""

>>> test "{ x?\"abc\" }: x"
{ x ? "abc" }: x
remaining input: ""

>>> test "{ ... }: x"
{ ... }: x
remaining input: ""

A let expression.

>>> test "let f = x: plus one x; in f seven"
let f = x: plus one x; in f seven
remaining input: ""

A let binding list may be empty, although it is silly.

>>> test "let in f x"
let in f x
remaining input: ""

>>> test "with x; y"
with x; y
remaining input: ""

>>> test "with{x=y;}; f x z"
with { x = y; }; f x z
remaining input: ""

-}
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
chain of function arguments (@f x y z@).

>>> :{
>>> test = parseTest $
>>>   fmap
>>>     (Text.intercalate "\n" . fmap (renderExpression RenderContext'Normal))
>>>     expressionListP
>>> :}

>>> test ""
<BLANKLINE>
remaining input: ""

>>> test "x y z"
x
y
z
remaining input: ""

>>> test "(a)b c(d)"
a
b
c
d
remaining input: ""

>>> test "a.\"b\"c"
a.b
c
remaining input: ""

>>> test "\"abc\" (f { x = y; })"
"abc"
f { x = y; }
remaining input: ""

>>> test "r re reck"
r
re
reck
remaining input: ""

>>> test "r re rec { } reck"
r
re
rec { }
reck
remaining input: ""

-}
expressionListP :: Parser [Expression]
expressionListP =
  P.many expressionP'listItem <?> "expression list"

{- | Parser for a single item within an expression list ('expressionListP').
This expression is not a function, a function application, or a let binding.

>>> :{
>>> test = parseTest
>>>      $ fmap (renderExpression RenderContext'Normal)
>>>      $ expressionP'listItem
>>> :}

>>> test "abc def"
abc
remaining input: "def"

>>> test "a.b c"
a.b
remaining input: "c"

>>> test "a.\"b\"c"
a.b
remaining input: "c"

>>> test "(a.b)c"
a.b
remaining input: "c"

>>> test "a.b(c)"
a.b
remaining input: "(c)"

>>> test "[ a b ]c"
[ a b ]
remaining input: "c"

>>> test "a[ b c ]"
a
remaining input: "[ b c ]"

>>> test "\"a\"b"
"a"
remaining input: "b"

-}
expressionP'listItem :: Parser Expression
expressionP'listItem =
  p <?> "expression list item"
  where
    p = do
      a <- expressionP'listItem'noDot
      b <- dotsP
      pure $ applyDots a b

{- | Like 'expressionP'listItem', but with the further restriction that the
expression may not be a dot.

>>> :{
>>> test = parseTest
>>>      $ fmap (renderExpression RenderContext'Normal)
>>>      $ expressionP'listItem'noDot
>>> :}

>>> test "a.b c"
a
remaining input: ".b c"

-}
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


--------------------------------------------------------------------------------
--  RenderContext
--------------------------------------------------------------------------------

data RenderContext
  = RenderContext'Normal
  | RenderContext'List
  | RenderContext'Call'lhs
  | RenderContext'Call'rhs
  | RenderContext'Dot'lhs


--------------------------------------------------------------------------------
--  General parsing stuff
--------------------------------------------------------------------------------

counterP :: Parser a -> Parser Natural
counterP p = go 0
  where
    go :: Natural -> Parser Natural
    go n = (p *> go (succ n)) <|> pure n
