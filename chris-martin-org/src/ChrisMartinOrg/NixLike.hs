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
- The concept of "set" is referred to as "dict" (this is not actually a language
  difference, I just use a different word to talk about the same concept)
- No @with@ keyword (todo)
- No comments (todo)

-}
module ChrisMartinOrg.NixLike where

import Control.Applicative ((<|>), (<*), (*>), (<*>), pure)
import Control.Arrow ((>>>))
import Control.Monad ((>>=), fail, mfilter)
import Text.Parsec ((<?>))
import Text.Parsec.Text (Parser)
import Data.Bool (Bool (..), (&&), (||), not)
import Data.Char (Char)
import Data.Eq (Eq (..))
import Data.Foldable (Foldable, asum, foldMap, foldl)
import Data.Function (($), (.))
import Data.Functor (Functor (..), (<$>))
import Data.Maybe (Maybe (..))
import Data.Ord (Ord (..))
import Data.Semigroup ((<>))
import Data.Text (Text)
import Numeric.Natural (Natural)
import Prelude (fromIntegral, Num (..))

import qualified Text.Parsec as P
import qualified Data.Char as Char
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Text as Text

{- $setup

>>> import Data.Either (Either (..), either)
>>> import Data.Function (id, const)
>>> import Prelude (putStrLn, putStr, print, Show, show, IO, String)

We'll use the @parseTest@ function a lot to test parsers. It's a lot like
'P.parseTest' from the parsec library, but it works on parsers of type 'Text'
rather than @'Show' a => a@. It also prints the unparsed input so we can verify
that our parser consumes the right amount of input.

>>> :{
>>> remainingInputTest :: Parser a -> Text -> IO ()
>>> remainingInputTest p input =
>>>   let
>>>     remainderP = P.many P.anyChar :: Parser String
>>>     p' = (("" <$ p) <|> remainderP) *> remainderP
>>>     r = either (const "") id $ P.parse p' "" input
>>>   in
>>>     putStr "remaining input: " *> print r
>>> :}

>>> :{
>>> parseTest :: Parser Text -> Text -> IO ()
>>> parseTest p input =
>>>   do
>>>     case P.parse p "" input of
>>>       Left err -> putStr "parse error at " *> print err
>>>       Right x -> putStrLn (Text.unpack x)
>>>     remainingInputTest p input
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

keywords :: [Text]
keywords =
  [ keyword'rec
  , keyword'let
  , keyword'in
  ]


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

-}
bareIdP :: Parser BareId
bareIdP =
  p <?> "bare identifier"
  where
    p = fmap BareId
      $ mfilter (not . (`List.elem` keywords))
      $ fmap Text.pack
      $ P.many1 (P.satisfy isBareIdentifierChar)

{- |

>>> test = parseTest (renderStrExpr <$> idExprP)

>>> test "a"
"a"
remaining input: ""

>>> test "\"a\""
"a"
remaining input: ""

-}
idExprP :: Parser StrExpr
idExprP =
  strExprP <|> (strExpr . bareIdText <$> bareIdP)


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

-}
strExprP'normal :: Parser StrExpr
strExprP'normal =
  p <?> "normal string literal"
  where
    p = fmap StrExpr $ dP *> P.many (antiquoteP <|> aP) <* dP

    dP = P.char '"'

    aP :: Parser StrExprPart
    aP = StrExprPart'Literal . Text.concat <$> P.many1 bP

    bP :: Parser Text
    bP = asum
      [ strEscapeP
      , Text.singleton <$> (P.try (P.char '$' <* P.notFollowedBy (P.char '{')))
      , Text.singleton <$> P.satisfy (\c -> c /= '$' && c /= '"')
      ]

strEscapeP :: Parser Text
strEscapeP =
  P.char '\\' *> asum
    [ "\\" <$ P.char '\\'
    , "\"" <$ P.char '"'
    , "\n" <$ P.char 'n'
    , "\r" <$ P.char 'r'
    , "\t" <$ P.char 't'
    , "${" <$ P.string "${"
    ]

antiquoteP :: Parser StrExprPart
antiquoteP =
  StrExprPart'Antiquote
    <$> braced (P.string "${") (P.char '}') expressionP
    <?> "antiquoted string"


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

>>> test = parseTest (renderStrExpr <$> (P.spaces *> strExprP'indented))

>>> test "''hello''x"
"hello"
remaining input: "x"

todo - The 'r' quasiquoter from raw-strings-qq might read better here.

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
    p = indentedString'joinLines . stripIndentation . trimEmptyLines <$> indentedStringP

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

-}
indentedStringP :: Parser IndentedString
indentedStringP =
  fmap IndentedString $
  P.between (P.string "''") (P.string "''") $
  indentedStringLineP `P.sepBy` P.char '\n'

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

{- | Parser for a single line of an 'IndentedString'.

>>> :{
>>> test = parseTest
>>>      $ fmap (Text.pack . show . renderIndentedStringLine)
>>>      $ indentedStringLineP
>>> :}

>>> test "abc"
parse error at (line 1, column 4):
unexpected end of input
expecting line content or end of line
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
  IndentedStringLine
    <$> spaceCountP <*> body <* end
    <?> "line of an indented string literal"

  where
    body =
      let p = (antiquoteP <|> lP) <?> "line content"
      in  StrExpr <$> P.many p

    end =
      let p = P.try $ P.lookAhead (P.string "''" <|> P.string "\n")
      in  p <?> "end of line"

    lP = StrExprPart'Literal . Text.pack <$> P.many1 mP

    mP = asum
      [ P.try $ P.char '\'' <* P.notFollowedBy (P.char '\'')
      , P.try $ P.char '$' <* P.notFollowedBy (P.char '{')
      , P.satisfy (\c -> c /= '\'' && c /= '$' && c /= '\n')
      ]

{- | Reads zero or more space characters and produces the number of them.

>>> test = parseTest (Text.pack . show <$> spaceCountP)

>>> test ""
0
remaining input: ""

>>> test "a"
0
remaining input: "a"

>>> test "  a  b"
2
remaining input: "a  b"

>>> test "  \n  "
2
remaining input: "\n  "

-}
spaceCountP :: Parser Natural
spaceCountP =
  fromIntegral . List.length <$> P.many (P.char ' ')

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
    { dictParamItem'variable :: Text
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

>>> item1 = DictParamItem "x" Nothing
>>> item2 = DictParamItem "y" (Just . ParamDefault . Expr'Str . strExpr $ "abc")

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
    DictParamItem a Nothing  -> renderIdentifier a
    DictParamItem a (Just b) -> renderIdentifier a <> " " <>
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
  a <|> b
  where
    a = Param'Id <$> (bareIdP <* P.char ':')
    b = Param'Dict <$> dictParamP

paramDefaultP :: Parser ParamDefault
paramDefaultP = fail "TODO"

dictParamP :: Parser DictParam
dictParamP = fail "TODO"

dictParamItemP :: Parser DictParamItem
dictParamItemP = fail "TODO"

applyArgs:: Expression -> [Expression] -> Expression
applyArgs =
  foldl (\acc b -> Expr'Call (CallExpr acc b))


--------------------------------------------------------------------------------
--  List
--------------------------------------------------------------------------------

-- | A list literal expression, starting with @[@ and ending with @]@.
data ListLiteral = ListLiteral [Expression]

{- |

>>> test = putStrLn . Text.unpack . renderListLiteral . ListLiteral

>>> test []
[ ]

>>> test [ Expr'Id (BareId "true") ]
[ true ]

>>> test [ Expr'Id (BareId "true"), Expr'Id (BareId "false") ]
[ true false ]

>>> call = Expr'Call (CallExpr (Expr'Id (BareId "f")) (Expr'Id (BareId "x")))

>>> test [ call ]
[ (f x) ]

>>> test [ call, Expr'Id (BareId "true") ]
[ (f x) true ]

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
  ListLiteral <$> braced (P.char '[') (P.char ']') expressionListP <?> "list"

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
  (noRec <|> rec) <?> "dict literal"
  where
    noRec = DictLiteral False <$> dictLiteralP'noRec

    rec = DictLiteral True <$>
          (P.try keywordP'rec *> P.spaces *> dictLiteralP'noRec)

    keywordP'rec =
      P.string (Text.unpack keyword'rec) <* P.lookAhead (P.space <|> P.char '{')

{- | Parser for a non-recursive (no @rec@ keyword) dict literal.
-}
dictLiteralP'noRec :: Parser [Binding]
dictLiteralP'noRec =
  P.char '{' *> P.spaces *> P.manyTill (bindingP <* P.spaces) (P.char '}')

{- | Parser for a chain of dict lookups (like @.a.b.c@).

>>> test = parseTest (Text.intercalate "\n" . fmap renderStrExpr <$> dotsP)

This parser /does/ match the empty string.

>>> test ""
<BLANKLINE>
remaining input: ""

If it is given only whitespace, it does not consume it.

>>> test "  "
<BLANKLINE>
remaining input: "  "

The simplest nonempty dot list.

>>> test ".a"
"a"
remaining input: ""

This parser should not consume any trailing whitespace beyond the dot list.

>>> test ".a "
"a"
remaining input: " "

Dot attributes are usually bare identifiers, but they may also be quoted.

>>> test ".\"a\""
"a"
remaining input: ""

Here we throw some extra whitespace into the middle, which makes no difference,
and some extra stuff onto the end, which does not get consumed.

>>> test ".a . b c"
"a"
"b"
remaining input: " c"

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
  P.option [] dotsP'lhs <?> "dots"

-- | Like 'dotsP', but fails unless it can parse at least one dot.
dotsP'lhs :: Parser [StrExpr]
dotsP'lhs =
  (:) <$> dotP <*> P.many dotP'leadingSpaces

{- |

>>> test = parseTest (renderStrExpr <$> dotP)

>>> test ".a"
"a"
remaining input: ""

>>> test ". a . b"
"a"
remaining input: " . b"

>>> test ". \"a\""
"a"
remaining input: ""

>>> test ". \"a\".b"
"a"
remaining input: ".b"

-}
dotP :: Parser StrExpr
dotP =
  P.try (P.char '.') *> P.spaces *> idExprP

dotP'leadingSpaces :: Parser StrExpr
dotP'leadingSpaces =
  P.try (P.spaces *> P.char '.') *> P.spaces *> idExprP

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
letExprP = fail "TODO"


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
    _ <- P.spaces *> P.char '=' *> P.spaces
    b <- expressionP
    _ <- P.spaces *> P.char ';'
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

{- | The primary, top-level expression parser. This is what you use to parse a
@.nix@ file.

>>> test = parseTest (renderExpression RenderContext'Normal <$> expressionP)

The empty string is /not/ a valid expression.

>>> test ""
parse error at (line 1, column 1):
unexpected end of input
expecting expression
TODO
remaining input: ""

A very simple expression: a one-letter bare identifier.

>>> test "a"
a
remaining input: ""

Parsing an expression should not consume any subsequent whitespace.

>>> test "a "
a
remaining input: " "

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

Dot parsing should also not consume trailing whitespace.

>>> test "a.b "
a.b
remaining input: " "

It looks odd when a subsequent expression appears after a dot expression with no
whitespace, but it is permitted.

>>> test "a.b\"c\""
a.b "c"
remaining input: ""

A simple list example.

>>> test "[ a b ]"
[ a b ]
remaining input: ""

A list with trailing whitespace.

>>> test "[ a b ] "
[ a b ]
remaining input: " "

A list that is in the left-hand side of a function call. This will fail at
runtime if the call is evaluated, because a list is not a function, but it
should /parse/ successfully.

>>> test "[ a b ] x"
[ a b ] x
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

A slightly bigger example where we're starting to nest more things.

>>> test "[ \"abc\" f { x = y; } ]"
[ "abc" f { x = y; } ]
remaining input: ""

>>> test "[ \"abc\" (f { x = y; }) ]"
[ "abc" (f { x = y; }) ]
remaining input: ""

-}
expressionP :: Parser Expression
expressionP =
  (a <|> b) <?> "expression"
  where
    a = Expr'Func <$> (FuncExpr <$> P.try paramP <*> expressionP)
    b = expressionListP >>= \case
          [] -> P.parserZero
          f : args -> pure $ applyArgs f args

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
  p <?> "expression list"
  where
    p = expressionP'listItem `sepBy` P.spaces

{- | Parser for a single item within an expression list ('expressionListP').
This expression is not a function, a function application, or a let binding.

>>> :{
>>> test = parseTest
>>>      $ fmap (renderExpression RenderContext'Normal)
>>>      $ expressionP'listItem
>>> :}

>>> test "abc def"
abc
remaining input: " def"

>>> test "a.b c"
a.b
remaining input: " c"

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
    p = applyDots
          <$> expressionP'listItem'noDot
          <*> dotsP

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
    [ fmap Expr'Str strExprP
    , fmap Expr'List listLiteralP
    , fmap Expr'Dict dictLiteralP
    , fmap Expr'Id bareIdP
    , expressionP'paren
    ]
    <?> "expression list item without a dot"

{- | Parser for a parenthesized expression, from opening parenthesis to closing
parenthesis. -}
expressionP'paren :: Parser Expression
expressionP'paren =
  braced (P.char '(') (P.char ')') expressionP



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

sepBy :: Parser a -> Parser b -> Parser [a]
p `sepBy` by =
  (p `sepBy'1` by) <|> pure []

-- todo: this backtracks on p, which could be unexpectedly expensive.
sepBy'1 :: Parser a -> Parser b -> Parser [a]
p `sepBy'1` by =
  (:) <$> p <*> P.many (P.try (by *> p))

braced :: Parser x -> Parser y -> Parser a -> Parser a
braced x y =
  P.between (x *> P.spaces) (P.spaces *> y)
