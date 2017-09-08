{-# LANGUAGE ApplicativeDo, LambdaCase, NoImplicitPrelude, OverloadedStrings, ScopedTypeVariables, ViewPatterns #-}

{- |

This module parses and evaluates a Nix-like language. I don't claim that it /is/ Nix, for two reasons:

1. Nix doesn't actually have a specification.
2. In the interest of laziness, I have only built out enough of it for my purpose at hand.

Notable differences from Nix:

- No built-in null, integer, or boolean types
- No @with@ keyword
- No @\@@ keyword
- All variables are bound; no @builtins@ and no infix operators (@+@, @-@, @//@)
- The concept of "set" is referred to as "dict"

-}
module ChrisMartinOrg.NixLike where

import Control.Applicative ((<|>), (<*), (*>), (<*>), pure)
import Control.Arrow ((>>>))
import Control.Monad ((>>=))
import Text.Parsec ((<?>))
import Text.Parsec.Text (Parser)
import Data.Bool (Bool (..), (&&), (||))
import Data.Char (Char)
import Data.Eq (Eq (..))
import Data.Foldable (Foldable, asum, foldMap, foldl)
import Data.Function (($), (.))
import Data.Functor (Functor (..), (<$>))
import Data.Maybe (Maybe (..))
import Data.Semigroup ((<>))
import Data.Text (Text)
import Prelude (undefined)

import qualified Text.Parsec as P
import qualified Data.Char as Char
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Text as Text

{- $setup

>>> import Prelude (($), putStrLn, putStr, print, IO, Either (..))

>>> :{
>>> parseTest :: Parser Text -> Text -> IO ()
>>> parseTest p input =
>>>   case P.parse p "" input of
>>>     Left err -> putStr "parse error at " *> print err
>>>     Right x -> putStrLn (Text.unpack x)
>>> :}

-}


--------------------------------------------------------------------------------
--  Identifiers
--------------------------------------------------------------------------------

-- | An identifier which /must/ be unquoted. For example, in a binding @x = y;@, the @x@ may be quoted, but the @y@ must be a bare identifier. The bare identifiers are a subset of the identifiers.
newtype BareId =
  BareId
    { bareIdText :: Text
    }

{- | An identifier can be /any/ string. In some cases this means we need to render it in quotes; see 'isUnquotableText'.

>>> test = putStrLn . Text.unpack . renderIdentifier

>>> test "abc"
abc

>>> test "a\"b"
"a\"b"

>>> test "-ab"
"-ab"

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
    StrExpr (Foldable.toList -> [StrLiteral x]) | isBareIdentifierName x -> x
    x -> renderStrExpr x

{- | Whether an identifier having this name can be rendered without quoting it. We allow a name to be a bare identifier, and thus to render unquoted, if all these conditions are met:

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

keywords :: [Text]
keywords = ["rec", "let", "in"]

{- | Letters, @-@, and @_@. -}
isBareIdentifierChar :: Char -> Bool
isBareIdentifierChar c =
  Char.isLetter c || c == '-' || c == '_'

{- |

>>> test = parseTest (bareIdText <$> bareIdP <* P.eof)

>>> test "-ab_c"
-ab_c

>>> test ""
failure

>>> test "a\"b"
failure

-}
bareIdP :: Parser BareId
bareIdP =
  fmap (BareId . Text.pack) (P.many1 (P.satisfy isBareIdentifierChar))

idExprP :: Parser StrExpr
idExprP =
  asum
    [ strExprP
    , fmap (strExpr . bareIdText) bareIdP
    ]


--------------------------------------------------------------------------------
--  String
--------------------------------------------------------------------------------

-- | A quoted string expression, which may be a simple string like @"hello"@ or a more complex string containing antiquotation like @"Hello, my name is ${name}!"@.
newtype StrExpr = StrExpr [StrExprPart]

data StrExprPart
  = StrLiteral Text
  | StrAntiquote Expression

{- |

>>> renderTest = putStrLn . Text.unpack . renderStrExpr . StrExpr

>>> renderTest []
""

>>> renderTest [ StrLiteral "hello" ]
"hello"

>>> renderTest [ StrLiteral "escape ${ this and \" this" ]
"escape \${ this and \" this"

>>> renderTest [ StrLiteral "Hello, my name is ", StrAntiquote (Expr'Id (BareId "name")), StrLiteral "!" ]
"Hello, my name is ${name}!"

-}
renderStrExpr :: StrExpr -> Text
renderStrExpr (StrExpr xs) =
    "\"" <> foldMap f xs <> "\""
  where
    f :: StrExprPart -> Text
    f = \case
      StrLiteral t -> strEscape t
      StrAntiquote e -> "${" <> renderExpression RenderContext'Normal e <> "}"

renderQuotedString :: Text -> Text
renderQuotedString x =
  "\"" <> strEscape x <> "\""

strEscape :: Text -> Text
strEscape =
  Text.replace "\"" "\\\"" .
  Text.replace "${" "\\${"

strExprP :: Parser StrExpr
strExprP = (strExprP'1 <|> strExprP'2) <?> "string"

strExprP'1 :: Parser StrExpr
strExprP'1 =
  do
    _ <- P.char '"'
    undefined

strExprP'2 :: Parser StrExpr
strExprP'2 =
  do
    _ <- P.string "''"
    undefined

-- | A simple string literal expression with no antiquotation.
strExpr :: Text -> StrExpr
strExpr =
  StrExpr . (\x -> [x]) . StrLiteral


--------------------------------------------------------------------------------
--  Function
--------------------------------------------------------------------------------

-- | A function expression.
data FuncExpr =
  FuncExpr
    { funcExpr'param :: Param -- ^ A declaration of the function's parameter
    , funcExpr'expression :: Expression -- ^ The body of the function; what it evaluates to
    }

-- | A function call expression.
data CallExpr =
  CallExpr
    { callExpr'function :: Expression -- ^ The function being called
    , calExpr'expression :: Expression -- ^ The argument to the function
    }

-- | The parameter to a function. All functions have a single parameter, but it's more complicated than that because it may also include dict destructuring.
data Param
  = Param'Id BareId -- ^ A simple single-parameter function
  | Param'Dict DictParam -- ^ Dict destructuring, which gives you something resembling multiple named parameters with default values

-- | A function parameter that does dict destructuring. See 'Param'.
data DictParam =
  DictParam
    { dictParam'items :: [DictParamItem] -- ^ The set of destructured identifiers, along with any default value each may have
    , dictParam'ellipsis :: Bool -- ^ Whether to allow additional keys beyond what is listed in the items, corresponding to the @...@ keyword
    }

data DictParamItem =
  DictParamItem
    { dictParamItem'variable :: Text -- ^ The bound variable
    , dictParamItem'default :: Maybe ParamDefault -- ^ The default value to be used if the key is not present in the dict
    }

-- | A default expression to use for a variable bound by a dict destructuring expression (see 'DictParamItem') if the key is not present in the dict.
newtype ParamDefault = ParamDefault Expression

renderParam :: Param -> Text
renderParam =
  \case
    Param'Id x -> renderBareId x <> ":"
    Param'Dict x -> renderDictParam x

{- |

>>> renderTest a b = putStrLn . Text.unpack . renderDictParam $ DictParam a b

>>> renderTest Map.empty False
{ }:

>>> renderTest Map.empty True
{ ... }:

>>> item1 = (Identifier "x", Nothing)
>>> item2 = (Identifier "y", Just . ParamDefault . Expr'Str. strExpr $ "abc")
>>> items = Map.fromList [ item1, item2 ]

>>> renderTest items False
{ x, y ? "abc" }:

>>> renderTest items True
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
      RenderContext'Normal -> False
      RenderContext'List   -> True
      RenderContext'Call1  -> True
      RenderContext'Call2  -> False
      RenderContext'Dot1   -> True

renderCallExpr :: RenderContext -> CallExpr -> Text
renderCallExpr cx (CallExpr a b) =
  if p then "(" <> x <> ")" else x

  where
    x = renderExpression RenderContext'Call1 a <> " " <>
        renderExpression RenderContext'Call2 b

    p = case cx of
      RenderContext'Normal -> False
      RenderContext'List   -> True
      RenderContext'Call1  -> False
      RenderContext'Call2  -> True
      RenderContext'Dot1   -> True

funcExprP :: Parser FuncExpr
funcExprP = undefined

callExprP :: Parser CallExpr
callExprP = undefined

paramP :: Parser Param
paramP = undefined

paramDefaultP :: Parser ParamDefault
paramDefaultP = undefined

dictParamP :: Parser DictParam
dictParamP = undefined

dictParamItemP :: Parser DictParamItem
dictParamItemP = undefined

applyArgs:: Expression -> [Expression] -> Expression
applyArgs =
  foldl (\acc b -> Expr'Call (CallExpr acc b))


--------------------------------------------------------------------------------
--  List
--------------------------------------------------------------------------------

-- | A list literal expression, starting with @[@ and ending with @]@.
data ListLiteral = ListLiteral [Expression]

{- |

>>> renderTest = putStrLn . Text.unpack . renderListLiteral . ListLiteral . Seq.fromList

>>> renderTest []
[ ]

>>> renderTest [ Expr'Id (BareId "true") ]
[ true ]

>>> renderTest [ Expr'Id (BareId "true"), Expr'Id (BareId "false") ]
[ true false ]

>>> call = Expr'Call (CallExpr (Expr'Id (BareId "f")) (Expr'Id (BareId "x")))

>>> renderTest [ call ]
[ (f x) ]

>>> renderTest [ call, Expr'Id (BareId "true") ]
[ (f x) true ]

-}
renderListLiteral :: ListLiteral -> Text
renderListLiteral =
  \case
    ListLiteral (Foldable.toList -> []) -> renderEmptyList
    ListLiteral (Foldable.toList -> values) -> "[ " <> foldMap (\v -> renderExpression RenderContext'List v <> " ") values <> "]"

renderEmptyList :: Text
renderEmptyList = "[ ]"

listLiteralP :: Parser ListLiteral
listLiteralP =
  p <?> "list"
  where
    p = do
      _ <- P.char '[' *> P.spaces
      x <- expressionListP
      _ <- P.spaces <* P.char ']'
      pure (ListLiteral x)


--------------------------------------------------------------------------------
--  Dict
--------------------------------------------------------------------------------

-- | A dict literal expression, starting with @{@ or @rec {@ and ending with @}@.
data DictLiteral =
  DictLiteral
    { dictLiteral'rec :: Bool -- ^ Whether the dict is recursive (denoted by the @rec@ keyword)
    , dictLiteral'bindings :: [Binding] -- ^ The bindings (everything between @{@ and @}@)
    }

-- | An expression of the form @person.name@ that looks up a key from a dict.
data Dot = Dot
  { dot'dict :: Expression
  , dot'key :: StrExpr
  }

renderDictLiteral :: DictLiteral -> Text
renderDictLiteral =
  \case
    DictLiteral _ [] -> renderEmptyDict
    DictLiteral True bs -> "rec { " <> renderBindingList bs <> " }"
    DictLiteral False bs -> "{ " <> renderBindingList bs <> " }"

renderEmptyDict :: Text
renderEmptyDict = "{ }"

renderDot :: Dot -> Text
renderDot (Dot a b) =
  renderExpression RenderContext'Dot1 a <> "." <> renderIdExpr b

dictLiteralP :: Parser DictLiteral
dictLiteralP =
  asum
    [ DictLiteral False <$> dictLiteralP'noRec
    , DictLiteral True <$> (P.string "rec" *> P.spaces *> dictLiteralP'noRec)
    ]

dictLiteralP'noRec :: Parser [Binding]
dictLiteralP'noRec =
  do
    _ <- P.char '{' *> P.spaces
    a <- P.sepBy1 bindingP P.spaces
    _ <- P.spaces *> P.char '}'
    pure a

{- | Parser for a chain of dict lookups (like @.a.b.c@).

>>> parseTest $ dotsP "" <* eof
[]

>>> parseTest $ dotsP ".a" <* eof
[IdExpr'BareId (BareId "a")]

>>> parseTest $ dotsP ".a . b" <* eof
[IdExpr'BareId (BareId "a"), IdExpr'BareId (BareId "b")]

-}
dotsP :: Parser [StrExpr]
dotsP =
  P.sepBy (P.char '.' *> P.spaces *> idExprP) P.spaces

applyDots :: Expression -> [StrExpr] -> Expression
applyDots =
  foldl (\acc b -> Expr'Dot (Dot acc b))


--------------------------------------------------------------------------------
--  Let
--------------------------------------------------------------------------------

-- | A @let@-@in@ expression.
data LetExpr =
  LetExpr
    { letExpr'bindings :: [Binding] -- ^ The bindings (everything between the @let@ and @in@ keywords)
    , letExpr'value :: Expression -- ^ The value (everything after the @in@ keyword)
    }

renderLetExpr :: LetExpr -> Text
renderLetExpr (LetExpr bs x) =
  if List.null bs
    then "let in " <> body
    else "let " <> renderBindingList bs <> " in " <> body
  where
    body = renderExpression RenderContext'Normal x

letExprP :: Parser LetExpr
letExprP = undefined


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
bindingP = undefined

bindingMapP :: Parser [Binding]
bindingMapP = undefined


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

{- | The primary, top-level expression parser. This is what you use to parse a @.nix@ file.

>>> :{
>>> test input =
>>>   case parse (expressionP <* P.eof) "" input of
>>>     Left err -> do putStr "parse error at "
>>>                    print err
>>>     Right x -> putStrLn . Text.unpack . renderExpression $ x
>>> :}

>>> test "[ true false ]"
[ true false ]

>>> test "f x"
f x

>>> test "[ true (f x) ]"
"[ true (f x) ]"

>>> [ 123 "abc" (f { x = y; }) ]
[ 123 "abc" (f { x = y; }) ]

>>> [ 123 "abc" f { x = y; } ]
[ 123 "abc" f { x = y; } ]

-}
expressionP :: Parser Expression
expressionP =
  asum
    [ fmap Expr'Func funcExprP
    , expressionListP >>= \case
        [] -> P.parserZero
        f : args -> pure $ applyArgs f args
    ]

{- | Parser for a list of expressions in a list literal (@[ x y z ]@) or in a chain of function arguments (@f x y z@).

>>> test = parseTest (Text.unlines . fmap (renderExpression RenderContext'Normal) <$> expressionListP <* P.eof)

>>> test ""

>>> test "x y z"
x
y
z

>>> test "(a)b c(d)"
a
b
c
d

>>> test "a.\"b\"c"
a.b
c

>>> test "123 ./foo.nix \"abc\" (f { x = y; })"
123
./foo.nix
"abc"
(f { x = y; })

-}
expressionListP :: Parser [Expression]
-- An item in an expression list can be surrounded by no parens (@a@), two parens (@(a)@), or one paren on the left (@(a).b@). Not just parens, but straight braces and curly braces.
expressionListP =
  P.sepBy expressionP'listItem P.spaces

{- | Parser for a single item within an expression list ('expressionListP'). This expression is not a function, a function application, or a let binding.

>>> test = parseTest (renderExpression RenderContext'Normal <$> expressionP'listItem)

>>> test "abc def"
abc

>>> test "a.b c"
a.b

>>> test "a.\"b\"c"
a.b

>>> test "(a.b)c"
a.b

>>> test "a.b(c)"
a.b

>>> test "[ a b ]c"
[ a b ]

>>> test "a[ b c ]"
a

>>> test "\"a\"b"
"a"

-}
expressionP'listItem :: Parser Expression
expressionP'listItem =
  applyDots <$> expressionP'listItem'noDot <*> dotsP

-- | Like 'expressionP'listItem', but with the further restriction that the expression may not be a dot.
expressionP'listItem'noDot :: Parser Expression
expressionP'listItem'noDot =
  asum
    [ fmap Expr'Str strExprP
    , fmap Expr'List listLiteralP
    , fmap Expr'Dict dictLiteralP
    , fmap Expr'Id bareIdP
    , expressionP'paren
    ]

-- | Parser for a parenthesized expression, from opening parenthesis to closing parenthesis.
expressionP'paren :: Parser Expression
expressionP'paren =
  P.char '(' *> P.spaces *> expressionP <* P.spaces <* P.char ')'


--------------------------------------------------------------------------------
--  RenderContext
--------------------------------------------------------------------------------

data RenderContext
  = RenderContext'Normal
  | RenderContext'List
  | RenderContext'Call1
  | RenderContext'Call2
  | RenderContext'Dot1
