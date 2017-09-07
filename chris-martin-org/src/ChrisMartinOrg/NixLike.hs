{-# LANGUAGE LambdaCase, NoImplicitPrelude, OverloadedStrings, ViewPatterns #-}

{- |

This module parses and evaluates a Nix-like language. I don't claim that it /is/ Nix, for two reasons:

1. Nix doesn't actually have a specification.
2. In the interest of laziness, I have only built out enough of it for my purpose at hand.

Notable differences from Nix:

- No integer type
- No @with@ keyword
- No @\@@ keyword
- All variables are bound; no @builtins@ and no infix operators (@+@, @-@, @//@)
- The concept of "set" is referred to as "dict"

-}
module ChrisMartinOrg.NixLike where

import Control.Applicative ((<|>), (<*), (*>), (<*>), pure)
import Control.Monad ((>>=))
import Text.Parsec ((<?>))
import Text.Parsec.Text (Parser)
import Data.Bool (Bool (..), otherwise, (&&), (||))
import Data.Char (Char)
import Data.Eq (Eq (..))
import Data.Foldable (asum, foldMap)
import Data.Function (($), (.), const)
import Data.Functor (Functor (..), void, ($>), (<$>), (<$))
import Data.Map (Map)
import Data.Maybe (Maybe (..))
import Data.Ord (Ord (..))
import Data.Semigroup ((<>))
import Data.Sequence (Seq)
import Data.Set (Set)
import Data.String (IsString)
import Data.Text (Text)
import Data.Tuple (uncurry)
import Prelude (undefined)
import Text.Show (Show)

import qualified Text.Parsec as P
import qualified Data.Char as Char
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as Text

{- $setup

>>> import Prelude (($), putStrLn)
>>> import Text.Parsec (parseTest)

-}


--------------------------------------------------------------------------------
--  Identifiers
--------------------------------------------------------------------------------

-- | An identifier which /must/ be unquoted. For example, in a binding @x = y;@, the @x@ may be quoted, but the @y@ must be a bare identifier. The bare identifiers are a subset of the identifiers.
newtype BareId = BareId Text
  deriving Show

-- | The general type representing the value of an identifier. An identifier can be /any/ string, including strings which cannot grammatically appear in code without being quoted. This makes them unsuitable for contexts in which string literals are permissible, since it would be ambiguous whether the quoted expression is an identifier or a string; for those contexts we use 'BareId' instead.
newtype Identifier = Identifier Text
  deriving (Eq, Ord, Show)

-- | An expression which evaluates to an 'Identifier'. This more general form of an identifier is permitted on the left-hand side of a 'Binding' (in the expression @x = y;@, @x@ may be an 'IdExpr'), and on the right-hand side of a 'Dot' (in the expression @x.y@, @y@ may be an 'IdExpr').
newtype IdExpr = IdExpr StrExpr
  deriving Show

{- | An identifier can be /any/ string. In some cases this means we need to render it in quotes; see 'isUnquotableText'.

>>> renderTest = putStrLn . Text.unpack . renderIdentifier . Identifier

>>> renderTest "abc"
abc

>>> renderTest "a\"b"
"a\"b"

>>> renderTest "-ab"
"-ab"

>>> renderTest ""
""

-}
renderIdentifier :: Identifier -> Text
renderIdentifier (Identifier x) =
  if isBareIdentifierName x then x else renderQuotedString x

renderBareId :: BareId -> Text
renderBareId (BareId x) = x

renderIdExpr :: IdExpr -> Text
renderIdExpr =
  \case
    IdExpr (StrExpr (Foldable.toList -> [StrLiteral x])) | isBareIdentifierName x -> x
    IdExpr x -> renderStrExpr x

{- | Whether an identifier having this name can be rendered without quoting it. We allow a name to be a bare identifier, and thus to render unquoted, if both of the following are true:

1. It starts with an ASCII letter or @_@ (safeIdChar1).
2. It contains only ASCII letters, numbers, @-@, and @_@ ('safeIdCharN').

>>> isBareIdentifierName "abc"
True

>>> isBareIdentifierName "_abc"
True

>>> isBareIdentifierName "a\"b"
False

>>> isBareIdentifierName "-ab"
False

>>> isBareIdentifierName ""
False

-}
isBareIdentifierName :: Text -> Bool
isBareIdentifierName (Text.uncons -> Nothing) = False
isBareIdentifierName (Text.uncons -> Just (x, xs)) =
  bareIdChar1 x && Text.all bareIdCharN xs

bareIdChar1 :: Char -> Bool
bareIdChar1 c =
  isAsciiLetter c || c == '_'

-- | Whether the character is an ASCII letters, number, @-@, or @_@.
bareIdCharN :: Char -> Bool
bareIdCharN c =
  isAsciiLetter c || c == '-' || c == '_'

-- | Whether a character matches @[a-zA-Z]@.
isAsciiLetter :: Char -> Bool
isAsciiLetter c =
  Char.isAsciiUpper c || Char.isAsciiLower c

bareIdP :: Parser BareId
bareIdP = undefined

idExprP :: Parser IdExpr
idExprP = undefined

identifierP :: Parser Identifier
identifierP = undefined


--------------------------------------------------------------------------------
--  Bool
--------------------------------------------------------------------------------

renderTrue :: IsString s => s
renderTrue = "true"

renderFalse :: IsString s => s
renderFalse = "false"

renderBool :: Bool -> Text
renderBool =
  \case
    True  -> renderTrue
    False -> renderFalse

{- |

>>> parseTest (boolP <* P.eof) "true"
True

>>> parseTest (boolP <* P.eof) "false"
False

>>> parseTest (boolP <* P.eof) "abc"
parse error at (line 1, column 1):
unexpected "a"
expecting bool

>>> parseTest (boolP <* P.eof) "trueq"
parse error at (line 1, column 5):
unexpected 'q'
expecting end of input

-}
boolP :: Parser Bool
boolP = ((trueP $> True) <|> (falseP $> False)) <?> "bool"

trueP :: Parser ()
trueP = void (P.string renderTrue) <?> "true"

falseP :: Parser ()
falseP = void (P.string renderFalse) <?> "false"


--------------------------------------------------------------------------------
--  Null
--------------------------------------------------------------------------------

renderNull :: IsString s => s
renderNull = "null"

{- |

>>> parseTest (nullP <* P.eof) "null"
()

>>> parseTest (nullP <* P.eof) "abc"
parse error at (line 1, column 1):
unexpected "a"
expecting null

-}
nullP :: Parser ()
nullP = void (P.string renderNull) <?> "null"


--------------------------------------------------------------------------------
--  Expression
--------------------------------------------------------------------------------

data Expression
  = Expr'Null
  | Expr'Bool Bool
  | Expr'Str  StrExpr
  | Expr'List ListLiteral
  | Expr'Dict DictLiteral
  | Expr'Dot  Dot
  | Expr'Id   BareId
  | Expr'Func FuncExpr
  | Expr'Call CallExpr
  | Expr'Let  LetExpr
  deriving Show

renderExpression :: Context -> Expression -> Text
renderExpression c =
  \case
    Expr'Null   -> renderNull
    Expr'Bool x -> renderBool x
    Expr'Str  x -> renderStrExpr x
    Expr'Dict x -> renderDictLiteral x
    Expr'List x -> renderListLiteral x
    Expr'Id   x -> renderBareId x
    Expr'Dot  x -> renderDot x
    Expr'Func x -> renderFuncExpr c x
    Expr'Call x -> renderCallExpr c x
    Expr'Let  x -> renderLetExpr x

{- todo

>>> parseTest (expressionP <* P.eof) "null"
Expr'Null

>>> parseTest (expressionP <* P.eof) "(null)"
Expr'Null

>>> parseTest (expressionP <* P.eof) "true"
Expr'Bool True

>>> parseTest (expressionP <* P.eof) "false"
Expr'Bool False

>>> parseTest (expressionP <* P.eof) "[ true false ]"
Expr'List (ListLiteral (fromList [Expr'Bool True, Expr'Bool False]))

>>> parseTest (expressionP <* P.eof) "f x"
Expr'Call (CallExpr (?) (?))

>>> parseTest (expressionP <* P.eof) "[ true (f x) ]"
Expr'List (ListLiteral (fromList [Expr'Bool True, Expr'Call (CallExpr (?) (?))]))

-}
expressionP :: Parser Expression
expressionP =
  optionalParens $ asum
    [ Expr'Null <$ nullP
    , Expr'Bool <$> boolP
    , Expr'List <$> listLiteralP
    , Expr'Str <$> strExprP
    , Expr'Let <$> letExprP
    , expressionP'fromBareId
    , expressionP'fromDictLiteral
    ]

expressionP'fromBareId :: Parser Expression
expressionP'fromBareId =
  bareIdP >>= \a -> asum
    [ pure (Expr'Id a)
    , Expr'Func <$> funcExprP' (Param'Id a)
    , Expr'Call <$> callExprP' (Expr'Id a)
    , Expr'Dot <$> dotP' (DictExpr'BareId a)
    ]

expressionP'fromDictLiteral =
  dictLiteralP >>= \a -> asum
    [ pure (Expr'Dict a)
    , Expr'Dot <$> dotP' (DictExpr'Lit a)
    ]

  {-
  | Expr'Dict DictLiteral
  | Expr'Dot  Dot
  | Expr'Id   BareId
  | Expr'Func FuncExpr
  | Expr'Call CallExpr
  -}

  -- Dot: can turn into a Dot
  -- Func: can turn into Call

optionalParens :: Parser a -> Parser a
optionalParens p =
    withParens <|> p
  where
    withParens =
      P.char '(' *> P.spaces *> optionalParens p <* P.spaces <* P.char ')'


--------------------------------------------------------------------------------
--  String
--------------------------------------------------------------------------------

-- | A quoted string expression, which may be a simple string like @"hello"@ or a more complex string containing antiquotation like @"Hello, my name is ${name}!"@.
newtype StrExpr = StrExpr (Seq StrExprPart)
  deriving Show

data StrExprPart
  = StrLiteral Text
  | StrAntiquote Expression
  deriving Show

class SimpleStr a
  where
    -- | A string literal with no antiquotation.
    str :: Text -> a

instance SimpleStr StrExpr    where str = StrExpr . Seq.singleton . StrLiteral
instance SimpleStr Expression where str = Expr'Str . StrExpr . Seq.singleton . StrLiteral

{- |

>>> renderTest = putStrLn . Text.unpack . renderStrExpr . StrExpr . Seq.fromList

>>> renderTest []
""

>>> renderTest [ StrLiteral "hello" ]
"hello"

>>> renderTest [ StrLiteral "escape ${ this and \" this" ]
"escape \${ this and \" this"

>>> renderTest [ StrLiteral "Hello, my name is ", StrAntiquote (expr (BareId "name")), StrLiteral "!" ]
"Hello, my name is ${name}!"

-}
renderStrExpr :: StrExpr -> Text
renderStrExpr (StrExpr xs) =
    "\"" <> foldMap f xs <> "\""
  where
    f :: StrExprPart -> Text
    f = \case
      StrLiteral t -> strEscape t
      StrAntiquote e -> "${" <> renderExpression Context'Normal e <> "}"

renderQuotedString :: Text -> Text
renderQuotedString x =
  "\"" <> strEscape x <> "\""

strEscape :: Text -> Text
strEscape =
  Text.replace "\"" "\\\"" .
  Text.replace "${" "\\${"

strExprP :: Parser StrExpr
strExprP = undefined


--------------------------------------------------------------------------------
--  Function
--------------------------------------------------------------------------------

-- | A function expression.
data FuncExpr =
  FuncExpr
    { funcExpr'param :: Param -- ^ A declaration of the function's parameter
    , funcExpr'expression :: Expression -- ^ The body of the function; what it evaluates to
    }
  deriving Show

-- | A function call expression.
data CallExpr =
  CallExpr
    { callExpr'function :: Expression -- ^ The function being called
    , calExpr'expression :: Expression -- ^ The argument to the function
    }
  deriving Show

-- | The parameter to a function. All functions have a single parameter, but it's more complicated than that because it may also include dict destructuring.
data Param
  = Param'Id BareId -- ^ A simple single-parameter function
  | Param'Dict DictParam -- ^ Dict destructuring, which gives you something resembling multiple named parameters with default values
  deriving Show

-- | A function parameter that does dict destructuring. See 'Param'.
data DictParam =
  DictParam
    { dictParam'items :: Map Identifier (Maybe ParamDefault) -- ^ The set of destructured identifiers, along with any default value each may have
    , dictParam'ellipsis :: Bool -- ^ Whether to allow additional keys beyond what is listed in the items, corresponding to the @...@ keyword
    }
  deriving Show

data DictParamItem =
  DictParamItem
    { dictParamItem'variable :: Identifier -- ^ The bound variable
    , dictParamItem'default :: Maybe ParamDefault -- ^ The default value to be used if the key is not present in the dict
    }
  deriving Show

-- | A default expression to use for a variable bound by a dict destructuring expression (see 'DictParamItem') if the key is not present in the dict.
newtype ParamDefault = ParamDefault Expression
  deriving Show

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
>>> item2 = (Identifier "y", Just . ParamDefault . str $ "abc")
>>> items = Map.fromList [ item1, item2 ]

>>> renderTest items False
{ x, y ? "abc" }:

>>> renderTest items True
{ x, y ? "abc", ... }:

-}
renderDictParam :: DictParam -> Text
renderDictParam (DictParam (fmap (uncurry DictParamItem) . Map.toList -> items) ellipsis) =
  case (items, ellipsis) of
    ([], False) -> "{ }:"
    ([], True)  -> "{ ... }:"
    (xs, False) -> "{ " <> Text.intercalate ", " (fmap renderDictParamItem xs) <> " }:"
    (xs, True)  -> "{ " <> Text.intercalate ", " (fmap renderDictParamItem xs) <> ", ... }:"

renderDictParamItem :: DictParamItem -> Text
renderDictParamItem =
  \case
    DictParamItem a Nothing -> renderIdentifier a
    DictParamItem a (Just b) -> renderIdentifier a <> " " <> renderParamDefault b

renderParamDefault :: ParamDefault -> Text
renderParamDefault =
  \case
    ParamDefault x -> "? " <> renderExpression Context'Normal x

renderFuncExpr :: Context -> FuncExpr -> Text
renderFuncExpr (funcNeedsParens -> p) (FuncExpr a b) =
  let x = renderParam a <> " " <> renderExpression Context'Normal b
  in  if p then "(" <> x <> ")" else x

funcNeedsParens :: Context -> Bool
funcNeedsParens =
  \case
    Context'List         -> True
    Context'CallFunction -> True
    Context'CallArgument -> True
    _                    -> False

renderCallExpr :: Context -> CallExpr -> Text
renderCallExpr (callNeedsParens -> p) (CallExpr a b) =
  let x = renderExpression Context'CallFunction a <> " " <>
          renderExpression Context'CallArgument b
  in  if p then "(" <> x <> ")" else x

callNeedsParens :: Context -> Bool
callNeedsParens =
  \case
    Context'List         -> True
    Context'CallArgument -> True
    _                    -> False

callExprP :: Parser CallExpr
callExprP = undefined

-- | Parser for the second half of a call expression, starting after the function.
callExprP' :: Expression -> Parser CallExpr
callExprP' a = undefined

funcExprP :: Parser FuncExpr
funcExprP = undefined

-- | Parser for the second half of a function expression, starting after the parameter.
funcExprP' :: Param -> Parser FuncExpr
funcExprP' a =
  FuncExpr a <$> (P.spaces *> P.char ':' *> P.spaces *> expressionP)

paramP :: Parser Param
paramP = undefined

paramDefaultP :: Parser ParamDefault
paramDefaultP = undefined

dictParamP :: Parser DictParam
dictParamP = undefined

dictParamItemP :: Parser DictParamItem
dictParamItemP = undefined


--------------------------------------------------------------------------------
--  List
--------------------------------------------------------------------------------

-- | A list literal expression, starting with @[@ and ending with @]@.
data ListLiteral = ListLiteral (Seq Expression)
  deriving Show

{- |

>>> renderTest = putStrLn . Text.unpack . renderListLiteral . Seq.fromList

>>> renderTest []
[ ]

>>> renderTest [ Expr'Bool True ]
[ true ]

>>> renderTest [ Expr'Bool True, Expr'Bool False ]
[ true false ]

>>> call = Expr'Call (CallExpr (Expr'Id (BareId "f")) (Expr'Id (BareId "x")))

>>> renderTest [ call ]
[ (f x) ]

>>> renderTest [ call, Expr'Bool True ]
[ (f x) true ]

-}
renderListLiteral :: ListLiteral -> Text
renderListLiteral =
  \case
    ListLiteral (Foldable.toList -> []) -> renderEmptyList
    ListLiteral (Foldable.toList -> values) -> "[ " <> foldMap (\v -> renderExpression Context'List v <> " ") values <> "]"

renderEmptyList :: Text
renderEmptyList = "[ ]"

listLiteralP :: Parser ListLiteral
listLiteralP = undefined


--------------------------------------------------------------------------------
--  Dict
--------------------------------------------------------------------------------

-- | A dict literal expression, starting with @{@ or @rec {@ and ending with @}@.
data DictLiteral =
  DictLiteral
    { dictLiteral'rec :: Bool -- ^ Whether the dict is recursive (denoted by the @rec@ keyword)
    , dictLiteral'bindings :: BindingMap -- ^ The bindings (everything between @{@ and @}@)
    }
  deriving Show

-- | An expression that can go on the left-hand side of a 'Dot' and is supposed to reduce to a dict.
data DictExpr
  = DictExpr'BareId BareId
  | DictExpr'Lit DictLiteral
  | DictExpr'Dot Dot
  deriving Show

renderDictExpr :: DictExpr -> Text
renderDictExpr =
  \case
    DictExpr'BareId x -> renderBareId x
    DictExpr'Lit x -> renderDictLiteral x
    DictExpr'Dot x -> renderDot x

-- | An expression of the form @person.name@ that looks up a key from a dict.
data Dot = Dot
  { dot'dict :: DictExpr
  , dot'key  :: IdExpr
  } deriving Show

renderDictLiteral :: DictLiteral -> Text
renderDictLiteral =
  \case
    DictLiteral _ bs | noBindings bs -> renderEmptyDict
    DictLiteral True bs -> "rec { " <> renderBindingMap bs <> " }"
    DictLiteral False bs -> "{ " <> renderBindingMap bs <> " }"

renderEmptyDict :: Text
renderEmptyDict = "{ }"

renderDot :: Dot -> Text
renderDot (Dot a b) =
  renderDictExpr a <> "." <> renderIdExpr b

dictExprP :: Parser DictExpr
dictExprP = undefined

dictLiteralP :: Parser DictLiteral
dictLiteralP = undefined

dotP :: Parser Dot
dotP = undefined

-- | Parser for the second half of a dot expression, including the dot.
dotP' :: DictExpr -> Parser Dot
dotP' = undefined


--------------------------------------------------------------------------------
--  Let
--------------------------------------------------------------------------------

-- | A @let@-@in@ expression.
data LetExpr =
  LetExpr
    { letExpr'bindings :: BindingMap -- ^ The bindings (everything between the @let@ and @in@ keywords)
    , letExpr'value :: Expression -- ^ The value (everything after the @in@ keyword)
    }
  deriving Show

renderLetExpr :: LetExpr -> Text
renderLetExpr (LetExpr bs x)
  | noBindings bs = "let in " <> body
  | otherwise = "let " <> renderBindingMap bs <> " in " <> body
  where body = renderExpression Context'Normal x

letExprP :: Parser LetExpr
letExprP = undefined


--------------------------------------------------------------------------------
--  Binding
--------------------------------------------------------------------------------

-- | A binding of the form @x = y;@ within a 'DictLiteral' or 'LetExpr'.
data Binding = Binding IdExpr Expression
  deriving Show

-- | You'll often want to consume this by using 'bindings' to convert it to @['Binding']@.
newtype BindingMap = BindingMap (Map IdExpr Expression)
  deriving Show

bindings :: BindingMap -> [Binding]
bindings (BindingMap x) =
  fmap (\(a, b) -> Binding a b) (Map.toList x)

noBindings :: BindingMap -> Bool
noBindings (BindingMap m) =
  Map.null m

renderBinding :: Binding -> Text
renderBinding (Binding a b) =
  renderIdExpr a <> " = " <> renderExpression Context'Normal b <> ";"

renderBindingMap :: BindingMap -> Text
renderBindingMap =
  \case
    (bindings -> []) -> ""
    (bindings -> bs) -> Text.intercalate " " (fmap renderBinding bs)

bindingP :: Parser Binding
bindingP = undefined

bindingMapP :: Parser BindingMap
bindingMapP = undefined


--------------------------------------------------------------------------------
--  Context
--------------------------------------------------------------------------------

data Context
  = Context'Normal
  | Context'List
  | Context'CallFunction
  | Context'CallArgument
  deriving Show
