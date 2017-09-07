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

import Control.Applicative ((<|>), (<*), (*>), (<*>))
import Text.Parsec ((<?>))
import Text.Parsec.Text (Parser)
import Data.Bool (Bool (..), otherwise, (&&), (||))
import Data.Char (Char)
import Data.Eq (Eq (..))
import Data.Function ((.), const)
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
>>> renderTest = putStrLn . Text.unpack . render

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

>>> renderTest (Identifier "abc")
abc

>>> renderTest (Identifier "a\"b")
"a\"b"

>>> renderTest (Identifier "-ab")
"-ab"

>>> renderTest (Identifier "")
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
    IdExpr x -> render x

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

class ToExpr a
  where
    expr :: a -> Expression

instance ToExpr Bool        where expr = Expr'Bool
instance ToExpr StrExpr     where expr = Expr'Str
instance ToExpr DictLiteral where expr = Expr'Dict
instance ToExpr Dot         where expr = Expr'Dot
instance ToExpr ListLiteral where expr = Expr'List
instance ToExpr BareId      where expr = Expr'Id
instance ToExpr FuncExpr    where expr = Expr'Func
instance ToExpr CallExpr    where expr = Expr'Call
instance ToExpr LetExpr     where expr = Expr'Let

renderExpression :: RenderContext -> Expression -> Text
renderExpression c =
  \case
    Expr'Null   -> renderNull
    Expr'Bool x -> render' c x
    Expr'Str  x -> render' c x
    Expr'Dict x -> render' c x
    Expr'List x -> render' c x
    Expr'Id   x -> render' c x
    Expr'Dot  x -> render' c x
    Expr'Func x -> render' c x
    Expr'Call x -> render' c x
    Expr'Let  x -> render' c x

{- todo

>>> parseTest (expressionP <* P.eof) "null"
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
  (Expr'Null <$  nullP ) <|>
  (Expr'Bool <$> parser) <|>
  (Expr'List <$> parser) <|>
  (Expr'Str  <$> parser) <|>
  (Expr'Let  <$> parser) <|>
  undefined
  {-
  | Expr'Dict DictLiteral
  | Expr'Dot  Dot
  | Expr'Id   BareId
  | Expr'Func FuncExpr
  | Expr'Call CallExpr
  -}


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

>>> renderTest (StrExpr (Seq.empty))
""

>>> renderTest (StrExpr (Seq.singleton (StrLiteral "hello")))
"hello"

>>> renderTest (StrExpr (Seq.singleton (StrLiteral "escape ${ this and \" this")))
"escape \${ this and \" this"

>>> renderTest (StrExpr (Seq.fromList [ StrLiteral "Hello, my name is ", StrAntiquote (expr (BareId "name")), StrLiteral "!" ]))
"Hello, my name is ${name}!"

-}
renderStrExpr :: StrExpr -> Text
renderStrExpr (StrExpr xs) =
    "\"" <> Foldable.foldMap f xs <> "\""
  where
    f :: StrExprPart -> Text
    f = \case
      StrLiteral t -> strEscape t
      StrAntiquote e -> "${" <> render e <> "}"

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
  = Param'Id Identifier -- ^ A simple single-parameter function
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
    Param'Id x -> render x <> ":"
    Param'Dict x -> render x

{- |

>>> renderTest (DictParam Map.empty False)
{ }:

>>> renderTest (DictParam Map.empty True)
{ ... }:

>>> item1 = (Identifier "x", Nothing)
>>> item2 = (Identifier "y", Just . ParamDefault . str $ "abc")
>>> items = Map.fromList [ item1, item2 ]

>>> renderTest (DictParam items False)
{ x, y ? "abc" }:

>>> renderTest (DictParam items True)
{ x, y ? "abc", ... }:

-}
renderDictParam :: DictParam -> Text
renderDictParam (DictParam (fmap (uncurry DictParamItem) . Map.toList -> items) ellipsis) =
  case (items, ellipsis) of
    ([], False) -> "{ }:"
    ([], True)  -> "{ ... }:"
    (xs, False) -> "{ " <> Text.intercalate ", " (fmap render xs) <> " }:"
    (xs, True)  -> "{ " <> Text.intercalate ", " (fmap render xs) <> ", ... }:"

renderDictParamItem :: DictParamItem -> Text
renderDictParamItem =
  \case
    DictParamItem a Nothing -> render a
    DictParamItem a (Just b) -> render a <> " " <> render b

renderParamDefault :: ParamDefault -> Text
renderParamDefault =
  \case
    ParamDefault x -> "? " <> render x

renderFuncExpr :: RenderContext -> FuncExpr -> Text
renderFuncExpr (funcNeedsParens -> p) (FuncExpr a b) =
  let x = render a <> " " <> render b
  in  if p then "(" <> x <> ")" else x

funcNeedsParens :: RenderContext -> Bool
funcNeedsParens =
  \case
    RenderContext'List         -> True
    RenderContext'CallFunction -> True
    RenderContext'CallArgument -> True
    _                          -> False

renderCallExpr :: RenderContext -> CallExpr -> Text
renderCallExpr (callNeedsParens -> p) (CallExpr a b) =
  let x = render' RenderContext'CallFunction a <> " " <>
          render' RenderContext'CallArgument b
  in  if p then "(" <> x <> ")" else x

callNeedsParens :: RenderContext -> Bool
callNeedsParens =
  \case
    RenderContext'List         -> True
    RenderContext'CallArgument -> True
    _                          -> False

callExprP :: Parser CallExpr
callExprP = undefined

funcExprP :: Parser FuncExpr
funcExprP = undefined

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

>>> renderTest (ListLiteral Seq.empty)
[ ]

>>> renderTest (ListLiteral (Seq.singleton (expr True)))
[ true ]

>>> renderTest (ListLiteral (Seq.fromList [expr True, expr False]))
[ true false ]

>>> call = expr (CallExpr (expr (BareId "f")) (expr (BareId "x")))

>>> renderTest (ListLiteral (Seq.singleton call))
[ (f x) ]

>>> renderTest (ListLiteral (Seq.fromList [call, expr True]))
[ (f x) true ]

-}
renderListLiteral :: ListLiteral -> Text
renderListLiteral =
  \case
    ListLiteral (Foldable.toList -> []) -> renderEmptyList
    ListLiteral (Foldable.toList -> values) -> "[ " <> Foldable.foldMap (\v -> render' RenderContext'List v <> " ") values <> "]"

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
  | DictExpr'Lit    DictLiteral
  | DictExpr'Dot    Dot
  deriving Show

renderDictExpr :: DictExpr -> Text
renderDictExpr =
  \case
    DictExpr'BareId x -> render x
    DictExpr'Lit    x -> render x
    DictExpr'Dot    x -> render x

-- | An expression of the form @person.name@ that looks up a key from a dict.
data Dot = Dot
  { dot'dict :: DictExpr
  , dot'key  :: IdExpr
  } deriving Show

renderDictLiteral :: DictLiteral -> Text
renderDictLiteral =
  \case
    DictLiteral _ bs | noBindings bs -> renderEmptyDict
    DictLiteral True bs -> "rec { " <> render bs <> " }"
    DictLiteral False bs -> "{ " <> render bs <> " }"

renderEmptyDict :: Text
renderEmptyDict = "{ }"

renderDot :: Dot -> Text
renderDot (Dot a b) =
  render a <> "." <> render b

dictExprP :: Parser DictExpr
dictExprP = undefined

dictLiteralP :: Parser DictLiteral
dictLiteralP = undefined

dotP :: Parser Dot
dotP = undefined


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
  | noBindings bs = "let in " <> render x
  | otherwise = "let " <> render bs <> " in " <> render x

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
  render a <> " = " <> render b <> ";"

renderBindingMap :: BindingMap -> Text
renderBindingMap =
  \case
    (bindings -> []) -> ""
    (bindings -> bs) -> Text.intercalate " " (fmap render bs)

bindingP :: Parser Binding
bindingP = undefined

bindingMapP :: Parser BindingMap
bindingMapP = undefined


--------------------------------------------------------------------------------
--  Render
--------------------------------------------------------------------------------

data RenderContext
  = RenderContext'Normal
  | RenderContext'List
  | RenderContext'CallFunction
  | RenderContext'CallArgument
  deriving Show

class Render a
  where
    {-# MINIMAL render | render' #-}

    render :: a -> Text
    render = render' RenderContext'Normal

    render' :: RenderContext -> a -> Text
    render' = const render

instance Render BareId        where render  = renderBareId
instance Render Binding       where render  = renderBinding
instance Render BindingMap    where render  = renderBindingMap
instance Render Bool          where render  = renderBool
instance Render CallExpr      where render' = renderCallExpr
instance Render DictExpr      where render  = renderDictExpr
instance Render DictLiteral   where render  = renderDictLiteral
instance Render DictParam     where render  = renderDictParam
instance Render DictParamItem where render  = renderDictParamItem
instance Render Dot           where render  = renderDot
instance Render Expression    where render' = renderExpression
instance Render FuncExpr      where render' = renderFuncExpr
instance Render Identifier    where render  = renderIdentifier
instance Render IdExpr        where render  = renderIdExpr
instance Render LetExpr       where render  = renderLetExpr
instance Render ListLiteral   where render  = renderListLiteral
instance Render Param         where render  = renderParam
instance Render ParamDefault  where render  = renderParamDefault
instance Render StrExpr       where render  = renderStrExpr


--------------------------------------------------------------------------------
--  Parsers
--------------------------------------------------------------------------------

class HasP a
  where
    parser :: Parser a

instance HasP BareId        where parser = bareIdP
instance HasP Binding       where parser = bindingP
instance HasP BindingMap    where parser = bindingMapP
instance HasP Bool          where parser = boolP
instance HasP CallExpr      where parser = callExprP
instance HasP DictExpr      where parser = dictExprP
instance HasP DictLiteral   where parser = dictLiteralP
instance HasP DictParam     where parser = dictParamP
instance HasP DictParamItem where parser = dictParamItemP
instance HasP Dot           where parser = dotP
instance HasP Expression    where parser = expressionP
instance HasP FuncExpr      where parser = funcExprP
instance HasP Identifier    where parser = identifierP
instance HasP IdExpr        where parser = idExprP
instance HasP LetExpr       where parser = letExprP
instance HasP ListLiteral   where parser = listLiteralP
instance HasP Param         where parser = paramP
instance HasP ParamDefault  where parser = paramDefaultP
instance HasP StrExpr       where parser = strExprP
