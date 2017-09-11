{-# LANGUAGE NoImplicitPrelude #-}

module Bricks.Types
  (
  -- * Expressions
    Expression (..)

  -- * Identifiers
  , BareId (..)

  -- * Functions
  -- ** Function calls
  , CallExpr (..)
  -- ** Function expressions
  , FuncExpr (..)
  -- ** Function parameters
  , Param (..)
  , DictParam (..)
  , DictParamItem (..)
  , ParamDefault (..)

  -- * Keywords
  , Keyword (..)

  -- * Bindings
  , Binding (..)

  -- ** @let@
  , LetExpr (..)

  -- ** @with@
  , With (..)

  -- * Lists
  , ListLiteral (..)

  -- * Dicts
  , DictLiteral (..)
  , Dot (..)

  -- * Strings
  , StrExpr (..)
  , StrExprPart (..)

  -- ** Indented strings
  , IndentedString (..)
  , IndentedStringLine (..)

  ) where

import Data.Bool       (Bool)
import Data.Maybe      (Maybe)
import Data.Text       (Text)
import Numeric.Natural (Natural)

{- | An identifier which /must/ be unquoted. For example, in a binding @x = y;@,
the @x@ may be quoted, but the @y@ must be a bare identifier. The bare
identifiers are a subset of the identifiers. -}
newtype BareId =
  BareId
    { bareIdText :: Text
    }

{- | A quoted string expression, which may be a simple string like @"hello"@ or
a more complex string containing antiquotation like @"Hello, my name is
${name}!"@. -}
newtype StrExpr = StrExpr [StrExprPart]

data StrExprPart
  = StrExprPart'Literal Text
  | StrExprPart'Antiquote Expression

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

-- | A list literal expression, starting with @[@ and ending with @]@.
data ListLiteral = ListLiteral [Expression]

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

-- | A @let@-@in@ expression.
data LetExpr =
  LetExpr
    { letExpr'bindings :: [Binding]
        -- ^ The bindings (everything between the @let@ and @in@ keywords)
    , letExpr'value :: Expression
        -- ^ The value (everything after the @in@ keyword)
    }

data With =
  With
    { with'dict :: Expression
    , with'expr :: Expression
    }

-- | A binding of the form @x = y;@ within a 'DictLiteral' or 'LetExpr'.
data Binding = Binding StrExpr Expression

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

newtype Keyword = Keyword Text
