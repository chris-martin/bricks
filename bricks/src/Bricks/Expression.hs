{-# LANGUAGE NoImplicitPrelude #-}

module Bricks.Expression
  (
  -- * Expressions
    Expression (..)

  -- * Strings
  , Str'Static
  , Str'Dynamic
  , Str'1 (..)

  -- * Lists
  , List

  -- * Dicts
  , Dict (..)
  , DictBinding (..)

  -- * Dict lookup
  , Dot (..)

  -- * Lambdas
  , Lambda (..)

  -- * Function parameters
  , Param (..)
  , DictPattern (..)
  , DictPattern'1 (..)

  -- * Function application
  , Apply (..)

  -- * @let@
  , Let (..)
  , LetBinding (..)

  -- * @with@
  , With (..)

  ) where

import Bricks.Bare

import Data.Bool       (Bool)
import Data.Maybe      (Maybe)
import Data.Text       (Text)

data Expression
  = Expr'Str    Str'Dynamic
  | Expr'List   List
  | Expr'Dict   Dict
  | Expr'Dot    Dot
  | Expr'Var    Bare
  | Expr'Lambda Lambda
  | Expr'Apply  Apply
  | Expr'Let    Let
  | Expr'With   With

{- | A fixed string value. We use the description "static" to mean the string
may not contain antiquotation, in contrast with 'Str'Dynamic' which can. -}
type Str'Static = Text

{- | A quoted string expression, which may be a simple string like @"hello"@ or
a more complex string containing antiquotation like @"Hello, my name is
${name}!"@.

We use the description "dynamic" to mean the string may contain antiquotation,
in contrast with 'Str'Static' which cannot. -}
type Str'Dynamic = [Str'1]

-- | One part of a 'Str'Dynamic'.
data Str'1
  = Str'1'Literal Str'Static
  | Str'1'Antiquote Expression

-- | A function expression.
data Lambda =
  Lambda
    { lambda'head :: Param
        -- ^ Declaration of the function's parameter
    , lambda'body :: Expression
        -- ^ Body of the function; what it evaluates to
    }

-- | A function application expression.
data Apply =
  Apply
    { apply'func :: Expression
        -- ^ The function being called
    , apply'arg :: Expression
        -- ^ The argument to the function
    }

{- | A parameter to a function. All functions have a single parameter, but it's
more complicated than that because it may also include dict destructuring. -}
data Param
  = Param'Bare Bare
      -- ^ A simple single-parameter function
  | Param'DictPattern DictPattern
      -- ^ Dict destructuring, which gives you something resembling multiple
      -- named parameters with default values

-- | A type of function parameter ('Param') that does dict destructuring.
data DictPattern =
  DictPattern
    { dictPattern'items :: [DictPattern'1]
        -- ^ The list of variables to pull out of the dict argument, along
        -- with any default value each may have
    , dictPattern'ellipsis :: Bool
        -- ^ Whether to allow additional keys beyond what is listed in the
        -- items, corresponding to the @...@ keyword
    }

-- | One item within a 'DictPattern'.
data DictPattern'1 =
  DictPattern'1
    { dictPattern'1'variable :: Bare
        -- ^ The bound variable
    , dictPattern'1'default :: Maybe Expression
        -- ^ The default value to be used if the key is not present in the dict
    }

-- | A list literal expression, starting with @[@ and ending with @]@.
type List = [Expression]

{- | A dict literal expression, starting with @{@ or @rec {@ and ending with
@}@. -}
data Dict =
  Dict
    { dict'rec :: Bool
        -- ^ Whether the dict is recursive (denoted by the @rec@ keyword)
    , dict'bindings :: [DictBinding]
        -- ^ The bindings (everything between @{@ and @}@)
    }

-- | A binding of the form @x = y;@ within a 'DictLiteral' or 'LetExpr'.
data DictBinding
  = DictBinding'Eq Str'Dynamic Expression
  | DictBinding'Inherit (Maybe Expression) [Str'Dynamic]

-- | An expression of the form @person.name@ that looks up a key from a dict.
data Dot = Dot
  { dot'dict :: Expression
  , dot'key  :: Str'Dynamic
  }

-- | A @let@-@in@ expression.
data Let =
  Let
    { let'bindings :: [LetBinding]
        -- ^ The bindings (everything between the @let@ and @in@ keywords)
    , let'value :: Expression
        -- ^ The value (everything after the @in@ keyword)
    }

{- | A semicolon-terminated binding within the binding list of a 'Let'
expression. -}
data LetBinding
  = LetBinding'Eq Str'Static Expression
      -- ^ A binding with an equals sign, of the form @x = y;@
  | LetBinding'Inherit (Maybe Expression) [Str'Static]
      -- ^ A binding using the @inherit@ keyword, of the form @inherit a b;@
      -- or @inherit (x) a b;@

-- | A @with@ expression.
data With =
  With
    { with'context :: Expression
    , with'value :: Expression
    }
