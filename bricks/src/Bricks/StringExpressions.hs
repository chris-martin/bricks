{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications           #-}

{- |

There are three types of strings in the AST:

  - 'Str'Unquoted'
  - 'Str'Static'
  - 'Str'Dynamic'

= Why variables are strings

Perhaps counterintuitively, we include variables under the umbrella of "string".
This is because the language itself somewhat conflates the two ideas, and indeed
a casual Bricks user may not even always be aware of which is which.

Consider the following (quite contrived) examples:

> let x = { a = 1; }; in
> let inherit (x) a;  in
> { inherit a; }

> let x = { "a b" = 1; }; in
> let inherit (x) "a b";  in
> { inherit "a b"; }

In the first, @a@ seems quite like a variable; in the second, @"a b"@ feels much
like a string (because we had to quote it, as it contains a space). But the
ASTs for these two expressions are (apart from the name change) identical.

-}
module Bricks.StringExpressions
  (

  -- * Unquoted strings
    Str'Unquoted (..)
  , str'unquoted'text

  -- * Static strings
  , Str'Static (..)

  -- * Dynamic strings
  , Str'Dynamic (..)
  , Str'1 (..)
  , strDynamic'toList
  , strDynamic'fromList
  , strDynamic'singleton

  -- * Conversions between the different types of strings
  , str'dynamicToStatic
  , str'staticToDynamic
  , str'unquoted'to'static
  , str'unquoted'to'dynamic

  ) where

-- Bricks
import Bricks.UnquotedString

-- Bricks internal
import           Bricks.Internal.Prelude
import           Bricks.Internal.Seq            (Seq)
import qualified Bricks.Internal.Seq            as Seq
import           Bricks.Internal.ShowExpression
import           Bricks.Internal.Text           (Text)
import qualified Bricks.Internal.Text           as Text


--------------------------------------------------------------------------------
--  Unquoted
--------------------------------------------------------------------------------

data Str'Unquoted = Str'Unquoted UnquotedString

str'unquoted'text :: Str'Unquoted -> Text
str'unquoted'text (Str'Unquoted x) = unquotedString'text x

instance ShowExpression Str'Unquoted
  where
    showExpression = Text.pack . show @Text . str'unquoted'text

instance Show Str'Unquoted
  where
    showsPrec = showsPrec'showExpression


--------------------------------------------------------------------------------
--  Static
--------------------------------------------------------------------------------

{- | A fixed string value. We use the description "static" to mean the string
may not contain antiquotation, in contrast with 'Str'Dynamic' which can. -}
data Str'Static = Str'Static Text

instance Semigroup Str'Static
  where
    Str'Static x <> Str'Static y = Str'Static (x <> y)

instance Monoid Str'Static
  where
    mempty = Str'Static mempty
    mappend = (<>)

instance ShowExpression Str'Static
  where
    showExpression (Str'Static x) = Text.pack (show @Text x)

instance Show (Str'Static)
  where
    showsPrec = showsPrec'showExpression


--------------------------------------------------------------------------------
--  Dynamic
--------------------------------------------------------------------------------

{- | A quoted string expression, which may be a simple string like @"hello"@ or
a more complex string containing antiquotation like @"Hello, my name is
${name}!"@. See 'Expr'Str'.

We use the description "dynamic" to mean the string may contain antiquotation,
in contrast with 'Str'Static' which cannot. -}
data Str'Dynamic expr =
  Str'Dynamic
    { strDynamic'toSeq :: Seq (Str'1 expr)
    }

instance Semigroup (Str'Dynamic expr)
  where
    Str'Dynamic x <> Str'Dynamic y = Str'Dynamic (x <> y)

instance Monoid (Str'Dynamic expr)
  where
    mempty = Str'Dynamic mempty
    mappend = (<>)

-- | One part of a 'Str'Dynamic'.
data Str'1 expr
  = Str'1'Literal Str'Static
  | Str'1'Antiquote expr

instance ShowExpression expr => ShowExpression (Str'Dynamic expr)
  where
    showExpression x =
      Text.unwords ["str", showExpression'list (strDynamic'toList x)]

instance ShowExpression expr => ShowExpression (Str'1 expr)
  where
    showExpression = \case
      Str'1'Literal (Str'Static x) -> showExpression'quoted'text x
      Str'1'Antiquote x -> Text.unwords ["antiquote", showExpression'paren x]

instance ShowExpression expr => Show (Str'Dynamic expr)
  where
    showsPrec = showsPrec'showExpression

instance ShowExpression expr => Show (Str'1 expr)
  where
    showsPrec = showsPrec'showExpression

strDynamic'toList :: Str'Dynamic expr -> [Str'1 expr]
strDynamic'toList =
  Seq.toList . strDynamic'toSeq

strDynamic'fromList :: [Str'1 expr] -> Str'Dynamic expr
strDynamic'fromList =
  Str'Dynamic . Seq.fromList

strDynamic'singleton :: Str'1 expr -> Str'Dynamic expr
strDynamic'singleton =
  Str'Dynamic . Seq.singleton


--------------------------------------------------------------------------------
--  Conversions between the different types of strings
--------------------------------------------------------------------------------

str'dynamicToStatic :: Str'Dynamic expr -> Maybe Str'Static
str'dynamicToStatic = strDynamic'toList >>> \case
  [Str'1'Literal x] -> Just x
  _                 -> Nothing

str'staticToDynamic :: Str'Static -> Str'Dynamic expr
str'staticToDynamic =
  strDynamic'singleton . Str'1'Literal

str'unquoted'to'static :: Str'Unquoted -> Str'Static
str'unquoted'to'static =
  Str'Static . str'unquoted'text

str'unquoted'to'dynamic :: Str'Unquoted -> Str'Dynamic expr
str'unquoted'to'dynamic =
  str'staticToDynamic . str'unquoted'to'static
