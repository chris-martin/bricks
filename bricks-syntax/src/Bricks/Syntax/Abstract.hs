{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}

module Bricks.Syntax.Abstract
  (
  -- * Expression
    Expr (..)

  -- * Variable
  , Var (..)

  -- * String
  , Str (..)

  -- * Lambda
  , Lambda (..)

  -- * Pattern
  , Pattern (..), Ellipsis (..), DictPattern (..), DictPatternItem (..)

  -- * Dict
  , Rec (..), Dict (..), DictItem (..)

  -- * Let
  , Let (..), LetItem (..)

  -- * Binary combinations of expressions
  , BinaryCombinator (..), Binary (..), binary'chain

  -- * Lists of expressions
  , ListCombinator (..)

  -- * Optionally-attached source information
  , Src, SrcUnwrap (..), SrcJoin (..), NoSource, WithSource, At (..)
  , at'unwrap, at'source

  -- * Discarding source information
  , expr'noSource, lambda'noSource, binary'noSource, pattern'noSource
  , dictPattern'noSource, dictPatternItem'noSource, dict'noSource
  , dictItem'noSource, let'noSource, letItem'noSource

  ) where

import Bricks.Syntax.Source

-- bricks-internal
import Bricks.Internal.List    as List
import Bricks.Internal.Prelude
import Bricks.Internal.Seq     (Seq)
import Bricks.Internal.Text    (Text)

-- base
import Data.String (IsString)


--------------------------------------------------------------------------------
--  Expr
--------------------------------------------------------------------------------

data Expr s
  = Expr'Var Var
  | Expr'Str Str
  | Expr'Binary BinaryCombinator (Binary s)
  | Expr'List ListCombinator (Seq (Src s (Expr s)))
  | Expr'Lambda (Lambda s)
  | Expr'Let (Let s)
  | Expr'Dict (Dict s)

deriving instance Eq (Expr NoSource)
deriving instance Eq src => Eq (Expr (WithSource src))
deriving instance Show (Expr NoSource)
deriving instance Show src => Show (Expr (WithSource src))

expr'noSource :: forall s. SrcUnwrap s => Expr s -> Expr NoSource
expr'noSource =
  \case
    Expr'Var      x -> Expr'Var x
    Expr'Str      x -> Expr'Str x
    Expr'Binary c x -> Expr'Binary c $ binary'noSource x
    Expr'List   c x -> Expr'List c $ fmap (expr'noSource @s . src'unwrap @s) x
    Expr'Lambda   x -> Expr'Lambda $ lambda'noSource x
    Expr'Let      x -> Expr'Let $ let'noSource x
    Expr'Dict     x -> Expr'Dict $ dict'noSource x


--------------------------------------------------------------------------------
--  Var
--------------------------------------------------------------------------------

newtype Var = Var Text
  deriving (Eq, IsString)

instance Show Var where
  showsPrec i (Var x) = showsPrec i x


--------------------------------------------------------------------------------
--  Str
--------------------------------------------------------------------------------

newtype Str = Str Text
  deriving (Eq, Semigroup, Monoid, IsString)

instance Show Str where
  showsPrec i (Str x) = showsPrec i x


--------------------------------------------------------------------------------
--  Lambda
--------------------------------------------------------------------------------

data Lambda s =
  Lambda
    { lambda'head :: Src s (Pattern s)
    , lambda'body :: Src s (Expr s)
    }

deriving instance Eq (Lambda NoSource)
deriving instance Eq src => Eq (Lambda (WithSource src))
deriving instance Show (Lambda NoSource)
deriving instance Show src => Show (Lambda (WithSource src))

lambda'noSource :: forall s. SrcUnwrap s => Lambda s -> Lambda NoSource
lambda'noSource x =
  Lambda
    { lambda'head = pattern'noSource @s . src'unwrap @s $ lambda'head x
    , lambda'body = expr'noSource @s . src'unwrap @s $ lambda'body x
    }


--------------------------------------------------------------------------------
--  Binary
--------------------------------------------------------------------------------

data BinaryCombinator = Apply | Lookup
  deriving (Eq, Show)

data Binary s =
  Binary
    { binary'1 :: Src s (Expr s)
    , binary'2 :: Src s (Expr s)
    }

deriving instance Eq (Binary NoSource)
deriving instance Eq src => Eq (Binary (WithSource src))
deriving instance Show (Binary NoSource)
deriving instance Show src => Show (Binary (WithSource src))

binary'noSource :: forall s. SrcUnwrap s => Binary s -> Binary NoSource
binary'noSource x =
  Binary
    { binary'1 = expr'noSource @s . src'unwrap @s $ binary'1 x
    , binary'2 = expr'noSource @s . src'unwrap @s $ binary'2 x
    }

binary'chain :: forall s. SrcJoin s => BinaryCombinator -> Src s (Expr s) -> Seq (Src s (Expr s)) -> Src s (Expr s)
binary'chain c = foldl $ src'join @s @(Expr s) @(Expr s) @(Expr s) f
  where
    f x y = Expr'Binary c (Binary x y)


--------------------------------------------------------------------------------
--  Pattern
--------------------------------------------------------------------------------

data Pattern s
  = Pattern'Var (Src s Var)
  | Pattern'Dict (Src s (DictPattern s))
  | Pattern'Both (Src s Var) (Src s (DictPattern s))

pattern'noSource :: forall s. SrcUnwrap s => Pattern s -> Pattern NoSource
pattern'noSource =
  \case
    Pattern'Var x -> Pattern'Var (src'unwrap @s x)
    Pattern'Dict y -> Pattern'Dict (dictPattern'noSource (src'unwrap @s @(DictPattern s) y))
    Pattern'Both x y -> Pattern'Both (src'unwrap @s x) (dictPattern'noSource (src'unwrap @s @(DictPattern s) y))

deriving instance Eq (Pattern NoSource)
deriving instance Eq src => Eq (Pattern (WithSource src))
deriving instance Show (Pattern NoSource)
deriving instance Show src => Show (Pattern (WithSource src))


--------------------------------------------------------------------------------
--  Ellipsis
--------------------------------------------------------------------------------

newtype Ellipsis = Ellipsis Bool
  deriving (Eq, Show)


--------------------------------------------------------------------------------
--  DictPattern
--------------------------------------------------------------------------------

data DictPattern s =
  DictPattern
    { dictPattern'items :: Src s (Seq (Src s (DictPatternItem s)))
    , dictPattern'ellipsis :: Ellipsis
    }

dictPattern'noSource :: forall s. SrcUnwrap s => DictPattern s -> DictPattern NoSource
dictPattern'noSource x =
  DictPattern
    { dictPattern'items = fmap (dictPatternItem'noSource @s . src'unwrap @s) . src'unwrap @s $ dictPattern'items x
    , dictPattern'ellipsis = dictPattern'ellipsis x
    }

deriving instance Eq (DictPattern NoSource)
deriving instance Eq src => Eq (DictPattern (WithSource src))
deriving instance Show (DictPattern NoSource)
deriving instance Show src => Show (DictPattern (WithSource src))


--------------------------------------------------------------------------------
--  DictPatternItem
--------------------------------------------------------------------------------

data DictPatternItem s =
  DictPatternItem
    { dictPatternItem'var :: Src s Var
    , dictPatternItem'default :: Maybe (Src s (Expr s))
    }

dictPatternItem'noSource :: forall s. SrcUnwrap s => DictPatternItem s -> DictPatternItem NoSource
dictPatternItem'noSource x =
  DictPatternItem
    { dictPatternItem'var = src'unwrap @s $ dictPatternItem'var x
    , dictPatternItem'default = fmap (expr'noSource @s . src'unwrap @s) $ dictPatternItem'default x
    }

deriving instance Eq (DictPatternItem NoSource)
deriving instance Eq src => Eq (DictPatternItem (WithSource src))
deriving instance Show (DictPatternItem NoSource)
deriving instance Show src => Show (DictPatternItem (WithSource src))


--------------------------------------------------------------------------------
--  Rec
--------------------------------------------------------------------------------

newtype Rec = Rec Bool
  deriving (Eq, Show)


--------------------------------------------------------------------------------
--  Dict
--------------------------------------------------------------------------------

data Dict s =
  Dict
    { dict'rec :: Rec
    , dict'items :: Src s (Seq (Src s (DictItem s)))
    }

deriving instance Eq (Dict NoSource)
deriving instance Eq src => Eq (Dict (WithSource src))
deriving instance Show (Dict NoSource)
deriving instance Show src => Show (Dict (WithSource src))

dict'noSource :: forall s. SrcUnwrap s => Dict s -> Dict NoSource
dict'noSource x =
  Dict
    { dict'rec = dict'rec x
    , dict'items = fmap (dictItem'noSource @s . src'unwrap @s) $ src'unwrap @s $ dict'items x
    }


--------------------------------------------------------------------------------
--  DictItem
--------------------------------------------------------------------------------

data DictItem s
  = DictItem'Eq (Src s (Expr s)) (Src s (Expr s))
  | DictItem'Inherit'Dict (Src s (Expr s)) (Seq (Src s Str))
  | DictItem'Inherit'Var (Seq (Src s Var))

deriving instance Eq (DictItem NoSource)
deriving instance Eq src => Eq (DictItem (WithSource src))
deriving instance Show (DictItem NoSource)
deriving instance Show src => Show (DictItem (WithSource src))

dictItem'noSource :: forall s. SrcUnwrap s => DictItem s -> DictItem NoSource
dictItem'noSource =
  \case
    DictItem'Eq a b -> DictItem'Eq (expr'noSource @s . src'unwrap @s $ a) (expr'noSource @s . src'unwrap @s $ b)
    DictItem'Inherit'Dict a xs -> DictItem'Inherit'Dict (expr'noSource @s . src'unwrap @s $ a) (fmap (src'unwrap @s @Str) xs)
    DictItem'Inherit'Var xs -> DictItem'Inherit'Var (fmap (src'unwrap @s @Var) xs)


--------------------------------------------------------------------------------
--  Let
--------------------------------------------------------------------------------

data Let s =
  Let
    { let'items :: Seq (Src s (LetItem s))
    , let'body :: Src s (Expr s)
    }

deriving instance Eq (Let NoSource)
deriving instance Eq src => Eq (Let (WithSource src))
deriving instance Show (Let NoSource)
deriving instance Show src => Show (Let (WithSource src))

let'noSource :: forall s. SrcUnwrap s => Let s -> Let NoSource
let'noSource x =
  Let
    { let'items = fmap (letItem'noSource @s . src'unwrap @s) $ let'items x
    , let'body = expr'noSource @s $ src'unwrap @s $ let'body x
    }


--------------------------------------------------------------------------------
--  LetItem
--------------------------------------------------------------------------------

data LetItem s
  = LetItem'Eq (Src s Var) (Expr s)
  | LetItem'Inherit (Src s (Expr s)) (Seq (Src s Var))

deriving instance Eq (LetItem NoSource)
deriving instance Eq src => Eq (LetItem (WithSource src))
deriving instance Show (LetItem NoSource)
deriving instance Show src => Show (LetItem (WithSource src))

letItem'noSource :: forall s. SrcUnwrap s => LetItem s -> LetItem NoSource
letItem'noSource =
  \case
    LetItem'Eq a b -> LetItem'Eq (src'unwrap @s a) (expr'noSource @s b)
    LetItem'Inherit a b -> LetItem'Inherit (expr'noSource @s $ src'unwrap @s a) (fmap (src'unwrap @s) b)


--------------------------------------------------------------------------------
--  List
--------------------------------------------------------------------------------

data ListCombinator = List | Concat
  deriving (Eq, Show)
