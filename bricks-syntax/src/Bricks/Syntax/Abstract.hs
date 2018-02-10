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

  -- * Atom (strings and variables)
  , AtomType (..), Atom (..)

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
  , ListCombinator (..), List (..)

  -- * Optionally-attached source information
  , Src, SrcUnwrap (..), SrcJoin (..), NoSource, WithSource, At (..)
  , at'unwrap, at'source

  -- * Discarding source information
  , expr'noSource, lambda'noSource, binary'noSource, pattern'noSource
  , dictPattern'noSource, dictPatternItem'noSource, dict'noSource
  , dictItem'noSource, let'noSource, letItem'noSource, list'noSource

  ) where

import Bricks.Syntax.Source

-- bricks-internal
import Bricks.Internal.List    as List
import Bricks.Internal.Prelude
import Bricks.Internal.Seq     (Seq)
import Bricks.Internal.Text    (Text)


--------------------------------------------------------------------------------
--  Expr
--------------------------------------------------------------------------------

data Expr s
  = Expr'Atom    Atom
  | Expr'Binary (Binary s)
  | Expr'List   (List   s)
  | Expr'Lambda (Lambda s)
  | Expr'Let    (Let    s)
  | Expr'Dict   (Dict   s)

deriving instance Eq   (Expr NoSource)
deriving instance Show (Expr NoSource)

deriving instance Eq   src => Eq   (Expr (WithSource src))
deriving instance Show src => Show (Expr (WithSource src))

expr'noSource :: forall s. SrcUnwrap s => Expr s -> Expr NoSource
expr'noSource =
  \case
    Expr'Atom   x -> Expr'Atom   $                 x
    Expr'Binary x -> Expr'Binary $ binary'noSource x
    Expr'List   x -> Expr'List   $   list'noSource x
    Expr'Lambda x -> Expr'Lambda $ lambda'noSource x
    Expr'Let    x -> Expr'Let    $    let'noSource x
    Expr'Dict   x -> Expr'Dict   $   dict'noSource x


--------------------------------------------------------------------------------
--  Atom
--------------------------------------------------------------------------------

data AtomType = Atom'Var | Atom'Str
  deriving (Eq, Show)

data Atom =
  Atom
    { atom'type :: AtomType
    , atom'text :: Text
    }
  deriving (Eq, Show)


--------------------------------------------------------------------------------
--  Lambda
--------------------------------------------------------------------------------

data Lambda s =
  Lambda
    { lambda'head :: Src s (Pattern s)
    , lambda'body :: Src s (Expr s)
    }

deriving instance Eq   (Lambda NoSource)
deriving instance Show (Lambda NoSource)

deriving instance Eq   src => Eq   (Lambda (WithSource src))
deriving instance Show src => Show (Lambda (WithSource src))

lambda'noSource :: forall s. SrcUnwrap s => Lambda s -> Lambda NoSource
lambda'noSource x =
  Lambda
    { lambda'head = f (lambda'head x)
    , lambda'body = g (lambda'body x)
    }
  where
    f = pattern'noSource @s . src'unwrap @s
    g = expr'noSource @s    . src'unwrap @s

--------------------------------------------------------------------------------
--  Binary
--------------------------------------------------------------------------------

data BinaryCombinator = Apply | Lookup
  deriving (Eq, Show)

data Binary s =
  Binary
    { binary'combinator :: BinaryCombinator
    , binary'1 :: Src s (Expr s)
    , binary'2 :: Src s (Expr s)
    }

deriving instance Eq   (Binary NoSource)
deriving instance Show (Binary NoSource)

deriving instance Eq   src => Eq   (Binary (WithSource src))
deriving instance Show src => Show (Binary (WithSource src))

binary'noSource :: forall s. SrcUnwrap s => Binary s -> Binary NoSource
binary'noSource x =
  Binary
    { binary'combinator = binary'combinator x
    , binary'1 = f (binary'1 x)
    , binary'2 = f (binary'2 x)
    }
  where
    f = expr'noSource @s . src'unwrap @s

binary'chain
  :: forall s. SrcJoin s
  => BinaryCombinator
  -> Src s (Expr s)
  -> Seq (Src s (Expr s))
  -> Src s (Expr s)
binary'chain c =
  foldl $ src'join @s @(Expr s) @(Expr s) @(Expr s) f
  where
    f x y = Expr'Binary (Binary c x y)


--------------------------------------------------------------------------------
--  Pattern
--------------------------------------------------------------------------------

data Pattern s
  = Pattern'Var Text
  | Pattern'Dict (DictPattern s)
  | Pattern'Both (Src s Text) (Src s (DictPattern s))

pattern'noSource :: forall s. SrcUnwrap s => Pattern s -> Pattern NoSource
pattern'noSource =
  \case
    Pattern'Var x    -> Pattern'Var x
    Pattern'Dict y   -> Pattern'Dict (g y)
    Pattern'Both x y -> Pattern'Both (f x) (h y)
  where
    f = src'unwrap @s
    g = dictPattern'noSource
    h = dictPattern'noSource . src'unwrap @s @(DictPattern s)

deriving instance Eq   (Pattern NoSource)
deriving instance Show (Pattern NoSource)

deriving instance Eq   src => Eq   (Pattern (WithSource src))
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
    { dictPattern'items    :: Seq (Src s (DictPatternItem s))
    , dictPattern'ellipsis :: Ellipsis
    }

dictPattern'noSource
  :: forall s. SrcUnwrap s => DictPattern s -> DictPattern NoSource
dictPattern'noSource x =
  DictPattern
    { dictPattern'items    = f (dictPattern'items x)
    , dictPattern'ellipsis = dictPattern'ellipsis x
    }
  where
    f = fmap (dictPatternItem'noSource @s . src'unwrap @s)

deriving instance Eq   (DictPattern NoSource)
deriving instance Show (DictPattern NoSource)

deriving instance Eq   src => Eq   (DictPattern (WithSource src))
deriving instance Show src => Show (DictPattern (WithSource src))


--------------------------------------------------------------------------------
--  DictPatternItem
--------------------------------------------------------------------------------

data DictPatternItem s
  = DictPatternItem'Var Text
  | DictPatternItem'Default (Src s Text) (Src s (Expr s))

dictPatternItem'noSource
  :: forall s. SrcUnwrap s => DictPatternItem s -> DictPatternItem NoSource
dictPatternItem'noSource =
  \case
    DictPatternItem'Var x -> DictPatternItem'Var x
    DictPatternItem'Default x y -> DictPatternItem'Default (f x) (g y)
  where
    f = src'unwrap @s
    g = expr'noSource @s . src'unwrap @s

deriving instance Eq   (DictPatternItem NoSource)
deriving instance Show (DictPatternItem NoSource)

deriving instance Eq   src => Eq   (DictPatternItem (WithSource src))
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
    { dict'rec   :: Rec
    , dict'items :: Seq (Src s (DictItem s))
    }

deriving instance Eq   (Dict NoSource)
deriving instance Show (Dict NoSource)

deriving instance Eq   src => Eq   (Dict (WithSource src))
deriving instance Show src => Show (Dict (WithSource src))

dict'noSource :: forall s. SrcUnwrap s => Dict s -> Dict NoSource
dict'noSource x =
  Dict
    { dict'rec   = dict'rec x
    , dict'items = f (dict'items x)
    }
  where
    f = fmap (dictItem'noSource @s . src'unwrap @s)


--------------------------------------------------------------------------------
--  DictItem
--------------------------------------------------------------------------------

data DictItem s
  = DictItem'Eq           (Src s (Expr s)) (Src s (Expr s))
  | DictItem'Inherit'Dict (Src s (Expr s)) (Seq (Src s Text))
  | DictItem'Inherit'Var                   (Seq (Src s Text))

deriving instance Eq   (DictItem NoSource)
deriving instance Show (DictItem NoSource)

deriving instance Eq   src => Eq   (DictItem (WithSource src))
deriving instance Show src => Show (DictItem (WithSource src))

dictItem'noSource :: forall s. SrcUnwrap s => DictItem s -> DictItem NoSource
dictItem'noSource =
  \case
    DictItem'Eq a b            -> DictItem'Eq           (f a) (f b)
    DictItem'Inherit'Dict a xs -> DictItem'Inherit'Dict (f a) (g xs)
    DictItem'Inherit'Var    xs -> DictItem'Inherit'Var        (g xs)
  where
    f = expr'noSource @s . src'unwrap @s
    g = fmap (src'unwrap @s @Text)


--------------------------------------------------------------------------------
--  Let
--------------------------------------------------------------------------------

data Let s =
  Let
    { let'items :: Seq (Src s (LetItem s))
    , let'body :: Src s (Expr s)
    }

deriving instance Eq   (Let NoSource)
deriving instance Show (Let NoSource)

deriving instance Eq   src => Eq   (Let (WithSource src))
deriving instance Show src => Show (Let (WithSource src))

let'noSource :: forall s. SrcUnwrap s => Let s -> Let NoSource
let'noSource x =
  Let
    { let'items = f (let'items x)
    , let'body  = g (let'body x)
    }
  where
    f = fmap (letItem'noSource @s . src'unwrap @s)
    g = expr'noSource @s . src'unwrap @s


--------------------------------------------------------------------------------
--  LetItem
--------------------------------------------------------------------------------

data LetItem s
  = LetItem'Eq      (Src s Text)     (Src s (Expr s))
  | LetItem'Inherit (Src s (Expr s)) (Seq (Src s Text))

deriving instance Eq   (LetItem NoSource)
deriving instance Show (LetItem NoSource)

deriving instance Eq   src => Eq   (LetItem (WithSource src))
deriving instance Show src => Show (LetItem (WithSource src))

letItem'noSource :: forall s. SrcUnwrap s => LetItem s -> LetItem NoSource
letItem'noSource =
  \case
    LetItem'Eq      a b -> LetItem'Eq      (f a) (g b)
    LetItem'Inherit a b -> LetItem'Inherit (g a) (h b)
  where
    f = src'unwrap @s
    g = expr'noSource @s . src'unwrap @s
    h = fmap (src'unwrap @s)


--------------------------------------------------------------------------------
--  List
--------------------------------------------------------------------------------

data ListCombinator = List'Id | List'Concat
  deriving (Eq, Show)

data List s =
  List
    { list'combinator :: ListCombinator
    , list'items      :: Seq (Src s (Expr s))
    }

deriving instance Eq   (List NoSource)
deriving instance Show (List NoSource)

deriving instance Eq   src => Eq   (List (WithSource src))
deriving instance Show src => Show (List (WithSource src))

list'noSource :: forall s. SrcUnwrap s => List s -> List NoSource
list'noSource x =
  List
    { list'combinator = list'combinator x
    , list'items      = f (list'items x)
    }
  where
    f = fmap (expr'noSource @s . src'unwrap @s)
