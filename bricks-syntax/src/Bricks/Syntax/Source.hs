{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TypeFamilies        #-}

{- | The framework that lets us generalize over expressions with and without
source information. -}

module Bricks.Syntax.Source
  (
  -- * Framework
    Src, SrcUnwrap (..), SrcJoin (..)

  -- * Option 1: NoSource
  , NoSource

  -- * Option 2: WithSource
  , WithSource, At (..), at'unwrap, at'source

  ) where

import Bricks.Internal.Prelude


--------------------------------------------------------------------------------
--  Src
--------------------------------------------------------------------------------

type family Src s a


--------------------------------------------------------------------------------
--  SrcUnwrap
--------------------------------------------------------------------------------

class SrcUnwrap s
  where
    src'unwrap :: Src s a -> a


--------------------------------------------------------------------------------
--  SrcJoin
--------------------------------------------------------------------------------

class SrcJoin s
  where
    src'join :: (Src s a -> Src s b -> c) -> (Src s a -> Src s b -> Src s c)


--------------------------------------------------------------------------------
--  NoSource
--------------------------------------------------------------------------------

{-

Expressions /without/ source information

-}

data NoSource

type instance Src NoSource a = a

instance SrcUnwrap NoSource
  where
    src'unwrap = id

instance SrcJoin NoSource
  where
    src'join = id


--------------------------------------------------------------------------------
--  WithSource
--------------------------------------------------------------------------------

{-

Expressions /with/ source information.

-}

data WithSource src

type instance Src (WithSource src) a = At src a

instance SrcUnwrap (WithSource src)
  where
    src'unwrap = at'unwrap

instance Semigroup src => SrcJoin (WithSource src)
  where
    src'join f x y = At (at'source x <> at'source y) (f x y)


--------------------------------------------------------------------------------
--  At
--------------------------------------------------------------------------------

{-

The type used with 'WithSource' that represents both a value and its source
information.

-}

data At src a = At src a
  deriving (Eq, Functor, Show)

instance (Semigroup src, Semigroup a) => Semigroup (At src a)
  where
    At src1 a1 <> At src2 a2 = At (src1 <> src2) (a1 <> a2)

instance (Monoid src, Monoid a) => Monoid (At src a)
  where
    mappend (At src1 a1) (At src2 a2) = At (mappend src1 src2) (mappend a1 a2)
    mempty = At mempty mempty

at'unwrap :: At src a -> a
at'unwrap (At _ a) = a

at'source :: At src a -> src
at'source (At src _) = src
