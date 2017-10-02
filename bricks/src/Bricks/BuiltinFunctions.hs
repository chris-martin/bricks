{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}

module Bricks.BuiltinFunctions where

import Bricks.Term
import Bricks.Type

-- Bricks internal
import           Bricks.Internal.Monad
import           Bricks.Internal.Prelude
import           Bricks.Internal.Seq     (Seq)
import           Bricks.Internal.Text    (Text)
import qualified Bricks.Internal.Text    as Text

-- Containers
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set

-- Base
import Data.Dynamic  (Dynamic, fromDynamic, toDyn)
import Data.IORef
import Data.Typeable (Typeable)
import Prelude       (Integer)
import System.IO     (IO)
import Text.Read     (readMaybe)

term'data :: forall a. Typeable a => Type a -> a -> Term
term'data (Type n) = Term'Data n . toDyn @a

fn'pure'parametric'arity1 :: (Term -> Term) -> Term
fn'pure'parametric'arity1 f =
  Term'Function $ \x -> pure $
  f x

fn'pure'parametric'arity2 :: (Term -> Term -> Term) -> Term
fn'pure'parametric'arity2 f =
  Term'Function $ \x -> pure $
  fn'pure'parametric'arity1 $ f x

fn'pure'parametric'arity3 :: (Term -> Term -> Term -> Term) -> Term
fn'pure'parametric'arity3 f =
  Term'Function $ \x -> pure $
  fn'pure'parametric'arity2 $ f x

fn'id :: Term
fn'id = fn'pure'parametric'arity1 id

fn'const :: Term
fn'const = fn'pure'parametric'arity2 const

{- | Function composition, in the traditional "backwards" order.

Read @f `fn'comp` g@ as "/f/ after /g/." -}

fn'comp :: Term
fn'comp = fn'pure'parametric'arity3 $ \f g x -> f /@\ (g /@\ x)

fn'flip :: Term
fn'flip = fn'pure'parametric'arity3 $ \f x y -> f /@@\ (y, x)

fn'dict'lookup :: Term
fn'dict'lookup = undefined

fn'or :: Term
fn'or =
  Term'Function $ \x -> cast'data type'boolean x <&>
  \case
    True -> fn'const /@\ term'data type'boolean True
    False -> assert'type type'boolean

fn'and :: Term
fn'and =
  Term'Function $ \x -> cast'data type'boolean x <&>
  \case
    False -> fn'const /@\ term'data type'boolean False
    True -> assert'type type'boolean

fn'string'append :: Term
fn'string'append =
  Term'Function $ \x -> cast'data type'string x <&> \x' ->
  Term'Function $ \y -> cast'data type'string y <&> \y' ->
  term'data type'string (Text.append x' y')

fn'dict'disallowExtraKeys :: Set Text -> Term
fn'dict'disallowExtraKeys allowedKeys =
  Term'Function $ undefined

fn'dict'merge'preferLeft :: Term
fn'dict'merge'preferLeft =
  Term'Function $ \x -> reduce'dict'keys x <&> \x' ->
  Term'Function $ \y -> reduce'dict'keys y <&> \y' ->
  Term'Dict'ReducedKeys $ Map.union x' y'

fn'dict'merge'preferRight :: Term
fn'dict'merge'preferRight = fn'flip /@\ fn'dict'merge'preferLeft

cast'data :: (MonadEval m, Typeable a) => Type a -> Term -> m a
cast'data = req fst

-- | Like 'fn'id', but fails if the argument is not of the given type.
assert'type :: Typeable a => Type a -> Term
assert'type t = Term'Function $ req snd t

req :: forall a b m. (MonadEval m, Typeable a)
  => ((a, Term) -> b)
  -> Type a
  -> Term
  -> m b
req s (Type n) = reduce'term >=> \case
  t@(Term'Data n' x) ->
    case fromDynamic @a x of
      Nothing -> bottom . Bottom $ "Expected " <> n <> ", got " <> n'
      Just a -> pure (s (a, t))
  x ->
    termTypeName x >>= \n' ->
      bottom . Bottom $ "Expected " <> n <> ", got " <> n'

fn'int'add :: Term
fn'int'add = undefined

fn'int'constructor :: Term
fn'int'constructor =
  Term'Function $ cast'data type'string >=> (
    Text.unpack >>> readMaybe @Integer >>>
    maybe (bottom . Bottom $ "invalid integer")
          (pure . term'data type'integer)
  )

standard'library :: Term
standard'library =
  Term'Dict'ReducedKeys . Map.fromList $
    [ ("add", fn'int'add)
    , ("integer", fn'int'constructor)
    , ("and", fn'and)
    , ("or", fn'or)
    , ("id", fn'id)
    , ("const", fn'const)
    ]
