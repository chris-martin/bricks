module Bricks.Internal.Prelude

  ( (<&>)

  , module Control.Applicative
  , module Control.Arrow
  , module Control.Monad
  , module Data.Bool
  , module Data.Char
  , module Data.Either
  , module Data.Eq
  , module Data.Foldable
  , module Data.Function
  , module Data.Functor
  , module Data.Maybe
  , module Data.Monoid
  , module Data.Semigroup
  , module Data.String
  , module Numeric.Natural
  , module Text.Show

  ) where

import Control.Applicative (pure, (*>), (<*), (<*>), (<|>))
import Control.Arrow       ((>>>))
import Control.Monad       ((>>=))
import Data.Bool           (Bool (False, True), not, (&&), (||))
import Data.Char           (Char)
import Data.Either         (Either (..))
import Data.Eq             (Eq ((/=), (==)))
import Data.Foldable       (asum, fold, foldMap, foldl, foldl1, foldr, foldr1)
import Data.Function       (id, ($), (&), (.))
import Data.Functor        (Functor, fmap, void, ($>), (<$), (<$>))
import Data.Maybe          (Maybe (Just, Nothing), catMaybes, maybe)
import Data.Monoid         (Monoid (mappend, mempty))
import Data.Semigroup      (Semigroup ((<>)))
import Data.String         (String)
import Numeric.Natural     (Natural)
import Text.Show           (Show (show, showList, showsPrec), shows)

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap
infixl 1 <&>
