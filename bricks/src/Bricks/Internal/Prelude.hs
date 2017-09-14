module Bricks.Internal.Prelude

  ( module Control.Applicative
  , module Control.Arrow
  , module Control.Monad
  , module Data.Bool
  , module Data.Char
  , module Data.Either
  , module Data.Eq
  , module Data.Foldable
  , module Data.Function
  , module Data.Maybe
  , module Data.Semigroup
  , module Data.String
  , module Numeric.Natural

  ) where

import Control.Applicative (pure, (*>), (<*), (<*>), (<|>))
import Control.Arrow       ((>>>))
import Control.Monad       ((>>=))
import Data.Bool           (Bool (False, True), not, (&&), (||))
import Data.Char           (Char)
import Data.Either         (Either (..))
import Data.Eq             (Eq ((/=), (==)))
import Data.Foldable       (asum, fold, foldMap, foldl, foldr)
import Data.Function       (($), (.))
import Data.Maybe          (Maybe (Just, Nothing), catMaybes, maybe)
import Data.Semigroup      ((<>))
import Data.String         (String)
import Numeric.Natural     (Natural)
