{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Bricks.Internal.List

  ( module Data.List
  , module Data.List.EitherFunctions

  , minimum, maximum

  ) where

-- either-list-functions
import Data.List.EitherFunctions

-- base
import           Data.List  hiding (maximum, minimum)
import qualified Data.List  as List
import           Data.Maybe (Maybe (..))
import           Data.Ord   (Ord (..))

{- |

>>> minimum [1,2,3]
Just 1

>>> minimum []
Nothing

-}
minimum :: Ord a => [a] -> Maybe a
minimum =
  \case
    [] -> Nothing
    xs -> Just (List.minimum xs)

{- |

>>> maximum [1,2,3]
Just 3

>>> maximum []
Nothing

-}
maximum :: Ord a => [a] -> Maybe a
maximum =
  \case
    [] -> Nothing
    xs -> Just (List.maximum xs)
