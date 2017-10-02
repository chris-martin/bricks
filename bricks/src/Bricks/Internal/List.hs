{-# LANGUAGE NoImplicitPrelude, LambdaCase #-}

module Bricks.Internal.List

  ( module Data.List
  , module Data.List.EitherFunctions

  , minimum, maximum

  ) where

-- either-list-functions
import Data.List.EitherFunctions

-- base
import Data.Ord (Ord (..))
import Data.Maybe (Maybe (..))
import Data.List hiding (minimum, maximum)
import qualified Data.List as List

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
