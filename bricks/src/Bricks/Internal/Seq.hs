{-# LANGUAGE NoImplicitPrelude #-}

module Bricks.Internal.Seq

  ( Seq

  , (<|), (|>)

  , adjust
  , adjustLast
  , concat
  , concatMap
  , dropWhileL
  , dropWhileR
  , empty
  , filter
  , fromList
  , intersperse
  , length
  , map
  , minimum
  , maximum
  , null
  , toList
  , trimWhile
  , singleton

  ) where

-- Containers
import Data.Sequence (Seq, adjust, dropWhileL, dropWhileR, empty, filter,
                      fromList, length, null, singleton, (<|), (|>))

-- Base
import           Data.Bool     (Bool (..))
import           Data.Foldable (Foldable)
import qualified Data.Foldable as Foldable
import           Data.Functor  (fmap)
import qualified Data.List     as List
import           Data.Maybe    (Maybe (..))
import           Data.Ord      (Ord)
import           Prelude       (Num (..))

-- | Like 'List.intersperse', but for 'Seq'.
intersperse :: a -> Seq a -> Seq a
intersperse x xs =
  fromList (List.intersperse x (Foldable.toList xs))

-- | 'Foldable.fold' specialized for 'Seq'.
concat :: Foldable f => f (Seq a) -> Seq a
concat = Foldable.fold

-- | Like 'Foldable.concatMap', but for 'Seq'.
concatMap :: Foldable f => (a -> Seq b) -> f a -> Seq b
concatMap f xs = concat (fmap f (Foldable.toList xs))

{- |

>>> minimum (fromList [1,2,3])
Just 1

>>> minimum empty
Nothing

-}
minimum :: Ord a => Seq a -> Maybe a
minimum xs =
  if null xs then Nothing else Just (List.minimum (Foldable.toList xs))

{- |

>>> maximum (fromList [1,2,3])
Just 3

>>> maximum empty
Nothing

-}
maximum :: Ord a => Seq a -> Maybe a
maximum xs =
  if null xs then Nothing else Just (List.maximum (Foldable.toList xs))

-- | Specialization of 'fmap'.
map :: (a -> b) -> Seq a -> Seq b
map = fmap

-- | Specialization of 'Foldable.toList' for 'Seq'.
toList :: Seq a -> [a]
toList = Foldable.toList

{- |

>>> adjustLast (+ 1) (fromList [1, 2, 3])
fromList [1,2,4]

>>> adjustLast (+ 1) empty
fromList []

-}
adjustLast :: (a -> a) -> Seq a -> Seq a
adjustLast f xs = adjust f (length xs - 1) xs

trimWhile :: (a -> Bool) -> Seq a -> Seq a
trimWhile f xs = dropWhileL f (dropWhileR f xs)
