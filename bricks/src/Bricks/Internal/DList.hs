module Bricks.Internal.DList where

type DList a = [a] -> [a]

-- | The empty dlist.
empty :: DList a
empty = id

-- | Append an element to a dlist.
snoc :: DList a -> a -> DList a
snoc xs x = xs . (x:)
infixl `snoc`

-- | Convert a dlist to a regular list.
toList :: DList a -> [a]
toList xs = xs []
