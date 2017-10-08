module Bricks.Internal.Map
  ( module Data.Map

  , exactKeys
  , restrictKeys

  ) where

-- Containers
import           Data.Map
import           Data.Set (Set)
import qualified Data.Set as Set

{- | This function was added in containers version 0.5.8 which we're not using
yet. -}

restrictKeys :: Ord k => Map k a -> Set k -> Map k a
m `restrictKeys` s = filterWithKey (\k _ -> k `Set.member` s) m

{- | If @s@ is a subset of the keys in @m@ then

> m exactKeys s = Right (m `restrictKeys` s)

Otherwise, @m exactKeys s = Left s'@ where @s'@ is the keys that are missing
from @m@. -}

-- | ==== Examples
--
-- >>> :{
-- >>> fromList [('a', 1), ('b', 2), ('c', 3)]
-- >>>   `exactKeys` Set.fromList ['a', 'b']
-- >>> :}
-- Right (fromList [('a',1),('b',2)])
--
-- >>> :{
-- >>> fromList [('a', 1), ('b', 2), ('c', 3)]
-- >>>   `exactKeys` Set.fromList ['a', 'x', 'y']
-- >>> :}
-- Left (fromList "xy")

exactKeys :: Ord k => Map k a -> Set k -> Either (Set k) (Map k a)
m `exactKeys` s =
  let
    s' = s `Set.difference` keysSet m
  in
    if Set.null s'
    then Right (m `restrictKeys` s)
    else Left s'
