module Bricks.Internal.Functor
  ( (<&>)
  ) where

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap
infixl 1 <&>
