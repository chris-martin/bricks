module Bricks.Internal.Magma where

import Control.Applicative (liftA2)

class Magma a
  where
    -- | Because this class is introduced chiefly for the sake of the @Term@
    -- instance, the name of this operator was chosen to evoke the style of
    -- the tree diagrams in SPJ's 1987 book "The Implementation of Functional
    -- Programming Languages."
    (/@\) :: a -> a -> a

-- | A magma such that 'leftId <+> x = x'.
class Magma a => LeftUnital a
  where
    leftUnit :: a

instance Magma a      => Magma      (IO a) where (/@\)    = liftA2 (/@\)
instance LeftUnital a => LeftUnital (IO a) where leftUnit = pure leftUnit
