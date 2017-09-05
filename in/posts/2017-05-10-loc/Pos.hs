newtype Pos = Pos Natural
  deriving (Eq, Ord)

instance Num Pos where
  fromInteger = Pos . checkForUnderflow . fromInteger
  Pos x + Pos y = Pos (x + y)
  Pos x - Pos y = Pos (checkForUnderflow (x - y))
  Pos x * Pos y = Pos (x * y)
  abs = id
  signum _ = Pos 1
  negate _ = throw Underflow

checkForUnderflow :: Natural -> Natural
checkForUnderflow n =
  if n == 0 then throw Underflow else n
