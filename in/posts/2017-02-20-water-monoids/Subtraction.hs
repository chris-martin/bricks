class Subtraction a where
    (-) :: a -> a -> a
    infixl 6 -

instance Subtraction Natural where
    a - b | a >= b    = a Prelude.- b
          | otherwise = b Prelude.- a

instance Subtraction a => Subtraction (Sum a) where
    Sum a - Sum b = Sum (a - b)
