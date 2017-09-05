{-# LANGUAGE GeneralizedNewtypeDeriving #-}

newtype Width = Width (Sum Natural)
    deriving (Eq, Monoid, Ord, Semigroup, Subtraction)

newtype Height = Height (Sum Natural)
    deriving (Eq, Monoid, Ord, Semigroup, Subtraction)

newtype Area = Area (Sum Natural)
    deriving (Eq, Monoid, Ord, Semigroup, Subtraction)
