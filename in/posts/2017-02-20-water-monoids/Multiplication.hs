{-# LANGUAGE MultiParamTypeClasses #-}

class Multiplication a b c where
    (*) :: a -> b -> c
    infixl 7 *

instance Multiplication Width Height Area where
    Width w * Height h = Area (w Prelude.* h)

instance Multiplication Height Width Area where
    Height h * Width w = Area (w Prelude.* h)
