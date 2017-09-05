class Monoid a where

    mappend :: a -> a -> a  -- An associative operation,
                            -- also called (<>)

    mempty :: a             -- Identity of (<>)
    
