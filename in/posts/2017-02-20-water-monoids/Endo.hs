newtype Endo a = Endo { appEndo :: a -> a }

instance Monoid (Endo a) where
    mempty = Endo id
    Endo f `mappend` Endo g = Endo (f . g)

f = fold [ Endo (+ 3)
         , Endo (`div` 2)
         , Endo (+ 10)
         ]
-- appEndo f 0
--    = appEndo (Endo (+ 3) <> Endo (`div` 2) <> Endo (+ 10)) 0
--    = ((+ 3) . (`div` 2) . (+ 10)) 0
--    = 8
