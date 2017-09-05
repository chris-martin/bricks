instance Monoid Structure where

    mempty = Structure mempty mempty mempty

    mappend (Structure left right water)
            (Structure left' right' water') =
        Structure (left <> left')
                  (right <> right')
                  (water <> water' <> waterBetween right left')
