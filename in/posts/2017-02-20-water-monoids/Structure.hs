data Structure = Structure
    { sLeft  :: LeftFace   -- 1. The outer shape
    , sRight :: RightFace
    , sArea  :: Area       -- 2. How much water it holds
    }
