waterBetween :: RightFace -> LeftFace -> Area
waterBetween face face' =
    fold $ go (Map.toAscList (coerce face :: Corners))
              (Map.toAscList (coerce face' :: Corners))
              mempty
  where
    go :: [(Height, Width)]
       -> [(Height, Width)]
       -> Height
       -> [Area]
    go l@((heightL, depthL) : restL)
       r@((heightR, depthR) : restR)
       floor =

        let area   = raised * width
            raised = floor' - floor
            width  = depthL <> depthR

            (floor', l', r') =
                case compare heightL heightR of
                    LT -> (heightL, restL, r    )
                    GT -> (heightR, l,     restR)
                    EQ -> (heightL, restL, restR)

        in  area : go l' r' floor'

    go _ _ _ = []
