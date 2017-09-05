emptyFace :: Face a => a
emptyFace = coerce (Map.empty :: Corners)

overlapFaces :: Face a => a -> a -> a
overlapFaces nearFace farFace = coerce (corners :: Corners)
  where
    near = coerce nearFace :: Corners
    far  = coerce farFace  :: Corners
    (nearHeight, nearWidth) = faceSize near
    far' = (<> nearWidth <> Width 1) <$> snd (Map.split nearHeight far)
    corners = near <> far'

faceSize :: Face a => a -> (Height, Width)
faceSize face = let corners = coerce face :: Corners
                in  if null corners
                        then mempty
                        else Map.findMax corners
