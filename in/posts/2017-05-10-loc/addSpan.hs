addSpan :: Span -> Area -> Area
addSpan b (Area as) =

  let
    -- Spans lower than b that do not abut or
    -- overlap b. These spans will remain
    -- completely intact in the result.
    unmodifiedSpansBelow :: Map Loc Terminus

    -- Spans greater than b that do not abut
    -- or overlap b. These spans will remain
    -- completely intact in the result.
    unmodifiedSpansAbove :: Map Loc Terminus

    -- The start location of a span that starts
    -- below b but doesn't end below b, if such
    -- a span exists. This span will be merged
    -- into the 'middle'.
    startBelow :: Maybe Loc

    -- The end location of a span that ends
    -- above b but doesn't start above b, if
    -- such a span exists. This span will be
    -- merged into the 'middle'.
    endAbove :: Maybe Loc

    -- b, plus any spans it abuts or overlaps.
    middle :: Map Loc Terminus

    (unmodifiedSpansBelow, startBelow) =
      let
        (below, _) = Map.split (Span.start b) as
      in
        case Map.maxViewWithKey below of
          Just ((l, Start), xs) -> (xs, Just l)
          _ -> (below, Nothing)


    (unmodifiedSpansAbove, endAbove) =
      let
        (_, above) = Map.split (Span.end b) as
      in
        case Map.minViewWithKey above of
          Just ((l, End), xs) -> (xs, Just l)
          _ -> (above, Nothing)

    middle = Map.fromList
        [ (minimum $ Foldable.toList startBelow
                  <> [Span.start b], Start)
        , (maximum $ Foldable.toList endAbove
                  <> [Span.end b], End)
        ]

  in
    Area $ unmodifiedSpansBelow
        <> middle
        <> unmodifiedSpansAbove
