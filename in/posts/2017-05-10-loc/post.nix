{ file-path, file-string, html-tags, scss, markdown, code }:

{
  title = "Loc, Span, and Area";
  date  = "2017 May 10";
  slug  = "loc";

  css = scss ./loc.scss;

  thumbnail = file-path ./thumb.png;

  abstract = ''
    A Haskell library for calculations on text file positions.
  '';

  twitter = {
    card = "summary_large_image";
    image = file-path ./twitter.png;
    description = ''
      A Haskell library for calculations on text file positions.
    '';
  };

  body = let
    h2 = x: html-tags.h2 (markdown x);
    p  = x: html-tags.p  (markdown x);
    li = x: html-tags.li (markdown x);

    inherit (html-tags) ol blockquote;

    haskell = code { language = "haskell"; };

    url = {
      joy              = "https://joyofhaskell.com/";
      haskell-src-exts = "https://hackage.haskell.org/package/haskell-src-exts";
      loc              = "https://hackage.haskell.org/package/loc";
      doctest          = "https://hackage.haskell.org/package/doctest";
      difference-list  = "https://en.wikipedia.org/wiki/Difference_list";
      pollard          = "https://toot.cafe/users/porges/updates/14791";
    };

  in [

    (p ''
      I started using [haskell-src-exts](${url.haskell-src-exts}) recently to
      parse Haskell files to turn them into LaTeX for
      [*The Joy of Haskell*](${url.joy}). I wasn’t used to this sort of parser
      that produces an AST that’s mapped back to the source file by line and
      column numbers, so it took me a while to wrap my head around what to do
      with its output.
    '')

    (p ''
      After a while, I settled on some types, invented some terminology, and
      published a library: [loc](${url.loc}). Here’s an example to serve as
      an overview of the concepts:
    '')

    (html ''
      <div class="example">
        <div>
          <img src="${file-path ./example.png}">
        </div>
      </div>
    '')

    (ul [
      (li "`Loc` — a cursor position, starting at the origin `1:1`")
      (li "`Span` — a nonempty contiguous region between two locs")
      (li "`Area` — a set of zero or more spans with gaps between them")
    ])

    ##################################################################

    (h2 "`Pos`")

    (p ''
      Since all of the numbers we’re dealing with in this domain are positive,
      I introduced a “positive integer” type. This is a newtype for `Natural`
      that doesn’t allow zero.
    '')

    (haskell (file-string ./Pos.hs))

    (p ''
      I’d love to have `toInteger :: Pos -> Integer` from the `Integral`
      typeclass; unfortunately, `Integral` is seriously overburdened class,
      and that would require also implementing `quotRem`. I don’t terribly
      mind that `negate` throws `Underflow`, but `quotRem :: Pos -> Pos ->
      (Pos, Pos)` is a level of nonsense that crosses a line for me.
    '')

    (p ''
      Instead I introduced my own `ToNat` class that I can use to convert
      positive numbers to natural numbers. (My opinion that great typeclasses
      only have one method continues to solidify.)
    '')

    (haskell ''
      class ToNat a where
        toNat :: a -> Natural

      instance ToNat Pos where
        toNat (Pos n) = n
    '')

    ##################################################################

    (h2 "`Line`, `Column`")

    (p ''
      We then add some newtypes to be more specific about whether we’re
      talking about line or column numbers.
    '')

    (haskell ''
      newtype Line = Line Pos
        deriving (Eq, Ord, Num, Real, Enum, ToNat)

      newtype Column = Column Pos
        deriving (Eq, Ord, Num, Real, Enum, ToNat)
    '')

    ##################################################################

    (h2 "`Loc`")

    (p "A `Loc` is a `Line` and a `Column`.")

    (haskell ''
      data Loc = Loc
        { line   :: Line
        , column :: Column
        }
        deriving (Eq, Ord)
    '')

    ##################################################################

    (h2 "`Span`")

    (p "A `Span` is a start `Loc` and an end `Loc`.")

    (haskell ''
      data Span = Span
        { start :: Loc
        , end   :: Loc
        } deriving (Eq, Ord)
    '')

    (p ''
      A `Span` is not allowed to be empty; in other words, `start` and `end`
      must be different. I don’t have an extremely compelling rationale for
      this, other than that empty spans didn’t make sense for my use case.
      Eliminating empty spans also, in my opinion, seems to eliminate some
      ambiguity when we describe an `Area` as a set of `Span`s.
    '')

    (p ''
      There are two functions for constructing a `Span`. They both reorder
      their arguments as appropriate to make sure the start comes before the
      end (so that spans are never backwards). They take different approaches
      to ensuring that spans are never empty: the first can throw an
      exception, whereas the second is typed as `Maybe`.
    '')

    (haskell ''
      fromTo :: Loc -> Loc -> Span
      fromTo a b =
        maybe (throw EmptySpan) id (fromToMay a b)

      fromToMay :: Loc -> Loc -> Maybe Span
      fromToMay a b =
        case compare a b of
          LT -> Just (Span a b)
          GT -> Just (Span b a)
          EQ -> Nothing
    '')

    (p ''
      As you can see here, I am not strictly opposed to writing partial
      functions in Haskell. I have two conditions for this, though:
    '')

    (ol [
      (li ''
        If a function can throw an exception, that fact must be clearly
        documented.
      '')
      (li ''
        A function that can throw an exception should be paired with a
        corresponding total function that does the same thing *without*
        the possibility of an exception.
      '')
    ])

    (p ''
      In other words, providing a partial function that might be more
      convenient in some cases is fine, but don’t *force* a user of your
      API to use a partial function.
    '')

    ##################################################################

    (h2 "`Area`")

    (p ''
      An `Area` is conceptually a set of `Span`s, so in my first attempt
      that’s exactly how I defined it.
    '')

    (haskell "newtype Area = Area (Set Span)")

    (p ''
      Unfortunately I couldn’t manage to write reasonably efficient union and
      difference operations with this representation. Here’s what I ended up
      with instead:
    '')

    (haskell ''
      data Terminus = Start | End
        deriving (Eq, Ord)

      newtype Area = Area (Map Loc Terminus)
        deriving (Eq, Ord)
    '')

    (p ''
      Rather than keeping a set of the spans, we keep a set of the spans’
      start and end positions, along with a tag indicating whether each is
      a start or an end. You should notice the drawback to this
      representation: it is now much less “correct by construction”. The
      map must contain an even number of `Loc`s, alternating between `Start`
      and `End`. Any operations we write using the `Area` constructor must
      take care to preserve that property.
    '')

    (p ''
      I’ll only cover one of the algorithms in this blog post: Adding a
      `Span` to an `Area`. We’re going to define a function with this type:
    '')

    (haskell "addSpan :: Span -> Area -> Area")

    (p ''
      `Data.Map` in the `containers` package provides an *O(log n)* operation
      to divide a map into keys that are less than and greater than some key:
    '')

    (haskell "split :: Ord k => k -> Map k a -> (Map k a, Map k a)")

    (p ''
      We’re going to use the `split` function twice: to split the area into
      `Loc`s that come *before the start* of the span we’re adding, and `Loc`s
      that come *after the end* of the span we’re adding. Then we’ll combine
      the stuff in the middle with the new span, and finally `mappend` all the
      maps back together.
    '')

    (haskell (file-string ./addSpan.hs))

    ##################################################################

    (h2 "`Show`")

    (p ''
      I defined custom `Show` and `Read` instances to be able to write terse
      [doctests](${url.doctest}) like
    '')

    (haskell ''
      >>> addSpan (read "1:1-6:1") (read "[1:1-3:1,6:1-6:2,7:4-7:5]")
      [1:1-6:2,7:4-7:5]
    '')

    (p ''
      I usually just implement `show` when I write a custom `Show` instance,
      but this time I thought I’d do it the right way and implement
      `showsPrec` instead. This [difference list](${url.difference-list})
      construction avoids expensive *O(n)* list concatenations.
    '')

    (haskell ''
      locShowsPrec :: Int -> Loc -> ShowS
      locShowsPrec _ (Loc l c) =
        shows l .
        showString ":" .
        shows c

      spanShowsPrec :: Int -> Span -> ShowS
      spanShowsPrec _ (Span a b) =
        locShowsPrec 10 a .
        showString "-" .
        locShowsPrec 10 b
    '')

    ##################################################################

    (h2 "`Read`")

    (p ''
      This was the first time I really explored `Read` in-depth. It’s a
      little rough, but surprisingly usable (despite not great documentation).
    '')

    (p ''
      The parser for `Pos` is based on the parser for `Natural`, applying
      `mfilter (/= 0)` to make the parser fail if the input represents a zero.
    '')

    (haskell ''
      posReadPrec :: ReadPrec Pos
      posReadPrec =
        Pos <$> mfilter (/= 0) readPrec
    '')

    (p "As a reminder, the type of `mfilter` is:")

    (haskell "mfilter :: MonadPlus m => (a -> Bool) -> m a -> m a")

    (p "The `Loc` parser uses a very typical `Applicative` pattern:")

    (haskell ''
      -- | Parses a single specific character.
      readPrecChar :: Char -> ReadPrec ()
      readPrecChar = void . readP_to_Prec . const . ReadP.char

      locReadPrec :: ReadPrec Loc
      locReadPrec =
        Loc              <$>
        readPrec         <*
        readPrecChar ':' <*>
        readPrec
    '')

    (p ''
      We used `mfilter` above to introduce failure into the `Pos` parser;
      for `Span` we’re going to use `empty`.
    '')

    (haskell "empty :: Alternative f => f a")

    (p ''
      First we use `fromToMay` to produce a `Maybe Span`, and then in the
      case where the result is `Nothing` we use `empty` to make the parser
      fail.
    '')

    (haskell ''
      spanReadPrec :: ReadPrec Span
      spanReadPrec =
        locReadPrec      >>= \a ->
        readPrecChar '-' *>
        locReadPrec      >>= \b ->
        maybe empty pure (fromToMay a b)
    '')

    ##################################################################

    (h2 "That’s all folks")

    (p ''
      This wasn’t intensely exciting or weird, but I want to produce more blog
      posts about doing normal stuff in Haskell. The package is called
      [loc](${url.loc}) on Hackage if you’d like to investigate further.
    '')

    (p ''
      The build tools for [*The Joy of Haskell*](${url.joy}) are turning into
      an interesting custom reinvention of Literate Haskell; stay tuned for
      updates on that!
    '')

    (h2 "Addendum")

    (p "[George Pollard asks](${url.pollard}):")

    (blockquote ''
      would it maybe make it easier to interpret Loc as a character position
      (à la vim)? then Span with start=end can be a 1-char span and there are
      no invalid ones
    '')

    (p ''
      This is a nice suggestion, and “no invalid spans” is an appealing pitch.
      The idea is that we could represent a span using two inclusive bounds
      *\[start, end\]* rather than an inclusive and an exclusive bound
      *\[start, end)*. Unfortunately, it would end up complicating the API a
      bit.
    '')

    (p ''
      Currently, the library is entirely agnostic of the text that the
      positions are referring to. This means there is no “plus one” operation
      on `Loc`, because the next `Loc` after *4:17* could be either *4:18* or
      *5:1* — we can’t tell without knowing the line lengths. Therefore, with
      inclusive ranges, you can’t tell whether span *4:16-4:17* abuts span
      *5:1-5:2* — at least, not without knowing whether the character at
      position *4:17* is a newline.
    '')

  ];
}
