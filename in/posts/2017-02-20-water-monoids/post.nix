{ file-path, file-string, scss, html-tags, markdown, code }:

let

  p  = x: html-tags.p  (markdown x);
  li = x: html-tags.li (markdown x);

  inherit (html-tags) h2 ol;

  haskell = code { language = "haskell"; };

  failed     = "https://medium.com/@bearsandsharks/i-failed-a-twitter-interview-52062fbb534b";
  monoid     = "https://www.stackage.org/haddock/lts-8.0/base-4.9.1.0/Data-Monoid.html";
  endo       = "https://www.stackage.org/haddock/lts-8.0/base-4.9.1.0/Data-Monoid.html#t:Endo";
  coerce     = "https://www.stackage.org/haddock/lts-8.1/base-4.9.1.0/Data-Coerce.html";
  github     = "https://github.com/chris-martin/rain-water/";
  chris-done = "http://chrisdone.com/posts/twitter-problem-loeb";
  semigroup  = "https://www.stackage.org/haddock/lts-8.0/base-4.9.1.0/Data-Semigroup.html";

in {
  title = "Water Monoids";
  date  = "2017 Feb 20";
  slug  = "water-monoids";

  css = scss ./water.css;

  thumbnail = file-path ./thumb.png;

  abstract = ''
    Solving that water-puddles problem, using as many monoids as possible.
  '';

  twitter = {
    card = "summary_large_image";
    image = file-path ./twitter.png;
    description = ''
      Solving that water-puddles problem, using as many monoids as possible.
    '';
  };

  body = [

    (p ''
      In the toy problem described by
      [*I Failed a Twitter Interview*](${failed}),
      we are given a list of integers
    '')

    (html ''<div style="text-align: center;">3, 2, 1, 4, 2</div>'')

    (p "representing the heights of walls")

    (html (file-string ./walls-unfilled.html))

    (p ''
      and we imagine pouring water onto this structure such that puddles
      accumulate in the gaps.
    '')

    (html (file-string ./walls-filled.html))

    (p ''
      We are then asked: How much water is held by the structure?
      (In the above example, the answer is 3).
    '')

    ###################################################################

    (h2 "Monoids")

    (p ''
      I find myself thinking about [monoids](${monoid}) a *lot* lately.
      Such a small, unassuming thing
    '')

    (haskell (file-string ./Monoid.hs))

    (p ''
      yet it stirs the imagination. If your type has a monoid, you can chain
      the elements of any collection together into a single element.
    '')

    (haskell ''
      fold :: (Foldable t, Monoid m) => t m -> m
    '')

    (p ''
      The obvious examples may be data structures like strings, which are
      chained together by concatenation.
    '')

    (haskell ''
      x = fold [ "one"
               , "two"
               , "three"
               ]
      -- x = "one" <> "two" <> "three"
      --   = "one" ++ "two" ++ "three"
      --   = "onetwothree"
    '')

    (p ''
      But a monoid operations aren’t just ways to merge data structures.
      My favorite example is [`Endo`](${endo}), which lets you chain
      together collections of functions.
    '')

    (haskell (file-string ./Endo.hs))

    (p ''
      Monoids compose extraordinarily readily. For example, if `a` and `b`
      have monoids, then the tuple `(a, b)` does as well.
    '')

    (haskell ''
      instance (Monoid a, Monoid b) => Monoid (a, b) where
          mempty = (mempty, mempty)
          (a1, b1) `mappend` (a2, b2) = (a1 <> a2, b1 <> b2)
    '')

    ###################################################################

    (h2 "The water monoid")

    (p ''
      I woke up one morning recently with the thought that we can define a
      monoid for these water-filled structures. When we place two of them
      side-by-side, they combine like this:
    '')

    (html ''
      <div class="water-monoid">
        <img src="${file-path ./structure.png}">
      </div>
    '')

    (p "A structure is represented by")

    (ol [
      (li [
        (p "What its outer shape looks like")
        (ol [
          (li "From the left, and")
          (li "From the right.")
        ])
        (p ''
          I call these its “faces”. Imagine the structure were as tall as you,
          and you were standing to the left or right of it; the face is
          comprised of the corners that are visible to you from that
          perspective.
        '')
      ])
      (li "The amount of water it holds.")
    ])

    (haskell (file-string ./Structure.hs))

    (p ''
      When filled with water, the structure is convex, so the left and right
      faces provide all the information we need to figure out what shape will
      result when we combine two structures.
    '')

    (haskell (file-string ./Structure-Monoid.hs))

    (p ''
      Not only does `Structure` have a monoid, but so do all of its fields;
      so `mempty` is defined quite simply as `Structure mempty mempty mempty`.
      The definition of `mappend` is similarly straightforward, with the
      exception that we also have to add in `waterBetween right left'` to
      include the water that puddles in the new gap between the two structures.
    '')

    ###################################################################

    (h2 "Arithmetic")

    (p ''
      Since there are no negative numbers in this problem, we’ll be using the
      `Natural` type, which represents nonnegative integers.
    '')

    (haskell "import Numeric.Natural (Natural)")

    (p ''
      The arithmetic in Haskell’s default prelude is a bit clumsy, so you want
      to be precise, it can be nice to define your own. For example, `Natural`
      has an instance of `Num`, which can get us into trouble because `(-)` is
      partial.
    '')

    (haskell ''
      λ> 1 - 2 :: Natural
      *** Exception: arithmetic underflow
    '')

    (p ''
      For the `Natural` type, I’d prefer to have the `(-)` function signify
      *absolute difference*. Fortunately we can define our own subtraction
      class and implement it however we want.
    '')

    (haskell (file-string ./Subtraction.hs))

    (p ''
      Not all numbers are the same, so let’s also define some types to assign
      meaning to the specific sorts of quantities we’re dealing with in this
      problem.
    '')

    (p ''
      We wrap the numbers in `Sum` so that we can automatically derive monoid
      instances that combine additively.
    '')

    (haskell (file-string ./Area.hs))

    (p ''
      We’ll need to multiply a `Width` by a `Height` to get an `Area`. Here we
      run into another limitation of `Num`: It assume we’re only multiplying
      values of the same type.
    '')

    (haskell "(*) :: Num a => a -> a -> a")

    (p ''
      So again let’s ignore the standard math and invent our own. Since this
      hetereogeneous multiplication involves more than one type, we need the
      language extension that allows multi-parameter type classes.
    '')

    (haskell (file-string ./Multiplication.hs))

    ###################################################################

    (h2 "Faces")

    (p ''
      Recall that we defined a structure’s shape in terms of its `LeftFace`
      and its `RightFace`. Now we’ll define those types and their monoids.
    '')

    (haskell (file-string ./Face.hs))

    (p ''
      Notice the subtle difference between how `mappend` is defined for each
      of these types. When we combine two faces, it matters whether we’re
      looking at them from the left or from the right.
    '')

    (p "This is what combining two left faces looks like:")

    (html ''
      <div class="water-monoid">
        <img src="${file-path ./left-face.png}">
      </div>
    '')

    (p ''
      The `emptyFace` and `overlapFaces` functions need to be polymorphic so
      we can use them for both the left or right face types. To make this easy,
      we can take advantage of the [`Coercible`](${coerce}) instances that
      newtypes get automatically, and define a `Face` as anything which can be
      converted back and forth to `Corners`.
    '')

    (haskell ''
      {-# LANGUAGE ConstraintKinds, FlexibleContexts #-}

      import Data.Coerce (Coercible, coerce)

      type Face a = (Coercible Corners a, Coercible a Corners)
    '')

    (p ''
      Now we can generically implement the face-combining logic, using
      `coerce` to convert `Corners` to `Face` and vice versa.
    '')

    (haskell (file-string ./overlapFaces.hs))

    (h2 "Water between two structures")

    (p ''
      The last nontrivial bit of coding is to compute the area of water
      between two opposing faces. Notice that the entire thing is a `fold`,
      and that here is where we use the `(*)` and `(-)` functions defined
      above.
    '')

    (haskell (file-string ./waterBetween.hs))

    (h2 "Folding it all together")

    (p ''
      We then define the construction of a structure with a single wall&hellip;
    '')

    (haskell ''
      structureSingleton :: Height -> Structure
      structureSingleton height = Structure face face mempty
        where
          face :: Face a => a
          face = coerce (Map.singleton height mempty :: Corners)
    '')

    (p ''
      And finally, chain all the walls together, using another fold!
    '')

    (haskell ''
      collectWater :: [Natural] -> Natural
      collectWater = coerce . sArea . foldMap (structureSingleton . coerce)
    '')

    ###################################################################

    (h2 "Notes")

    (p "You can see the complete working code on [GitHub](${github}).")

    (p ''
      In this post I don’t give much thought to efficiency; I haven’t bothered
      to benchmark this code, and I suspect its runtime may be quadratic.
    '')

    (p ''
      In case you are wondering *Does it really take this much code to write a
      Haskell program?* &mdash; No; what I’ve done here is overkill, just for
      fun and learning.
    '')

    (p ''
      If you are interested in optimization or brevity, check out
      [Chris Done’s](${chris-done}) work on the subject, which includes a
      very nice concise solution in Haskell using scans.
    '')

    (p ''
      To simplify explanation, I avoided mentioning
      [`Semigroup`](${semigroup}),  but it is something you should be aware
      of. Semigroup complicates things because in Haskell it has some
      historical baggage. Ideally the two classes would look like this:
    '')

    (haskell ''
      class Semigroup a where
          (<>) :: a -> a -> a

      class Semigroup a => Monoid a where
          mempty :: a
    '')

    (p ''
      However, because semigroups were added to Haskell after monoids,
      `Monoid` does not have this constraint, and it has a `mappend` method
      which is redundant to `(<>)`.
    '')

  ];
}
