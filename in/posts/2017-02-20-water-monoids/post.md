--------------------------------------------------------------------------------
title:    Water Monoids
date:     2017 Feb 20
slug:     water-monoids
css:      water.css
thumbnail: thumb.png
abstract: Solving that water-puddles problem,
          using as many monoids as possible.

twitter card:        summary_large_image
twitter image:       twitter.png
twitter description: Solving that water-puddles problem,
                     using as many monoids as possible.
--------------------------------------------------------------------------------

In the toy problem described by [*I Failed a Twitter Interview*][failed],
we are given a list of integers

  [failed]: https://medium.com/@bearsandsharks/i-failed-a-twitter-interview-52062fbb534b

<div style="text-align: center;">3, 2, 1, 4, 2</div>

representing the heights of walls

<table class="water unfilled">
  <tr>
    <td></td>
    <td></td>
    <td></td>
    <td class="wall"></td>
    <td></td>
  </tr>
  <tr>
    <td class="wall"></td>
    <td></td>
    <td></td>
    <td class="wall"></td>
    <td></td>
  </tr>
  <tr>
    <td class="wall"></td>
    <td class="wall"></td>
    <td></td>
    <td class="wall"></td>
    <td class="wall"></td>
  </tr>
  <tr>
    <td class="wall"></td>
    <td class="wall"></td>
    <td class="wall"></td>
    <td class="wall"></td>
    <td class="wall"></td>
  </tr>
  <tr>
    <td>3</td>
    <td>2</td>
    <td>1</td>
    <td>4</td>
    <td>2</td>
  </tr>
</table>

and we imagine pouring water onto this structure such that puddles accumulate in
the gaps.

<table class="water filled">
  <tr>
    <td></td>
    <td></td>
    <td></td>
    <td class="wall"></td>
    <td></td>
  </tr>
  <tr>
    <td class="wall"></td>
    <td class="water"></td>
    <td class="water"></td>
    <td class="wall"></td>
    <td></td>
  </tr>
  <tr>
    <td class="wall"></td>
    <td class="wall"></td>
    <td class="water"></td>
    <td class="wall"></td>
    <td class="wall"></td>
  </tr>
  <tr>
    <td class="wall"></td>
    <td class="wall"></td>
    <td class="wall"></td>
    <td class="wall"></td>
    <td class="wall"></td>
  </tr>
</table>

We are then asked: How much water is held by the structure?
(In the above example, the answer is 3).

## Monoids

I find myself thinking about [monoids][monoid] a *lot* lately.
Such a small, unassuming thing

  [monoid]: https://www.stackage.org/haddock/lts-8.0/base-4.9.1.0/Data-Monoid.html

```haskell
class Monoid a where

    mappend :: a -> a -> a  -- An associative operation,
                            -- also called (<>)

    mempty :: a             -- Identity of (<>)
```

yet it stirs the imagination. If your type has a monoid, you can chain the
elements of any collection together into a single element.

```haskell
fold :: (Foldable t, Monoid m) => t m -> m
```

The obvious examples may be data structures like strings, which are chained
together by concatenation.

```haskell
x = fold [ "one"
         , "two"
         , "three"
         ]
-- x = "one" <> "two" <> "three"
--   = "one" ++ "two" ++ "three"
--   = "onetwothree"
```

But a monoid operations aren't just ways to merge data structures. My favorite
example is [`Endo`][endo], which lets you chain together collections of
functions.

  [endo]: https://www.stackage.org/haddock/lts-8.0/base-4.9.1.0/Data-Monoid.html#t:Endo

```haskell
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
```

Monoids compose extraordinarily readily.
For example, if `a` and `b` have monoids, then the tuple `(a, b)` does as well.

```haskell
instance (Monoid a, Monoid b) => Monoid (a, b) where
    mempty = (mempty, mempty)
    (a1, b1) `mappend` (a2, b2) = (a1 <> a2, b1 <> b2)
```

## The water monoid

I woke up one morning recently with the thought that we can define a monoid for
these water-filled structures. When we place two of them side-by-side, they
combine like this:

<div class="water-monoid"><img src="${structure.png}"></div>

A structure is represented by

<ol>
  <li>
    <p>What its outer shape looks like</p>
    <ol>
      <li>From the left, and
      <li>From the right.
    </ol>
    <p>I call these its “faces”. Imagine the structure were as tall as you, and
    you were standing to the left or right of it; the face is comprised of the
    corners that are visible to you from that perspective.</p>
  <li>The amount of water it holds.
</ol>

```haskell
data Structure = Structure
    { sLeft  :: LeftFace   -- 1. The outer shape
    , sRight :: RightFace
    , sArea  :: Area       -- 2. How much water it holds
    }
```

When filled with water, the structure is convex, so the left and right faces
provide all the information we need to figure out what shape will result when
we combine two structures.

```haskell
instance Monoid Structure where

    mempty = Structure mempty mempty mempty

    mappend (Structure left right water)
            (Structure left' right' water') =
        Structure (left <> left')
                  (right <> right')
                  (water <> water' <> waterBetween right left')
```

Not only does `Structure` have a monoid, but so do all of its fields; so
`mempty` is defined quite simply as `Structure mempty mempty mempty`. The
definition of `mappend` is similarly straightforward, with the exception that we
also have to add in `waterBetween right left'` to include the water that puddles
in the new gap between the two structures.

## Arithmetic

Since there are no negative numbers in this problem, we'll be using the
`Natural` type, which represents nonnegative integers.

```haskell
import Numeric.Natural (Natural)
```

The arithmetic in Haskell's default prelude is a bit clumsy, so you want to be
precise, it can be nice to define your own. For example, `Natural` has an
instance of `Num`, which can get us into trouble because `(-)` is partial.

```haskell
λ> 1 - 2 :: Natural
*** Exception: arithmetic underflow
```

For the `Natural` type, I'd prefer to have the `(-)` function signify *absolute
difference*. Fortunately we can define our own subtraction class and implement
it however we want.

```haskell
class Subtraction a where
    (-) :: a -> a -> a
    infixl 6 -

instance Subtraction Natural where
    a - b | a >= b    = a Prelude.- b
          | otherwise = b Prelude.- a

instance Subtraction a => Subtraction (Sum a) where
    Sum a - Sum b = Sum (a - b)
```

Not all numbers are the same, so let's also define some types to assign meaning
to the specific sorts of quantities we're dealing with in this problem.

We wrap the numbers in `Sum` so that we can automatically derive monoid
instances that combine additively.

```haskell
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

newtype Width = Width (Sum Natural)
    deriving (Eq, Monoid, Ord, Semigroup, Subtraction)

newtype Height = Height (Sum Natural)
    deriving (Eq, Monoid, Ord, Semigroup, Subtraction)

newtype Area = Area (Sum Natural)
    deriving (Eq, Monoid, Ord, Semigroup, Subtraction)
```

We'll need to multiply a `Width` by a `Height` to get an `Area`. Here we run
into another limitation of `Num`: It assume we're only multiplying values of the
same type.

```haskell
(*) :: Num a => a -> a -> a
```

So again let's ignore the standard math and invent our own. Since this
hetereogeneous multiplication involves more than one type, we need the language
extension that allows multi-parameter type classes.

```haskell
{-# LANGUAGE MultiParamTypeClasses #-}

class Multiplication a b c where
    (*) :: a -> b -> c
    infixl 7 *

instance Multiplication Width Height Area where
    Width w * Height h = Area (w Prelude.* h)

instance Multiplication Height Width Area where
    Height h * Width w = Area (w Prelude.* h)
```

## Faces

Recall that we defined a structure's shape in terms of its `LeftFace` and its
`RightFace`. Now we'll define those types and their monoids.

```haskell
import Data.Map (Map)
import qualified Data.Map as Map

type Corners = Map Height Width

newtype LeftFace = LeftFace Corners

newtype RightFace = RightFace Corners

instance Monoid LeftFace where
    mempty = emptyFace
    mappend near far = overlapFaces near far

instance Monoid RightFace where
    mempty = emptyFace
    mappend far near = overlapFaces near far
```

Notice the subtle difference between how `mappend` is defined for each of these
types. When we combine two faces, it matters whether we're looking at them from
the left or from the right.

This is what combining two left faces looks like:

<div class="water-monoid"><img src="${left-face.png}"></div>

The `emptyFace` and `overlapFaces` functions need to be polymorphic so we can
use them for both the left or right face types. To make this easy, we can take
advantage of the [`Coercible`][coerce] instances that newtypes get
automatically, and define a `Face` as anything which can be converted back and
forth to `Corners`.

  [coerce]: https://www.stackage.org/haddock/lts-8.1/base-4.9.1.0/Data-Coerce.html

```haskell
{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}

import Data.Coerce (Coercible, coerce)

type Face a = (Coercible Corners a, Coercible a Corners)
```

Now we can generically implement the face-combining logic, using `coerce` to
convert `Corners` to `Face` and vice versa.


```haskell
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
```

## Water between two structures

The last nontrivial bit of coding is to compute the area of water between two
opposing faces. Notice that the entire thing is a `fold`, and that here is where
we use the `(*)` and `(-)` functions defined above.

```haskell
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
```

## Folding it all together

We then define the construction of a structure with a single wall&hellip;

```haskell
structureSingleton :: Height -> Structure
structureSingleton height = Structure face face mempty
  where
    face :: Face a => a
    face = coerce (Map.singleton height mempty :: Corners)
```

And finally, chain all the walls together, using another fold!

```haskell
collectWater :: [Natural] -> Natural
collectWater = coerce . sArea . foldMap (structureSingleton . coerce)
```

## Notes

You can see the complete working code on [GitHub][github].

  [github]: https://github.com/chris-martin/rain-water/

In this post I don't give much thought to efficiency; I haven't bothered to
benchmark this code, and I suspect its runtime may be quadratic.

In case you are wondering *Does it really take this much code to write a Haskell
program?* &mdash; No; what I've done here is overkill, just for fun and
learning.

If you are interested in optimization or brevity, check out
[Chris Done's][chris-done] work on the subject, which includes a very nice
concise solution in Haskell using scans.

  [chris-done]: http://chrisdone.com/posts/twitter-problem-loeb

To simplify explanation, I avoided mentioning [`Semigroup`][semigroup], but it
is something you should be aware of. Semigroup complicates things because in
Haskell it has some historical baggage. Ideally the two classes would look like
this:

  [semigroup]: https://www.stackage.org/haddock/lts-8.0/base-4.9.1.0/Data-Semigroup.html

```haskell
class Semigroup a where
    (<>) :: a -> a -> a

class Semigroup a => Monoid a where
    mempty :: a
```

However, because semigroups were added to Haskell after monoids, `Monoid` does
not have this constraint, and it has a `mappend` method which is redundant to `(<>)`.
