{ code, html-tags, markdown }:

{
  title = "Some Applicative Functors";
  date  = "2017 Mar 12";
  slug  = "some-applicative-functors";

  abstract = ''
    Everybody’s favorite applicatives, and their semigroups.
  '';

  body = let
    haskell = code { language = "haskell"; };

    p = x: html-tags (markdown p);

    inherit (html-tags) h2 br;

    github  = "https://github.com/chris-martin/acme-functors";
    hackage = "https://hackage.haskell.org/package/acme-functors";

  in [
    (p ''
      Types are great. Lifting them into some sort of applicative functor
      makes them even better. This post is an homage to our favorite
      applicatives, and to the semigroups with which they are instrinsically
      connected.
    '')

    #######################################################################

    (h2 "Lifted-but-why")

    (p ''
      **`LiftedButWhy`** is a boring functor that just has one value and
      no other structure or interesting properties.
    '')

    (haskell ''
      data LiftedButWhy a =

        -- A value that has been lifted
        -- for some damned reason
        LiftedButWhy a

        deriving (Eq, Functor, Show)
    '')

    (p ''
      … Okay, to be honest, this one is *nobody’s* favorite, but it is
      included here for completeness.
    '')

    (haskell ''
      instance Applicative LiftedButWhy where

        pure = LiftedButWhy

        LiftedButWhy f <*> LiftedButWhy a =
          LiftedButWhy (f a)
    '')

    br

    (haskell ''
      instance Monad LiftedButWhy where

        LiftedButWhy a >>= f = f a
    '')

    br

    (haskell ''
      instance Semigroup a =>
          Semigroup (LiftedButWhy a) where

        LiftedButWhy x <> LiftedButWhy y =
          LiftedButWhy (x <> y)
    '')

    br

    (haskell ''
      instance Monoid a =>
          Monoid (LiftedButWhy a) where

        mempty = LiftedButWhy mempty
    '')

    #######################################################################

    (h2 "Or-not")

    (p ''
      **`OrNot`** is somehow slightly more interesting than `LiftedButWhy`,
      even though it may actually contain *less*. Instead of a value, there
      might *not* be a value.
    '')

    (p ''
      When you combine stuff with `(<*>)` or `(<>)`, all of the values need
      to be present. If any of them are absent, the whole expression evaluates
      to `Nope`.
    '')

    (haskell ''
      data OrNot a =
        ActuallyYes a -- Some normal value
        | Nope        -- Chuck Testa
        deriving (Eq, Functor, Show)
    '')

    (p ''
      If you have a function `f` that might not actually be there, and a
      value `a` that might not actually be there, lifted application `(<*>)`
      gives you `f a` only if both of them are actually there.
    '')

    (haskell ''
      instance Applicative OrNot where

        pure = ActuallyYes

        ActuallyYes f <*> ActuallyYes a =
          ActuallyYes (f a)
        _ <*> _ = Nope
    '')

    br

    (haskell ''
      instance Monad OrNot where

        ActuallyYes a >>= f = f a
        Nope          >>= _ = Nope
    '')

    (p ''
      If you have value `a` that may not actually be there, and another
      value `a'` that might not actually be there, the lifted semigroup
      operation `(<>)` gives you `a <> a'` only if both of them are
      actually there.
    '')

    (haskell ''
      instance Semigroup a =>
          Semigroup (OrNot a) where

        ActuallyYes a <> ActuallyYes a' =
          ActuallyYes (a <> a')
        _ <> _ = Nope
    '')

    br

    (haskell ''
      instance Monoid a =>
          Monoid (OrNot a) where

        mempty = ActuallyYes mempty
    '')

    #######################################################################

    (h2 "Two")

    (p "**`Two`** is *two* values. Yep. Just two values.")

    (haskell ''
      data Two a = Two
        { firstOfTwo  :: a -- One value
        , secondOfTwo :: a -- Another value
        } deriving (Eq, Functor, Show)
    '')

    (p ''
      If you have two functions `f` and `g` and two values `a` and `a'`, then
      you can apply them with `(<*>)` to get two results `f a` and `g a'`.
    '')

    (haskell ''
      instance Applicative Two where

        pure a = Two a a

        Two f g <*> Two a a' = Two (f a) (g a')
    '')

    br

    (haskell ''
      instance Semigroup a =>
          Semigroup (Two a) where

        Two x y <> Two x' y' =
          Two (x <> x') (y <> y')
    '')

    br

    (haskell ''
      instance Monoid a =>
          Monoid (Two a) where

        mempty = Two mempty mempty
    '')

    #######################################################################

    (h2 "Any-number-of")

    (p ''
      **`AnyNumberOf`** starts to get exciting. Any number of values you want.
      Zero … one … two … three … four … five … The possibilities are *truly*
      endless.
    '')

    (haskell ''
      data AnyNumberOf a =

        -- One value, and maybe even more after that!
        OneAndMaybeMore a (AnyNumberOf a)

        -- Oh. Well this is less fun.
        | ActuallyNone

        deriving (Eq, Functor, Show)
    '')

    (p "Here’s an alias for `OneAndMaybeMore` which provides some brevity:")

    (haskell ''
      (~~) :: a -> AnyNumberOf a -> AnyNumberOf a
      (~~) = OneAndMaybeMore
      infixr 5 ~~
    '')

    (p ''
      You can use the applicative functor to apply any number of functions
      to any number of arguments.
    '')

    (haskell ''
      instance Applicative AnyNumberOf where

        pure a = OneAndMaybeMore a ActuallyNone

        OneAndMaybeMore f fs <*> OneAndMaybeMore x xs =
          OneAndMaybeMore (f x) (fs <*> xs)
        _ <*> _ = ActuallyNone
    '')

    (p "Example:")

    (haskell ''
         ((+ 1) ~~ (* 2) ~~       ActuallyNone)
      <*> (  1  ~~    6  ~~ 37 ~~ ActuallyNone)
       =  (  7  ~~   12  ~~       ActuallyNone)
    '')

    (p ''
      This example demonstrates how when there are more arguments than
      functions, any excess arguments (in this case, the `37`) are ignored.
    '')

    (p ''
      The operation of combining some number of `a` with some other number of
      `a` is sometimes referred to as *zipping*.
    '')

    (haskell ''
      instance Semigroup a =>
          Semigroup (AnyNumberOf a) where

        OneAndMaybeMore x xs <> OneAndMaybeMore y ys =
          OneAndMaybeMore (x <> y) (xs <> ys)
        _ <> _ = ActuallyNone
    '')

    br

    (haskell ''
      instance Monoid a =>
          Monoid (AnyNumberOf a) where

        mempty = mempty ~~ mempty
    '')

    #######################################################################

    (h2 "One-or-more")

    (p ''
      **`OneOrMore`** is more restrictive than `AnyNumberOf`, yet somehow
      actually *more* interesting, because it excludes that dull situation
      where there aren’t any values at all.
    '')

    (haskell ''
      data OneOrMore a = OneOrMore

        -- Definitely at least this one
        { theFirstOfMany :: a

        -- And perhaps others
        , possiblyMore :: AnyNumberOf a

        } deriving (Eq, Functor, Show)
    '')

    br

    (haskell ''
      instance Applicative OneOrMore where

        pure a = OneOrMore a ActuallyNone

        OneOrMore f fs <*> OneOrMore x xs =
          OneOrMore (f x) (fs <*> xs)
    '')

    br

    (haskell ''
      instance Semigroup a =>
          Semigroup (OneOrMore a) where

        OneOrMore a more <> OneOrMore a' more' =
          OneOrMore a (more <> OneAndMaybeMore a' more')
    '')

    br

    (haskell ''
      instance Monoid a =>
          Monoid (OneOrMore a) where

        mempty = OneOrMore mempty ActuallyNone
    '')

    #######################################################################

    (h2 "Also-extra-thing")

    (p ''
      **`Also extraThing`** is a functor in which each value has an
      `extraThing` of some other type that tags along with it.
    '')

    (haskell ''
      data (Also extraThing) a = Also

        -- A value
        { withoutExtraThing :: a

        -- An additional thing that tags along
        , theExtraThing :: extraThing

        } deriving (Eq, Functor, Show)
    '')

    (p ''
      Dragging the `extraThing` along can be a bit of a burden. It prevents
      `Also extraThing` from being an applicative functor — unless the
      `extraThing` can pull its weight by bringing a monoid to the table.
    '')

    (haskell ''
      instance Monoid extraThing =>
          Applicative (Also extraThing) where

        pure = (`Also` mempty)

        (f `Also` extra1) <*> (a `Also` extra2) =
          f a
          `Also` (extra1 <> extra2)
    '')

    br

    (haskell ''
      instance (Semigroup extraThing, Semigroup a) =>
          Semigroup ((Also extraThing) a) where

        (a `Also` extra1) <> (a' `Also` extra2) =
          (a <> a')
          `Also` (extra1 <> extra2)
    '')

    br

    (haskell ''
      instance (Monoid extraThing, Monoid a) =>
          Monoid ((Also extraThing) a) where

        mempty = Also mempty mempty
    '')

    #######################################################################

    (h2 "Or-instead-other-thing")

    (p ''
      **`OrInstead otherThing`** is a functor in which, instead of having a
      value, can actually just have some totally unrelated `otherThing`
      instead.
    '')

    (p ''
      When you combine stuff with `(<*>)` or `(<>)`, all of the values need
      to be present. If any of them are the `otherThing` instead, then the
      whole expression evaluates to the combination of the `otherThing`s.
    '')

    (haskell ''
      data (OrInstead otherThing) a =

        -- A normal value
        NotInstead a

        -- Some totally unrelated other thing
        | Instead otherThing

        deriving (Eq, Functor, Show)
    '')

    (p ''
      The possibility of having an `otherThing` obstructs this functor’s
      ability to be applicative, much like the extra thing in `Also extraThing`
      does. In this case, since we do not need an empty value for the
      `otherThing`, it needs only a semigroup to be in compliance.
    '')

    (haskell ''
      instance Semigroup otherThing =>
          Applicative (OrInstead otherThing) where

        pure = NotInstead

        NotInstead f <*> NotInstead a =
          NotInstead (f a)
        Instead other1 <*> Instead other2 =
          Instead (other1 <> other2)
        Instead other <*> _ = Instead other
        _ <*> Instead other = Instead other
    '')

    br

    (haskell ''
      instance (Semigroup otherThing, Semigroup a) =>
          Semigroup ((OrInstead otherThing) a) where

        NotInstead a <> NotInstead a' =
          NotInstead (a <> a')
        Instead other1 <> Instead other2 =
          Instead (other1 <> other2)
        Instead other <> _ = Instead other
        _ <> Instead other = Instead other
    '')

    br

    (haskell ''
      instance (Semigroup otherThing, Monoid a) =>
          Monoid ((OrInstead otherThing) a) where

        mempty = NotInstead mempty
    '')

    #######################################################################

    (h2 "Or-instead-first-thing")

    (p ''
      **`OrInsteadFirst otherThing`** looks a lot like `OrInstead otherThing`,
      but it manages to always be an applicative functor — and even a monad
      too — by handling the `otherThing`s a bit more hamfistedly.
    '')

    (p ''
      When you combine stuff with `(<*>)` or `(<>)`, all of the values need to
      be present. If any of them are the `otherThing` instead, then the whole
      expression evaluates to the *first* `otherThing` encountered, ignoring
      any additional `otherThing`s that may subsequently pop up.
    '')

    (haskell ''
      data (OrInsteadFirst otherThing) a =

        -- A normal value
        NotInsteadFirst a

        -- Some totally unrelated other thing
        | InsteadFirst otherThing

        deriving (Eq, Functor, Show)
    '')

    br

    (haskell ''
      instance Applicative (OrInsteadFirst otherThing) where

        pure = NotInsteadFirst

        NotInsteadFirst f  <*> NotInsteadFirst a =
          NotInsteadFirst (f a)
        InsteadFirst other <*> _ = InsteadFirst other
        _ <*> InsteadFirst other = InsteadFirst other
    '')

    br

    (haskell ''
      instance Monad (OrInsteadFirst otherThing) where

        InsteadFirst other >>= _ = InsteadFirst other
        NotInsteadFirst a  >>= f = f a
    '')

    br

    (haskell ''
      instance (Semigroup otherThing, Semigroup a) =>
          Semigroup ((OrInsteadFirst otherThing) a) where

        NotInsteadFirst a <> NotInsteadFirst a' =
          NotInsteadFirst (a <> a')
        InsteadFirst other <> _ = InsteadFirst other
        _ <> InsteadFirst other = InsteadFirst other
    '')

    br

    (haskell ''
      instance (Semigroup otherThing, Monoid a) =>
          Monoid ((OrInsteadFirst otherThing) a) where

        mempty = NotInsteadFirst mempty
    '')

    #######################################################################

    (h2 "Determined-by-parameter")

    (p ''
      **`DeterminedBy parameter`** is a value that… well, we’re not really
      sure what it is. We’ll find out once a `parameter` is provided.
    '')

    (p ''
      The mechanism for deciding *how* the value is determined from the
      `parameter` is opaque; all you can do is test it with different
      parameters and see what results. There aren’t even `Eq` or `Show`
      instances, which is annoying.
    '')

    (haskell ''
      data DeterminedBy parameter a =
        Determination ((->) parameter a)
        deriving Functor
    '')

    br

    (haskell ''
      instance Applicative (DeterminedBy parameter) where

        pure a = Determination (\_ -> a)

        Determination f <*> Determination a =
          Determination (\x -> f x (a x))
    '')

    br

    (haskell ''
      instance Monad (DeterminedBy parameter) where

        Determination fa >>= ff =
          Determination (\x ->
            let Determination f = ff (fa x)
            in  f x)
    '')

    br

    (haskell ''
      instance Semigroup a =>
          Semigroup ((DeterminedBy parameter) a) where

        Determination f <> Determination g =
          Determination (\x -> f x <> g x)
    '')

    br

    (haskell ''
      instance Monoid a =>
          Monoid ((DeterminedBy parameter) a) where

        mempty = Determination (\_ -> mempty)
    '')

    br br br br br br

    #######################################################################

    (h2 "Footnotes")

    (p ''`LiftedButWhy` is `Identity`.'')
    (p ''`OrNot` is `Maybe`, but with the monoid that is appropriate for its applicative.'')
    (p ''`Two` doesn’t have an analogue in any standard library as far as I know.'')
    (p ''`AnyNumberOf` is `ZipList`, with the appropriate semigroup added.'')
    (p ''`OneOrMore` is like `NonEmpty`, but with instances that match `ZipList`.'')
    (p ''`Also` is `(,)` — also known as the 2-tuple.'')
    (p ''`OrInstead` is `AccValidation` from the *validation* package.'')
    (p ''`OrInsteadFirst` is `Either`.'')
    (p ''`DeterminedBy` is `(->)`, also known as a *function*, whose monad is also known as `Reader`.'')

    (p ''
      This text is also available on [GitHub](${github}) and as
      [acme-functors](${hackage}) on Hackage.
    '')

  ];
}
