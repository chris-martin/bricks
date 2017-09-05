{ file-path, html-tags, html, code, markdown }:

{
  title = "Elephant variable reassignment";
  date  = "2017 Aug 27";
  slug  = "elephant-variable-reassignment";

  thumbnail = file-path ./thumb.png;

  abstract = ''
    An example using the `(.=)` operator from `microlens-mtl` to write
    Haskell code that resembles reassignment of mutable variables.
  '';

  twitter = {
    card = "summary_large_image";
    image = file-path ./twitter.png;
    description = ''
      Using (.=) from microlens-mtl to write Haskell that resembles
      reassignment of mutable variables.
    '';
  };

  body = let
    stackoverflow = "https://stackoverflow.com/questions/43525193";
    microlens-mtl = "https://hackage.haskell.org/package/microlens-mtl";
    github        = "https://github.com/chris-martin/elephant";

    inherit (html-tags) blockquote ol;

    p  = x: html-tags.p  (markdown x);
    li = x: html-tags.li (markdown x);

    haskell = code { language = "haskell"; };
    
  in [

    (p ''
      This demonstration was inspired by a
      [Stack Overflow question](${stackoverflow}) which asks:
    '')

    (blockquote [
      (p "How can I re-assign a variable in a function in Haskell?")
      (p "For example,")
      (haskell ''
        elephant = 0

        setElephant x =
          elephant = x
      '')
    ])

    (p ''
      The short answer is that we don't have a language-level mechanism to
      model that sort of thing in Haskell, and it's generally not what you
      want anyway.
    '')

    (p ''
      Here I give a *long answer*, demonstrating how to use lenses and the
      state monad to produce something that looks like a direct translation
      of the imperative-programming concept of reassigning a variable.
    '')

    (p ''
      We will be using the `(.=)` operator (which also has an alias called
      `assign`) from the [`microlens-mtl`](${microlens-mtl}) library.
    '')

    (haskell "(.=) :: MonadState s m => ASetter s s a b -> b -> m ()")

    (p ''
      The first argument (having type `ASetter`) is the lens which specifies
      *what part of* the state to update, and the second is the new value to
      set at the position specified by the lens.
    '')

    (h2 "The code")

    (haskell ''
      {-# LANGUAGE ConstraintKinds, InstanceSigs, PackageImports #-}

      module Elephant where
    '')

    (p ''
      I'm using the `PackageImports` extension just for clarity, to show
      what packages these imported modules are coming from.
    '')

    (haskell ''
      import "base"          Control.Monad.IO.Class    ( MonadIO (liftIO) )
      import "microlens"     Lens.Micro                ( Lens', lens )
      import "microlens-mtl" Lens.Micro.Mtl            ( (.=), use )
      import "mtl"           Control.Monad.State.Class ( MonadState )
      import "transformers"  Control.Monad.Trans.State ( StateT (runStateT) )
    '')

    (p "Let's assume that elephants can be modeled by integers.")

    (haskell "type Elephant = Integer")

    (p ''
      We're going to want our program state to have an elephant, so first
      we'll define what it means to have an elephant. A lens captures this
      notion nicely.
    '')

    (haskell ''
      class HasElephant a
        where
          elephant :: Lens' a Elephant
    '')

    (p ''We'll say that a monad “has elephant state” if'')

    (ol [
      (li "it has state, and")
      (li "its state has an elephant.")
    ])

    (haskell ''
      type HasElephantState s m =
        (MonadState s m, HasElephant s)
    '')

    (p ''
      (The `HasElephantState` type alias is what we need the
      `ConstraintKinds` extension for.)
    '')

    (p ''
      Now we can define the action that the question asks for. It operates in
      some monad that has elephant state, and it assigns a value for the
      elephant in that state.
    '')

    (haskell ''
      setElephant
        :: HasElephantState s m
        => Elephant -> m ()
      setElephant x =
        elephant .= x
    '')

    (p ''
      Let's also define an action that prints the elephant. In addition to
      elephant state, this context also requires I/O.
    '')

    (haskell ''
      printElephant
        :: (HasElephantState s m, MonadIO m)
        => m ()
      printElephant =
        do
          e <- use elephant
          liftIO (putStrLn ("The current elephant is " ++ show e))
    '')

    (p ''
      The African forest elephant (Loxodonta cyclotis) is a forest-dwelling
      species of elephant found in the Congo Basin. For this demonstration
      we will use `Congo` as an example of something that has an elephant.
    '')

    (haskell ''
      data Congo = Congo
        { congoElephant :: Elephant }
    '')

    (p ''
      We must define the way in which `Congo` has an elephant. The lens
      defines how to get an elephant from the Congo, and how to put an
      elephant into the Congo.
    '')

    (haskell ''
      instance HasElephant Congo
        where
          elephant :: Lens' Congo Elephant
          elephant =
            lens
              congoElephant
              (\a b -> a{ congoElephant = b })
    '')

    (p ''
      You'll often see people use Template Haskell or generics to generate
      lenses automatically, but here we defined the lens manually.
    '')

    (p ''
      Now we can write a program. It has the same context as above: elephant
      state and I/O. Our program prints the value of `elephant`, then changes
      the value of `elephant`, then prints it again.
    '')

    (haskell ''
      main'
        :: (HasElephantState s m, MonadIO m)
        => m ()
      main' =
        do
          printElephant
          setElephant 2
          printElephant
    '')

    (p ''
      Then we can convert this program to `IO`. We specialize `main'` as
      `StateT Congo IO ()`, and we use a Congo with elephant zero as the
      program's initial state.
    '')

    (haskell ''
      main :: IO ()
      main =
        let
          program =
            main' :: StateT Congo IO ()
          initialState =
            Congo{ congoElephant = 0 }
        in
          () <$ runStateT program initialState
    '')

    (p "When we run this program, its output is:")

    (code {} ''
      The current elephant is 0
      The current elephant is 2
    '')

    (html ''<hr style="margin: 2em 0;">'')

    (p "*You can find this code on [GitHub](${github}).*")

  ];
}
