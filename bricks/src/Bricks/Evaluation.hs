{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}

{- |

This module lets you evaluate Bricks expressions. First 'expression'to'term's
converts the abstract syntax tree ('Expression') into an enriched version of the
lambda calculus ('Term'). Then we perform /graph reduction/, repeatedly applying
simplifications until we arrive at an irreducible term.

When we substitute an argument into a lambda body to perform beta-conversion, we
do so by substituting a 'Pointer' of the argument rather than the term itself.
This gives rise to /sharing/, thus turning the tree into a general graph, and
helps avoid reducing the same expression more than once.

= /The Implementation of Functional Programming Languages/

The design of Bricks evaluation is in large part based on Simon Peyton Jones's
1987 book
<https://www.microsoft.com/en-us/research/publication/the-implementation-of-functional-programming-languages/ The Implementation of Functional Programming Languages>.
In attempt to keep the Bricks API documentation mostly self-contained, we avoid
making frequent references to this work throughout. Instead, here we give a list
of some important connections to the book:

  - The rationale for immediately converting the AST into another data structure
    rather than performing any transformations directly on the AST comes from
    page 38.

  - A Bricks function defined using a dict pattern turns into a
    "pattern-matching lambda abstraction"; this term is introduced on page 61.

  - Page 185 introduces the style of drawing ASTs to which the '/@\' operator
    alludes.

  - /Pointer substitution/ is described on page 208.

  - The implementation of 'term'substitute' closely follows the description of
    /Instantiate/, page 210.

  - Page 20 introduces the name capture problem. Pages 199 and 210 discuss how
    we avoid it by only reducing the top-level redex, which has no free
    variables.

  - On page 233 starts the discussion of how letrec expressions are instantiated
    as cyclic graphcs.

-}
module Bricks.Evaluation where

-- Bricks
import Bricks.BuiltinFunctions
import Bricks.Term
import Bricks.Type

-- Bricks internal
import           Bricks.Internal.Monad
import           Bricks.Internal.Prelude
import           Bricks.Internal.Text    (Text)
import qualified Bricks.Internal.Text    as Text
import           Bricks.Internal.Map (Map)
import qualified Bricks.Internal.Map as Map

-- Containers
import           Data.Set (Set)
import qualified Data.Set as Set

-- Base
import Data.Dynamic  (fromDynamic, toDyn)
import Data.IORef
import Data.Typeable (Typeable)
import Prelude       (error)
import System.IO     (IO)

newtype Eval a = Eval { unEval :: ExceptT Bottom IO a }
  deriving (Functor, Applicative, Monad, MonadError Bottom, MonadIO)

instance MonadEval Eval
  where

    reduce'term :: Term -> Eval Term
    reduce'term =
      \case
        Term'Pointer p ->
          readTermPtr p >>= reduce'term

        t@(Term'Data _ _) -> pure t
        t@(Term'Function _) -> pure t
        t@(Term'Lambda _ _) -> pure t
        t@(Term'List _) -> pure t
        t@(Term'Dict _) -> pure t
        t@(Term'Dict'ReducedKeys _) -> pure t
        Term'Var x ->
          bottom . Bottom $ "Free variable: " <> x

        Term'LetRec map body -> do
          -- Create a pointer for each of the let bindings
          map' <- traverse create'pointer map :: Eval (Map Text Term)
          -- Substitute each of the bindings into each of the others
          traverse_ (instantiate'many map') map'
          -- Substitute all of the bindings into the body
          instantiate'many map' body >>= reduce'term

        Term'Apply f value ->
          reduce'term f >>= \case

            Term'Function f' ->
              f' value >>= reduce'term

            -- The function is a lambda, so it can be applied to an argument.
            Term'Lambda pattern body ->
              case pattern of

                -- A simple single-variable pattern
                TermPattern'Simple var ->
                  create'pointer value >>= \value'ptr ->
                    instantiate'one var value'ptr body

                -- A dict pattern
                TermPattern'Dict vars ->
                  reduce'dict'keys value >>= \values ->
                    case Map.exactKeys values vars of
                      Left missingKeys -> bottom . Bottom $
                        "Dict lookup failed: " <>
                        Text.show (Set.toList missingKeys)
                      Right values' -> instantiate'many values' body

            t ->
              termTypeName t >>= \n ->
                bottom . Bottom $ "Expected function, got " <> n

    reduce'dict'keys :: Term -> Eval (Map Text Term)
    reduce'dict'keys = reduce'term >=> \case
      t@(Term'Dict d) ->
        undefined
      Term'Dict'ReducedKeys x ->
        pure x
      x ->
        termTypeName x >>= \n' ->
          bottom . Bottom $ "Expected dict, got " <> n'

does'termPattern'bind :: Text -> TermPattern -> Bool
does'termPattern'bind n = \case
  TermPattern'Simple x  -> n == x
  TermPattern'Dict   xs -> n `Set.member` xs

{- | @instantiate var value body@ produces a copy of the term @body@,
substituting @value@ for free occurrences of @var@. -}

instantiate'one
  :: forall m. MonadEval m
  => Text  -- ^ @var@   - Variable name
  -> Term  -- ^ @value@ - The argument being substituted. We assume that this
           --             term has no free variables; or else we will suffer
           --             the /name capture problem/.
  -> Term  -- ^ @body@  - The term being copied ("instantiated")
  -> m Term

-- The numbered comments within this definition are nearly verbatim from
-- page 210 of /The Implementation of Functional Programming Languages/.
-- There SPJ denotes this construction as body[value/var].

instantiate'one var value =
  go
  where
    go :: Term -> m Term
    go body = case body of
      Term'Var x ->

        -- 1. If /body/ is a variable x and /var/ = x, then return /value/
        --    (here we substitute /value/ for an occurrence of /var/).
        if x == var then pure value

        -- 2. If /body/ is a variable x and /var/ ≠ x, then return /body/.
        else pure body

      -- 3. If /body/ is a constant or built-in function, then return /body/.
      Term'Data _ _   -> pure body -- constant
      Term'Function _ -> pure body -- built-in function

      -- 4. If /body/ is an application (e1 e2), then return the application
      --    (e1[value/var] e2[value/var]).
      Term'Apply a b -> Term'Apply <$> go a <*> go b

      Term'Lambda a b ->
        -- 5. If /body/ is a lambda abstraction λx.E and /var/ = x, then return
        --    /body/ - the new lambda abstraction binds /var/ anew, so no
        --    substitutions should occur inside it, and hence we can avoid
        --    instantiating it altogether.
        if does'termPattern'bind var a then pure body

        -- 6. If /body/ is a lambda abstraction λx.E and /var/ ≠ x, then return
        --    λx.E[value/var] - we must instantiate the body in case there are
        --    any free occurrences of /var/ inside it.
        else Term'Lambda a <$> go b

      Term'LetRec a b ->
        -- The same reasoning as (5) and (6) - If the let expression binds
        -- /var/, then we do nothing. Otherwise we substitute everywhere.
        if Map.member var a then pure body
        else Term'LetRec <$> traverse go a <*> go b

      Term'List xs -> Term'List <$> traverse go xs

      Term'Dict x -> Term'Dict <$> undefined

      Term'Dict'ReducedKeys x -> Term'Dict'ReducedKeys <$> traverse go x

      Term'Pointer p -> go =<< readTermPtr p
        -- todo - let this function return whether it made any substitutions.
        -- If it didn't, then just return the pointer.

instantiate'many
  :: forall m. MonadEval m
  => Map Text Term -- ^ @values@
  -> Term          -- ^ @body@
  -> m Term
instantiate'many values body =
  foldr f (pure body) (Map.toList values)
  where
    f :: (Text, Term) -> m Term -> m Term
    f (var, value) bod = instantiate'one var value =<< bod

reduce'to'type :: Typeable a => Type a -> Term -> IO (Either Bottom a)
reduce'to'type typ =
  (reduce'term >=> cast'data typ) >>> unEval >>> runExceptT

reduce'to'type'or'throw :: (HasCallStack, Typeable a) => Type a -> Term -> IO a
reduce'to'type'or'throw typ =
  reduce'to'type typ >=>
  either (error . Text.unpack . displayBottom) pure

{-
        -- The function is a lambda with a dict pattern.
        Term'Lambda'DictPattern dp body ->

          -- Reduce the argument, and require it to be a dict.
          reduce'term value >>= \case
            Term'Dict dict -> undefined
            x -> termTypeName x >>= \n -> evalError ("Expected dict, got " <> n)
-}

{-
    t@(Term'Pointer p) ->
      (readTermPtr p >>= reduce >>= writeTermPtr p) $> t
    x -> pure x
-}

{-
  case t of

    Term'Apply f x ->
      create'pointer x >>= \value ->
      reduce'term f >>= \case
        Term'Lambda var body -> term'substitute var value body
        Term'Lambda'DictPattern pattern body ->
          -- Reduce the argument, and require it to be a dict.
          reduce'term value >>= \case
            Term'Dict dict -> undefined
              -- Reduce all of the dict's keys.

            _ -> pure $ Term'Error "Expected dict, got something else"

          -- todo:
          --  -
          --  - fail if there are extra keys and no ellipsis
          --  - fail if there are missing keys
          --  - perform substitutions
        _ -> pure $ Term'Error "Expected function, got something else"

    Term'Var x -> pure $ Term'Error $
      "Unbound variable: " <> render'strStatic'unquotedIfPossible x

    Term'Pointer (Pointer ref) -> do
      t' <- readIORef ref
      t'' <- reduce'term t'
      writeIORef ref t''
      pure t''

    _ -> pure t

-}
