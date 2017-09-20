{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}

{- | This module lets you evaluate Bricks expressions. First
'expression'to'term's converts the abstract syntax tree ('Expression')
nto an enriched version of the lambda calculus ('Term'). Then we perform
/graph reduction/, repeatedly applying simplifications until we arrive at
an irreducible term.

When we substitute an argument into a lambda body to perform beta-conversion,
we do so by substituting a 'Pointer' of the argument rather than the term
itself. This gives rise to /sharing/, thus turning the tree into a general
graph, and helps avoid reducing the same expression more than once.

= /The Implementation of Functional Programming Languages/

This module is in large part based on Simon Peyton Jones's 1987 book
<https://www.microsoft.com/en-us/research/publication/the-implementation-of-functional-programming-languages/ The Implementation of Functional Programming Languages>.
In attempt to keep the Bricks API documentation mostly self-contained, we avoid
making frequent references to this work throughout. Instead, here we give a
list of some important connections to the book:

  - A rationale for immediately converting the AST into another data
    structure rather than performing transformations directly on the
    AST comes from page 38.

  - /Pointer substitution/ is described on page 208.

  - The implementation of 'term'substitute' closely follows the
    description of /Instantiate/, page 210.

-}
module Bricks.Evaluation where

-- Bricks
import Bricks.Expression
import Bricks.Rendering
import Bricks.UnquotedString

-- Bricks internal
import           Bricks.Internal.Prelude
import           Bricks.Internal.Seq     (Seq)
import qualified Bricks.Internal.Seq     as Seq
import           Bricks.Internal.Text    (Text)
import qualified Bricks.Internal.Text    as Text

-- Containers
import           Data.Map (Map)
import qualified Data.Map as Map

-- Base
import Data.IORef
import System.IO  (IO)

data Term a
  = Term'Data a
  | Term'Function (Function a)
  | Term'Lambda Text (Term a)
  | Term'Lambda'DictPattern (Term'DictPattern a) (Term a)
  | Term'Str Text
  | Term'List (Seq (Term a))
  | Term'Dict (DictTerm a)
  | Term'Var Text
  | Term'Apply (Term a) (Term a)
  | Term'Pointer (Pointer a)
  | Term'Error Text

create'pointer :: Term a -> IO (Term a)
create'pointer x = case x of
  Term'Pointer _ -> pure x  -- The term is already a pointer, nothing to do
  _              -> Term'Pointer . Pointer <$> newIORef x

(/@\) :: Term a -> Term a -> Term a
(/@\) = Term'Apply
infixl /@\

data Term'DictPattern a =
  Term'DictPattern
    { term'dictPattern'items :: Map Text (Maybe (Term a))
    , term'dictPattern'ellipsis :: Bool
    }

data DictTerm a =
  DictTerm
    { dictTerm'map :: Map Text (Term a)
    , dictTerm'seq :: Seq (Term a, Term a)
    }

instance Semigroup (DictTerm a)
  where
    DictTerm a1 b1 <> DictTerm a2 b2 = DictTerm (a1 <> a2) (b1 <> b2)

instance Monoid (DictTerm a)
  where
    mappend = ((<>))
    mempty = DictTerm mempty mempty

data Function a = Function (Term a -> IO (Term a))

function'id :: Term a
function'id = Term'Function $ Function pure

function'dict'lookup :: Term a
function'dict'lookup = undefined

function'str'concat :: Term a
function'str'concat =
  function'of'str $ \s ->
  function'of'str $ \s' ->
  Term'Str $ Text.append s s'

function'of'str :: (Text -> Term a) -> Term a
function'of'str =
  Term'Function . Function . require'str

require'str :: (Text -> Term a) -> Term a -> IO (Term a)
require'str f = reduce'term >>> (<&> \case
  Term'Str x       -> f x
  e@(Term'Error _) -> e
  _                -> Term'Error "expected string")

function'dict'merge'preferLeft :: Term a
function'dict'merge'preferLeft = undefined

function'dict'merge'preferRight :: Term a
function'dict'merge'preferRight = undefined

expression'to'term :: Expression -> IO (Term a)
expression'to'term =
  \case
    Expr'Var x -> var'to'term x
    Expr'Str x -> str'to'term x
    Expr'List x -> list'to'term x
    Expr'Dict x -> dict'to'term x
    Expr'Dot x -> dot'to'term x
    Expr'Lambda x -> lambda'to'term x
    Expr'Apply x -> apply'to'term x
    Expr'Let x ->
      undefined
    Expr'With x ->
      undefined

var'to'term :: Str'Unquoted -> IO (Term a)
var'to'term =
  pure . Term'Var . str'unquotedToStatic

apply'to'term :: Apply -> IO (Term a)
apply'to'term (Apply a b) =
  (/@\) <$> (expression'to'term a) <*> (expression'to'term b)

str'to'term :: Str'Dynamic -> IO (Term a)
str'to'term (Str'Dynamic (Seq.toList -> xs)) =
  case xs of
    [] -> pure $ Term'Str ""
    ys -> foldr1 (\x y -> function'str'concat /@\ x /@\ y)
            <$> traverse str'1'to'term ys

str'1'to'term :: Str'1 -> IO (Term a)
str'1'to'term = \case
  Str'1'Literal x -> pure $ Term'Str x
  Str'1'Antiquote x -> expression'to'term x

list'to'term :: List -> IO (Term a)
list'to'term (List x) =
  Term'List <$> traverse expression'to'term x

dict'to'term :: Dict -> IO (Term a)
dict'to'term = undefined

dot'to'term :: Dot -> IO (Term a)
dot'to'term (Dot a b) = do
  a' <- expression'to'term a
  b' <- expression'to'term b
  pure $ function'dict'lookup /@\ a' /@\ b'

lambda'to'term :: Lambda -> IO (Term a)
lambda'to'term (Lambda head body) =
  case head of

    -- For a simple named parameter, the AST translates directly into the
    -- lambda calculus.
    Param'Name x ->
      Term'Lambda (str'unquotedToStatic x)
        <$> expression'to'term body

    -- For dict patterns, we have a special kind of term.
    Param'DictPattern dp ->
      Term'Lambda'DictPattern
        <$> dictPattern'to'termDictPattern dp
        <*> expression'to'term body

    -- For a named parameter /and/ a dict pattern, we nest the dict pattern
    -- lambda inside a regular lambda.
    Param'Both (str'unquotedToStatic -> x) dp ->
      Term'Lambda x
      <$> (
        Term'Lambda'DictPattern
          <$> dictPattern'to'termDictPattern dp
          <*> expression'to'term body
      )

dictPattern'to'termDictPattern :: DictPattern -> IO (Term'DictPattern a)
dictPattern'to'termDictPattern (DictPattern (Seq.toList -> items) ellipsis) =
  Term'DictPattern
    <$> (Map.fromList <$> traverse f items)
    <*> pure ellipsis
  where
    f (DictPattern'1 (str'unquotedToStatic -> a) b) =
      (a,) <$> traverse expression'to'term b

{- | @term'substitute var value body@ produces a copy of the term @body@,
substituting @value@ for free occurrences of @var@. -}
term'substitute
  :: Text   -- ^ @var@   - Variable name
  -> Term a -- ^ @value@ - The argument being substituted
  -> Term a -- ^ @body@  - The term being copied
  -> IO (Term a)

-- The numbered comments within this definition are nearly verbatim from
-- page 210 of /The Implementation of Functional Programming Languages/.
-- There SPJ denotes this construction as body[value/var] and refers to the
-- substitution process as "instantiation."

term'substitute var value = go
  where
    go body = case body of

      Term'Var x ->

        -- 1. If /body/ is a variable x and /var/ = x, then return /value/
        --    (here we substitute /value/ for an occurrence of /var/).
        if x == var then pure value

        -- 2. If /body/ is a variable x and /var/ ≠ x, then return /body/.
        else pure body

      -- 3. If /body/ is a constant or built-in function, then return /body/.
      Term'Data _     -> pure body -- constant
      Term'Str _      -> pure body -- constant
      Term'Function _ -> pure body -- built-in function

      -- 4. If /body/ is an application (e1 e2), then return the application
      --    (e1[value/var] e2[value/var]).
      Term'Apply a b -> Term'Apply <$> go a <*> go b

      Term'Lambda a b ->
        -- 5. If /body/ is a lambda abstraction λx.E and /var/ = x, then return
        --    /body/ - the new lambda abstraction binds /var/ anew, so no
        --    substitutions should occur inside it, and hence we can avoid
        --    instantiating it altogether.
        if a == var then pure body

        -- 6. If /body/ is a lambda abstraction λx.E and /var/ ≠ x, then return
        --    λx.E[value/var] - we must instantiate the lambda abstraction in
        --    case there are any free occurrences of /var/ inside it.
        else Term'Lambda a <$> go b

      Term'Lambda'DictPattern (Term'DictPattern map ellipsis) b ->
        Term'Lambda'DictPattern
          <$> (
            -- Substitute into all of the dict pattern's default expressions.
            Term'DictPattern
              <$> traverse (traverse go) map
              <*> pure ellipsis
          )
          <*> (
            -- As with (5), if the lambda abstraction binds /var/ anew
            -- (in this case, via of the entries in the dict pattern),
            -- then there is no substitution to perform in the lambda body.
            if Map.member var map then pure b

            -- If the dict pattern does /not/ rebind /var/, then we must
            -- instantiate the body, as we do in (6).
            else go b
          )

      Term'List xs -> Term'List <$> traverse go xs

      Term'Dict _ -> undefined

      -- When substituting into a pointer, we do /not/ overwrite the pointer.
      -- The body is a template for instantiation which creates a /copy/ of it.
      -- todo - Is this right?
      Term'Pointer (Pointer ref) -> readIORef ref >>= go

      -- There's no substituting into an error; the error just propagates.
      Term'Error _ -> pure body

data Pointer a = Pointer
  { pointer'ref :: IORef (Term a)
  }

reduce'term :: Term a -> IO (Term a)
reduce'term t =
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
