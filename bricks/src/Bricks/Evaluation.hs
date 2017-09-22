{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE ViewPatterns               #-}

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

  - Page 20 introduces the name capture problem. Pages 199 and 210 discuss
    how we avoid it by only reducing the top-level redex, which has no free
    variables.

-}
module Bricks.Evaluation where

-- Bricks
import Bricks.Expression
import Bricks.UnquotedString

-- Bricks internal
import           Bricks.Internal.Prelude
import           Bricks.Internal.Seq          (Seq)
import qualified Bricks.Internal.Seq          as Seq
import           Bricks.Internal.Text         (Text)
import qualified Bricks.Internal.Text         as Text
import           Bricks.Internal.Transformers

-- Containers
import           Data.Map (Map)
import qualified Data.Map as Map

-- Base
import Data.Dynamic  (Dynamic, fromDynamic, toDyn)
import Data.IORef
import Data.Typeable (Typeable)
import System.IO     (IO)

newtype Eval a = Eval (ExceptT Text IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

data Term
  = Term'Data Text Dynamic
  | Term'Function (Term -> Eval Term)
  | Term'Lambda Text Term
  | Term'Lambda'DictPattern Term'DictPattern Term
  | Term'List (Seq Term)
  | Term'Dict DictTerm
  | Term'Var Text
  | Term'Apply Term Term
  | Term'Pointer TermPtr

termTypeName :: Term -> Eval Text
termTypeName = \case
  Term'Data x _   -> pure x
  Term'Function _ -> pure "built-in function"
  Term'Lambda _ _ -> pure "lambda"
  Term'Lambda'DictPattern _ _ -> pure "lambda"
  Term'List _     -> pure "list"
  Term'Dict _     -> pure "dict"
  Term'Var _      -> pure "variable"
  Term'Apply _ _  -> pure "function application"
  Term'Pointer p  -> readTermPtr p >>= termTypeName

create'pointer :: Term -> Eval Term
create'pointer x = case x of
  Term'Pointer _ -> pure x  -- The term is already a pointer, don't make another
  _              -> Term'Pointer <$> liftIO (newIORef x)

dereference :: Term -> Eval Term
dereference = \case
  Term'Pointer p -> readTermPtr p >>= dereference
  x -> pure x

(/@\) :: Term -> Term -> Term
(/@\) = Term'Apply
infixl /@\

type TermPtr = IORef Term

newTermPtr :: Term -> Eval Term
newTermPtr x = case x of
   Term'Pointer _ -> pure x  -- The term is already a pointer, nothing to do
   _              -> Term'Pointer <$> liftIO (newIORef x)

readTermPtr :: TermPtr -> Eval Term
readTermPtr = liftIO . readIORef

writeTermPtr :: TermPtr -> Term -> Eval ()
writeTermPtr ptr = liftIO . writeIORef ptr

evalError :: Text -> Eval a
evalError = Eval . throwE

data Term'DictPattern =
  Term'DictPattern
    { term'dictPattern'items :: Map Text (Maybe Term)
    , term'dictPattern'ellipsis :: Bool
    }

data DictTerm =
  DictTerm
    { dictTerm'map :: Map Text Term
    , dictTerm'seq :: Seq (Term, Term)
    }

instance Semigroup DictTerm
  where
    DictTerm a1 b1 <> DictTerm a2 b2 = DictTerm (a1 <> a2) (b1 <> b2)

instance Monoid DictTerm
  where
    mappend = ((<>))
    mempty = DictTerm mempty mempty

function'id :: Term
function'id = Term'Function pure

term'data :: forall a. Typeable a => Type a -> a -> Term
term'data (Type n) = Term'Data n . toDyn @a

function'const'data :: forall a. Typeable a => Type a -> a -> Term
function'const'data t = Term'Function . const . pure . term'data t

function'dict'lookup :: Term
function'dict'lookup = undefined

function'boolean'or :: Term
function'boolean'or =
  Term'Function $ \x -> require'data'value type'boolean x <&> \case
    True -> function'const'data type'boolean True
    False -> Term'Function $ require'data'term type'boolean

function'boolean'and :: Term
function'boolean'and =
  Term'Function $ \x -> require'data'value type'boolean x <&> \case
    False -> function'const'data type'boolean False
    True -> Term'Function $ require'data'term type'boolean

function'string'append :: Term
function'string'append =
  Term'Function $ \x -> require'data'value type'string x <&> \x' ->
  Term'Function $ \y -> require'data'value type'string y <&> \y' ->
  term'data type'string $ Text.append x' y'

function'dict'merge'preferLeft :: Term
function'dict'merge'preferLeft = undefined

function'dict'merge'preferRight :: Term
function'dict'merge'preferRight = undefined

data Type a = Type { type'name :: Text }

type'boolean :: Type Bool
type'boolean = Type "boolean"

type'string :: Type Text
type'string = Type "string"

require'data'value :: Typeable a => Type a -> Term -> Eval a
require'data'value = req fst

require'data'term :: Typeable a => Type a -> Term -> Eval Term
require'data'term = req snd

req :: forall a b. Typeable a => ((a, Term) -> b) -> Type a -> Term -> Eval b
req s (Type n) = reduce >=> \case
  t@(Term'Data n' x) ->
    case fromDynamic @a x of
      Nothing -> evalError ("Expected " <> n <> ", got " <> n')
      Just a -> pure (s (a, t))
  x ->
    termTypeName x >>= \n' -> evalError ("Expected " <> n <> ", got " <> n')

expression'to'term :: Expression -> Eval Term
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

var'to'term :: Str'Unquoted -> Eval Term
var'to'term =
  pure . Term'Var . str'unquotedToStatic

apply'to'term :: Apply -> Eval Term
apply'to'term (Apply a b) =
  (/@\) <$> expression'to'term a <*> expression'to'term b

str'to'term :: Str'Dynamic -> Eval Term
str'to'term (Str'Dynamic (Seq.toList -> xs)) =
  case xs of
    [] -> pure $ term'data type'string ""
    ys -> foldr1 (\x y -> function'string'append /@\ x /@\ y)
            <$> traverse str'1'to'term ys

str'1'to'term :: Str'1 -> Eval Term
str'1'to'term = \case
  Str'1'Literal x -> pure $ term'data type'string x
  Str'1'Antiquote x -> expression'to'term x

list'to'term :: List -> Eval Term
list'to'term (List x) =
  Term'List <$> traverse expression'to'term x

dict'to'term :: Dict -> Eval Term
dict'to'term = undefined

dot'to'term :: Dot -> Eval Term
dot'to'term (Dot a b) = do
  a' <- expression'to'term a
  b' <- expression'to'term b
  pure $ function'dict'lookup /@\ a' /@\ b'

lambda'to'term :: Lambda -> Eval Term
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

dictPattern'to'termDictPattern :: DictPattern -> Eval Term'DictPattern
dictPattern'to'termDictPattern (DictPattern (Seq.toList -> items) ellipsis) =
  Term'DictPattern
    <$> (Map.fromList <$> traverse f items)
    <*> pure ellipsis
  where
    f (DictPattern'1 (str'unquotedToStatic -> a) b) =
      (a,) <$> traverse expression'to'term b

{- | @instantiate var value body@ produces a copy of the term @body@,
substituting @value@ for free occurrences of @var@. -}
instantiate
  :: Text  -- ^ @var@   - Variable name
  -> Term  -- ^ @value@ - The argument being substituted. We assume that this
           --             term has no free variables; or else we will suffer
           --             the /name capture problem/.
  -> Term  -- ^ @body@  - The term being copied
  -> Eval Term

-- The numbered comments within this definition are nearly verbatim from
-- page 210 of /The Implementation of Functional Programming Languages/.
-- There SPJ denotes this construction as body[value/var].

instantiate var value =
  go
  where
    go :: Term -> Eval Term
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
        if a == var then pure body

        -- 6. If /body/ is a lambda abstraction λx.E and /var/ ≠ x, then return
        --    λx.E[value/var] - we must instantiate the body in case there are
        --    any free occurrences of /var/ inside it.
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

      -- todo - Is this right?
      Term'Pointer _ -> evalError "Cannot substitute through a pointer"

reduce :: Term -> Eval Term
reduce =
  \case
    Term'Apply f value ->
      reduce f >>= \case

        -- The function is normal lambda. Perform pointer substitution.
        Term'Lambda var body ->
          create'pointer value >>= \value'ptr ->
            instantiate var value'ptr body

        -- The function is a lambda with a dict pattern.
        Term'Lambda'DictPattern dp body ->

          -- Reduce the argument, and require it to be a dict.
          reduce value >>= \case
            Term'Dict dict -> undefined
            x -> termTypeName x >>= \n -> evalError ("Expected dict, got " <> n)

    t@(Term'Pointer p) ->
      (readTermPtr p >>= reduce >>= writeTermPtr p) $> t
    x -> pure x

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
