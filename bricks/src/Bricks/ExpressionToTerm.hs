{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{- |

Conversion from 'Expression' (the AST produced by the parser) to 'Term'
(an augmented form of the lambda calculus used for evaluation).

-}
module Bricks.ExpressionToTerm where

-- Bricks
import Bricks.BuiltinFunctions
import Bricks.Expression
import Bricks.Term
import Bricks.Type

-- Bricks internal
import qualified Bricks.Internal.List    as List
import           Bricks.Internal.Prelude
import qualified Bricks.Internal.Seq     as Seq
import           Bricks.Internal.Text    (Text)

-- Containers
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set

-- Base
import Control.Applicative (liftA2)
import System.IO           (IO)

expression'to'term :: Expression -> IO Term
expression'to'term =
  \case
    Expr'Var x -> var'to'term x
    Expr'Str x -> str'to'term x
    Expr'List x -> list'to'term x
    Expr'Dict x -> dict'to'term x
    Expr'Dot x -> dot'to'term x
    Expr'Lambda x -> lambda'to'term x
    Expr'Apply x -> apply'to'term x
    Expr'Let x -> let'to'term x

var'to'term :: Var -> IO Term
var'to'term = pure . Term'Var . var'text

apply'to'term :: Apply -> IO Term
apply'to'term x =
  do
    a <- expression'to'term (apply'func x)
    b <- expression'to'term (apply'arg x)
    pure $ a /@\ b

str'to'term :: Str'Dynamic -> IO Term
str'to'term x =
  case Seq.toList (strDynamic'toSeq x) of
    [] -> pure $ term'data type'string ""
    ys -> foldr1 (liftA2 f) $ fmap str'1'to'term ys
  where
    f a b = fn'string'append /@@\ (a, b)

str'1'to'term :: Str'1 -> IO Term
str'1'to'term = \case
  Str'1'Literal x -> pure $ term'data type'string (str'static'text x)
  Str'1'Antiquote x -> expression'to'term x

list'to'term :: List -> IO Term
list'to'term x =
  Term'List <$> traverse expression'to'term (list'expressions x)

dict'to'term :: Dict -> IO Term
dict'to'term = undefined

dot'to'term :: Dot -> IO Term
dot'to'term x =
  do
    a <- expression'to'term (dot'dict x)
    b <- expression'to'term (dot'key x)
    pure $ fn'dict'lookup /@@\ (a, b)

let'to'term :: Let -> IO Term
let'to'term x =
  Term'LetRec <$> bindings <*> body

  where
    bindings :: IO (Map Text Term)
    bindings =
      Map.fromList . List.concat <$>
      traverse letBinding'to'term (let'bindings x)

    body :: IO Term
    body =
      expression'to'term $ let'value x

letBinding'to'term :: LetBinding -> IO [(Text, Term)]
letBinding'to'term =
  \case
    LetBinding'Eq a b ->
      do
        b' <- expression'to'term b
        pure [(var'text a, b')]
    LetBinding'Inherit d xs ->
      do
        -- Use a pointer for the dict we're inheriting from, to avoid
        -- having to reduce it more than once.
        p <- newTermPtr =<< expression'to'term d

        pure
          $ fmap (\x ->
            ( var'text x
            , fn'dict'lookup /@@\ (p, term'data type'string (var'text x))
            ))
          $ Seq.toList xs


--------------------------------------------------------------------------------
--  Converting a lambda expression to a lambda term
--------------------------------------------------------------------------------

lambda'to'term :: Lambda -> IO Term
lambda'to'term x =
  do
    body <- expression'to'term (lambda'body x)
    case lambda'head x of
      Param'Name var       -> lambda'to'term'simple var body
      Param'DictPattern dp -> lambda'to'term'dictPattern dp body
      Param'Both var dp    -> lambda'to'term'both var dp body

lambda'to'term'simple :: Var -> Term -> IO Term
lambda'to'term'simple var body =
  -- For a simple named parameter, the AST translates directly into the
  -- lambda calculus.
  pure $ TermPattern'Simple (var'text var) |-> body

lambda'to'term'dictPattern :: DictPattern -> Term -> IO Term
lambda'to'term'dictPattern dp body = do
  -- For dict patterns, we have to do a few more things:
  let names = dictPattern'names dp

    -- 1. If there is no ellipsis, add a check to fail if there are
    --    extra keys in the argument.
  let h = if dictPattern'ellipsis dp then fn'id
          else fn'dict'disallowExtraKeys names

  defs <- dictPattern'defaults dp

    -- 2. Insert a dict-merging function to apply default arguments.
  let g = fn'dict'merge'preferLeft /@\ Term'Dict'ReducedKeys defs

  let f = TermPattern'Dict names |-> body

  pure $ fn'comp /@@\ (fn'comp /@@\ (f, g), h)

lambda'to'term'both :: Var -> DictPattern -> Term -> IO Term
lambda'to'term'both var dp body =
  -- For a named parameter /and/ a dict pattern, we nest the dict pattern
  -- lambda inside a regular lambda.
  lambda'to'term'simple var =<< lambda'to'term'dictPattern dp body

dictPattern'names :: DictPattern -> Set Text
dictPattern'names (DictPattern xs _) =
  Set.fromList . fmap f . Seq.toList $ xs
  where
    f = var'text . dictPattern'1'name

dictPattern'defaults :: DictPattern -> IO (Map Text Term)
dictPattern'defaults (DictPattern xs _) =
  Map.fromList . catMaybes <$> traverse f (Seq.toList xs)
  where
    f :: DictPattern'1 -> IO (Maybe (Text, Term))
    f x =
      case dictPattern'1'default x of
        Nothing -> pure Nothing
        Just d ->
          do
            a <- expression'to'term d
            pure $ Just (var'text (dictPattern'1'name x), a)
