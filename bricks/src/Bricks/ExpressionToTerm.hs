{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

{- |

Conversion from 'Expression' (the AST produced by the parser) to 'Term'
(an augmented form of the lambda calculus used for evaluation).

-}
module Bricks.ExpressionToTerm where

-- Bricks
import Bricks.BuiltinFunctions
import Bricks.Expression
import Bricks.StringExpressions
import Bricks.Term
import Bricks.Type
import Bricks.UnquotedString

-- Bricks internal
import           Bricks.Internal.Prelude
import qualified Bricks.Internal.Seq     as Seq
import           Bricks.Internal.Text    (Text)

-- Containers
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set

expression'to'term :: Expression -> Term
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

var'to'term :: UnquotedString -> Term
var'to'term = Term'Var . unquotedString'text

apply'to'term :: Apply -> Term
apply'to'term (Apply a b) =
  expression'to'term a /@\ expression'to'term b

str'to'term :: Str'Dynamic Expression -> Term
str'to'term (Str'Dynamic (Seq.toList -> xs)) =
  case xs of
    [] -> term'data type'string ""
    ys -> foldr1 f $ fmap str'1'to'term ys
  where
    f x y = fn'string'append /@@\ (x, y)

str'1'to'term :: Str'1 Expression -> Term
str'1'to'term = \case
  Str'1'Literal (Str'Static x) -> term'data type'string x
  Str'1'Antiquote x -> expression'to'term x

list'to'term :: List -> Term
list'to'term (List x) =
  Term'List $ fmap expression'to'term x

dict'to'term :: Dict -> Term
dict'to'term = undefined

dot'to'term :: Dot -> Term
dot'to'term (Dot a b) =
  fn'dict'lookup /@@\ (expression'to'term a, expression'to'term b)


--------------------------------------------------------------------------------
--  Converting a lambda expression to a lambda term
--------------------------------------------------------------------------------

lambda'to'term :: Lambda -> Term
lambda'to'term (Lambda head (expression'to'term -> body)) =
  case head of
    Param'Name var       -> lambda'to'term'simple var body
    Param'DictPattern dp -> lambda'to'term'dictPattern dp body
    Param'Both var dp    -> lambda'to'term'both var dp body

lambda'to'term'simple :: UnquotedString -> Term -> Term
lambda'to'term'simple var body =
  -- For a simple named parameter, the AST translates directly into the
  -- lambda calculus.
  TermPattern'Simple (unquotedString'text var) |-> body

lambda'to'term'dictPattern :: DictPattern -> Term -> Term
lambda'to'term'dictPattern dp body =
  -- For dict patterns, we have to do a few more things:
  let
    names = dictPattern'names dp

    -- 1. If there is no ellipsis, add a check to fail if there are
    --    extra keys in the argument.
    h = if dictPattern'ellipsis dp then fn'id
        else fn'dict'disallowExtraKeys names

    -- 2. Insert a dict-merging function to apply default arguments.
    g = fn'dict'merge'preferLeft /@\
          Term'Dict'ReducedKeys (dictPattern'defaults dp)

    f = TermPattern'Dict names |-> body
  in
    fn'comp /@@\ (fn'comp /@@\ (f, g), h)

lambda'to'term'both :: UnquotedString -> DictPattern -> Term -> Term
lambda'to'term'both var dp body =
  -- For a named parameter /and/ a dict pattern, we nest the dict pattern
  -- lambda inside a regular lambda.
  lambda'to'term'simple var $ lambda'to'term'dictPattern dp body

dictPattern'names :: DictPattern -> Set Text
dictPattern'names (DictPattern xs _) =
  Set.fromList . fmap f . Seq.toList $ xs
  where
    f = unquotedString'text . dictPattern'1'name

dictPattern'defaults :: DictPattern -> Map Text Term
dictPattern'defaults (DictPattern xs _) =
  Map.fromList . catMaybes . fmap f . Seq.toList $ xs
  where
    f :: DictPattern'1 -> Maybe (Text, Term)
    f x = dictPattern'1'default x <&> \d ->
            ( unquotedString'text . dictPattern'1'name $ x
            , expression'to'term d )
