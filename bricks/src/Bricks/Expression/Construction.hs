{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}

{- | Functions for constructing 'Expression's that match the 'Show'
implementations.

This module is only designed for testing and REPL use. It isn't re-exported
into the main Bricks API because it's a bit messy:

- It introduces some superfluous typeclasses for the sake of brevity.
- There are a lot of terse function names here that would clash with other
  things easily.
- Some functions are partial, such as those that require strings that can be
  rendered unquoted.
- It uses string overloading in a way that the regular API probably shouldn't.
- The functions are oriented toward constructing 'Expression's, skipping over
  the intermediary types they're composed of, which is convenient but may make
  them insufficient for some use cases.

-}
module Bricks.Expression.Construction where

-- Bricks
import Bricks.Expression
import Bricks.UnquotedString

-- Bricks internal
import           Bricks.Internal.Prelude
import qualified Bricks.Internal.Seq     as Seq
import           Bricks.Internal.Text    (Text)
import qualified Bricks.Internal.Text    as Text

-- Base
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.String        (IsString (fromString))


--------------------------------------------------------------------------------
--  Lambdas
--------------------------------------------------------------------------------

lambda :: Param'Builder -> Expression -> Expression
lambda a b =
  Expr'Lambda $ Lambda (buildParam a) b


--------------------------------------------------------------------------------
--  Function application
--------------------------------------------------------------------------------

apply :: Expression -> Expression -> Expression
apply a b =
  Expr'Apply $ Apply a b


--------------------------------------------------------------------------------
--  Variables
--------------------------------------------------------------------------------

var :: Text -> Expression
var =
  Expr'Var . unquotedString'orThrow


--------------------------------------------------------------------------------
--  Dots
--------------------------------------------------------------------------------

dot :: Expression -> Expression -> Expression
dot a b =
  Expr'Dot $ Dot a b


--------------------------------------------------------------------------------
--  Let
--------------------------------------------------------------------------------

let'in :: [LetBinding] -> Expression -> Expression
let'in a b =
  Expr'Let $ Let (Seq.fromList a) b


--------------------------------------------------------------------------------
--  Dicts
--------------------------------------------------------------------------------

dict :: [DictBinding] -> Expression
dict =
  Expr'Dict . Dict False . Seq.fromList

rec'dict :: [DictBinding] -> Expression
rec'dict =
  Expr'Dict . Dict False . Seq.fromList


--------------------------------------------------------------------------------
--  Overloaded 'binding' function
--------------------------------------------------------------------------------

class Binding a b | b -> a
  where
    binding :: a -> Expression -> b

instance Binding Expression DictBinding
  where
    binding = DictBinding'Eq

instance Binding Text LetBinding
  where
    binding = LetBinding'Eq


--------------------------------------------------------------------------------
--  Overloaded 'inherit' functions
--------------------------------------------------------------------------------

class IsInherit a
  where
    fromInherit :: Inherit -> a

instance IsInherit DictBinding
  where
    fromInherit = DictBinding'Inherit

instance IsInherit LetBinding
  where
    fromInherit = LetBinding'Inherit

inherit :: IsInherit a => [Text] -> a
inherit =
  fromInherit . Inherit Nothing . Seq.fromList

inherit'from :: IsInherit a => Expression -> [Text] -> a
inherit'from x y =
  fromInherit $ Inherit (Just x) (Seq.fromList y)


--------------------------------------------------------------------------------
--  Dynamic strings
--------------------------------------------------------------------------------

str :: [Str'1'] -> Expression
str =
  Expr'Str . Str'Dynamic . Seq.fromList . fmap unStr'1'

antiquote :: Expression -> Str'1'
antiquote =
  Str'1' . Str'1'Antiquote

-- | A newtype for 'Str'1' just so we can give it the 'IsString' instance
-- which would be dubiously appropriate for the actual 'Str'1' type.
newtype Str'1' = Str'1' { unStr'1' :: Str'1 }

instance IsString Str'1'
  where
    fromString = Str'1' . Str'1'Literal . Text.pack


--------------------------------------------------------------------------------
--  Overloaded 'param' function
--------------------------------------------------------------------------------

class IsParam a
  where
    param :: Text -> a

instance IsParam Param'Builder
  where
    param x = paramBuilder $ Param'Name $ unquotedString'orThrow x

instance IsParam DictPattern'1
  where
    param x = DictPattern'1 (unquotedString'orThrow x) Nothing


--------------------------------------------------------------------------------
--  Param builder
--------------------------------------------------------------------------------

newtype Param'Builder = Param'Builder (NonEmpty Param)
  deriving Semigroup

buildParam :: Param'Builder -> Param
buildParam (Param'Builder xs) =
  foldr1 mergeParams xs

paramBuilder :: Param -> Param'Builder
paramBuilder x =
  Param'Builder (x :| [])

pattern :: [DictPattern'1] -> Param'Builder
pattern xs =
  paramBuilder $ Param'DictPattern $ DictPattern (Seq.fromList xs) False

def :: Expression -> DictPattern'1 -> DictPattern'1
def b (DictPattern'1 a _) =
  DictPattern'1 a (Just b)

ellipsis :: Param'Builder
ellipsis =
  paramBuilder $ Param'DictPattern $ DictPattern Seq.empty True

{- | Combine two params, merging dict patterns with 'mergeDictPatterns' and
preferring the right-hand-side when names conflict. -}
mergeParams :: Param -> Param -> Param
mergeParams = (+)
  where
    (+) :: Param -> Param -> Param
    -- A name on the right overrides a name on the left
    Param'Both _n1 p1 + Param'Name n2 = Param'Both n2 p1
    -- The simplest combinations: turning one or the other into both
    Param'Name n + Param'DictPattern p = Param'Both n p
    Param'DictPattern p + Param'Name n = Param'Both n p
    -- Otherwise a name on the left gets overridden by anything on the right
    Param'Name _n + x = x
    -- Combinations that require merging the dict patterns
    Param'DictPattern p1 + Param'DictPattern p2 =
      Param'DictPattern (mergeDictPatterns p1 p2)
    Param'DictPattern p1 + Param'Both n p2 =
      Param'Both n (mergeDictPatterns p1 p2)
    Param'Both _n1 p1 + Param'Both n2 p2 =
      Param'Both n2 (mergeDictPatterns p1 p2)
    Param'Both n p1 + Param'DictPattern p2 =
      Param'Both n (mergeDictPatterns p1 p2)

{- | Combine two dict patterns, taking the concatenation of the item list, and
the Boolean /or/ of the ellipsis flag. -}
mergeDictPatterns :: DictPattern -> DictPattern -> DictPattern
mergeDictPatterns = (+)
  where
    DictPattern xs1 e1 + DictPattern xs2 e2 =
      DictPattern (xs1 <> xs2) (e1 || e2)
