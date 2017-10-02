{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

module Bricks.Term where

-- Bricks internal
import           Bricks.Internal.Monad
import           Bricks.Internal.Prelude
import           Bricks.Internal.Seq     (Seq)
import           Bricks.Internal.Text    (Text)
import qualified Bricks.Internal.Text    as Text

-- Containers
import Data.Map (Map)
import Data.Set (Set)

-- Base
import           Data.Dynamic  (Dynamic, toDyn)
import           Data.IORef
import           Data.Set      (Set)
import qualified Data.Set      as Set
import           Data.Typeable (Typeable)
import           System.IO     (IO)

data Bottom = Bottom Text

displayBottom :: Bottom -> Text
displayBottom (Bottom message) = "Error: " <> message

class (Monad m, MonadIO m, MonadError Bottom m) => MonadEval m
  where
    reduce'term :: Term -> m Term
    reduce'dict'keys :: Term -> m (Map Text Term)

type Function = forall m. MonadEval m => Term -> m Term

data Term
  = Term'Data Text Dynamic
      -- ^ The name of the data type, and a value of that type.
  | Term'Function Function
  | Term'Lambda TermPattern Term
      -- ^ The head and body of a lambda expression.
  | Term'LetRec (Map Text Term) Term
  | Term'List (Seq Term)
  | Term'Dict (Seq (Term, Term))
  | Term'Dict'ReducedKeys (Map Text Term)
  | Term'Var Text
  | Term'Apply Term Term
  | Term'Pointer TermPtr

{- | Alias for 'Term'Apply'. The name is an allusion to the AST depictions in
/The Implementation of Functional Programming Languages/, where "/f/ applied to
/x/" is drawn as:

>   @
>  ╱ ╲
> f   x

For a function of two parameters, see the corresponding '/@@\' operator. -}

(/@\) :: Term -> Term -> Term
(/@\) = Term'Apply
infixl /@\

{- | Like '/@\', but for a function applied to two arguments. Depicted as an
abstract syntax tree, @f /\@\@\\ (x, y)@ looks like this:

>     @
>    ╱ ╲
>   @   y
>  ╱ ╲
> f   x

-}

(/@@\) :: Term -> (Term, Term) -> Term
f /@@\ (x, y) = (f /@\ x) /@\ y
infixl /@@\

{- | Alias for 'Term'Lambda'. -}

(|->) :: TermPattern -> Term -> Term
(|->) = Term'Lambda
infixl |->

data TermPattern
  = TermPattern'Simple Text
  | TermPattern'Dict (Set Text)

type TermPtr = IORef Term

create'pointer :: MonadIO m => Term -> m Term
create'pointer x = case x of
  Term'Pointer _ -> pure x  -- The term is already a pointer, don't make another
  _              -> Term'Pointer <$> liftIO (newIORef x)

dereference :: MonadIO m => Term -> m Term
dereference = \case
  Term'Pointer p -> readTermPtr p >>= dereference
  x -> pure x

newTermPtr :: MonadIO m => Term -> m Term
newTermPtr x = case x of
   Term'Pointer _ -> pure x  -- The term is already a pointer, nothing to do
   _              -> Term'Pointer <$> liftIO (newIORef x)

readTermPtr :: MonadIO m => TermPtr -> m Term
readTermPtr = liftIO . readIORef

writeTermPtr :: MonadIO m => TermPtr -> Term -> m ()
writeTermPtr ptr = liftIO . writeIORef ptr

bottom :: MonadError Bottom m => Bottom -> m a
bottom = throwError
