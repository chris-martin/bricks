{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Bricks.Rendering
  (
  -- * @Render@
    Render

  -- * Expressions
  , render'expression
  , render'expression'listContext
  , render'expression'dotLeftContext
  , render'expression'applyLeftContext
  , render'expression'applyRightContext
  , render'expression'inParens
  , render'expression'dictKey

  -- * Variables
  , render'var

  -- * Strings
  , str'escape
  , render'strStatic'unquotedIfPossible
  , render'strStatic'quoted
  , render'strDynamic'unquotedIfPossible
  , render'strDynamic'quoted

  -- * Lists
  , render'list

  -- * Dicts
  , render'dict
  , render'dictBinding

  -- * Dict lookup
  , render'dot

  -- * Lambdas
  , render'lambda

  -- * Function parameters
  , render'param
  , render'dictPattern
  , render'dictPattern'1

  -- * Function application
  , render'apply

  -- * @let@
  , render'let
  , render'letBinding

  ) where

-- Bricks
import Bricks.Expression
import Bricks.Keyword
import Bricks.UnquotedString

-- Bricks internal
import           Bricks.Internal.Prelude
import qualified Bricks.Internal.Seq     as Seq
import           Bricks.Internal.Text    (Text)
import qualified Bricks.Internal.Text    as Text

--------------------------------------------------------------------------------

-- $setup
--
-- >>> import Bricks.Expression.Construction

--------------------------------------------------------------------------------

type Render a = a -> Text

--------------------------------------------------------------------------------

{- | Insert escape sequences for rendering normal double-quoted (@"@) strings.
-}

str'escape :: Text -> Text
str'escape =
  Text.replace "\"" "\\\"" .
  Text.replace "${" "\\${" .
  Text.replace "\n" "\\n" .
  Text.replace "\r" "\\r" .
  Text.replace "\t" "\\t" .
  Text.replace "\\" "\\\\"

{- | Render an unquoted string in unquoted form. -}

render'var :: Render Var
render'var = var'text

{- | Render a static string, in unquoted form if possible. -}

render'strStatic'unquotedIfPossible :: Render Str'Static
render'strStatic'unquotedIfPossible s@(Str'Static x) =
  if text'canBeUnquoted x then x else render'strStatic'quoted s

{- | Render a static string, in quoted form. -}

render'strStatic'quoted :: Render Str'Static
render'strStatic'quoted (Str'Static x) =
  "\"" <> str'escape x <> "\""

{- | Render a dynamic string, in unquoted form if possible. -}

render'strDynamic'unquotedIfPossible :: Render Str'Dynamic
render'strDynamic'unquotedIfPossible d =
  case str'dynamic'to'static d of
    Just s  -> render'strStatic'unquotedIfPossible s
    Nothing -> render'strDynamic'quoted d

{- | Render a dynamic string, in quoted form. -}

render'strDynamic'quoted :: Render Str'Dynamic
render'strDynamic'quoted xs =
  "\"" <> foldMap r (strDynamic'toSeq xs) <> "\""
  where
    r :: Str'1 -> Text
    r = \case
      Str'1'Literal (Str'Static x) -> str'escape x
      Str'1'Antiquote x -> "${" <> render'expression x <> "}"

{- | Render a lambda parameter: everything from the beginning of a lambda, up to
but not including the @:@ that separates the head from the body of the lambda.
-}

render'param :: Render Param
render'param =
  \case
    Param'Name a        -> render'var a
    Param'DictPattern b -> render'dictPattern b
    Param'Both a b      -> render'var a <> "@" <>
                           render'dictPattern b

{- | Render a dict pattern (@{ a, b ? c, ... }@). -}

render'dictPattern :: Render DictPattern
render'dictPattern (DictPattern bs e) =
  if Seq.null xs
    then "{ }"
    else "{ " <> Text.intercalate ", " xs <> " }"
  where
    xs =
      Seq.map render'dictPattern'1 bs <>
      if e then Seq.singleton "..." else Seq.empty

{- | Render a single item in a 'DictPattern'. -}

render'dictPattern'1 :: Render DictPattern'1
render'dictPattern'1 =
  \case
    DictPattern'1 a Nothing  -> render'var a
    DictPattern'1 a (Just b) -> render'var a <> " ? " <>
                                render'expression b

{- | Render a lambda expression (@x: y@). -}

render'lambda :: Render Lambda
render'lambda (Lambda a b) =
  render'param a <> ": " <> render'expression b

{- | Render a function application expression (@f x@). -}

render'apply :: Render Apply
render'apply (Apply a b) =
  render'expression'applyLeftContext a <> " " <>
  render'expression'applyRightContext b

{- | Render a list literal (@[@ ... @]@). -}

render'list :: Render List
render'list (List xs) =
  "[ " <> r xs <> "]"
  where
    r = Text.concat . fmap (\x -> render'expression'listContext x <> " ")

{- | Render a dict literal (@{@ ... @}@). -}

render'dict :: Render Dict
render'dict (Dict rec bs) =
  (if rec then keywordText keyword'rec <> " " else "") <>
  "{ " <> r bs <> "}"
  where
    r = Text.concat . fmap (\b -> render'dictBinding b <> " ")

{- | Render a binding within a 'Dict', including the trailing semicolon. -}

render'dictBinding :: Render DictBinding
render'dictBinding =
  \case
    DictBinding'Eq a b ->
      render'expression'dictKey a <> " = " <> render'expression b <> ";"
    DictBinding'Inherit'Dict a b ->
      "inherit " <> render'expression'inParens a <>
      Text.concatMap (\x -> " " <> render'strStatic'unquotedIfPossible x) b <>
      ";"
    DictBinding'Inherit'Var a ->
      "inherit" <> Text.concatMap (\x -> " " <> render'var x) a <> ";"

{- | Render a dot expression (@a.b@). -}

render'dot :: Render Dot
render'dot (Dot a b) =
  render'expression'dotLeftContext a <> "." <> render'expression'dictKey b

{- | Render a @let@-@in@ expression. -}

render'let :: Render Let
render'let (Let bs x) =
  keywordText keyword'let <> " " <> r bs <>
  keywordText keyword'in <> " " <> render'expression x
  where
    r = Text.concat . fmap (\b -> render'letBinding b <> " ")

{- | Render a binding within a 'Let', including the trailing semicolon. -}

render'letBinding :: Render LetBinding
render'letBinding =
  \case
    LetBinding'Eq a b ->
      render'var a <> " = " <> render'expression b <> ";"
    LetBinding'Inherit a b ->
      "inherit " <> render'expression'inParens a <>
      Text.concatMap (\x -> " " <> render'var x) b <> ";"

{- | Render an expression. -}

-- | ==== Examples
--
-- >>> :{
-- >>> render'expression
-- >>>   (lambda
-- >>>     (param "a" <> pattern
-- >>>       [ dict'param "f"
-- >>>       , dict'param "b" & def (apply (var "g") (var "x"))
-- >>>       ] <> ellipsis)
-- >>>     (apply (var "f") (var "b")))
-- >>> :}
-- "a@{ f, b ? g x, ... }: f b"

render'expression :: Render Expression
render'expression =
  \case
    Expr'Str    x -> render'strDynamic'quoted x
    Expr'Dict   x -> render'dict x
    Expr'List   x -> render'list x
    Expr'Var    x -> render'var x
    Expr'Dot    x -> render'dot x
    Expr'Lambda x -> render'lambda x
    Expr'Apply  x -> render'apply x
    Expr'Let    x -> render'let x

{- | Render an expression in a list context. -}

render'expression'listContext :: Render Expression
render'expression'listContext x =
  case x of
    Expr'Lambda _ -> render'expression'inParens x
    Expr'Apply  _ -> render'expression'inParens x
    Expr'Let    _ -> render'expression'inParens x
    _             -> render'expression x

{- | Render an expression in the context of the left-hand side of a 'Dot'. -}

render'expression'dotLeftContext :: Render Expression
render'expression'dotLeftContext = render'expression'listContext

{- | Render an expression in the context of the left-hand side of an 'Apply'. -}

render'expression'applyLeftContext :: Render Expression
render'expression'applyLeftContext x =
  case x of
    Expr'Lambda _ -> render'expression'inParens x
    Expr'Let    _ -> render'expression'inParens x
    _             -> render'expression x

{- | Render an expression in the context of the right-hand side of an 'Apply'.
-}

render'expression'applyRightContext :: Render Expression
render'expression'applyRightContext x =
  case x of
    Expr'Apply  _ -> render'expression'inParens x
    Expr'Let    _ -> render'expression'inParens x
    _             -> render'expression x

render'expression'inParens :: Render Expression
render'expression'inParens x =
  "(" <> render'expression x <> ")"

render'expression'dictKey :: Render Expression
render'expression'dictKey = \case
  Expr'Str x -> render'strDynamic'unquotedIfPossible x
  x -> "${" <> render'expression x <> "}"
