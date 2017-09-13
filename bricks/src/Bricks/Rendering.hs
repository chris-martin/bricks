{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Bricks.Rendering where

-- Bricks
import Bricks.Bare
import Bricks.Expression
import Bricks.IndentedString
import Bricks.Keyword

-- Text
import           Data.Text (Text)
import qualified Data.Text as Text

-- Base
import Data.Bool      (Bool (..))
import Data.Foldable  (foldMap)
import Data.Function  ((.))
import Data.Functor   (Functor (..))
import Data.Maybe     (Maybe (..))
import Data.Semigroup ((<>))
import Prelude        (fromIntegral)

type Render a = a -> Text

-- | Insert escape sequences for rendering normal double-quoted (@"@) strings.
escape'normal :: Text -> Text
escape'normal =
  Text.replace "\"" "\\\"" .
  Text.replace "${" "\\${" .
  Text.replace "\n" "\\n" .
  Text.replace "\r" "\\r" .
  Text.replace "\t" "\\t"

-- | Insert escape sequences for rendering indented strings (quoted with @''@).
escape'indented :: Text -> Text
escape'indented =
  Text.replace "${" "\\${" .
  Text.replace "'" "\\'"

-- | Render a bare string, in bare (unquoted) form.
render'bare :: Render Bare
render'bare = bare'str

-- | Render a static string, in bare (unquoted) form if possible.
render'strStatic'maybeBare :: Render Str'Static
render'strStatic'maybeBare x =
  if canBeBare'str x then x else render'strStatic'quoted x

-- | Render a static string, in quoted form.
render'strStatic'quoted :: Render Str'Static
render'strStatic'quoted x =
  "\"" <> escape'normal x <> "\""

-- | Render a dynamic string, in bare (unquoted) form if possible.
render'strDynamic'maybeBare :: Render Str'Dynamic
render'strDynamic'maybeBare d =
  case str'dynamicToStatic d of
    Just s  -> render'strStatic'maybeBare s
    Nothing -> render'strDynamic'quoted d

-- | Render a dynamic string, in quoted form.
render'strDynamic'quoted :: Render Str'Dynamic
render'strDynamic'quoted xs =
  "\"" <> foldMap r xs <> "\""
  where
    r = \case
      Str'1'Literal x   -> escape'normal x
      Str'1'Antiquote x -> "${" <> render'expression x <> "}"

-- | Render one line of an indented string ('InStr').
render'inStr'1 :: Render InStr'1
render'inStr'1 (InStr'1 n xs) =
  Text.replicate (fromIntegral n) " " <> foldMap r xs
  where
    r = \case
      Str'1'Literal x   -> escape'indented x
      Str'1'Antiquote x -> "${" <> render'expression x <> "}"

-- | Render a lambda parameter: everything from the beginning of a lambda, up
-- to but not including the @:@ that separates the head from the body of the
-- lambda.
render'param :: Render Param
render'param =
  \case
    Param'Bare x -> render'bare x
    Param'DictPattern x -> render'dictPattern x

-- | Render a dict pattern (@{ a, b ? c, ... }@).
render'dictPattern :: Render DictPattern
render'dictPattern =
  \case
    DictPattern [] False -> "{ }"
    DictPattern [] True  -> "{ ... }"
    DictPattern xs False -> "{ " <> r xs <> " }"
    DictPattern xs True  -> "{ " <> r xs <> ", ... }"
  where
    r = Text.intercalate ", " . fmap render'dictPattern'1

-- | Render a single item in a 'DictPattern'.
render'dictPattern'1 :: Render DictPattern'1
render'dictPattern'1 =
  \case
    DictPattern'1 a Nothing  -> render'bare a
    DictPattern'1 a (Just b) -> render'bare a <> " ? " <> render'expression b

-- | Render a lambda expression (@x: y@).
render'lambda :: Render Lambda
render'lambda (Lambda a b) =
  render'param a <> ": " <> render'expression b

-- | Render a function application expression (@f x@).
render'apply :: Render Apply
render'apply (Apply a b) =
  render'expression'applyLeftContext a <> " " <>
  render'expression'applyRightContext b

-- | Render a list literal (@[ ... ]@).
render'list :: Render List
render'list xs =
  "[ " <> r xs <> "]"
  where
    r = Text.concat . fmap (\x -> render'expression'listContext x <> " ")

-- | Render a dict literal (@{ ... }@).
render'dict :: Render Dict
render'dict =
  \case
    Dict False bs ->     "{ " <> r bs <> "}"
    Dict True  bs -> "rec { " <> r bs <> "}"
  where
    r = Text.concat . fmap (\b -> render'dictBinding b <> "; ")

-- | Render a binding within a 'Dict', without the trailing semicolon.
render'dictBinding :: Render DictBinding
render'dictBinding =
  \case
    DictBinding'Eq a b ->
      render'expression'dictKey a <> " = " <> render'expression b
    DictBinding'Inherit Nothing xs ->
      "inherit" <> r'inheritItems xs
    DictBinding'Inherit (Just a) xs ->
      "inherit (" <> render'expression a <> ")" <> r'inheritItems xs
  where
    r'inheritItems = foldMap (\x -> " " <> render'strStatic'maybeBare x)

-- | Render a dot expression (@a.b@).
render'dot :: Render Dot
render'dot (Dot a b) =
  render'expression'dotLeftContext a <> "." <> render'expression'dictKey b

-- | Render a @let@-@in@ expression.
render'let :: Render Let
render'let (Let bs x) =
  "let " <> r bs <> "in " <> render'expression x
  where
    r = Text.concat . fmap (\b -> render'letBinding b <> "; ")

-- | Render a binding within a 'Let', without the trailing semicolon.
render'letBinding :: Render LetBinding
render'letBinding =
  \case
    LetBinding'Eq a b ->
      render'strStatic'maybeBare a <> " = " <> render'expression b
    LetBinding'Inherit Nothing xs ->
      "inherit" <> r'inheritItems xs
    LetBinding'Inherit (Just a) xs ->
      "inherit (" <> render'expression a <> ")" <> r'inheritItems xs
  where
    r'inheritItems = foldMap (\x -> " " <> render'strStatic'maybeBare x)

-- | Render a @with@ expression.
render'with :: Render With
render'with (With a b) =
  keywordText keyword'with <> " " <>
  render'expression a <> "; " <>
  render'expression b

-- | Render an expression.
render'expression :: Render Expression
render'expression =
  \case
    Expr'Str    x -> render'strDynamic'quoted x
    Expr'Dict   x -> render'dict x
    Expr'List   x -> render'list x
    Expr'Var    x -> render'bare x
    Expr'Dot    x -> render'dot x
    Expr'Lambda x -> render'lambda x
    Expr'Apply  x -> render'apply x
    Expr'Let    x -> render'let x
    Expr'With   x -> render'with x

-- | Render an expression in a list context.
render'expression'listContext :: Render Expression
render'expression'listContext x =
  case x of
    Expr'Lambda _ -> render'expression'inParens x
    Expr'Apply  _ -> render'expression'inParens x
    Expr'Let    _ -> render'expression'inParens x
    Expr'With   _ -> render'expression'inParens x
    _             -> render'expression x

-- | Render an expression in the context of the left-hand side of a 'Dot'.
render'expression'dotLeftContext :: Render Expression
render'expression'dotLeftContext = render'expression'listContext

-- | Render an expression in the context of the left-hand side of an 'Apply'.
render'expression'applyLeftContext :: Render Expression
render'expression'applyLeftContext x =
  case x of
    Expr'Lambda _ -> render'expression'inParens x
    Expr'Let    _ -> render'expression'inParens x
    Expr'With   _ -> render'expression'inParens x
    _             -> render'expression x

-- | Render an expression in the context of the right-hand side of an 'Apply'.
render'expression'applyRightContext :: Render Expression
render'expression'applyRightContext x =
  case x of
    Expr'Apply  _ -> render'expression'inParens x
    Expr'Let    _ -> render'expression'inParens x
    Expr'With   _ -> render'expression'inParens x
    _             -> render'expression x

render'expression'inParens :: Render Expression
render'expression'inParens x =
  "(" <> render'expression x <> ")"

render'expression'dictKey :: Render Expression
render'expression'dictKey = \case
  Expr'Str x -> render'strDynamic'maybeBare x
  x -> "${" <> render'expression x <> "}"
