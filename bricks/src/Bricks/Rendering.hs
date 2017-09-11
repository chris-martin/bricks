{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Bricks.Rendering where

import Bricks.Identifiers
import Bricks.Keywords
import Bricks.Types

import Control.Arrow  ((>>>))
import Data.Bool      (Bool (..))
import Data.Foldable  (Foldable, foldMap)
import Data.Function  ((.))
import Data.Functor   (Functor (..))
import Data.Maybe     (Maybe (..))
import Data.Semigroup ((<>))
import Data.Text      (Text)
import Prelude        (fromIntegral)

import qualified Data.Foldable as Foldable
import qualified Data.List     as List
import qualified Data.Text     as Text

renderIdentifier :: Text -> Text
renderIdentifier x =
  if isBareIdentifierName x then x else renderQuotedString x

renderBareId :: BareId -> Text
renderBareId (BareId x) = x

renderIdExpr :: StrExpr -> Text
renderIdExpr =
  \case
    StrExpr (Foldable.toList -> [StrExprPart'Literal x])
      | isBareIdentifierName x -> x
    x -> renderStrExpr x

renderStrExpr :: StrExpr -> Text
renderStrExpr (StrExpr xs) =
  "\"" <> renderStrExprParts xs <> "\""

renderStrExprParts :: [StrExprPart] -> Text
renderStrExprParts = foldMap renderStrExprPart

renderStrExprPart :: StrExprPart -> Text
renderStrExprPart =
  \case
    StrExprPart'Literal t -> strEscape t
    StrExprPart'Antiquote e -> renderAntiquote e

renderAntiquote :: Expression -> Text
renderAntiquote e =
  "${" <> renderExpression RenderContext'Normal e <> "}"

renderQuotedString :: Text -> Text
renderQuotedString x =
  "\"" <> strEscape x <> "\""

strEscape :: Text -> Text
strEscape =
  Text.replace "\"" "\\\"" .
  Text.replace "${" "\\${" .
  Text.replace "\n" "\\n" .
  Text.replace "\r" "\\r" .
  Text.replace "\t" "\\t"

renderIndentedStringLine :: IndentedStringLine -> Text
renderIndentedStringLine (IndentedStringLine n (StrExpr xs)) =
  Text.replicate (fromIntegral n) " " <> renderStrExprParts xs

renderIndentedStringLines :: [IndentedStringLine] -> Text
renderIndentedStringLines =
  foldMap renderIndentedStringLine

renderParam :: Param -> Text
renderParam =
  \case
    Param'Id x -> renderBareId x <> ":"
    Param'Dict x -> renderDictParam x

renderDictParam :: DictParam -> Text
renderDictParam (DictParam items ellipsis) =
  case Foldable.toList items of
    [] -> if ellipsis then "{ ... }:" else "{ }:"
    xs -> "{ " <> Text.intercalate ", " (fmap renderDictParamItem xs) <>
          (if ellipsis then ", ... }:" else " }:")

renderDictParamItem :: DictParamItem -> Text
renderDictParamItem =
  \case
    DictParamItem a Nothing  -> renderBareId a
    DictParamItem a (Just b) -> renderBareId a <> " " <>
                                renderParamDefault b

renderParamDefault :: ParamDefault -> Text
renderParamDefault (ParamDefault x) =
  "? " <> renderExpression RenderContext'Normal x

renderFuncExpr :: RenderContext -> FuncExpr -> Text
renderFuncExpr cx (FuncExpr a b) =
  if p then "(" <> x <> ")" else x

  where
    x = renderParam a <> " " <>
        renderExpression RenderContext'Normal b

    p = case cx of
      RenderContext'Normal   -> False
      RenderContext'List     -> True
      RenderContext'Call'lhs -> True
      RenderContext'Call'rhs -> False
      RenderContext'Dot'lhs  -> True

renderCallExpr :: RenderContext -> CallExpr -> Text
renderCallExpr cx (CallExpr a b) =
  if p then "(" <> x <> ")" else x

  where
    x = renderExpression RenderContext'Call'lhs a <> " " <>
        renderExpression RenderContext'Call'rhs b

    p = case cx of
      RenderContext'Normal   -> False
      RenderContext'List     -> True
      RenderContext'Call'lhs -> False
      RenderContext'Call'rhs -> True
      RenderContext'Dot'lhs  -> True

renderListLiteral :: ListLiteral -> Text
renderListLiteral =
  \case
    ListLiteral (Foldable.toList -> []) -> renderEmptyList
    ListLiteral (Foldable.toList -> values) ->
      "[ " <>
      foldMap (\v -> renderExpression RenderContext'List v <> " ") values <>
      "]"

renderEmptyList :: Text
renderEmptyList = "[ ]"

renderDictLiteral :: DictLiteral -> Text
renderDictLiteral =
  \case
    DictLiteral True  [] -> "rec " <> renderEmptyDict
    DictLiteral False [] -> renderEmptyDict
    DictLiteral True  bs -> "rec { " <> renderBindingList bs <> " }"
    DictLiteral False bs -> "{ " <> renderBindingList bs <> " }"

renderEmptyDict :: Text
renderEmptyDict = "{ }"

renderDot :: Dot -> Text
renderDot (Dot a b) =
  renderExpression RenderContext'Dot'lhs a <> "." <> renderIdExpr b

renderLetExpr :: LetExpr -> Text
renderLetExpr (LetExpr bs x) =
  if List.null bs
    then "let in " <> body
    else "let " <> renderBindingList bs <> " in " <> body
  where
    body = renderExpression RenderContext'Normal x

renderWith :: With -> Text
renderWith (With a b) =
  (keywordText keyword'with) <> " " <>
  renderExpression RenderContext'Normal a <> "; " <>
  renderExpression RenderContext'Normal b

renderBinding :: Binding -> Text
renderBinding (Binding a b) =
  renderIdExpr a <> " = " <> renderExpression RenderContext'Normal b <> ";"

renderBindingList :: Foldable f => f Binding -> Text
renderBindingList =
  Foldable.toList >>> \case
    [] -> ""
    bs -> Text.intercalate " " (fmap renderBinding bs)

renderExpression :: RenderContext -> Expression -> Text
renderExpression c =
  \case
    Expr'Str  x -> renderStrExpr x
    Expr'Dict x -> renderDictLiteral x
    Expr'List x -> renderListLiteral x
    Expr'Id   x -> renderBareId x
    Expr'Dot  x -> renderDot x
    Expr'Func x -> renderFuncExpr c x
    Expr'Call x -> renderCallExpr c x
    Expr'Let  x -> renderLetExpr x
    Expr'With x -> renderWith x

data RenderContext
  = RenderContext'Normal
  | RenderContext'List
  | RenderContext'Call'lhs
  | RenderContext'Call'rhs
  | RenderContext'Dot'lhs
