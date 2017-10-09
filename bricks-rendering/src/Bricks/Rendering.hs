{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Bricks.Rendering
  (
  -- * @Render@
    Render
  , RenderContext (..)
  , renderContext'default
  , renderContext'terse

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

  -- ** Static strings
  , render'strStatic'unquotedIfPossible
  , render'strStatic'quoted

  -- ** Dynamic strings
  , render'strDynamic'unquotedIfPossible
  , render'strDynamic'quoted
  , render'str'1

  -- ** Indented strings
  , render'str'indented
  , render'str'indented'1

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
import           Bricks.Internal.Seq     (Seq)
import qualified Bricks.Internal.Seq     as Seq
import           Bricks.Internal.Text    (Text)
import qualified Bricks.Internal.Text    as Text

-- base
import Prelude (Num (..), fromIntegral)

--------------------------------------------------------------------------------

-- $setup
--
-- >>> import Bricks.Expression.Construction

--------------------------------------------------------------------------------

type Render a = RenderContext -> a -> Text

data RenderContext =
  RenderContext
    { renderContext'indentStart :: Natural
    , renderContext'indentStep :: Natural
    , renderContext'lineBreaks :: Bool
    }

renderContext'default :: RenderContext
renderContext'default =
  RenderContext
    { renderContext'indentStart = 0
    , renderContext'indentStep = 2
    , renderContext'lineBreaks = True
    }

renderContext'terse :: RenderContext
renderContext'terse =
  renderContext'default
    { renderContext'lineBreaks = False
    }

indentMore :: RenderContext -> RenderContext
indentMore x =
  x{ renderContext'indentStart = renderContext'indentStart x +
                                 renderContext'indentStep x }

indentation :: Natural -> Text
indentation n = Text.replicate (fromIntegral n) " "


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
render'var _cx v = var'text v

{- | Render a static string, in unquoted form if possible. -}

render'strStatic'unquotedIfPossible :: Render Str'Static
render'strStatic'unquotedIfPossible _cx s =
  let
    x = str'static'text s
  in
    if text'canBeUnquoted x
      then x
      else render'strStatic'quoted _cx s

{- | Render a static string, in quoted form. -}

render'strStatic'quoted :: Render Str'Static
render'strStatic'quoted _cx x =
  "\"" <> (str'escape . str'static'text) x <> "\""

{- | Render a dynamic string, in unquoted form if possible. -}

render'strDynamic'unquotedIfPossible :: Render Str'Dynamic
render'strDynamic'unquotedIfPossible cx d =
  case str'dynamic'to'static d of
    Just s  -> render'strStatic'unquotedIfPossible cx s
    Nothing -> render'strDynamic'quoted cx d

{- | Render a dynamic string, in quoted form. -}

render'strDynamic'quoted :: Render Str'Dynamic
render'strDynamic'quoted cx xs =
  "\"" <> foldMap (render'str'1 cx) (strDynamic'toSeq xs) <> "\""

render'str'1 :: Render Str'1
render'str'1 cx =
  \case
    Str'1'Literal x -> (str'escape . str'static'text) x
    Str'1'Antiquote x ->
      let cx' = cx{ renderContext'lineBreaks = False }
      in  "${" <> render'expression cx' x <> "}"

render'str'indented :: Render InStr
render'str'indented cx (inStr'dedent . inStr'trim -> (InStr xs _)) =
  "''\n" <>
  Text.concatMap (render'str'indented'1 (indentMore cx)) xs <>
  indentation (renderContext'indentStart cx) <> "''"

render'str'indented'1 :: Render InStr'1
render'str'indented'1 cx x =
  indentation (inStr'1'level x + renderContext'indentStart cx) <>
  Text.concatMap (render'str'1 cx) (inStr'1'str x) <>
  "\n"

{- | Render a lambda parameter: everything from the beginning of a lambda, up to
but not including the @:@ that separates the head from the body of the lambda.
-}

render'param :: Render Param
render'param cx =
  \case
    Param'Name a        -> render'var cx a
    Param'DictPattern b -> render'dictPattern cx b
    Param'Both a b      -> render'var cx a <> "@" <>
                           render'dictPattern cx b

{- | Render a dict pattern (@{ a, b ? c, ... }@). -}

render'dictPattern :: Render DictPattern
render'dictPattern cx (DictPattern bs e) =
  if Seq.null xs
    then "{ }"
    else "{ " <> Text.intercalate ", " xs <> " }"
  where
    xs :: Seq Text
    xs =
      Seq.map (render'dictPattern'1 cx) bs <>
      if e then Seq.singleton "..." else Seq.empty

{- | Render a single item in a 'DictPattern'. -}

render'dictPattern'1 :: Render DictPattern'1
render'dictPattern'1 cx =
  \case
    DictPattern'1 a Nothing  -> render'var cx a
    DictPattern'1 a (Just b) -> render'var cx a <> " ? " <>
                                render'expression cx b

{- | Render a lambda expression (@x: y@). -}

render'lambda :: Render Lambda
render'lambda cx x =
  render'param cx (lambda'head x) <> ":" <> sp <>
  render'expression cx' (lambda'body x)
  where
    sp = if lbr
         then "\n" <> indentation (renderContext'indentStart cx)
         else " "
    lbr = renderContext'lineBreaks cx
    cx' = indentMore cx

{- | Render a function application expression (@f x@). -}

render'apply :: Render Apply
render'apply cx x =
  render'expression'applyLeftContext cx (apply'func x) <> " " <>
  render'expression'applyRightContext cx (apply'arg x)

{- | Render a list literal (@[@ ... @]@). -}

render'list :: Render List
render'list cx x =
  if Seq.null (list'expressions x)
  then "[ ]"
  else "[" <> sp <> r (list'expressions x) <> "]"
  where
    r = Text.concat . fmap
          (\y ->
            (if lbr then indentation (renderContext'indentStart cx') else "") <>
            render'expression'listContext cx' y <> sp
          )
    sp = if lbr then "\n" else " "
    cx' = indentMore cx
    lbr = renderContext'lineBreaks cx

{- | Render a dict literal (@{@ ... @}@). -}

render'dict :: Render Dict
render'dict cx x =
  (if dict'rec x then keywordText keyword'rec <> " " else "") <>
  if Seq.null (dict'bindings x)
    then "{ }"
    else "{" <> sp <> r (dict'bindings x) <>
         (if lbr then indentation (renderContext'indentStart cx) else "") <> "}"
  where
    r = Text.concat . fmap
          (\b ->
            (if lbr then indentation (renderContext'indentStart cx') else "") <>
            render'dictBinding cx' b <> sp
          )
    sp = if lbr then "\n" else " "
    cx' = indentMore cx
    lbr = renderContext'lineBreaks cx

{- | Render a binding within a 'Dict', including the trailing semicolon. -}

render'dictBinding :: Render DictBinding
render'dictBinding cx =
  \case
    DictBinding'Eq a b ->
      render'expression'dictKey cx a <> " = " <>
      render'expression cx' b <> ";"
    DictBinding'Inherit'Dict a b ->
      "inherit " <> render'expression'inParens cx a <>
      Text.concatMap
        (\x ->
          " " <> render'strStatic'unquotedIfPossible cx x
        ) b <>
      ";"
    DictBinding'Inherit'Var a ->
      "inherit" <> Text.concatMap (\x -> " " <> render'var cx x) a <> ";"
  where
    cx' = indentMore cx

{- | Render a dot expression (@a.b@). -}

render'dot :: Render Dot
render'dot cx x =
  render'expression'dotLeftContext cx (dot'dict x) <> "." <>
  render'expression'dictKey cx (dot'key x)

{- | Render a @let@-@in@ expression. -}

render'let :: Render Let
render'let cx x =
  keywordText keyword'let <> sp <> r (let'bindings x) <>
  (if lbr then indentation (renderContext'indentStart cx) else "") <>
  keywordText keyword'in <> sp <>
  (if lbr then indentation (renderContext'indentStart cx') else "") <>
  render'expression cx' (let'value x)
  where
    r = Text.concat . fmap
          (\b ->
            (if lbr then indentation (renderContext'indentStart cx') else "") <>
            render'letBinding cx' b <> sp
          )
    cx' = indentMore cx
    sp = if lbr then "\n" else " "
    lbr = renderContext'lineBreaks cx

{- | Render a binding within a 'Let', including the trailing semicolon. -}

render'letBinding :: Render LetBinding
render'letBinding cx =
  \case
    LetBinding'Eq a b ->
      render'var cx a <> " = " <> render'expression cx' b <> ";"
    LetBinding'Inherit a b ->
      "inherit " <> render'expression'inParens cx a <>
      Text.concatMap (\x -> " " <> render'var cx x) b <> ";"
  where
    cx' = indentMore cx

{- | Render an expression. -}

-- | ==== Examples
--
-- >>> :{
-- >>> render'expression renderContext'terse
-- >>>   (lambda
-- >>>     (param "a" <> pattern
-- >>>       [ dict'param "f"
-- >>>       , dict'param "b" & def (apply (var "g") (var "x"))
-- >>>       ] <> ellipsis)
-- >>>     (apply (var "f") (var "b")))
-- >>> :}
-- "a@{ f, b ? g x, ... }: f b"

render'expression :: Render Expression
render'expression cx =
  \case
    Expr'Str x -> render'strDynamic'quoted cx x
    Expr'Str'Indented x -> render'str'indented cx x
    Expr'Dict x -> render'dict cx x
    Expr'List x -> render'list cx x
    Expr'Var x -> render'var cx x
    Expr'Dot x -> render'dot cx x
    Expr'Lambda x -> render'lambda cx x
    Expr'Apply x -> render'apply cx x
    Expr'Let x -> render'let cx x

{- | Render an expression in a list context. -}

render'expression'listContext :: Render Expression
render'expression'listContext cx x =
  case x of
    Expr'Lambda _ -> render'expression'inParens cx x
    Expr'Apply _ -> render'expression'inParens cx x
    Expr'Let _ -> render'expression'inParens cx x
    _ -> render'expression cx x

{- | Render an expression in the context of the left-hand side of a 'Dot'. -}

render'expression'dotLeftContext :: Render Expression
render'expression'dotLeftContext = render'expression'listContext

{- | Render an expression in the context of the left-hand side of an 'Apply'. -}

render'expression'applyLeftContext :: Render Expression
render'expression'applyLeftContext cx x =
  case x of
    Expr'Lambda _ -> render'expression'inParens cx x
    Expr'Let    _ -> render'expression'inParens cx x
    _ -> render'expression cx x

{- | Render an expression in the context of the right-hand side of an 'Apply'.
-}

render'expression'applyRightContext :: Render Expression
render'expression'applyRightContext cx x =
  case x of
    Expr'Apply _ -> render'expression'inParens cx x
    Expr'Let _ -> render'expression'inParens cx x
    _ -> render'expression cx x

render'expression'inParens :: Render Expression
render'expression'inParens cx x =
  "(" <> render'expression cx x <> ")"

render'expression'dictKey :: Render Expression
render'expression'dictKey cx = \case
  Expr'Str x -> render'strDynamic'unquotedIfPossible cx x
  x -> "${" <> render'expression cx x <> "}"
