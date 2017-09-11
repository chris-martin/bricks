{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell #-}

import Bricks
import Bricks.Test.Internal

import Hedgehog (property, (===))

import qualified Hedgehog

main = runTests $$(Hedgehog.discover)

{- | Any identifier can be /any/ string. In some cases this means we need to
render it in quotes; see 'isUnquotableText'. -}
prop_render_identifier = property $ do

  let test = renderIdentifier

  test "abc"  === [text|abc|]
  test "a\"b" === [text|"a\"b"|]
  test "-ab"  === [text|-ab|]
  test ""     === [text|""|]

prop_render_string = property $ do

  let test = renderStrExpr . StrExpr

  test []                              === [text|""|]
  test [ StrExprPart'Literal "hello" ] === [text|"hello"|]

  test [ StrExprPart'Literal "escape ${ this and \" this" ]
    === [text|"escape \${ this and \" this"|]

  test [ StrExprPart'Literal "Hello, my name is "
       , StrExprPart'Antiquote (Expr'Id (BareId "name"))
       , StrExprPart'Literal "!"
       ]
    === [text|"Hello, my name is ${name}!"|]

prop_render_indented_string_line = property $ do

  let test n xs = renderIndentedStringLine $ IndentedStringLine n (StrExpr xs)

  test 2 [ StrExprPart'Literal "abc"
         , StrExprPart'Antiquote (Expr'Id $ BareId "x")
         ]
    === [text|  abc${x}|]

prop_render_dict_param = property $ do

  let test a b = renderDictParam $ DictParam a b

  test [] False === [text|{ }:|]
  test [] True  === [text|{ ... }:|]

  let
    item1 = DictParamItem (BareId "x") Nothing
    item2 = DictParamItem (BareId "y") $
      Just . ParamDefault . Expr'Str $ StrExpr [ StrExprPart'Literal "abc" ]

  test [ item1, item2 ] False === [text|{ x, y ? "abc" }:|]
  test [ item1, item2 ] True  === [text|{ x, y ? "abc", ... }:|]

prop_render_list = property $ do

  let test = renderListLiteral . ListLiteral

  test []                       === [text|[ ]|]
  test [ Expr'Id (BareId "a") ] === [text|[ a ]|]
  test [ Expr'Id (BareId "a")
       , Expr'Id (BareId "b") ] === [text|[ a b ]|]

  let call = Expr'Call $ CallExpr (Expr'Id (BareId "f")) (Expr'Id (BareId "x"))

  test [ call ]                       === [text|[ (f x) ]|]
  test [ call, Expr'Id (BareId "a") ] === [text|[ (f x) a ]|]
