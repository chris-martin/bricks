{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

import Bricks
import Bricks.Test.Hedgehog
import Bricks.Test.QQ

import Hedgehog (property, (===))

import qualified Hedgehog

main = runTests $$(Hedgehog.discover)

prop_render_identifier = property $ do

  let test = render'strStatic'maybeBare

  test "abc"  === [text|abc|]
  test "a\"b" === [text|"a\"b"|]
  test "-ab"  === [text|-ab|]
  test ""     === [text|""|]

prop_render_string_dynamic_quoted = property $ do

  let test = render'strDynamic'quoted

  test []                        === [text|""|]
  test [ Str'1'Literal "hello" ] === [text|"hello"|]

  test [ Str'1'Literal "escape ${ this and \" this" ]
    === [text|"escape \${ this and \" this"|]

  test [ Str'1'Literal "Hello, my name is "
       , Str'1'Antiquote (Expr'Var (BareUnsafe "name"))
       , Str'1'Literal "!"
       ]
    === [text|"Hello, my name is ${name}!"|]

prop_render_indented_string_line = property $ do

  let test n xs = render'inStr'1 $ InStr'1 n xs

  test 2 [ Str'1'Literal "abc"
         , Str'1'Antiquote (Expr'Var $ BareUnsafe "x")
         ]
    === [text|  abc${x}|]

prop_render_dict_pattern = property $ do

  let test a b = render'dictPattern $ DictPattern a b

  test [] False === [text|{ }|]
  test [] True  === [text|{ ... }|]

  let
    item1 = DictPattern'1 (BareUnsafe "x") Nothing
    item2 = DictPattern'1 (BareUnsafe "y") $
      Just $ Expr'Str [ Str'1'Literal "abc" ]

  test [ item1, item2 ] False === [text|{ x, y ? "abc" }|]
  test [ item1, item2 ] True  === [text|{ x, y ? "abc", ... }|]

prop_render_list = property $ do

  let test = render'list

  test []                            === [text|[ ]|]
  test [ Expr'Var (BareUnsafe "a") ] === [text|[ a ]|]
  test [ Expr'Var (BareUnsafe "a")
       , Expr'Var (BareUnsafe "b") ] === [text|[ a b ]|]

  let call = Expr'Apply $ Apply (Expr'Var (BareUnsafe "f"))
                                (Expr'Var (BareUnsafe "x"))

  test [ call ]                            === [text|[ (f x) ]|]
  test [ call, Expr'Var (BareUnsafe "a") ] === [text|[ (f x) a ]|]
