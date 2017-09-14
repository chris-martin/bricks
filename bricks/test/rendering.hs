{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

-- Bricks
import Bricks

-- Bricks internal
import Bricks.Internal.Prelude

-- Bricks test
import Bricks.Test.Hedgehog
import Bricks.Test.QQ

-- Hedgehog
import           Hedgehog (property, (===))
import qualified Hedgehog

main = runTests $$(Hedgehog.discover)

prop_render_identifier = property $ do

  let test = render'strStatic'unquotedIfPossible

  test "abc"  === [text|abc|]
  test "a\"b" === [text|"a\"b"|]
  test "-ab"  === [text|-ab|]
  test ""     === [text|""|]

prop_render_string_dynamic_quoted = property $ do

  let test = render'strDynamic'quoted . strDynamic'fromList

  test []                        === [text|""|]
  test [ Str'1'Literal "hello" ] === [text|"hello"|]

  test [ Str'1'Literal "escape ${ this and \" this" ]
    === [text|"escape \${ this and \" this"|]

  test [ Str'1'Literal "Hello, my name is "
       , Str'1'Antiquote (Expr'Var (Str'Unquoted'Unsafe "name"))
       , Str'1'Literal "!"
       ]
    === [text|"Hello, my name is ${name}!"|]

prop_render_indented_string_line = property $ do

  let test n xs = render'inStr'1 $ InStr'1 n (strDynamic'fromList xs)

  test 2 [ Str'1'Literal "abc"
         , Str'1'Antiquote (Expr'Var $ Str'Unquoted'Unsafe "x")
         ]
    === [text|  abc${x}|]

prop_render_dict_pattern = property $ do

  let test a b = render'dictPattern $ DictPattern a b

  test [] False === [text|{ }|]
  test [] True  === [text|{ ... }|]

  let
    item1 = DictPattern'1 (Str'Unquoted'Unsafe "x") Nothing
    item2 = DictPattern'1 (Str'Unquoted'Unsafe "y") $
      Just $ Expr'Str (strDynamic'singleton (Str'1'Literal "abc"))

  test [ item1, item2 ] False === [text|{ x, y ? "abc" }|]
  test [ item1, item2 ] True  === [text|{ x, y ? "abc", ... }|]

prop_render_list = property $ do

  let test = render'list . List

  test []                                     === [text|[ ]|]
  test [ Expr'Var (Str'Unquoted'Unsafe "a") ] === [text|[ a ]|]
  test [ Expr'Var (Str'Unquoted'Unsafe "a")
       , Expr'Var (Str'Unquoted'Unsafe "b") ] === [text|[ a b ]|]

  let call = Expr'Apply $ Apply (Expr'Var (Str'Unquoted'Unsafe "f"))
                                (Expr'Var (Str'Unquoted'Unsafe "x"))

  test [ call ]                            === [text|[ (f x) ]|]
  test [ call, Expr'Var (Str'Unquoted'Unsafe "a") ] === [text|[ (f x) a ]|]
