{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

-- Bricks
import Bricks
import Bricks.Expression.Construction

-- Bricks internal
import Bricks.Internal.Prelude

-- Bricks test
import Bricks.Test.Hedgehog
import Bricks.Test.QQ

-- Hedgehog
import           Hedgehog (Property, property, (===))
import qualified Hedgehog

-- Base
import System.IO (IO)

main :: IO ()
main = runTests $$(Hedgehog.discover)

prop_render_expression :: Property
prop_render_expression = property $ do

  render'expression (dot (var "a") (str ["b"])) === [text|a.b|]

  render'expression (dot (var "a") (var "b"))   === [text|a.${b}|]

  render'expression (dot (str ["a"])
    (str ["b", antiquote (var "c")]))           === [text|"a"."b${c}"|]

  render'expression
    (lambda
      (param "a" <> pattern
        [ dict'param "f"
        , dict'param "b" & def (apply (var "g") (var "x"))
        ] <> ellipsis)
      (apply (var "f") (var "b")))
    === [text|a@{ f, b ? g x, ... }: f b|]

  render'expression
    (let'in
      [ let'eq "d" (dict
        [ dict'eq (str ["a"]) (str ["b", antiquote (var "c")])
        , dict'inherit'from (var "x") ["y", "z"]
        ])]
      (dot (var "d") (str ["y"])))
    === [text|let d = { a = "b${c}"; inherit (x) y z; }; in d.y|]

prop_render_identifier :: Property
prop_render_identifier = property $ do

  let test = render'strStatic'unquotedIfPossible . Str'Static

  test "abc"  === [text|abc|]
  test "a\"b" === [text|"a\"b"|]
  test "-ab"  === [text|-ab|]
  test ""     === [text|""|]

prop_render_string_dynamic_quoted :: Property
prop_render_string_dynamic_quoted = property $ do

  let test = render'strDynamic'quoted . strDynamic'fromList

  test []                                     === [text|""|]
  test [ Str'1'Literal (Str'Static "hello") ] === [text|"hello"|]

  test [ Str'1'Literal (Str'Static "escape ${ this and \" this") ]
    === [text|"escape \${ this and \" this"|]

  test [ Str'1'Literal (Str'Static "Hello, my name is ")
       , Str'1'Antiquote $ var "name"
       , Str'1'Literal (Str'Static "!")
       ]
    === [text|"Hello, my name is ${name}!"|]

prop_render_dict_pattern :: Property
prop_render_dict_pattern = property $ do

  let test a b = render'dictPattern $ DictPattern a b

  test [] False === [text|{ }|]
  test [] True  === [text|{ ... }|]

  let
    item1 = dict'param "x"
    item2 = dict'param "y" & def (str ["abc"])

  test [ item1, item2 ] False === [text|{ x, y ? "abc" }|]
  test [ item1, item2 ] True  === [text|{ x, y ? "abc", ... }|]

prop_render_list :: Property
prop_render_list = property $ do

  let test = render'list . List

  test []                   === [text|[ ]|]
  test [ var "a" ]          === [text|[ a ]|]
  test [ var "a", var "b" ] === [text|[ a b ]|]

  let call = Expr'Apply $ Apply (var "f") (var "x")

  test [ call ]          === [text|[ (f x) ]|]
  test [ call, var "a" ] === [text|[ (f x) a ]|]
