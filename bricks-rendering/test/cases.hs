{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

-- Bricks
import Bricks.Expression
import Bricks.Expression.Construction
import Bricks.Rendering

-- Bricks internal
import           Bricks.Internal.Prelude
import qualified Bricks.Internal.Seq     as Seq
import           Bricks.Internal.Text    (Text)

-- Bricks test
import Bricks.Test.Hedgehog
import Bricks.Test.QQ

-- Hedgehog
import           Hedgehog (Property, property, withTests, (===))
import qualified Hedgehog

-- Base
import System.IO (IO)

main :: IO ()
main = runTests $$(Hedgehog.discover)

prop_render_expression :: Property
prop_render_expression = withTests 1 $ property $ do

  render'expression
    renderContext'default
    (dot (var "a") (str ["b"]))
    === [text|a.b|]

  render'expression
    renderContext'default
    (dot (var "a") (var "b"))
    === [text|a.${b}|]

  render'expression
    renderContext'default
    (dot (str ["a"]) (str ["b", antiquote (var "c")]))
    === [text|"a"."b${c}"|]

  render'expression
    renderContext'default
    (lambda
      (param "a" <> pattern
        [ dict'param "f"
        , dict'param "b" & def (apply (var "g") (var "x"))
        ] <> ellipsis)
      (apply (var "f") (var "b")))
    === [text|┃a@{ f, b ? g x, ... }:
              ┃f b|]

  render'expression
    renderContext'default
    (let'in
      [ let'eq "d" (dict
        [ dict'eq (str ["a"]) (str ["b", antiquote (var "c")])
        , dict'inherit'from (var "x") ["y", "z"]
        ])]
      (dot (var "d") (str ["y"])))
    === [text|┃let
              ┃  d = {
              ┃      a = "b${c}";
              ┃      inherit (x) y z;
              ┃    };
              ┃in
              ┃  d.y|]

prop_render_identifier :: Property
prop_render_identifier = withTests 1 $ property $ do

  let
    test :: Text -> Text
    test x =
      render'strStatic'unquotedIfPossible renderContext'default $
      Str'Static x Nothing

  test "abc"  === [text|abc|]
  test "a\"b" === [text|"a\"b"|]
  test "-ab"  === [text|-ab|]
  test ""     === [text|""|]

prop_render_string_dynamic_quoted :: Property
prop_render_string_dynamic_quoted = withTests 1 $ property $ do

  let
    test :: [Str'1] -> Text
    test xs =
      render'strDynamic'quoted renderContext'default $
      Str'Dynamic (Seq.fromList xs) Nothing

  test []                                             === [text|""|]
  test [ Str'1'Literal (Str'Static "hello" Nothing) ] === [text|"hello"|]

  test [ Str'1'Literal (Str'Static "escape ${ this and \" this" Nothing) ]
    === [text|"escape \${ this and \" this"|]

  test [ Str'1'Literal (Str'Static "Hello, my name is " Nothing)
       , Str'1'Antiquote (var "name")
       , Str'1'Literal (Str'Static "!" Nothing)
       ]
    === [text|"Hello, my name is ${name}!"|]

prop_render_dict_pattern :: Property
prop_render_dict_pattern = withTests 1 $ property $ do

  let
    test :: [DictPattern'1] -> Bool -> Text
    test a b =
      render'dictPattern renderContext'default $
      DictPattern (Seq.fromList a) b

  test [] False === [text|{ }|]
  test [] True  === [text|{ ... }|]

  let
    item1 = dict'param "x"
    item2 = dict'param "y" & def (str ["abc"])

  test [ item1, item2 ] False === [text|{ x, y ? "abc" }|]
  test [ item1, item2 ] True  === [text|{ x, y ? "abc", ... }|]

prop_render_list :: Property
prop_render_list = withTests 1 $ property $ do

  let
    test :: [Expression] -> Text
    test x =
      render'list renderContext'default $
      List (Seq.fromList x) Nothing

  test []                   === [text|[ ]|]
  test [ var "a" ]          === [text|┃[
                                      ┃  a
                                      ┃]|]
  test [ var "a", var "b" ] === [text|┃[
                                      ┃  a
                                      ┃  b
                                      ┃]|]

  let a = apply (var "f") (var "x")

  test [ a ]          === [text|┃[
                                ┃  (f x)
                                ┃]|]
  test [ a, var "a" ] === [text|┃[
                                ┃  (f x)
                                ┃  a
                                ┃]|]
