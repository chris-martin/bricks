{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell #-}

import Bricks
import Bricks.Test.Internal

import Control.Applicative ((<|>))
import Data.Either (Either (..))
import Data.Function (const)
import Data.Functor (($>), void)
import Data.Maybe (Maybe (..), catMaybes)
import Data.Semigroup ((<>))
import Data.Text (Text)
import Hedgehog (property, (===))
import Text.Parsec.Text (Parser)

import qualified Data.Text as Text
import qualified Hedgehog
import qualified Text.Parsec as P

main = runTests $$(Hedgehog.discover)

{- | We'll use the @parseTest@ function a lot to test parsers. It's a lot like
'P.parseTest' from the parsec library, but it works on parsers of type 'Text'
rather than @'Show' a => a@. It also prints the unparsed input so we can verify
that our parser consumes the right amount of input, and it prints a message if
the parser fails and consumes input. -}
parseTest :: Parser Text -> Text -> Text
parseTest p input =
  Text.intercalate "\n" $
  catMaybes

    [ Just $ case P.parse p "" input of
        Left err ->
          "Parse error at " <>
          Text.replace "\n" "\n - " (Text.pack (show err))
        Right x -> x

    , let
        p' = do
          _ <- p
          r <- P.many1 P.anyChar
          pure $ "Remaining input: " <> Text.pack (show r)
      in
        case P.parse p' "" input of
          Left _  -> Nothing
          Right x -> Just x

    , case P.parse (void p <|> pure ()) "" input of
        Left _  -> Just "Parser failed and consumed input"
        Right _ -> Nothing

    ]

prop_bare_identifier_parser = property $ do

  let test = parseTest $ fmap bareIdText $ bareIdP

  test "-ab_c" === [text|-ab_c|]

  test ""      === [text|Parse error at (line 1, column 1):
                        | - unexpected end of input
                        | - expecting bare identifier|]

  test "a\"b"  === [text|a
                        |Remaining input: "\"b"|]

  test "a b"   === [text|a
                        |Remaining input: "b"|]

  -- The bare identifier parser doesn't backtrack.
  -- Note how in this example it consumes "rec" when it fails.
  test "rec { }" === [text|Parse error at (line 1, column 4):
                          | - unexpected " "
                          |Parser failed and consumed input|]

prop_identifier_parser = property $ do

  let test = parseTest $ fmap renderStrExpr $ idExprP

  test "a"     === [text|"a"|]

  test "\"a\"" === [text|"a"|]

  test "a b"   === [text|"a"
                        |Remaining input: "b"|]

prop_string_parser_normal = property $ do

  let test = parseTest $ fmap renderStrExpr $ strExprP'normal

  test "\"a\""        === [text|"a"|]

  test "\"a\" x"      === [text|"a"
                               |Remaining input: "x"|]

  test "\"a ${b} c\"" === [text|"a ${b} c"|]

  test "\"a${ b }c\"" === [text|"a${b}c"|]

  test "\"$\""        === [text|"$"|]

  test "\"a$\""       === [text|"a$"|]

  test "\"\\${\""     === [text|"\${"|]

  test "\"a\\${\""    === [text|"a\${"|]

prop_string_parser_indented = property $ do

  let test = parseTest
           $ fmap renderStrExpr
           $ P.spaces *> strExprP'indented

  test "''hello''x"    === [text|"hello"
                                |Remaining input: "x"|]

  test "''hello'' x"   === [text|"hello"
                                |Remaining input: "x"|]

  test [text|  ''
            |    one
            |    two
            |  ''x|] === [text|"one\ntwo"
                              |Remaining input: "x"|]

  test [text|  ''
            |    one
            |
            |    two
            |  ''x|] === [text|"one\n\ntwo"
                              |Remaining input: "x"|]

prop_indented_string_parser = property $ do

  let test = parseTest
           $ fmap
               ( Text.pack . show
               . (\(IndentedString xs) -> renderIndentedStringLine <$> xs)
               )
           $ P.spaces *> indentedStringP

  test [text|  ''
            |    one
            |    two
            |  ''x|] === [text|["","    one","    two","  "]
                              |Remaining input: "x"|]

  test [text|  ''
            |    one
            |
            |    two
            |  ''x|] === [text|["","    one","","    two","  "]
                              |Remaining input: "x"|]

  test "'''' x"   === [text|[""]
                           |Remaining input: "x"|]

  test "''abc''"  === [text|["abc"]|]

  test "''\n''"   === [text|["",""]|]

  test "''  \n''" === [text|["  ",""]|]

  test "''   abc\ndef''" === [text|["   abc","def"]|]

prop_indented_string_line_parser = property $ do

  let test = parseTest
           $ fmap (Text.pack . show . renderIndentedStringLine)
           $ indentedStringLineP

  test "abc" === [text|Parse error at (line 1, column 4):
                      | - unexpected end of input
                      | - expecting "$", "'", "\n", "''" or "${"
                      |Parser failed and consumed input|]

  test "\n"   === [text|""
                       |Remaining input: "\n"|]

  test "  \n" === [text|"  "
                       |Remaining input: "\n"|]

  test "   abc\ndef" === [text|"   abc"
                              |Remaining input: "\ndef"|]

  test "   abc''x"   === [text|"   abc"
                              |Remaining input: "''x"|]

prop_dict_param_start_parser = property $ do

  let test = parseTest $ (P.try dictParamStartP $> "yes") <|> pure "no"

  test "{a, b}:"     === [text|yes
                              |Remaining input: " b}:"|]

  test "{a ? 4, b}:" === [text|yes
                              |Remaining input: " 4, b}:"|]

  test "{ }: x"      === [text|yes
                              |Remaining input: " x"|]

  -- { } is not enough to determine whether we're parsing a dict param, because
  -- if it isn't followed by a colon, then it's actually an empty dict literal.
  test "{ } x"       === [text|no
                              |Remaining input: "{ } x"|]

  test "{ ... }:"    === [text|yes
                              |Remaining input: " }:"|]

prop_dots_parser = property $ do

  let test = parseTest
           $ fmap (Text.intercalate "\n" . fmap renderStrExpr)
           $ dotsP

  -- The dots parser /does/ match the empty string.
  test ""         === [text||]

  -- The simplest nonempty dot list
  test ".a"       === [text|"a"|]

  -- The dots parser consumes any trailing whitespace beyond the dot list.
  test ".a "      === [text|"a"|]

  -- Dot attributes are usually bare identifiers, but they may also be quoted.
  test ".\"a\""   === [text|"a"|]

  -- Here we throw some extra whitespace into the middle, which makes no
  -- difference, and some extra stuff onto the end, which does not get consumed.
  test ".a . b c" === [text|"a"
                           |"b"
                           |Remaining input: "c"|]

  -- Another example of a quoted dot, this time following an unquoted dot
  test ".a.\"b\"" === [text|"a"
                           |"b"|]

  -- If quotes or braces are involved, the stuff that follows a dot expression
  -- can directly abut it with no whitespace in between.
  test [text|.a."b"x|] === [text|"a"
                                |"b"
                                |Remaining input: "x"|]

  test [text|.a.b"x"|] === [text|"a"
                                |"b"
                                |Remaining input: "\"x\""|]

  test ".a.b(x)" === [text|"a"
                          |"b"
                          |Remaining input: "(x)"|]

prop_expression_parser = property $ do

  let test = parseTest
           $ fmap (renderExpression RenderContext'Normal)
           $ expressionP

  -- The empty string is /not/ a valid expression.
  test "" === [text|Parse error at (line 1, column 1):
                   | - unexpected end of input
                   | - expecting expression|]

  -- A very simple expression: a one-letter bare identifier
  test "a"         === [text|a|]

  -- Parsing an expression consumes any subsequent whitespace.
  test "a "        === [text|a|]

  -- When there are multiple expressions, that is parsed as a function call.
  test "f x"       === [text|f x|]

  -- Expressions can directly abut each other, so it's important that the
  -- expression parser is also able to read an expression even when another
  -- expression directly follows it.
  test "f[x y]"    === [text|f [ x y ]|]

  -- A simple example of parsing a dot expression
  test "a.b"       === [text|a.b|]

  -- Dot parsing also consumes trailing whitespace.
  test "a.b "      === [text|a.b|]

  -- It looks odd when a subsequent expression appears after a dot expression
  -- with no whitespace, but it is permitted.
  test "a.b\"c\""  === [text|a.b "c"|]

  -- A simple list example
  test "[ a b ]"   === [text|[ a b ]|]

  -- A list with trailing whitespace that get consumed
  test "[ a b ] "  === [text|[ a b ]|]

  -- A list that is in the left-hand side of a function call. This will fail at
  -- runtime if the call is evaluated, because a list is not a function, but it
  -- should /parse/ successfully.
  test "[ a b ] x" === [text|[ a b ] x|]

  -- The same thing with other weird stuff on the left-hand side of a function
  -- call.
  test "{ a = b; } x" === [text|{ a = b; } x|]

  test "{ } x"        === [text|{ } x|]

  -- Note that the case where an empty dict is on the left-hand side of a
  -- function call looks very similar to the case where a function expression
  -- using dict deconstruction with no bindings. The only difference is the
  -- colon.
  test "{ }: x"      === [text|{ }: x|]

  -- A list with a function call inside
  test "[ (f x) ]"   === [text|[ (f x) ]|]

  test "[ a (f x) ]" === [text|[ a (f x) ]|]

  -- A minimal dict literal
  test "{ x = y; }"  === [text|{ x = y; }|]

  -- The left-hand side of a binding is allowed to be anything, even something
  -- that would not be valid as a bare identifier, if it's in quotes.
  test "{ \"a b\" = y; }"  === [text|{ "a b" = y; }|]

  -- It may even be the empty string.
  test "{ \"\" = y; }"     === [text|{ "" = y; }|]

  -- None of the conventional whitespace within a dict literal is mandatory.
  test "{x=y;}"            === [text|{ x = y; }|]

  -- A simple dict literal with two bindings
  test "{ x = y; a = b; }" === [text|{ x = y; a = b; }|]

  -- The same thing without any whitespace
  test "{x=y;a=b;}" === [text|{ x = y; a = b; }|]

  -- A simple function
  test "x : y" === [text|x: y|]

  -- Whitespace before the colon is unconventional, but allowed.
  test "x : y" === [text|x: y|]

  -- The space after the colon is not mandatory. (In Nix, this example would be
  -- parsed as the string "x:y", but here we do not support URI literals.)
  test "x:y"   === [text|x: y|]

  -- A slightly bigger example where we're starting to nest more things
  test "[ \"abc\" f { x = y; } ]"   === [text|[ "abc" f { x = y; } ]|]

  test "[ \"abc\" (f { x = y; }) ]" === [text|[ "abc" (f { x = y; }) ]|]

  -- This is not valid a expression (though the first bit of it is).
  test "a b: c"   === [text|a b
                           |Remaining input: ": c"|]

  -- This is not a valid expression.
  test "(a b: c)" === [text|Parse error at (line 1, column 5):
                           | - unexpected ":"
                           | - expecting expression list item or ")"
                           |Parser failed and consumed input|]

  -- Some functions that use dict deconstruction
  test "{ a, b, c ? x, ... }: g b (f a c)"
    === [text|{ a, b, c ? x, ... }: g b (f a c)|]

  test "{ x, ... }: f x"  === [text|{ x, ... }: f x|]
  test "{ x?\"abc\" }: x" === [text|{ x ? "abc" }: x|]
  test "{ ... }: x"       === [text|{ ... }: x|]

  -- A let expression
  test "let f = x: plus one x; in f seven"
    === [text|let f = x: plus one x; in f seven|]

  -- A let binding list may be empty, although it is silly.
  test "let in f x" === [text|let in f x|]

  test "with x; y"  === [text|with x; y|]

  test "with{x=y;}; f x z" === [text|with { x = y; }; f x z|]

prop_dot_parser = property $ do

  let test = parseTest $ fmap renderStrExpr $ dotP

  test ".a"        === [text|"a"|]

  test ". \"a\""   === [text|"a"|]

  test ". a . b"   === [text|"a"
                            |Remaining input: ". b"|]

  test ". \"a\".b" === [text|"a"
                            |Remaining input: ".b"|]

prop_expression_list_parser = property $ do

  let test = parseTest
           $ fmap ( Text.intercalate "\n"
                  . fmap (renderExpression RenderContext'Normal)
                  )
           $ expressionListP

  test "" === [text||]

  test "x y z" === [text|x
                        |y
                        |z|]


  test "(a)b c(d)" === [text|a
                            |b
                            |c
                            |d|]

  test "a.\"b\"c" === [text|a.b
                           |c|]

  test "\"abc\" (f { x = y; })" === [text|"abc"
                                         |f { x = y; }|]

  test "r re reck" === [text|r
                            |re
                            |reck|]

  test "r re rec { } reck" === [text|r
                                    |re
                                    |rec { }
                                    |reck|]

prop_expression_list_item_parser = property $ do

  let test = parseTest
           $ fmap (renderExpression RenderContext'Normal)
           $ expressionP'listItem

  test "abc def"  === [text|abc
                           |Remaining input: "def"|]
  test "a.b c"    === [text|a.b
                           |Remaining input: "c"|]
  test "a.\"b\"c" === [text|a.b
                           |Remaining input: "c"|]
  test "(a.b)c"   === [text|a.b
                           |Remaining input: "c"|]
  test "a.b(c)"   === [text|a.b
                           |Remaining input: "(c)"|]
  test "[ a b ]c" === [text|[ a b ]
                           |Remaining input: "c"|]
  test "a[ b c ]" === [text|a
                           |Remaining input: "[ b c ]"|]
  test "\"a\"b"   === [text|"a"
                           |Remaining input: "b"|]

prop_expression_list_item_no_dot_parser = property $ do

  let test = parseTest
           $ fmap (renderExpression RenderContext'Normal)
           $ expressionP'listItem'noDot

  test "a.b c" === [text|a
                        |Remaining input: ".b c"|]
