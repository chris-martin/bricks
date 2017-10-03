{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

-- Bricks
import Bricks

-- Bricks internal
import           Bricks.Internal.Prelude
import           Bricks.Internal.Text    (Text)
import qualified Bricks.Internal.Text    as Text
import qualified Bricks.Internal.List    as List
import qualified Bricks.Internal.Seq    as Seq

-- Bricks test
import Bricks.Test.Hedgehog
import Bricks.Test.QQ

-- Parsec
import qualified Text.Parsec      as P
import           Text.Parsec.Text (Parser)

-- Hedgehog
import           Hedgehog (Property, property, (===))
import qualified Hedgehog

-- Base
import System.IO (IO)
import Text.Show (show)

main :: IO ()
main = runTests $$(Hedgehog.discover)

{- | We'll use the @parseTest@ function a lot to test parsers. It's a bit like
'P.parseTest' from the Parsec library, but it works on parsers of type 'Text'
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

prop_parse_str_unquoted :: Property
prop_parse_str_unquoted = property $ do

  let test = parseTest $ fmap str'unquoted'text $ parse'strUnquoted

  test "-ab_c" === [text|-ab_c|]

  test ""      === [text|Parse error at (line 1, column 1):
                        | - unexpected end of input|]

  test "a\"b"  === [text|a
                        |Remaining input: "\"b"|]

  test "a b"   === [text|a
                        |Remaining input: "b"|]

  -- The unquoted string parser doesn't backtrack.
  -- Note how in this example it fails and consumes input.
  test "rec { }" === [text|Parse error at (line 1, column 4):
                          | - unexpected " "
                          |Parser failed and consumed input|]

prop_parse_expression_dictKey :: Property
prop_parse_expression_dictKey = property $ do

  let test = parseTest $ fmap render'expression $ parse'expression'dictKey

  test "a"      === [text|"a"|]

  test "\"a\""  === [text|"a"|]

  test "a b"    === [text|"a"
                         |Remaining input: "b"|]

  test "${a.b}" === [text|a.b|]

prop_parse_strDynamic_normalQ :: Property
prop_parse_strDynamic_normalQ = property $ do

  let test = parseTest
           $ fmap render'strDynamic'quoted
           $ parse'strDynamic'normalQ

  test "\"a\""        === [text|"a"|]

  test "\"a\" x"      === [text|"a"
                               |Remaining input: "x"|]

  test "\"a ${b} c\"" === [text|"a ${b} c"|]

  test "\"a${ b }c\"" === [text|"a${b}c"|]

  test "\"$\""        === [text|"$"|]

  test "\"a$\""       === [text|"a$"|]

  test "\"\\${\""     === [text|"\${"|]

  test "\"a\\${\""    === [text|"a\${"|]

prop_parse_strDynamic_indentedQ :: Property
prop_parse_strDynamic_indentedQ = property $ do

  let test = parseTest
           $ fmap render'strDynamic'quoted
           $ P.spaces *> parse'strDynamic'indentedQ

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

prop_parse_inStr :: Property
prop_parse_inStr = property $ do

  let test = parseTest
           $ fmap (
                Text.pack . show
                . List.concatMap (Seq.toList . inStr'1'toStrParts)
                . inStr'toList
             )
           $ P.spaces *> parse'inStr

  test [text|  ''
            |    one
            |    two
            |  ''x|] === [text|["\n","    ","one","\n","    ","two","\n","  "]
                              |Remaining input: "x"|]

  test [text|  ''
            |    one
            |
            |    two
            |  ''x|] === [text|["\n","    ","one","\n","\n","    ","two","\n","  "]
                              |Remaining input: "x"|]

  test "'''' x"   === [text|[]
                           |Remaining input: "x"|]

  test "''abc''"  === [text|["abc"]|]

  test "''\n''"   === [text|["\n"]|]

  test "''  \n''" === [text|["  ","\n"]|]

  test "''   abc\ndef''" === [text|["   ","abc","\n","def"]|]

prop_parse_inStr_line :: Property
prop_parse_inStr_line = property $ do

  let test = parseTest
           $ fmap (Text.pack . show . Seq.toList . inStr'1'toStrParts)
           $ parse'inStr'1

  test "abc" === [text|Parse error at (line 1, column 4):
                      | - unexpected end of input
                      | - expecting "$", "'", "\n", "''" or "${"
                      |Parser failed and consumed input|]

  test "\n"   === [text|["\n"]|]

  test "  \n" === [text|["  ","\n"]|]

  test "   abc\ndef" === [text|["   ","abc","\n"]
                              |Remaining input: "def"|]

  test "   abc''x"   === [text|["   ","abc"]
                              |Remaining input: "''x"|]

prop_parse_dict_pattern_start :: Property
prop_parse_dict_pattern_start = property $ do

  let test = parseTest $ (P.try parse'dictPattern'start $> "yes") <|> pure "no"

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

prop_parse_dict_pattern :: Property
prop_parse_dict_pattern = property $ do

  let test = parseTest $ fmap render'dictPattern $ parse'dictPattern

  test "{}"               === [text|{ }|]
  test "{...}"            === [text|{ ... }|]
  test "{a}"              === [text|{ a }|]
  test "{a , b}"          === [text|{ a, b }|]
  test "{a , b ? c, ...}" === [text|{ a, b ? c, ... }|]

prop_parse_dot_rhs_chain :: Property
prop_parse_dot_rhs_chain = property $ do

  let test = parseTest
           $ fmap (Text.intercalate "\n" . fmap render'expression)
           $ parse'dot'rhs'chain

  -- The dots parser /does/ match the empty string.
  test ""         === [text||]

  -- The simplest nonempty dot list
  test ".a"       === [text|"a"|]

  -- The dots parser consumes any trailing whitespace beyond the dot list.
  test ".a "      === [text|"a"|]

  -- Dot attributes are usually unquoted strings, but they may also be quoted.
  test ".\"a\""   === [text|"a"|]
  test ". \"a\""  === [text|"a"|]

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

  test ".a.b(x)"   === [text|"a"
                            |"b"
                            |Remaining input: "(x)"|]

  test ". a . b"   === [text|"a"
                            |"b"|]

  test ". \"a\".b" === [text|"a"
                            |"b"|]

  test ". \"a\".${b}" === [text|"a"
                               |b|]

prop_parse_expression :: Property
prop_parse_expression = property $ do

  let test = parseTest $ fmap render'expression $ parse'expression

  -- The empty string is /not/ a valid expression.
  test "" === [text|Parse error at (line 1, column 1):
                   | - unexpected end of input
                   | - expecting expression|]

  -- A very simple expression: a one-letter variable
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

  -- The left-hand side of a dict binding is allowed to be any expression.
  test "{ \"a b\" = y; }"  === [text|{ "a b" = y; }|]
  test "{ ${x} = y; }"     === [text|{ ${x} = y; }|]

  -- It may even be the empty string.
  test "{ \"\" = y; }"     === [text|{ "" = y; }|]

  -- None of the conventional whitespace within a dict literal is mandatory.
  test "{x=y;}"            === [text|{ x = y; }|]

  -- A simple dict literal with two bindings
  test "{ x = y; a = b; }" === [text|{ x = y; a = b; }|]

  -- The same thing without any whitespace
  test "{x=y;a=b;}" === [text|{ x = y; a = b; }|]

  -- Dicts with 'inherit' bindings
  test "{ inherit a; }"       === [text|{ inherit a; }|]
  test "{ inherit a b; }"     === [text|{ inherit a b; }|]
  test "{ inherit (x) a b; }" === [text|{ inherit (x) a b; }|]

  -- An inherit binding can be empty, although it is weird.
  test "{ inherit; }"   === [text|{ inherit; }|]

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

  test "{ x, ... }: f x"    === [text|{ x, ... }: f x|]
  test "{ x?\"abc\" }: x"   === [text|{ x ? "abc" }: x|]
  test "{ ... }: x"         === [text|{ ... }: x|]
  test "a@{ x, ... }: f x"  === [text|a@{ x, ... }: f x|]
  test "a@{ x?\"abc\" }: x" === [text|a@{ x ? "abc" }: x|]
  test "a@{ ... }: x"       === [text|a@{ ... }: x|]

  -- A let expression
  test "let f = x: plus one x; in f seven"
    === [text|let f = x: plus one x; in f seven|]

  -- A let binding list may be empty, although it is silly.
  test "let in f x" === [text|let in f x|]

  -- Indented strings do not support any escape sequences.
  test [text|''
            |  There \ is \n no \$ escape.
            |''|] === [text|"There \\ is \\n no \\$ escape."|]

  -- Therefore if you want to include something like '' in an indented string,
  -- you have to put it inside an antiquote.
  test [text|''
            |  Isn't it
            |  ${"''"}interesting
            |''
            |] === [text|"Isn't it\n${"''"}interesting"|]

  -- Comments
  test [text|let                -- hi
            |  x {- ! -} = "a"; -- yep
            | in                -- lol
            |   f x
            |] === [text|let x = "a"; in f x|]

  -- Nested block comments
  test [text|f{- a
            |  -- b
            |    {- c {- d
            |    -}-} e
            |  -}x|] === "f x"

prop_parse_expression_list :: Property
prop_parse_expression_list = property $ do

  let test = parseTest
           $ fmap (Text.intercalate "\n" . fmap render'expression)
           $ parse'expressionList

  test ""                       === [text||]

  test "x y z"                  === [text|x
                                         |y
                                         |z|]


  test "(a)b c(d)"              === [text|a
                                         |b
                                         |c
                                         |d|]

  test "a.\"b\"c"               === [text|a.b
                                         |c|]

  test "\"abc\" (f { x = y; })" === [text|"abc"
                                         |f { x = y; }|]

  -- Parsing lists of variables that are similar to keywords
  test "r re reck"              === [text|r
                                         |re
                                         |reck|]
  test "r re rec { } reck"      === [text|r
                                         |re
                                         |rec { }
                                         |reck|]
  test "l le lets"              === [text|l
                                         |le
                                         |lets|]
  test "i ins"                  === [text|i
                                         |ins|]
  test "inheri inherits"        === [text|inheri
                                         |inherits|]

prop_parse_expression_list_item :: Property
prop_parse_expression_list_item = property $ do

  let test = parseTest $ fmap render'expression $ parse'expressionList'1

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

prop_parse_expression_list_item_no_dot :: Property
prop_parse_expression_list_item_no_dot = property $ do

  let test = parseTest $ fmap render'expression $ parse'expressionList'1'noDot

  test "a.b c" === [text|a
                        |Remaining input: ".b c"|]
