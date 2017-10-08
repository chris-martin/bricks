{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Bricks.Expression
  (
  -- * Expressions
    Expression (..)
  , expression'source
  , expression'discardSource

  -- * Variables
  -- $variables
  , Var (..)
  , var'text
  , var'to'str'static
  , var'to'str'dynamic
  , var'discardSource

  -- * Static strings
  , Str'Static (..)
  , str'static'append
  , str'static'discardSource

  -- * Dynamic strings
  , Str'Dynamic (..)
  , Str'1 (..)
  , str'1'discardSource
  , str'dynamic'append
  , str'dynamic'normalize
  , str'dynamic'discardSource

  -- ** Conversions between types of strings
  , str'dynamic'to'static
  , str'static'to'dynamic

  -- * Lists
  , List (..)
  , list'discardSource

  -- * Dicts
  , Dict (..)
  , dict'discardSource
  , DictBinding (..)
  , dictBinding'discardSource

  -- * Dict lookup
  , Dot (..)
  , expression'applyDots
  , dot'discardSource

  -- * Lambdas
  , Lambda (..)
  , lambda'discardSource

  -- * Function parameters
  , Param (..)
  , param'discardSource
  , DictPattern (..)
  , dictPattern'discardSource
  , DictPattern'1 (..)
  , dictPattern'1'discardSource

  -- * Function application
  , Apply (..)
  , expression'applyArgs
  , apply'discardSource

  -- * @let@
  , Let (..)
  , let'discardSource
  , LetBinding (..)
  , letBinding'discardSource

  ) where

-- Bricks
import Bricks.Source
import Bricks.UnquotedString

-- Bricks internal
import           Bricks.Internal.List    as List
import           Bricks.Internal.Prelude
import           Bricks.Internal.Seq     (Seq)
import qualified Bricks.Internal.Seq     as Seq
import           Bricks.Internal.Text    (Text)
import qualified Bricks.Internal.Text    as Text

data Expression
  = Expr'Var Var
      -- ^ A variable, such as @x@.
  | Expr'Str Str'Dynamic
      -- ^ A string, quoted either in the traditional form using a single
      --   double-quote (@"@ ... @"@) or in the \"indented string\" form using
      --   two single-quotes (@''@ ... @''@).
  | Expr'List List
      -- ^ A list is an ordered collection of expressions.
  | Expr'Dict Dict
      -- ^ A dict is an unordered enumerated mapping from strings.
  | Expr'Dot Dot
      -- ^ A dot expression (named after the @.@ character it contains) looks up
      --   the value at a particular key in a dict.
  | Expr'Lambda Lambda
      -- ^ A lambda expression @x: y@ where @x@ is the parameter.
  | Expr'Apply Apply
      -- ^ The application of a function to a single argument.
  | Expr'Let Let
      -- ^ A /let/-/in/ expression consists of a list of variable bindings
      --   followed by an expression.

expression'source :: Expression -> Maybe SourceRange
expression'source =
  \case
    Expr'Var x    -> var'source x
    Expr'Str x    -> strDynamic'source x
    Expr'List x   -> list'source x
    Expr'Dict x   -> dict'source x
    Expr'Dot x    -> dot'source x
    Expr'Lambda x -> lambda'source x
    Expr'Apply x  -> apply'source x
    Expr'Let x    -> let'source x

expression'discardSource :: Expression -> Expression
expression'discardSource =
  \case
    Expr'Var x    -> Expr'Var $ var'discardSource x
    Expr'Str x    -> Expr'Str $ str'dynamic'discardSource x
    Expr'List x   -> Expr'List $ list'discardSource x
    Expr'Dict x   -> Expr'Dict $ dict'discardSource x
    Expr'Dot x    -> Expr'Dot $ dot'discardSource x
    Expr'Lambda x -> Expr'Lambda $ lambda'discardSource x
    Expr'Apply x  -> Expr'Apply $ apply'discardSource x
    Expr'Let x    -> Expr'Let $ let'discardSource x


--------------------------------------------------------------------------------
--  Variables
--------------------------------------------------------------------------------

{- | A /variable/ @x@, as in the lambda calculus sense, is in one of two
positions:

  1. A binding, which may take a number of forms:
       - @x:@ ... ('Param'Name')
       - @let x =@ ... @; in@ ... ('LetBinding'Eq')
       - @let inherit (@ ... @) x; in@ ... ('LetBinding'Inhherit')
  2. A contextual reference to a lambda head or /let/ binding in which @x@ is
     bound:
       - The expression @x@ by itself
       - An @inherit@ binding in a dict expression ('DictBinding'Inherit'Var')

==== Syntax

Variables are always written without quotes.

Unquoted strings are used for variables ('Expr'Var') and places that bind
variables ('Lambda' and 'Let').

-}

data Var =
  Var
    { var'str :: UnquotedString
    , var'source :: Maybe SourceRange
    }

var'text :: Var -> Text
var'text (Var x _) = unquotedString'text x

var'to'str'static :: Var -> Str'Static
var'to'str'static x =
  Str'Static (var'text x) (var'source x)

var'to'str'dynamic :: Var -> Str'Dynamic
var'to'str'dynamic =
  str'static'to'dynamic . var'to'str'static

var'discardSource :: Var -> Var
var'discardSource x =
  Var
    { var'str = var'str x
    , var'source = Nothing
    }


--------------------------------------------------------------------------------
--  Static strings
--------------------------------------------------------------------------------

{- | A fixed string value. We use the description "static" to mean the string
may not contain antiquotation, in contrast with 'Str'Dynamic' which can. -}

data Str'Static =
  Str'Static
    { str'static'text :: Text
    , str'static'source :: Maybe SourceRange
    }

str'static'append :: Str'Static -> Str'Static -> Str'Static
str'static'append (Str'Static t1 s1) (Str'Static t2 s2) =
  Str'Static (Text.append t1 t2) (sourceRangeMaybe'join s1 s2)

instance Semigroup Str'Static where (<>) = str'static'append

str'static'discardSource :: Str'Static -> Str'Static
str'static'discardSource x =
  Str'Static
    { str'static'text = str'static'text x
    , str'static'source = Nothing
    }


--------------------------------------------------------------------------------
--  Dynamic strings
--------------------------------------------------------------------------------

{- | A /dynamic string/ is a quoted string expression, which may be a simple
string like @"hello"@ or a more complex string containing antiquotation like
@"Hello, my name is ${name}!"@. See 'Expr'Str'.

We use the description "dynamic" to mean the string may contain antiquotation,
in contrast with 'Str'Static' which cannot.

This is the type of string expressions ('Expr'Str').

==== String syntax

A /string/ may be quoted either in the traditional form using a single
double-quote (@"@ ... @"@):

> "one\ntwo"

or in the \"indented string\" form using two single-quotes (@''@ ... @''@):

> ''
>   one
>   two
> ''

Both of these examples reduce to the same value, because leading whitespace is
stripped from indented strings.

Either may contain \"antiquotation\" (also known as \"string interpolation\") to
conveniently concatenate string-valued variables into the string.

> "Hello, my name is ${name}!"

Normal strings may contain the following escape sequences:

  - @\\\\@ → @\\@
  - @\\"@  → @"@
  - @\\${@ → @${@
  - @\\n@  → newline
  - @\\r@  → carriage return
  - @\\t@  → tab

The indented string form does not interpret any escape sequences. -}

data Str'Dynamic =
  Str'Dynamic
    { strDynamic'toSeq :: Seq Str'1
    , strDynamic'source :: Maybe SourceRange
    }

str'dynamic'discardSource :: Str'Dynamic -> Str'Dynamic
str'dynamic'discardSource x =
  Str'Dynamic
    { strDynamic'source = Nothing
    , strDynamic'toSeq = fmap str'1'discardSource (strDynamic'toSeq x)
    }

str'dynamic'append :: Str'Dynamic -> Str'Dynamic -> Str'Dynamic
str'dynamic'append (Str'Dynamic x1 y1) (Str'Dynamic x2 y2) =
  Str'Dynamic (Seq.append x1 x2) (sourceRangeMaybe'join y1 y2)

instance Semigroup Str'Dynamic where (<>) = str'dynamic'append

{- | One part of a 'Str'Dynamic'. -}

data Str'1
  = Str'1'Literal Str'Static
  | Str'1'Antiquote Expression

str'1'discardSource :: Str'1 -> Str'1
str'1'discardSource =
  \case
    Str'1'Literal x -> Str'1'Literal (str'static'discardSource x)
    Str'1'Antiquote x -> Str'1'Antiquote (expression'discardSource x)

{- | Simplify a dynamic string by combining consecutive pieces of static text.
-}

-- | ==== Examples
--
-- >>> :{
-- >>> str :: Text -> Str'1
-- >>> str x = Str'1'Literal $ Str'Static x Nothing
-- >>>
-- >>> var :: Text -> Str'1
-- >>> var x = Str'1'Antiquote . Expr'Var $
-- >>>         Var (unquotedString'orThrow x) Nothing
-- >>> :}
--
-- >>> :{
-- >>> str'dynamic'normalize $ Str'Dynamic (Seq.fromList
-- >>>   [str "a", str "b", var "x", var "y", str "c", str "d"]) Nothing
-- >>> :}
-- str ["ab", antiquote (var "x"), antiquote (var "y"), "cd"]

str'dynamic'normalize :: Str'Dynamic -> Str'Dynamic
str'dynamic'normalize s =
  s{ strDynamic'toSeq = f (strDynamic'toSeq s) }
  where
    f = Seq.fromList
      . List.concat
      . List.map (\case
          Right xs -> [Str'1'Literal (List.foldr1 str'static'append xs)]
          Left xs -> xs
        )
      . List.groupEither
      . List.map (\case
          Str'1'Literal x -> Right x
          x -> Left x
        )
      . Seq.toList


--------------------------------------------------------------------------------
--  Conversions between types of strings
--------------------------------------------------------------------------------

-- | ==== Examples
--
-- >>> str'dynamic'to'static $ Str'Dynamic (Seq.fromList []) Nothing
-- Just ""
--
-- >>> a = Str'1'Literal (Str'Static "hi" Nothing)
--
-- >>> b = Str'1'Antiquote $ Expr'Var $ Var (unquotedString'orThrow "x") Nothing
--
-- >>> str'dynamic'to'static $ Str'Dynamic (Seq.fromList [ a ]) Nothing
-- Just "hi"
--
-- >>> str'dynamic'to'static $ Str'Dynamic (Seq.fromList [ a, b ]) Nothing
-- Nothing

str'dynamic'to'static :: Str'Dynamic -> Maybe Str'Static
str'dynamic'to'static x =
  case Seq.toList (strDynamic'toSeq x) of
    []                -> Just (Str'Static "" (strDynamic'source x))
    [Str'1'Literal a] -> Just (a{ str'static'source = strDynamic'source x })
    _                 -> Nothing

str'static'to'dynamic :: Str'Static -> Str'Dynamic
str'static'to'dynamic x =
  Str'Dynamic (Seq.singleton (Str'1'Literal x)) (str'static'source x)


--------------------------------------------------------------------------------
--  Lambda
--------------------------------------------------------------------------------

{- | A function expressed as a lambda abstraction.

==== Syntax

A lambda expression ('Expr'Lambda') has the form @x: y@ where @x@ is the
function parameter to bind in the function body @y@.

This is a function that turns a name into a greeting:

> name: "Hello, ${name}!"

The function parameter can also be a /dict pattern/, which looks like this:

> { a, b, c ? "another" }: "Hello, ${a}, ${b}, and ${c}!"

That function accepts a dict and looks up the keys @a@, @b@, and @c@ from it,
applying the default value @"another"@ to @c@ if it is not present in the dict.
Dict patterns therefore give us something that resembles functions with named
parameters and default arguments.

By default, a lambda defined with a dict pattern fails to evaluate if the dict
argument contains keys that are not listed in the pattern. To prevent it from
failing, you can end the pattern with @ ... @:

> ({ a, ... }: x) { a = "1"; b = "2"; }

Every function has a single parameter. If you need multiple parameters, you have
to curry:

> a: b: [ a b ]

-}

data Lambda =
  Lambda
    { lambda'head :: Param
        -- ^ Declaration of the function's parameter
    , lambda'body :: Expression
        -- ^ Body of the function; what it evaluates to
    , lambda'source :: Maybe SourceRange
    }

lambda'discardSource :: Lambda -> Lambda
lambda'discardSource x =
  Lambda
    { lambda'head = param'discardSource (lambda'head x)
    , lambda'body = expression'discardSource (lambda'body x)
    , lambda'source = Nothing
    }


--------------------------------------------------------------------------------
--  Apply
--------------------------------------------------------------------------------

{- | The application of a function to a single argument.

==== Syntax

An function application expression ('Expr'Apply') looks like this:

> f x

If a function has multiple (curried) parameters, you can chain them together
like so:

> f x y z

-}

data Apply =
  Apply
    { apply'func :: Expression
        -- ^ The function being called
    , apply'arg :: Expression
        -- ^ The argument to the function
    , apply'source :: Maybe SourceRange
    }

expression'applyArgs
  :: Expression   -- ^ Function
  -> [Expression] -- ^ Args
  -> Expression   -- ^ Function application
expression'applyArgs =
  foldl f
  where
    f acc b =
      Expr'Apply (Apply acc b src)
      where
        src =
          sourceRangeMaybe'join
            (expression'source acc)
            (expression'source b)

apply'discardSource :: Apply -> Apply
apply'discardSource x =
  Apply
    { apply'func = expression'discardSource (apply'func x)
    , apply'arg = expression'discardSource (apply'arg x)
    , apply'source = Nothing
    }


--------------------------------------------------------------------------------
--  Param
--------------------------------------------------------------------------------

{- | A parameter to a 'Lambda'. All functions have a single parameter, but it's
more complicated than that because it may also include dict destructuring. -}

data Param
  = Param'Name Var
      -- ^ A simple single-parameter function
  | Param'DictPattern DictPattern
      -- ^ Dict destructuring, which gives you something resembling multiple
      -- named parameters with default values
  | Param'Both Var DictPattern
      -- ^ Both a param name /and/ a dict pattern, separated by the @\@@
      -- keyword

param'discardSource :: Param -> Param
param'discardSource =
  \case
    Param'Name x ->
      Param'Name (var'discardSource x)
    Param'DictPattern x ->
      Param'DictPattern (dictPattern'discardSource x)
    Param'Both x y ->
      Param'Both (var'discardSource x) (dictPattern'discardSource y)


--------------------------------------------------------------------------------
--  Dict pattern
--------------------------------------------------------------------------------

{- | A type of function parameter ('Param') that does dict destructuring. -}

data DictPattern =
  DictPattern
    { dictPattern'items :: Seq DictPattern'1
        -- ^ The list of keys to pull out of the dict, along with any default
        -- value each may have
    , dictPattern'ellipsis :: Bool
        -- ^ Whether to allow additional keys beyond what is listed in the
        -- items, corresponding to the @...@ keyword
    }

dictPattern'discardSource :: DictPattern -> DictPattern
dictPattern'discardSource x =
  DictPattern
    { dictPattern'items = fmap dictPattern'1'discardSource (dictPattern'items x)
    , dictPattern'ellipsis = dictPattern'ellipsis x
    }

{- | One item within a 'DictPattern'. -}
data DictPattern'1 =
  DictPattern'1
    { dictPattern'1'name :: Var
        -- ^ The name of the key to pull out of the dict
    , dictPattern'1'default :: Maybe Expression
        -- ^ The default value to be used if the key is not present in the dict
    }

dictPattern'1'discardSource :: DictPattern'1 -> DictPattern'1
dictPattern'1'discardSource x =
  DictPattern'1
    { dictPattern'1'name = var'discardSource (dictPattern'1'name x)
    , dictPattern'1'default =
        fmap expression'discardSource (dictPattern'1'default x)
    }


--------------------------------------------------------------------------------
--  List
--------------------------------------------------------------------------------

{- | A list is an ordered collection.

==== Syntax

A list expression ('Expr'List') starts with @[@, ends with @]@, and contains any
number of expressions in between.

The empty list:

> [ ]

A list containing three variables:

> [ a b c ]

Lambdas, function applications, @let@-@in@ expressions, and @with@ expressions
must be parenthesized when in a list.

> [
>   (x: f x y)
>   (g y)
>   (let a = y; in f a a)
>   (with d; f x a)
> ]

-}

data List =
  List
    { list'expressions :: Seq Expression
    , list'source :: Maybe SourceRange
    }

list'discardSource :: List -> List
list'discardSource x =
  List
    { list'expressions = fmap expression'discardSource (list'expressions x)
    , list'source = Nothing
    }


--------------------------------------------------------------------------------
--  Dict
--------------------------------------------------------------------------------

{- | A dict is an unordered enumerated mapping from strings.

==== Syntax

A dict expression ('Expr'Dict') starts with @{@ or @rec {@, ends with @}@, and
contains any number of 'DictBinding's in between.

The empty dict (with no bindings):

> { }

A dict with two bindings:

> {
>   a = "one";
>   b = "one two";
> }

By default, dict bindings cannot refer to each other. For that, you need the
@rec@ keyword to create a /recursive/ dict.

> rec {
>   a = "one";
>   b = "${a} two";
> }

In either case, the order of the bindings does not matter.

The left-hand side of a dict binding may be a quoted string (in the traditional
@"@ ... @"@ style, not the indented-string @''@ style), which make it possible
for them to be strings that otherwise couldn't be expressed unquoted, such as
strings containing spaces:

> { "a b" = "c"; }

The left-hand side of a dict may even be an arbitrary expression, using the @${@
... @}@ form:

> let x = "a b"; in { ${x} = "c"; }

Dicts also support the @inherit@ keyword:

> { inherit a; inherit (x) c d; }

The previous expression is equivalent to:

> { a = a; c = x.c; d = x.d; }

-}

data Dict =
  Dict
    { dict'rec :: Bool
        -- ^ Whether the dict is recursive (denoted by the @rec@ keyword)
    , dict'bindings :: Seq DictBinding
        -- ^ The bindings (everything between @{@ and @}@)
    , dict'source :: Maybe SourceRange
    }

dict'discardSource :: Dict -> Dict
dict'discardSource x =
  Dict
    { dict'rec = dict'rec x
    , dict'bindings = fmap dictBinding'discardSource (dict'bindings x)
    , dict'source = Nothing
    }

{- | A binding within a 'Dict'. -}

data DictBinding
  = DictBinding'Eq Expression Expression
      -- ^ A binding of the form @x = y;@
  | DictBinding'Inherit'Dict Expression (Seq Str'Static)
  | DictBinding'Inherit'Var (Seq Var)

dictBinding'discardSource :: DictBinding -> DictBinding
dictBinding'discardSource =
  \case
    DictBinding'Eq a b ->
      DictBinding'Eq
        (expression'discardSource a)
        (expression'discardSource b)
    DictBinding'Inherit'Dict a xs ->
      DictBinding'Inherit'Dict
        (expression'discardSource a)
        (fmap str'static'discardSource xs)
    DictBinding'Inherit'Var xs ->
      DictBinding'Inherit'Var
        (fmap var'discardSource xs)


--------------------------------------------------------------------------------
--  Dot
--------------------------------------------------------------------------------

{- | The /dot/ function looks up a value (or a list of values) from a dict.

==== Syntax

A dot expression is named after the @.@ character it contains. @a.b@ looks up
value at key @b@ in the dict @a@.

The examples in this section all reduce to \"Z\".

> { a = "Z"; }.a

> let x = { a = "Z"; }; in x.a

> { x = { a = "Z"; }; }.x.a

The right-hand side of a dot may be a quoted string (in the traditional @"@ ...
@"@ style, not the indented-string @''@ style):

> { a = "Z"; }."a"

The right-hand side of a dot may even be an arbitrary expression, using the @${@
... @}@ form:

> { a = "Z"; }.${ let b = "a"; in b }

-}

data Dot =
  Dot
    { dot'dict :: Expression
    , dot'key :: Expression
    , dot'source :: Maybe SourceRange
    }

expression'applyDots
  :: Expression   -- ^ Dict
  -> [Expression] -- ^ Lookups
  -> Expression   -- ^ Dot expression
expression'applyDots =
  foldl f
  where
    f acc b =
      Expr'Dot (Dot acc b src)
      where
        src =
          sourceRangeMaybe'join
            (expression'source acc)
            (expression'source b)

dot'discardSource :: Dot -> Dot
dot'discardSource x =
  Dot
    { dot'dict = expression'discardSource (dot'dict x)
    , dot'key = expression'discardSource (dot'key x)
    , dot'source = Nothing
    }


--------------------------------------------------------------------------------
--  Let
--------------------------------------------------------------------------------

{- | ==== Syntax

A /let/-/in/ expression ('Expr'Let') looks like this:

> let
>   greet = x: "Hello, ${x}!";
>   name = "Chris";
> in
>   greet name

/Let/ bindings, like dict bindings, may also use the @inherit@ keyword.

> let
>   d = { greet = x: "Hello, ${x}!"; name = "Chris"; }
>   inherit (d) greet name;
> in
>   greet name

The previous example also demonstrates how the bindings in a /let/ expression
may refer to each other (much like a dict with the @rec@ keyword). As with
dicts, the order of the bindings does not matter. -}

data Let =
  Let
    { let'bindings :: Seq LetBinding
        -- ^ The bindings (everything between the @let@ and @in@ keywords)
    , let'value :: Expression
        -- ^ The value (everything after the @in@ keyword)
    , let'source :: Maybe SourceRange
    }

let'discardSource :: Let -> Let
let'discardSource x =
  Let
    { let'bindings = fmap letBinding'discardSource (let'bindings x)
    , let'value = expression'discardSource (let'value x)
    , let'source = Nothing
    }

{- | A semicolon-terminated binding within the binding list of a 'Let'
expression. -}

data LetBinding
  = LetBinding'Eq Var Expression
      -- ^ A binding with an equals sign, of the form @x = y;@
  | LetBinding'Inherit Expression (Seq Var)
      -- ^ A binding using the @inherit@ keyword, of the form @inherit (x) a b;@

letBinding'discardSource :: LetBinding -> LetBinding
letBinding'discardSource =
  \case
    LetBinding'Eq a b ->
      LetBinding'Eq
        (var'discardSource a)
        (expression'discardSource b)
    LetBinding'Inherit a b ->
      LetBinding'Inherit
        (expression'discardSource a)
        (fmap var'discardSource b)


--------------------------------------------------------------------------------
--  Show
--------------------------------------------------------------------------------

{- | This instance is designed for doctests and REPL experimentation. The format
is designed to strike a balance in verbosity between the derived 'Show'
implementations (which are unwieldily long) and the Bricks language itself
(which is quite terse but unsuitable for demonstrating the parser, as outputting
a Bricks rendering of parse result wouldn't illumunate anyone's understanding of
the AST that the 'Show' instances are here to depict). -}

instance Show Expression        where show = Text.unpack . show'expression

instance Show Var               where show = Text.unpack . show'var
instance Show Str'Static        where show = Text.unpack . show'str'static
instance Show Str'Dynamic       where show = Text.unpack . show'str'dynamic
instance Show Str'1             where show = Text.unpack . show'str'1
instance Show List              where show = Text.unpack . show'list
instance Show Dict              where show = Text.unpack . show'dict
instance Show DictBinding       where show = Text.unpack . show'dictBinding
instance Show Dot               where show = Text.unpack . show'dot
instance Show Lambda            where show = Text.unpack . show'lambda
instance Show Param             where show = Text.unpack . show'param
instance Show DictPattern       where show = Text.unpack . show'dictPattern
instance Show DictPattern'1     where show = Text.unpack . show'dictPattern'1
instance Show Apply             where show = Text.unpack . show'apply
instance Show Let               where show = Text.unpack . show'let
instance Show LetBinding        where show = Text.unpack . show'letBinding

show'expression :: Expression -> Text
show'expression =
  \case
    Expr'Var x    -> show'var x
    Expr'Str x    -> show'str'dynamic x
    Expr'List x   -> show'list x
    Expr'Dict x   -> show'dict x
    Expr'Dot x    -> show'dot x
    Expr'Lambda x -> show'lambda x
    Expr'Apply x  -> show'apply x
    Expr'Let x    -> show'let x

source'comment :: Maybe SourceRange -> Maybe Text
source'comment =
  fmap $ \x -> "{- " <> show'sourceRange x <> " -}"

show'var :: Var -> Text
show'var (Var x s) =
  maybe "" (<> " ") (source'comment s) <>
  "var " <> (Text.show @Text . unquotedString'text) x

show'str'static :: Str'Static -> Text
show'str'static (Str'Static x s) =
  maybe "" (<> " ") (source'comment s) <>
  Text.show @Text x

show'str'dynamic :: Str'Dynamic -> Text
show'str'dynamic (Str'Dynamic xs s) =
  maybe "" (<> " ") (source'comment s) <>
  "str [" <> Text.intercalateMap ", " show'str'1 xs <> "]"

show'str'1 :: Str'1 -> Text
show'str'1 =
  \case
    Str'1'Literal (Str'Static x s) ->
      maybe "" (<> " ") (source'comment s) <> Text.show @Text x
    Str'1'Antiquote x ->
      "antiquote (" <> show'expression x <> ")"

show'list :: List -> Text
show'list (List xs s) =
  maybe "" (<> " ") (source'comment s) <>
  "list [" <> Text.intercalateMap ", " show'expression xs <> "]"

show'dict :: Dict -> Text
show'dict (Dict r bs s) =
  maybe "" (<> " ") (source'comment s) <>
  (if r then "rec'dict [" else "dict [") <>
  Text.intercalateMap ", " show'dictBinding bs <> "]"

show'dictBinding :: DictBinding -> Text
show'dictBinding =
  \case
    DictBinding'Eq a b ->
      "dict'eq (" <> show'expression a <> ") (" <> show'expression b <> ")"
    DictBinding'Inherit'Var xs ->
      "dict'inherit [" <>
      Text.intercalateMap ", " (Text.show @Text . var'text) xs <> "]"
    DictBinding'Inherit'Dict from xs ->
      "dict'inherit'from (" <> show'expression from <> ") [" <>
      Text.intercalateMap ", " show'str'static xs <> "]"

show'dot :: Dot -> Text
show'dot (Dot a b s) =
  maybe "" (<> " ") (source'comment s) <>
  "dot (" <> show'expression a <> ") (" <> show'expression b <> ")"

show'lambda :: Lambda -> Text
show'lambda (Lambda a b s) =
  maybe "" (<> " ") (source'comment s) <>
  "lambda (" <> show'param a <> ") (" <> show'expression b <> ")"

show'param :: Param -> Text
show'param =
  \case
    Param'Name a -> "param " <> Text.show @Text (var'text a)
    Param'DictPattern b -> show'dictPattern b
    Param'Both a b -> "param " <> Text.show @Text (var'text a) <>
                      " <> " <> show'dictPattern b

show'dictPattern :: DictPattern -> Text
show'dictPattern (DictPattern xs e) =
  "pattern [" <> Text.intercalateMap ", " show'dictPattern'1 xs <> "]" <>
  (if e then " <> ellipsis" else "")

show'dictPattern'1 :: DictPattern'1 -> Text
show'dictPattern'1 (DictPattern'1 a mb) =
  "dict'param " <> Text.show @Text (var'text a) <>
  maybe "" (\b -> " & def (" <> show'expression b <> ")") mb

show'apply :: Apply -> Text
show'apply (Apply a b s) =
  maybe "" (<> " ") (source'comment s) <>
  "apply (" <> show'expression a <> ") (" <> show'expression b <> ")"

show'let :: Let -> Text
show'let (Let xs y s) =
  maybe "" (<> " ") (source'comment s) <>
  "let'in [" <> Text.intercalateMap ", " show'letBinding xs <> "] (" <>
  show'expression y <> ")"

show'letBinding :: LetBinding -> Text
show'letBinding =
  \case
    LetBinding'Eq a b ->
      "let'eq " <> Text.show @Text (var'text a) <>
      " (" <> show'expression b <> ")"
    LetBinding'Inherit from xs ->
      "let'inherit'from (" <> show'expression from <> ") [" <>
      Text.intercalateMap ", " (Text.show @Text . var'text) xs <> "]"
