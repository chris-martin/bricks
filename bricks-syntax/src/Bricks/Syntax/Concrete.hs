{-# LANGUAGE LambdaCase #-}

module Bricks.Syntax.Concrete
  (
  ) where

import Bricks.Syntax.Concrete.Identifier
import Bricks.Syntax.Concrete.Keyword

{-

data List =


data ConcreteExpr

  = ConcreteExpr'Str (Str src)
      -- ^ @"A traditional single-line string, supporting escape sequences and
      --   ${antiquotation}."@
  | ConcreteExpr'Prose (Prose src)
      -- ^ @
      --   '' <>
      --     A "prose string," used for writing paragraphs.
      --     <Antiquotation> is optional and customizable.
      --     There are no escape sequences.
      --   ''
      --   @
  | ConcreteExpr'Box (Box src)
      -- ^ @
      --   ┌ $[]
      --   │A "box string," used for writing code.
      --   │$[antiquotation] is optional and customizable.
      --   │There are no escape sequences.
      --   └


--------------------------------------------------------------------------------
--  Var
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

data Var src = Var Identifier src
  deriving Show

var'text :: Var src -> Text
var'text (Var x _) = unquotedString'text x

var'source :: Var src -> src
var'source (Var _ x) = x


--------------------------------------------------------------------------------
--  Chars
--------------------------------------------------------------------------------

data Chars src = Chars Text src
  deriving Show

chars'append :: Semigroup src => Chars src -> Chars src -> Chars src
chars'append (Str'Static t1 s1) (Str'Static t2 s2) =
  Str'Static (Text.append t1 t2) (sourceRangeMaybe'join s1 s2)

instance Semigroup src => Semigroup (Chars src) where (<>) = chars'append

instance Monoid src => Monoid (Chars src) where mempty = Chars mempty mempty


--------------------------------------------------------------------------------
--  Str
--------------------------------------------------------------------------------

{- | A /traditional string/ may be a simple string like @"hello"@ or a more
complex string containing antiquotation like @"Hello, my name is ${name}!"@. -}

data Str src = Str (Seq (Str1, src)) src
  deriving Show

data Str1 src = Str1'Text Text src | Str1'Expr (Expression src) src
  deriving Show

instance PartialSemigroup Str1
  where
    Str1'Text x <>? Str1'Text y = Just (Str1'Text (Text.append x y))
    _ <>? _ = Nothing

str'append :: Semigroup src => Str src -> Str src -> Str src
str'append (Str str1 source1) (Str str2 source2) =
  Str'Dynamic (Seq.append str1 str2) (range'join source1 source2)

instance Semigroup src => Semigroup Str where (<>) = str'append

instance Monoid src => Monoid Str where mempty = Str mempty mempty

{- | Simplify a string by combining consecutive pieces of static text. -}

-- | ==== Examples
--
-- >>> :{
-- >>> str :: Text -> (Either Text (Expression ()), ())
-- >>> str x = (Left x (), ())
-- >>>
-- >>> var :: Text -> (Either Text (Expression ()), ())
-- >>> var x = (Right (Expr'Var (Var (unquotedString'orThrow x)) ()), ())
-- >>> :}
--
-- >>> :{
-- >>> str'normalize
-- >>>   (Str [str "a", str "b", var "x", var "y", str "c", str "d"]) ())
-- >>> :}
-- Str ["ab", antiquote (var "x"), antiquote (var "y"), "cd"] ()

str'normalize :: Str -> Str
str'normalize (Str xs src) = Str (groupAndConcat xs) src

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


--------------------------------------------------------------------------------
--  Antiquote style
--------------------------------------------------------------------------------

data AntiquoteStyle =
  AntiquoteStyle
    { antiquoteStyle'dollar :: Bool
    , antiquoteStyle'braceType :: BraceType
    }
  deriving Show

data BraceType
  = BraceType'Curly -- ^ @{}@
  | BraceType'Square -- ^ @[]@
  | BraceType'Angle -- ^ @<>@
  | BraceType'Paren -- ^ @()@
  | BraceType'Guillemet -- ^ @«»@
  | BraceType'CJK -- ^ @「」@
  deriving Show

--------------------------------------------------------------------------------
--  Prose strings
--------------------------------------------------------------------------------

{- | A "prose string," delimited by two or more single-quotes @''@. -}

data Prose =
  Prose
    { prose'string :: Str'Dynamic
    , prose'source :: Maybe SourceRange
    , prose'antiquoteStyle :: Maybe AntiquoteStyle
    , prose'extraQuotes :: Natural
    }
  deriving Show

prose'discardSource :: Prose -> Prose
prose'discardSource x =
  Prose
    { prose'string = str'dynamic'discardSource (prose'string x)
    , prose'source = Nothing
    , prose'antiquoteStyle = prose'antiquoteStyle x
    , prose'extraQuotes = prose'extraQuotes x
    }


--------------------------------------------------------------------------------
--  Box strings
--------------------------------------------------------------------------------

data Box =
  Box
    { box'lines :: Seq Str'Dynamic
    , box'source :: Maybe SourceRange
    , box'antiquoteStyle :: Maybe AntiquoteStyle
    }
  deriving Show

box'discardSource :: Box -> Box
box'discardSource x =
  Box
    { box'lines = fmap str'dynamic'discardSource (box'lines x)
    , box'source = Nothing
    , box'antiquoteStyle = box'antiquoteStyle x
    }


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
  deriving Show

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
  deriving Show

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
  deriving Show

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
  deriving Show

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
  deriving Show

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
  deriving Show

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
  deriving Show

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
  deriving Show

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
  deriving Show

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
  deriving Show

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
  deriving Show

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

-}
