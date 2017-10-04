{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications           #-}

module Bricks.Expression
  (
  -- * Expressions
    Expression (..)

  -- * Variables
  -- $variables
  , Var (..)
  , var'text
  , var'to'str'static
  , var'to'str'dynamic

  -- * Static strings
  , Str'Static (..)
  , str'static'append

  -- * Dynamic strings
  , Str'Dynamic (..)
  , Str'1 (..)
  , strDynamic'toList
  , strDynamic'fromList
  , strDynamic'singleton
  , str'dynamic'normalize

  -- ** Conversions between types of strings
  , str'dynamic'to'static
  , str'static'to'dynamic

  -- * Lists
  , List (..)

  -- * Dicts
  , Dict (..)
  , DictBinding (..)

  -- * Dict lookup
  , Dot (..)
  , expression'applyDots

  -- * Lambdas
  , Lambda (..)

  -- * Function parameters
  , Param (..)
  , DictPattern (..)
  , DictPattern'1 (..)

  -- * Function application
  , Apply (..)
  , expression'applyArgs

  -- * @let@
  , Let (..)
  , LetBinding (..)

  ) where

-- Bricks
import Bricks.UnquotedString

-- Bricks internal
import           Bricks.Internal.List           as List
import           Bricks.Internal.Prelude
import           Bricks.Internal.Seq            (Seq)
import qualified Bricks.Internal.Seq            as Seq
import           Bricks.Internal.ShowExpression
import           Bricks.Internal.Text           (Text)
import qualified Bricks.Internal.Text           as Text

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

data Var = Var UnquotedString

var'text :: Var -> Text
var'text (Var x) = unquotedString'text x

var'to'str'static :: Var -> Str'Static
var'to'str'static =
  Str'Static . var'text

var'to'str'dynamic :: Var -> Str'Dynamic
var'to'str'dynamic =
  str'static'to'dynamic . var'to'str'static


--------------------------------------------------------------------------------
--  Static strings
--------------------------------------------------------------------------------

{- | A fixed string value. We use the description "static" to mean the string
may not contain antiquotation, in contrast with 'Str'Dynamic' which can. -}

data Str'Static = Str'Static Text

str'static'append :: Str'Static -> Str'Static -> Str'Static
str'static'append (Str'Static t1) (Str'Static t2) =
  Str'Static (Text.append t1 t2)

instance Semigroup Str'Static
  where
    (<>) = str'static'append

instance Monoid Str'Static
  where
    mempty = Str'Static mempty
    mappend = (<>)

instance ShowExpression Str'Static
  where
    showExpression (Str'Static x) = Text.pack (show @Text x)

instance Show (Str'Static)
  where
    showsPrec = showsPrec'showExpression


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
    }

instance Semigroup Str'Dynamic
  where
    Str'Dynamic x <> Str'Dynamic y = Str'Dynamic (x <> y)

instance Monoid Str'Dynamic
  where
    mempty = Str'Dynamic mempty
    mappend = (<>)

{- | One part of a 'Str'Dynamic'. -}

data Str'1
  = Str'1'Literal Str'Static
  | Str'1'Antiquote Expression

instance ShowExpression Str'Dynamic
  where
    showExpression x =
      Text.unwords ["str", showExpression'list (strDynamic'toList x)]

instance ShowExpression Str'1
  where
    showExpression = \case
      Str'1'Literal (Str'Static x) -> showExpression'quoted'text x
      Str'1'Antiquote x -> Text.unwords ["antiquote", showExpression'paren x]

instance Show Str'Dynamic
  where
    showsPrec = showsPrec'showExpression

instance Show Str'1
  where
    showsPrec = showsPrec'showExpression

strDynamic'toList :: Str'Dynamic -> [Str'1]
strDynamic'toList =
  Seq.toList . strDynamic'toSeq

strDynamic'fromList :: [Str'1] -> Str'Dynamic
strDynamic'fromList =
  Str'Dynamic . Seq.fromList

strDynamic'singleton :: Str'1 -> Str'Dynamic
strDynamic'singleton =
  Str'Dynamic . Seq.singleton

{- | Simplify a dynamic string by combining consecutive pieces of static text.
-}

-- | ==== Examples
--
-- >>> str = Str'1'Literal . Str'Static
-- >>> var = Str'1'Antiquote . Expr'Var . Var . unquotedString'orThrow
--
-- >>> :{
-- >>> str'dynamic'normalize $ Str'Dynamic $ Seq.fromList
-- >>>   [str "a", str "b", var "x", var "y", str "c", str "d"]
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
-- >>> str'dynamic'to'static $ Str'Dynamic $ Seq.fromList []
-- Just ""
--
-- >>> a = Str'1'Literal (Str'Static "hi")
--
-- >>> b = Str'1'Antiquote $ Expr'Var $ Var $ unquotedString'orThrow "x"
--
-- >>> str'dynamic'to'static $ Str'Dynamic $ Seq.fromList [ a ]
-- Just "hi"
--
-- >>> str'dynamic'to'static $ Str'Dynamic $ Seq.fromList [ a, b ]
-- Nothing

str'dynamic'to'static :: Str'Dynamic -> Maybe Str'Static
str'dynamic'to'static = strDynamic'toList >>> \case
  []                -> Just (Str'Static "")
  [Str'1'Literal x] -> Just x
  _                 -> Nothing

str'static'to'dynamic :: Str'Static -> Str'Dynamic
str'static'to'dynamic =
  strDynamic'singleton . Str'1'Literal


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
    }

expression'applyArgs
  :: Expression   -- ^ Function
  -> [Expression] -- ^ Args
  -> Expression   -- ^ Function application
expression'applyArgs =
  foldl (\acc b -> Expr'Apply (Apply acc b))


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

{- | One item within a 'DictPattern'. -}
data DictPattern'1 =
  DictPattern'1
    { dictPattern'1'name :: Var
        -- ^ The name of the key to pull out of the dict
    , dictPattern'1'default :: Maybe Expression
        -- ^ The default value to be used if the key is not present in the dict
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

newtype List = List (Seq Expression)
  deriving (Monoid, Semigroup)


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
    }

{- | A binding of the form @x = y;@ within a 'DictLiteral' or 'LetExpr'. -}

data DictBinding
  = DictBinding'Eq Expression Expression
  | DictBinding'Inherit'Dict Expression (Seq Str'Static)
  | DictBinding'Inherit'Var (Seq Var)


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
    , dot'key  :: Expression
    }

expression'applyDots
  :: Expression   -- ^ Dict
  -> [Expression] -- ^ Lookups
  -> Expression   -- ^ Dot expression
expression'applyDots =
  foldl (\acc b -> Expr'Dot (Dot acc b))


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
    }

{- | A semicolon-terminated binding within the binding list of a 'Let'
expression. -}

data LetBinding
  = LetBinding'Eq Var Expression
      -- ^ A binding with an equals sign, of the form @x = y;@
  | LetBinding'Inherit Expression (Seq Var)
      -- ^ A binding using the @inherit@ keyword, of the form @inherit (x) a b;@


--------------------------------------------------------------------------------
--  Show
--------------------------------------------------------------------------------

{- | This instance is designed for doctests and REPL experimentation. The format
is designed to strike a balance in verbosity between the derived 'Show'
implementations (which are unwieldily long) and the Bricks language itself
(which is quite terse but unsuitable for demonstrating the parser, as outputting
a Bricks rendering of parse result wouldn't illumunate anyone's understanding of
the AST that the 'Show' instances are here to depict). -}

instance Show Expression        where showsPrec = showsPrec'showExpression

instance Show Var               where showsPrec = showsPrec'showExpression
instance Show List              where showsPrec = showsPrec'showExpression
instance Show Dict              where showsPrec = showsPrec'showExpression
instance Show DictBinding       where showsPrec = showsPrec'showExpression
instance Show Dot               where showsPrec = showsPrec'showExpression
instance Show Lambda            where showsPrec = showsPrec'showExpression
instance Show Param             where showsPrec = showsPrec'showExpression
instance Show DictPattern       where showsPrec = showsPrec'showExpression
instance Show DictPattern'1     where showsPrec = showsPrec'showExpression
instance Show Apply             where showsPrec = showsPrec'showExpression
instance Show Let               where showsPrec = showsPrec'showExpression
instance Show LetBinding        where showsPrec = showsPrec'showExpression

instance ShowExpression Expression
  where
    showExpression = \case
      Expr'Var x    -> showExpression x
      Expr'Str x    -> showExpression x
      Expr'List x   -> showExpression x
      Expr'Dict x   -> showExpression x
      Expr'Dot x    -> showExpression x
      Expr'Lambda x -> showExpression x
      Expr'Apply x  -> showExpression x
      Expr'Let x    -> showExpression x

instance ShowExpression Var
  where
    showExpression (Var x) =
      "var " <> (Text.pack . show @Text . unquotedString'text) x

instance ShowExpression List
  where
    showExpression (List xs) = Text.unwords ["list", showExpression'list xs]

instance ShowExpression Dict
  where
    showExpression (Dict r bs) =
      Text.unwords
        [if r then "rec'dict" else "dict", showExpression'list bs]

instance ShowExpression DictBinding
  where
    showExpression = \case
      DictBinding'Eq a b ->
        Text.unwords ["dict'eq", showExpression'paren a, showExpression'paren b]
      DictBinding'Inherit'Dict from xs ->
        "inherit'fromDict " <>
        showExpression'paren from <> " " <>
        showExpression'list xs
      DictBinding'Inherit'Var xs ->
        "dict'inherit' " <> showExpression'list xs

instance ShowExpression Dot
  where
    showExpression (Dot a b) =
      Text.unwords ["dot", showExpression'paren a, showExpression'paren b]

instance ShowExpression Lambda
  where
    showExpression (Lambda a b) =
      Text.unwords ["lambda", showExpression'paren a, showExpression'paren b]

instance ShowExpression Param
  where
    showExpression = \case
      Param'Name a -> "param " <> Text.pack (show @Text (var'text a))
      Param'DictPattern b -> showExpression b
      Param'Both a b -> "param " <> Text.pack (show @Text (var'text a)) <>
                        " <> " <> showExpression b

instance ShowExpression DictPattern
  where
    showExpression = \case
      DictPattern xs e ->
        Text.intercalate " <> " $
        [Text.unwords ["pattern", showExpression'list xs]] <>
        (if e then ["ellipsis"] else [])

instance ShowExpression DictPattern'1
  where
    showExpression (DictPattern'1 a mb) =
      "dict'param " <> (Text.pack $ show @Text $ var'text a) <>
      maybe "" (\b -> " & def " <> showExpression'paren b) mb

instance ShowExpression Apply
  where
    showExpression (Apply a b) =
      Text.unwords ["apply", showExpression'paren a, showExpression'paren b]

instance ShowExpression Let
  where
    showExpression (Let xs y) =
      Text.unwords ["let'in", showExpression'list xs, showExpression'paren y]

instance ShowExpression LetBinding
  where
    showExpression = \case
      LetBinding'Eq a b ->
        Text.unwords ["binding", showExpression a, showExpression'paren b]
      LetBinding'Inherit from xs ->
        "inherit'from " <> showExpression'paren from <> showExpression'list xs
