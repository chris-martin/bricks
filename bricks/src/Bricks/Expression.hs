{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Bricks.Expression
  (
  -- * Expressions
    Expression (..)

  -- * Strings
  , Str'Static
  , Str'Dynamic
  , Str'1 (..)

  -- * String conversions
  , str'dynamicToStatic
  , str'staticToDynamic
  , str'unquotedToDynamic

  -- * Lists
  , List

  -- * Dicts
  , Dict (..)
  , DictBinding (..)

  -- * Dict lookup
  , Dot (..)

  -- * Lambdas
  , Lambda (..)

  -- * Function parameters
  , Param (..)
  , DictPattern (..)
  , DictPattern'1 (..)

  -- * Function application
  , Apply (..)

  -- * @let@
  , Let (..)
  , LetBinding (..)

  -- * @with@
  , With (..)

  -- * @inherit@
  , Inherit (..)

  ) where

-- Bricks
import Bricks.UnquotedString

-- Bricks internal
import           Bricks.Internal.Prelude
import           Bricks.Internal.Seq     (Seq)
import qualified Bricks.Internal.Seq     as Seq
import           Bricks.Internal.Text    (Text)

data Expression
  = Expr'Var Str'Unquoted
      -- ^ A /variable/, such as @x@.
  | Expr'Str Str'Dynamic
      -- ^ A /string/ may be quoted either in the traditional form using a
      -- single double-quote (@"@...@"@):
      --
      -- > "one\ntwo"
      --
      -- or in the "indented string" form using two single-quotes (@''@...@''@):
      --
      -- > ''
      -- >   one
      -- >   two
      -- > ''
      --
      -- Both of these examples reduce to the same value, because leading
      -- whitespace is stripped from indented strings.
      --
      -- Either may contain "antiquotation" (also known as "string
      -- interpolation") to conveniently concatenate string-valued variables
      -- into the string.
      --
      -- > "Hello, my name is ${name}!"
      --
      -- Normal strings may contain the following escape sequences:
      --
      --  - @\\\\@ → @\\@
      --  - @\\"@  → @"@
      --  - @\\${@ → @${@
      --  - @\\n@  → newline
      --  - @\\r@  → carriage return
      --  - @\\t@  → tab
      --
      -- The indented string form does not interpret any escape sequences.
  | Expr'List List
      -- ^ A /list/ is an ordered collection of expressions.
      --
      -- The empty list:
      --
      -- > [ ]
      --
      -- A list containing three variables:
      --
      -- > [ a b c ]
      --
      -- Lambdas, function applications, @let@-@in@ expressions, and @with@
      -- expressions must be parenthesized when in a list.
      --
      -- > [
      -- >   (x: f x y)
      -- >   (g y)
      -- >   (let a = y; in f a a)
      -- >   (with d; f x a)
      -- > ]
  | Expr'Dict Dict
      -- ^ A /dict/ is an unordered extensional mapping from strings.
      --
      -- The empty dict (with no bindings):
      --
      -- > { }
      --
      -- A dict with two bindings:
      --
      -- > {
      -- >   a = "one";
      -- >   b = "one two";
      -- > }
      --
      -- By default, dict bindings cannot refer to each other. For that, you
      -- need the @rec@ keyword to create a /recursive/ dict.
      --
      -- > rec {
      -- >   a = "one";
      -- >   b = "${a} two";
      -- > }
      --
      -- In either case, the order of the bindings does not matter.
      --
      -- The left-hand side of a dict binding may be a quoted string (in the
      -- traditional @"@...@"@ style, not the indented-string @''@ style),
      -- which make it possible for them to be strings that otherwise couldn't
      -- be expressed unquoted, such as strings containing spaces:
      --
      -- > { "a b" = "c"; }
      --
      -- The left-hand side of a dict may even be an arbitrary expression,
      -- using the @${@ ... @}@ form:
      --
      -- > let x = "a b"; in { ${x} = "c"; }
      --
      -- Dicts also support the @inherit@ keyword:
      --
      -- > { inherit a; inherit (x) c d; }
      --
      -- The previous expression is equivalent to:
      --
      -- > { a = a; c = x.c; d = x.d; }
  | Expr'Dot Dot
      -- ^ A /dot/ expression (named after the @.@ character it contains)
      -- looks up the value in a dict.
      --
      -- The examples in this section all reduce to "Z".
      --
      -- > { a = "Z"; }.a
      --
      -- > let x = { a = "Z"; }; in x.a
      --
      -- > { x = { a = "Z"; }; }.x.a
      --
      -- The right-hand side of a dot may be a quoted string (in the
      -- traditional @"@...@"@ style, not the indented-string @''@ style):
      --
      -- > { a = "Z"; }."a"
      --
      -- The right-hand side of a dot may even be an arbitrary expression,
      -- using the @${@ ... @}@ form:
      --
      -- > { a = "Z"; }.${ let b = "a"; in b }
  | Expr'Lambda Lambda
      -- ^ A /lambda/ expression @x: y@ where @x@ is the parameter.
      --
      -- This is a function that turns a name into a greeting:
      --
      -- > name: "Hello, ${name}!"
      --
      -- The function parameter can also be a /dict pattern/, which looks like
      -- this:
      --
      -- > { a, b, c ? "another" }: "Hello, ${a}, ${b}, and ${c}!"
      --
      -- That function accepts a dict and looks up the keys @a@, @b@, and @c@
      -- from it, applying the default value @"another"@ to @c@ if it is not
      -- present in the dict. Dict patterns therefore give us something that
      -- resembles functions with named parameters and default arguments.
      --
      -- By default, a lambda defined with a dict pattern fails to evaluate if
      -- the dict argument contains keys that are not listed in the pattern.
      -- To prevent it from failing, you can end the pattern with @...@:
      --
      -- > ({ a, ... }: x) { a = "1"; b = "2"; }
      --
      -- Every function has a single parameter. If you need multiple
      -- parameters, you have to curry:
      --
      -- > a: b: [ a b ]
  | Expr'Apply Apply
      -- ^ Function /application/:
      --
      -- > f x
      --
      -- If a function has multiple (curried) parameters, you can chain them
      -- together like so:
      --
      -- > f x y z
  | Expr'Let Let
      -- ^ A /let/-/in/ expression:
      --
      -- > let
      -- >   greet = x: "Hello, ${x}!";
      -- >   name = "Chris";
      -- > in
      -- >   greet name
      --
      -- /Let/ bindings, like dict bindings, may also use the @inherit@ keyword.
      --
      -- > let
      -- >   d = { greet = x: "Hello, ${x}!"; name = "Chris"; }
      -- >   inherit (d) greet name;
      -- > in
      -- >   greet name
      --
      -- The previous example also demonstrates how the bindings in a /let/
      -- expression may refer to each other (much like a dict with the @rec@
      -- keyword). As with dicts, the order of the bindings does not matter.
  | Expr'With With
      -- ^ A /with/ expression is similar to a /let/-/in/ expression, but the
      -- bindings come from a dict.
      --
      -- > with {
      -- >   greet = x: "Hello, ${x}!";
      -- >   name = "Chris";
      -- > };
      -- >   greet name

{- | A fixed string value. We use the description "static" to mean the string
may not contain antiquotation, in contrast with 'Str'Dynamic' which can. -}
type Str'Static = Text

{- | A quoted string expression, which may be a simple string like @"hello"@ or
a more complex string containing antiquotation like @"Hello, my name is
${name}!"@. See 'Expr'Str'.

We use the description "dynamic" to mean the string may contain antiquotation,
in contrast with 'Str'Static' which cannot. -}
type Str'Dynamic = Seq Str'1

str'dynamicToStatic :: Str'Dynamic -> Maybe Str'Static
str'dynamicToStatic = Seq.toList >>> \case
  [Str'1'Literal x] -> Just x
  _                 -> Nothing

str'staticToDynamic :: Str'Static -> Str'Dynamic
str'staticToDynamic x =
  Seq.singleton (Str'1'Literal x)

str'unquotedToDynamic :: Str'Unquoted -> Str'Dynamic
str'unquotedToDynamic =
  str'staticToDynamic . str'unquotedToStatic

-- | One part of a 'Str'Dynamic'.
data Str'1
  = Str'1'Literal Str'Static
  | Str'1'Antiquote Expression

-- | A function expression. See 'Expr'Lambda'.
data Lambda =
  Lambda
    { lambda'head :: Param
        -- ^ Declaration of the function's parameter
    , lambda'body :: Expression
        -- ^ Body of the function; what it evaluates to
    }

-- | A function application expression. See 'Expr'Apply'.
data Apply =
  Apply
    { apply'func :: Expression
        -- ^ The function being called
    , apply'arg :: Expression
        -- ^ The argument to the function
    }

{- | A parameter to a 'Lambda'. All functions have a single parameter, but it's
more complicated than that because it may also include dict destructuring. -}
data Param
  = Param'Var Str'Unquoted
      -- ^ A simple single-parameter function
  | Param'DictPattern DictPattern
      -- ^ Dict destructuring, which gives you something resembling multiple
      -- named parameters with default values
  | Param'Both Str'Unquoted DictPattern
      -- ^ Both a variable name /and/ a dict pattern, separated by the @\@@
      -- keyword

-- | A type of function parameter ('Param') that does dict destructuring.
data DictPattern =
  DictPattern
    { dictPattern'items :: Seq DictPattern'1
        -- ^ The list of variables to pull out of the dict argument, along
        -- with any default value each may have
    , dictPattern'ellipsis :: Bool
        -- ^ Whether to allow additional keys beyond what is listed in the
        -- items, corresponding to the @...@ keyword
    }

-- | One item within a 'DictPattern'.
data DictPattern'1 =
  DictPattern'1
    { dictPattern'1'variable :: Str'Unquoted
        -- ^ The bound variable
    , dictPattern'1'default :: Maybe Expression
        -- ^ The default value to be used if the key is not present in the dict
    }

{- | A list literal expression, starting with @[@ and ending with @]@.
See 'Expr'List'. -}
type List = Seq Expression

{- | A dict literal expression, starting with @{@ or @rec {@ and ending with
@}@. See 'Expr'Dict'. -}
data Dict =
  Dict
    { dict'rec :: Bool
        -- ^ Whether the dict is recursive (denoted by the @rec@ keyword)
    , dict'bindings :: Seq DictBinding
        -- ^ The bindings (everything between @{@ and @}@)
    }

-- | A binding of the form @x = y;@ within a 'DictLiteral' or 'LetExpr'.
data DictBinding
  = DictBinding'Eq Expression Expression
  | DictBinding'Inherit Inherit

{- | An expression of the form @person.name@ that looks up a key from a dict.
See 'Expr'Dot'. -}
data Dot =
  Dot
    { dot'dict :: Expression
    , dot'key  :: Expression
    }

-- | A @let@-@in@ expression. See 'Expr'Let'.
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
  = LetBinding'Eq Str'Static Expression
      -- ^ A binding with an equals sign, of the form @x = y;@
  | LetBinding'Inherit Inherit
      -- ^ A binding using the @inherit@ keyword, of the form @inherit a b;@
      -- or @inherit (x) a b;@

-- | A @with@ expression. See 'Expr'With'.
data With =
  With
    { with'context :: Expression
    , with'value :: Expression
    }

data Inherit =
  Inherit
    { inherit'from :: Maybe Expression
    , inherit'list :: Seq Str'Static
    }
