{- |

__Bricks__ is a lazy functional language that resembles
<https://nixos.org/nix/manual/#ch-expression-language Nix>.

This module serves as fairly exhaustive overview of the entire package, and
should usually serve as your go-to place to start when reading the Bricks
documentation if you want an /in-depth/ understanding of how everything works.
It is a fairly /large/ module, and probably ought to be used via a qualified
import.

> import qualified Bricks

If you just want to use Bricks for common cases, look at the simple API in
"Bricks.Prelude" instead. That module is much smaller and is designed to be
imported unqualified.

> import Bricks.Prelude

-}
module Bricks

  ( parse'file

  -------------------------------------------------

  -- * Module overview
  -- $module-overview

  -------------------------------------------------

  -- * Differences from Nix
  -- $differences-from-nix

  -------------------------------------------------

  -- * Expressions
  , Expression (..)
  , expression'source
  , expression'discardSource
  -- ** Rendering expressions
  , render'expression
  , render'expression'listContext
  , render'expression'dotLeftContext
  , render'expression'applyLeftContext
  , render'expression'applyRightContext
  , render'expression'inParens
  , render'expression'dictKey
  -- ** Parsing expressions
  , parse'expression
  , parse'expression'paren
  , parse'expression'antiquote
  , parse'expression'dictKey
  -- ** Parsing lists of expressions
  , parse'expressionList
  , parse'expressionList'1
  , parse'expressionList'1'noDot

  -------------------------------------------------

  -- * Variables
  , Var (..)
  , var'text
  , render'var
  , parse'var
  , var'to'str'static
  , var'to'str'dynamic
  , var'discardSource

  -------------------------------------------------

  -- * Strings
  , str'escape
  , parse'str'within'normalQ
  , parse'str'escape'normalQ
  -- ** Static strings
  , Str'Static (..)
  , str'static'append
  , str'static'discardSource
  , str'static'to'dynamic
  , render'strStatic'unquotedIfPossible
  , render'strStatic'quoted
  , parse'strStatic
  , parse'strStatic'quoted
  , parse'strStatic'unquoted
  -- ** Dynamic strings
  , Str'Dynamic (..)
  , Str'1 (..)
  , str'1'discardSource
  , str'dynamic'append
  , str'dynamic'normalize
  , str'dynamic'discardSource
  , str'dynamic'to'static
  , render'strDynamic'unquotedIfPossible
  , render'strDynamic'quoted
  , render'str'1
  , parse'str'dynamic
  -- ** Unquoted strings
  , UnquotedString
  , unquotedString'try
  , unquotedString'orThrow
  , unquotedString'text
  , text'canBeUnquoted
  , char'canBeUnquoted
  , parse'strUnquoted
  -- ** Indented strings
  , InStr (..)
  , InStr'1 (..)
  , inStr'1'toStrParts
  , inStr'toList
  , inStr'to'strDynamic
  , inStr'to'strStatic
  , inStr'level
  , inStr'dedent
  , inStr'trim
  , inStr'discardSource
  , inStr'1'discardSource
  , render'str'indented
  , render'str'indented'1
  , parse'inStr
  , parse'inStr'1

  -------------------------------------------------

  -- * Lists
  , List (..)
  , list'discardSource
  , render'list
  , parse'list

  -------------------------------------------------

  -- * Dicts
  , Dict (..)
  , keyword'rec
  , dict'discardSource
  , render'dict
  , parse'dict
  , parse'dict'rec
  , parse'dict'noRec
  -- ** Dict bindings
  , DictBinding (..)
  , dictBinding'discardSource
  , render'dictBinding
  , parse'dictBinding
  , parse'dictBinding'inherit
  , parse'dictBinding'eq
  -- ** Dict lookup (dot)
  , Dot (..)
  , dot'discardSource
  , expression'applyDots
  , render'dot
  , parse'dot'rhs'chain

  -------------------------------------------------

  -- * Functions
  -- ** Lambdas
  , Lambda (..)
  , lambda'discardSource
  , render'lambda
  , parse'lambda
  -- ** Function parameters
  , Param (..)
  , param'discardSource
  , render'param
  , parse'param
  , parse'param'var
  , parse'param'noVar
  -- ** Dict patterns
  , DictPattern (..)
  , dictPattern'discardSource
  , DictPattern'1 (..)
  , dictPattern'1'discardSource
  , render'dictPattern
  , render'dictPattern'1
  , parse'dictPattern
  , parse'dictPattern'start
  -- ** Function application
  , Apply (..)
  , apply'discardSource
  , expression'applyArgs
  , render'apply

  -------------------------------------------------

  -- * @let@
  , Let (..)
  , let'discardSource
  , keyword'let
  , keyword'in
  , render'let
  , parse'let
  -- ** @let@ bindings
  , LetBinding (..)
  , letBinding'discardSource
  , render'letBinding
  , parse'letBinding
  , parse'letBinding'eq
  , parse'letBinding'inherit

  -------------------------------------------------

  -- * @inherit@
  , keyword'inherit

  -------------------------------------------------

  -- * Keywords
  , Keyword
  , keywords
  , keywordString
  , keywordText
  , parse'keyword

  -------------------------------------------------

  -- * Comments and whitespace
  , keyword'inlineComment
  , parse'spaces
  , parse'comment
  , parse'comment'inline
  , parse'comment'block

  -------------------------------------------------

  -- * Rendering
  , Render
  , RenderContext (..)
  , renderContext'default
  , renderContext'terse

  -------------------------------------------------

  ) where

import Bricks.Expression
import Bricks.Keyword
import Bricks.Parsec
import Bricks.Rendering
import Bricks.UnquotedString

{- $module-overview

== Modules that are re-exported from "Bricks"

The following modules are re-exported from this top-level "Bricks" module in
their entireties.

Modules related to syntax:

  - "Bricks.Keyword" - Enumerates the language's keywords
  - "Bricks.UnquotedString" - Defines the rules for what strings are allowed to
    appear unquoted in Bricks code
  - "Bricks.Expression" - Defines most of the types related to the AST, notably
    'Expression'
  - "Bricks.Parsec" - Defines all of the Parsec parsers for parsing Bricks code
    into 'Expression's
  - "Bricks.Rendering" - Defines all of the renderers for turning 'Expression's
    into Bricks code

Modules related to evaluation:

  - "Bricks.Term" - ...
  - "Bricks.ExpressionToTerm" - ...
  - "Bricks.Evaluation" - ...

== Modules that are /not/ re-exported from "Bricks"

  - "Bricks.Expression.Construction" - Functions for constructing 'Expression's
    in a way that matches their 'Show' implementations.

-}

{- $differences-from-nix

Bricks is heavily based on
<https://nixos.org/nix/manual/#ch-expression-language the Nix language>,
but there are a number of significant differences. Most of the differences
involve the /removal/ of some feature for the sake of simplicity in both
implementation and use.

This list is not comprehensive.

== Top-level Bricks expressions may contain no free variables

There are a number of cases where variables are allowed to appear free in a
top-level Nix expression: @true@, @false@, @null@, @builtins@, and anything
within the body of a @with@ expression (sort of - we'll elaborate on that
complicated issue further below). None of those cases are present in Bricks:
__/All variables must be explicitly bound/__.

Throughout this section we will more thoroughly address specific Nix built-in
variables and how to translate Nix expressions that use them into equivalent
Bricks code.

== Bricks has no built-in Boolean values

Nix has built-in @true@ and @false@ variables, a handful of operators on them
(@&&@, @||@, @!@), and an @if@-@then@-@else@ construct.

> nix-repl> true && false
> false

> nix-repl> true || false
> true

> nix-repl> !true
> false

> nix-repl> if true then "a" else "b"
> "a"

None of these features are present in Bricks.

__todo:__ Show how we can use the standard library instead.

== Bricks has no built-in @null@

__todo:__ Show how we can use the standard library instead.

== Bricks has no integer literals

__todo:__ Show how we can use the standard library instead.

== Bricks has no built-in @builtins@

__todo:__ Show how we can use the standard library instead.

== Bricks has no infix operators (@+@, @-@, @//@, et cetera)

__todo:__ Show how we can use the standard library instead.

== Bricks has no @with@ expression

The Nix language has a @with@ construct which introduces the contents of a dict
into the lexical scope.

The simple reason for omitting this feature is that it can easily lead to code
that is difficult to trace. When @with@ expressions are nested, it is often
unclear where a variable has come into scope from.

The more subtle reason not to include the @with@ construct is that it introduces
a significant departure from the lambda calulus. Consider the expression @(with
d; x)@. In this expression, is @x@ free or bound? It is neither; it exists in
some state of uncertainty where it /may or may not be/ bound, depending on the
value of @d@.

This has a practical consequence: When you use a @with@ expression, you
sacrifice referential transparency. Consider the following Nix expression:

> nix-repl> let v = (with { x = "a"; }; x); in (x: v) "b"
> "a"

If we attempt to reduce this expression by replacing @v@ with its definition,

> nix-repl> (x: (with { x = "a"; }; x)) "b"
> "b"

then it no longer evaluates to the same value. We find this unacceptable.
Bricks avoids the problem by simply not implementing this feature.

== Bricks allows a list on the right-hand side of the @.@ operator

The following example is syntactically valid Nix code, but it fails to evaluate:

> nix-repl> { x = "a"; y = "b"; }.${[ "x" "y" ]}
> error: value is a list while a string was expected

We expand the meaning of the @.@ operator such that if the expression on the
right-hand side evaluates to a list, then the entire expression evaluates to a
list:

> bricks-repl> { x = "a"; y = "b"; }.${[ "x" "y" ]}
> [ "a" "b" ]

Furthermore, when the expression on the right-hand side is a list literal (an
expression of the form @[@ ... @]@), the antiquotation (wrapping the expression
in @${@ ... @}@) may be omitted:

> bricks-repl> { x = "a"; y = "b"; }.[ "x" "y" ]
> [ "a" "b" ]

This provides a convenient alternative to many situations in which one might use
the @with@ keyword in Nix. For example, where in Nix we might write

> ghcWithPackages (p: with p; [ base containers text ]);

we may write this equivalently in Bricks as

> ghcWithPackages (p: p.[ "base" "containers" "text" ]);

== Bricks does not have URI literals

If a string literal is a URI, it can be written in Nix without quotes.

We have chosen not to include this feature because it provides very little
convenience and steals some syntax from lambda expressions. Consider the
following Nix example:

> nix-repl> (let x = "a"; in y: x) "b"
> "a"

If we remove the space between after the colon (@:@), we get something entirely
different:

> nix-repl> (let x = "a"; in y:x) "b"
> error: attempt to call something which is not
> a function but a string, at (string):1:1

Because @y:x@ contains a colon, Nix interprets it as a URI and parses it as the
string @"y:x"@ (this is the "string" to which the error message refers), rather
than as a lambda.

In Bricks, by contrast, the colon in a lambda is /not/ required to be followed
by whitespace, and the previous example works as we would like.

> bricks-repl> (let x = "a"; in y:x) "b"
> "a"

== Bricks does not have path literals

In Nix, an unquoted string that contains a slash is interpreted as a filesystem
path.

Path literals have some subtle syntax rules. A common mistake is forgetting
to always include a slash in the path. For an example, @./foo.nix@ is a URI:

> nix-repl> ./foo.nix
> /home/chris/foo.nix

But @foo.nix@, without the leading @./@, is parsed differently:

> nix-repl> foo.nix
> error: undefined variable ‘foo’ at (string):1:1

As with URI literals, we find that the unquoted form for paths does not provide
enough convenience to compensate for its potential for confusion, so we have
opted to omit it.

== Bricks does not have a built-in @import@ function

In Nix, a path literal that does not start with a slash (such as @./foo.nix@) is
interpreted as a /relative/ path, and the Nix parser immediately resolves it
with respect to the directory in which the Nix file resides (as we saw in the
example above, where it resolved to @\/home\/chris\/foo.nix@).

We love being able to use relative imports, but we don't like needing a built-in
language feature to do it. Fortunately, Bricks can achieve the same effect by
using an ordinary function instead.

__todo:__ Explain how imports work in the standard library, once it is
implemented.

In doing this, we buy back some purity that Nix's @import@ lacks. By passing the
path argument through a function parameter, rather than deriving it implicitly
from the context of "which file did the expression /come from/?" we eliminate a
case where an expression's meaning depends on something other than the values of
the formal parameters that bind its free variables.

There is another benefit to the Bricks approach: While Nix import syntax is
restricted to static paths only (the argument to @import@ cannot contain free
variables), Bricks has no such limitation.

== Bricks uses Haskell-style comments

The Bricks inline comment keyword is @--@; in Nix it is @#@.

The Bricks block comment form is @{\-@ ... @-\}@; in Nix it is @/*@ ... @*/@.

This decision was made merely due to the Bricks authors' aesthetic preference
and affinity for Haskell.

== Bricks block comments may be nested

Although Nix does have block comments,

> nix-repl> /* */ "a"
> "a"

Nix does not support /nested/ block comments:

> nix-repl> /* /* */ */ "a"
> error: syntax error, unexpected '*', at (string):1:10

Bricks does:

@
bricks-repl> {\- {\- -\} -\} "a"
"a"
@

== Bricks does not support escape sequences in indented strings

Within the indented string form (@''@ ... @''@), Nix supports the following
unorthodox escape sequences:

  - @''${@ → @${@
  - @'''@ → @''@
  - @''\\n@ → newline
  - @''\\r@ → carriage return
  - @''\\t@ → tab

> nix-repl> ''ab''\ncd''
> "ab\ncd"

Bricks does not support any of these. If you want to include any of these
strings within an indented string, you can use antiquotation:

> bricks-repl> ''ab${"\n"}cd''
> "ab\ncd"

Or you can interpret escape sequences at runtime by passing your string through
some function in the standard library that does this sort of thing (__todo:__
discuss said function, once it exists).

== Bricks does not allow quotes in /let/ bindings

In Nix, the left-hand side of a /let/ binding is allowed to be a quoted string.
This lets you create variables that aren't valid as variable expressions (when
you /refer to/ a variable, it may /not/ be quoted), which puts you in a weird
sitation where there is a variable in scope which can only be referred to by
inheriting it into a dict.

> nix-repl> let "a b" = "c"; in { inherit "a b"; }
> { "a b" = "c"; }

This oddity does not seem to serve any real purpose, so we have omitted it.

== The Nix "set" concept is renamed to "dict" in Bricks

The Nix concept of "set" is referred to as "dict" in Bricks. This is not
actually a language difference; we just use a different word to talk about the
same thing. We believe that "dict" is a more familiar term for this data
structure, and that Nix's use of "set" conflicts unnecessarily with the more
common usage of the word.

-}
