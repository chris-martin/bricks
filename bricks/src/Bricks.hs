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

  (
  -------------------------------------------------

  -- * Module overview
  -- $module-overview

  -------------------------------------------------

  -- * Differences from Nix
  -- $differences-from-nix

  -------------------------------------------------

  -- * Expressions
    Expression (..)
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
  , parse'expression'dictKey
  -- ** Parsing lists of expressions
  , parse'expressionList
  , parse'expressionList'1
  , parse'expressionList'1'noDot

  -------------------------------------------------

  -- * Strings
  , str'escape
  , parse'str'within'normalQ
  , parse'str'escape'normalQ
  -- ** Static strings
  , Str'Static (..)
  , render'strStatic'unquotedIfPossible
  , render'strStatic'quoted
  , parse'strStatic
  , parse'strStatic'quoted
  , parse'strStatic'unquoted
  -- ** Dynamic strings
  , Str'Dynamic (..)
  , Str'1 (..)
  , strDynamic'toList
  , strDynamic'fromList
  , strDynamic'singleton
  , render'strDynamic'unquotedIfPossible
  , render'strDynamic'quoted
  , parse'strDynamic'quoted
  , parse'strDynamic'normalQ
  , parse'strDynamic'indentedQ
  -- ** Unquoted strings
  , UnquotedString
  , unquotedString'try
  , unquotedString'orThrow
  , unquotedString'text
  , Str'Unquoted (..)
  , str'unquoted'text
  , text'canBeUnquoted
  , char'canBeUnquoted
  , render'strUnquoted
  , parse'strUnquoted
  -- ** String conversions
  , str'dynamic'to'static
  , str'static'to'dynamic
  , str'unquoted'to'static
  , str'unquoted'to'dynamic
  -- ** Indented strings
  , InStr (..)
  , inStr'toList
  , inStr'join
  , inStr'level
  , inStr'dedent
  , inStr'trim
  , render'inStr'1
  , parse'inStr
  , parse'inStr'1
  -- ** Single line of an indented string
  , InStr'1 (..)
  , inStr'1'nonEmpty
  , inStr'1'empty
  , inStr'1'modifyLevel

  -------------------------------------------------

  -- * Lists
  , List (..)
  , render'list
  , parse'list

  -------------------------------------------------

  -- * Dicts
  , Dict (..)
  , keyword'rec
  , render'dict
  , parse'dict
  , parse'dict'rec
  , parse'dict'noRec
  -- ** Dict bindings
  , DictBinding (..)
  , render'dictBinding
  , parse'dictBinding
  , parse'dictBinding'inherit
  , parse'dictBinding'eq
  -- ** Dict lookup (dot)
  , Dot (..)
  , expression'applyDots
  , render'dot
  , parse'dot'rhs'chain

  -------------------------------------------------

  -- * Functions
  -- ** Lambdas
  , Lambda (..)
  , render'lambda
  , parse'lambda
  -- ** Function parameters
  , Param (..)
  , render'param
  , parse'param
  , parse'param'var
  , parse'param'noVar
  -- ** Dict patterns
  , DictPattern (..)
  , DictPattern'1 (..)
  , render'dictPattern
  , render'dictPattern'1
  , parse'dictPattern
  , parse'dictPattern'start
  -- ** Function application
  , Apply (..)
  , expression'applyArgs
  , render'apply

  -------------------------------------------------

  -- * @let@
  , Let (..)
  , keyword'let
  , keyword'in
  , render'let
  , parse'let
  -- ** @let@ bindings
  , LetBinding (..)
  , render'letBinding
  , parse'letBinding
  , parse'letBinding'eq
  , parse'letBinding'inherit

  -------------------------------------------------

  -- * @inherit@
  , Inherit (..)
  , keyword'inherit
  , render'inherit
  , parse'inherit

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

  -- * Miscellanea
  , Render
  , parse'antiquote

  -------------------------------------------------

  ) where

import Bricks.Expression
import Bricks.IndentedString
import Bricks.Keyword
import Bricks.Parsing
import Bricks.Rendering
import Bricks.StringExpressions
import Bricks.UnquotedString

{- $module-overview

== Modules that are re-exported from "Bricks"

The following modules are re-exported from this top-level "Bricks" module in
their entireties.

Modules related to syntax:

  - "Bricks.Keyword" - Enumerates the language's keywords
  - "Bricks.IndentedString" - Deals with the whitespace cleanup performed when
    parsing indented strings (@''@...@''@)
  - "Bricks.UnquotedString" - Defines the rules for what strings are allowed to
    appear unquoted in Bricks code
  - "Bricks.Expression" - Defines most of the types related to the AST, notably
    'Expression'
  - "Bricks.Parsing" - Defines all of the Parsec parsers for parsing Bricks code
    into 'Expression's
  - "Bricks.Rendering" - Defines all of the renderers for turning 'Expression's
    into Bricks code
  - "Bricks.StringExpressions" - Defines the three types of strings in the AST:
    unquoted, static, and dynamic

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

Furthermore, when the expression on the right-hand side is a list literal
(an expression of the form @[@...@]@), the antiquotation (wrapping the
expression in @${@...@}@) may be omitted:

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

The Bricks block comment form is @{\-@...@-\}@; in Nix it is @/*@...@*/@.

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

Within the indented string form (@''@...@''@), Nix supports the following
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

== The Nix "set" concept is renamed to "dict" in Bricks

The Nix concept of "set" is referred to as "dict" in Bricks. This is not
actually a language difference; we just use a different word to talk about the
same thing. We believe that "dict" is a more familiar term for this data
structure, and that Nix's use of "set" conflicts unnecessarily with the more
common usage of the word.

-}
