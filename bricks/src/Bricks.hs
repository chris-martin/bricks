{- | __Bricks__ is a lazy functional language that strongly resembles Nix.

Notable differences from Nix:

- No built-in null, integer, or boolean types
- No @builtins@
- No infix operators (@+@, @-@, @//@)
- No @if@-@then@-@else@
- No @with@
- No URI literals
- No escape sequences in indented strings (@''@...@''@)
- The inline comment keyword is @--@ rather than @#@
- There are block comments in the form @{\-@...@-\}@
- The concept of "set" is referred to as "dict" (this is not actually a language
  difference, we just use a different word to talk about the same concept)

The following modules are re-exported from this module in their entireties:

- Syntax
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
- Evaluation
  - "Bricks.Term" - ...
  - "Bricks.ExpressionToTerm" - ...
  - "Bricks.Evaluation" - ...

Other modules:

- "Bricks.Expression.Construction" - Functions for constructing 'Expression's
  in a way that matches their 'Show' implementations.

-}
module Bricks

  (
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
  , Str'Unquoted (..)
  , str'unquoted'text
  , UnquotedString (..)
  , unquotedString'try
  , unquotedString'orThrow
  , text'canBeUnquoted
  , char'canBeUnquoted
  , render'strUnquoted
  , parse'strUnquoted
  -- ** String conversions
  , str'dynamicToStatic
  , str'staticToDynamic
  , str'unquotedToDynamic
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
