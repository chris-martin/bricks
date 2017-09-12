{- | *Bricks* is Nix-like language.

Notable differences from Nix:

- No built-in null, integer, or boolean types
- No @builtins@ and no infix operators (@+@, @-@, @//@)
- No URI literals
- The concept of "set" is referred to as "dict" (this is not actually a language
  difference, I just use a different word to talk about the same concept)

-}
module Bricks
  ( module Bricks.Bare
  , module Bricks.IndentedString
  , module Bricks.Keyword
  , module Bricks.Parsing
  , module Bricks.Rendering
  , module Bricks.Expression
  ) where

import Bricks.Bare
import Bricks.IndentedString
import Bricks.Keyword
import Bricks.Parsing
import Bricks.Rendering
import Bricks.Expression
