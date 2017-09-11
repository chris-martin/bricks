{- | *Bricks* is Nix-like language.

Notable differences from Nix:

- No built-in null, integer, or boolean types
- No @\@@ keyword
- No @builtins@ and no infix operators (@+@, @-@, @//@)
- No URI literals
- The concept of "set" is referred to as "dict" (this is not actually a language
  difference, I just use a different word to talk about the same concept)
- No comments (todo - will add both inline and block comments)

-}
module Bricks
  ( module Bricks.Identifiers
  , module Bricks.Keywords
  , module Bricks.Parsing
  , module Bricks.Rendering
  , module Bricks.Types
  ) where

import Bricks.Identifiers
import Bricks.Keywords
import Bricks.Parsing
import Bricks.Rendering
import Bricks.Types
