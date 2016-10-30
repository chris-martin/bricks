--------------------------------------------------------------------------------
title:     Adjacency
date:      2015 Mar 11
thumbnail: thumbnail.png
slug:      adjacency
abstract:  The most terse operator in any language is adjacency, or in other
           words, no operator at all.
--------------------------------------------------------------------------------

Languages try to allocate the most terse bits of syntax to the most
common operations. The most terse operator is adjacency, or in other
words, no operator at all. So the semantics of adjacency tend to
reflect some central tenet of the language.

A quick code example:

```
a b c
```

- In a lisp (adding some parens),
  this is "a list of *a*, *b*, and *c*".
  Adjacency is cons.

- In a concatenative language like J,
  this is "*a* ∘ *b* ∘ *c*".
  Adjacency is function composition.

- In a functional language like Haskell,
  this is "(*a* applied to *b*) applied to *c*".
  Adjacency is function application.

- In a procedural language (adding some newlines/semicolons),
  this is "do *a*, do *b*, then do *c*".
  Adjacency is IO sequencing.

When you look at adjacency as a function, Python seems not that all
that different from Haskell.

Python:

```python
print('a') ; print('b') ; print('c')

print('a') ; print(sys.stdin.readline()) ; print('c')
```

Haskell:

```haskell
putStrLn "a" >> putStrLn "b" >> putStrLn "c"

putStrLn "a" >> getLine >>= putStrLn >> putStrLn "c"
```

Some code is very IO-heavy, and Haskell realizes this, so it has
a `do` syntax which adopts the procedural convention of using
newline/semicolon adjacency to denote IO sequencing.

```haskell
do ; putStrLn "a" ; putStrLn "b" ; putStrLn "c"

do ; putStrLn "a" ; b <- getLine ; putStrLn b ; putStrLn "c"
```

There are other types that compose using `>>` and `>>=` too (an easy
example is `Maybe`), and they can also be used with the `do` syntax.

Shit, I almost accidentally wrote a monad tutorial.
