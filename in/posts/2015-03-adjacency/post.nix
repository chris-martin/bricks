{ file-path, html-tags, markdown, code }:

let
  p = x: html-tags.p (markdown x);
  inherit (html-tags) ul li;

in {
  title = "Adjacency";
  date  = "2015 Mar 11";
  slug  = "adjacency";

  thumbnail = file-path ./thumbnail.png;

  abstract = ''
    The most terse operator in any language is adjacency, or in other words,
    no operator at all.
  '';

  body = [

    (p ''
      Languages try to allocate the most terse bits of syntax to the most
      common operations. The most terse operator is adjacency, or in other
      words, no operator at all. So the semantics of adjacency tend to
      reflect some central tenet of the language.
    '')

    (p "A quick code example:")

    (code {} "a b c")

    (ul [
      (li ''
        In a lisp (adding some parens),
        this is "a list of *a*, *b*, and *c*".
        Adjacency is cons.
      '')
      (li ''
        In a concatenative language like J,
        this is "*a* ∘ *b* ∘ *c*".
        Adjacency is function composition.
      '')
      (li ''
        In a functional language like Haskell,
        this is "(*a* applied to *b*) applied to *c*".
        Adjacency is function application.
      '')
      (li ''
        In a procedural language (adding some newlines/semicolons),
        this is "do *a*, do *b*, then do *c*".
        Adjacency is IO sequencing.
      '')
    ])

    (p ''
      When you look at adjacency as a function, Python seems not that all
      that different from Haskell.
    '')

    (p "Python:")

    (code { language = "python"; } ''
      print('a') ; print('b') ; print('c')

      print('a') ; print(sys.stdin.readline()) ; print('c')
    '')

    (p "Haskell:")

    (code { language = "haskell"; } ''
      putStrLn "a" >> putStrLn "b" >> putStrLn "c"

      putStrLn "a" >> getLine >>= putStrLn >> putStrLn "c"
    '')

    (p ''
      Some code is very IO-heavy, and Haskell realizes this, so it has
      a `do` syntax which adopts the procedural convention of using
      newline/semicolon adjacency to denote IO sequencing.
    '')

    (code { language = "haskell"; } ''
      do ; putStrLn "a" ; putStrLn "b" ; putStrLn "c"

      do ; putStrLn "a" ; b <- getLine ; putStrLn b ; putStrLn "c"
    '')

    (p ''
      There are other types that compose using `>>` and `>>=` too (an easy
      example is `Maybe`), and they can also be used with the `do` syntax.
    '')

    (p "Shit, I almost accidentally wrote a monad tutorial.")

  ];
}
