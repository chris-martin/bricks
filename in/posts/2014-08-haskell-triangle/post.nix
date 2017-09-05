{ file-path, file-string, html, code }:

{
  title = "Unwanted Haskell triangle";
  date  = "2014 Aug 3";
  slug  = "unwanted-haskell-triangle";

  thumbnail = file-path ./triangle.png;

  redirect-from =
    "/haskell/java/stackoverflow/2014/08/03/unwanted-haskell-triangle/";

  abstract = ''
    Sometimes I see a Java question, write myself a Haskell solution, then
    sadly read over the thread of people who will never know about it.
  '';

  body = let
    stackoverflow = "https://stackoverflow.com/questions/25091218/print-triangle-pattern-using-java";
    p = html.p {};
    h2 = html.h2 {};
    h3 = html.h3 {};
    java = code { language = "java"; };
    haskell = code { language = "haskell"; };
  in [

    (html.raw (file-string ./tweet2.html))

    (p ''
      It’s getting increasingly painful to read Stack Overflow newb questions,
      because imperative languages are so unnecessarily difficult.
    '')

    (h2 "How do I do ____ in Java?")

    (p (markdown ''
      [This question on Stack Overflow](${stackoverflow}) is about producing
      this string (using Java):
    ''))

    (code {} ''
      1
      0 1
      1 0 1
      0 1 0 1
    '')

    (p ''
      The answers given are okay as far as Java goes (although they’re both
      subtly incorrect because they include an extra space at the end of each
      line).
    '')

    (java ''
      int i, j, a = 1;
      for (i = 1; i <= 4; i++){
          a = i % 2;
          for(j=1; j<=i; j++) {
              System.out.print(a+ " ");
              a = a ^ 1;
          }
          System.out.println();
      }
    '')

    (p "This one’s rather clever.")

    (java ''
      String str = "";
      for (int i = 1; i <= 4; i++) {
          str = (i % 2) + " " + str;
          System.out.println(str);
      }
    '')

    (p ''
      But the thread is frustrating to me because it doesn’t seem like the asker
      could have learned anything about algorithm construction. None of the
      answers break the problem into smaller logical components. Their
      correctness isn’t constructive; you kind of just have to look at it,
      convince yourself that it’s correct, and wonder how you could have come up
      with it on your own.
    '')

    (h2 "Here’s how you’d do it in Haskell.")

    (p ''
      I want to write a more instructive answer. I turn to my latest pet
      language. A few weeks ago it would’ve been Scala. At the moment it’s
      Haskell.
    '')

    (h3 "Alternation")

    (p (markdown ''
      For starters, at the core of the problem is an alternating sequence of `1`
      and `0`.
    ''))

    (haskell ''
      > let alternation = map (`mod` 2) [1..]
      > take 10 alternation
      [1,0,1,0,1,0,1,0,1,0]
    '')

    (p (markdown ''
      Sometimes we need the sequence to start with `0`, so let’s parameterize
      it.
    ''))

    (haskell ''
      > let alternation start = map (`mod` 2) [start..]
      > take 10 $ alternation 0
      [0,1,0,1,0,1,0,1,0,1]
      > take 10 $ alternation 1
      [1,0,1,0,1,0,1,0,1,0]
    '')

    (p ''
      Already we have something here that the other answers don’t: A very small
      piece of the puzzle solved in a way that we can analyze and test in
      isolation.
    '')

    (h3 "Lines")

    (p (markdown ''
      We’re going to be putting these sequences together in space-separated
      strings, so we import `intercalate :: [a] -> [[a]] -> [a]` , which (for
      `Char` lists) resembles Scala’s `Iterable.mkString` or Python’s
      `str.join`.
    ''))

    (haskell ''
      > import Data.List (intercalate)
    '')

    (p "Now we can define each line as a function of its index.")

    (haskell ''
      > let line i = intercalate " " $ map show $ take i $ alternation i
      > line 3
      "1 0 1"
      > line 4
      "0 1 0 1"
    '')

    (p ''
      Note how pleasant it is that it’s trivial to test the program at every
      step of the design.
    '')

    (h3 "Triangles")

    (p (markdown ''
      A triangle of size `n` consists of the first `n` lines joined with a
      newline.
    ''))

    (haskell ''
      let triangle n = intercalate "\n" $ map line [1..n]
      > putStrLn $ triangle 4
      1
      0 1
      1 0 1
      0 1 0 1
    '')

    (h3 "In summation")

    (haskell (file-string ./main.hs))

    (p (markdown ''
      I find it amusing here that I actually wrote as much (if not *more*) code
      here than the Java implementations. I attribute a lot of that to our
      ability to make concepts like `alternation`, `line`, and `triangle`
      explicit and therefore to give them names. Contrary to stereotype, the
      Java is terse and symbol-heavy, and the Haskell uses more human words.
    ''))

    (h2 "Do you have a moment to talk about our Lord and Savior Jesus Christ?")

    (html.raw (file-string ./tweet1.html))

    (html.raw (file-string ./tweet2.html))

    (p (markdown ''
      I wish we’d do this more often. I wish we’d look at these Java questions
      and say: "Look, first you need to *understand* the problem on a deeper
      level - So let’s implement it with more tractable mathematics ... Now that
      you understand the basics, we can get into more difficult coding topics
      like control flow and Java."
    ''))

    (p "Teaching, trolling, or proselytizing?")

    (h2 "Addendum")

    (p "Here’s a slightly revised program thanks to some feedback.")

    (html.raw (file-string ./tweet-franklinchen.html))

    (haskell (file-string ./main-revised.hs))

    (p ''
      I like how it expresses the zero-one sequence in more straightforward
      manner without being tricky with arithmetic.
    '')

  ];
}
