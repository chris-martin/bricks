{ file-path, file-string, html, code, markdown, latex }:

let
  erdos  = "http://en.wikipedia.org/wiki/Erd%C5%91s_number";
  rms    = "http://en.wikipedia.org/wiki/Richard_Stallman";
  koans  = "http://www.catb.org/~esr/writings/unix-koans/";
  bars   = "https://github.com/chris-martin/bars";
  random = "http://tldp.org/LDP/abs/html/randomvar.html";

  bash = code { language = "bash"; };

  p = x: html.p {} (markdown x);
  h2 = html.h2 {};

in {
  title = "Randomization pipeline";
  date  = "2009 June 20";
  slug  = "randomization-pipeline";

  thumbnail = file-path ./thumbnail.png;

  abstract = "Random selection from a stream in constant space.";

  body = [

    (p ''
      One disappointing aspect of my CS experience is that I don’t get to
      write many interesting algorithms for real life. I think I came into
      this field expecting to see a lot of abstract computing problems
      requiring clever solutions. What I’ve experienced, however, is that
      most computing exercises tends to lean heavily to one side of the
      theory-practice spectrum. A math-centric discussion of a clever
      algorithm may deal with proofs of correctness or asymptotic runtime,
      but will rarely consider its practical application.
    '')

    (p ''
      In the software engineering world, we focus on higher-level
      architecture, and sometimes use the term *clever* pejoratively.
      The software goal is to abstract out the algorithms entirely so we
      can leave those pesky *implementation details* to libraries written
      by people with smaller [Erdős numbers](${erdos}) and greater
      [eccentricities](${rms}).
    '')

    (p ''
      There is certainly nothing wrong with this mentality. Writing javascript
      for the web without a framework may be more fun for a while, but it is
      ultimately foolish. You won’t be able to address nearly as many browser
      bugs within the scope of your project as the library maintainers have,
      and you don’t want to waste time replicating that effort anyway.
      Although it can be temptingly enjoyable to delve into the murky depths
      of writing low-level code and implementing your own generic data
      structures, I’ve had a hard time finding opportunities to get my hands
      dirty with anything outside of purely academic endeavours.
    '')

    (p ''
      So I get excited when I encounter a real problem with a nontrivial
      solution involving math and progamming. I want to select a random file
      from some part of the filesystem, so I need a script (let’s call it
      `choose`) that can pick a random line from its input stream, so that
      I can get a random file with:
    '')

    (bash "find -type f | choose")

    (p ''
      This problem poses no actual difficulty, of course - the obvious
      solution is to read all *n* lines, pick a random number *i* uniformly
      on *[1, n]*, and print the *i*<sup>th</sup> line:
    '')

    (bash (file-string ./naive.sh))

    (p ''
      But this just doesn’t feel like it follows the [Way of Unix](${koans}),
      because it requires holding the entire list in memory just to eventually
      write a single line. We’re piping streams without using the pipelining to
      our advantage.  I’d like to be able to do this using constant space.
    '')

    #####################################################################

    (h2 "The Shuffle")

    (p ''
      My initial reaction is that it probably is not possible, but I think
      back to a neat little in-place array randomizer introduced in algorithms
      class. This is not all that related my current problem, but I’d mention
      it as sort of a source of inspiration. It’s a simple array shuffle:
    '')

    (code {} ''
      function shuffle(S)
          for a in 1..|S|
              b := randomly select integer from [i, n]
              swap(S, a, b)
    '')

    (p ''
      It’s not a difficult exercise to show that this produces a uniformly
      random distribution of array permutations. Let *pr(x<sub>i</sub>, j)* be
      the probability that element starting at position *i* ends up at position
      *j*. For the distribution to be uniform, this probability must be *1 / n*
      for all *i* and *j*.
    '')

    (p ''
      First, an element can only end up in position *1* if it is selected on the
      first iteration, with probability *1 / n*. Then with strong induction we
      can show:
    '')

    (latex ''
      \[
      \begin{align*}
          \textrm{pr}(x,j)
          & = \left( 1 - \sum_{i=1}^{j-1}(\textrm{pr}(x,i)) \right)
              \left( \frac{1}{n-j+1} \right) \\
          & = \left( 1 - \frac{j-1}{n} \right)
              \left( \frac{1}{n-j+1} \right)
            = \frac{1}{n}
      \end{align*}
      \]
    '')

    (p ''
      The math wasn’t really necessary there, because this process is fairly
      intuitive. You can think of it as a random version of an in-place
      insertion sort, wherein the array is divided into *randomized* and
      *unrandomized* segments (instead of *sorted* and *unsorted*.)
    '')

    (p "This is also useful for generating a random subset of size *r*:")

    (code {} ''
      function subset(S, r)
          for a in 1..r
              b := randomly select integer from [i, n]
              swap(S, a, b)
          return S[1..r]
    '')

    (p ''
      That was quite a digression. But the point was, you can generate uniform
      randomness in some odd, unobvious ways.
    '')

    #####################################################################

    (h2 "One Random Element")

    (p ''
      But to get back to the task at hand: picking a random element from an
      input stream in constant space. I can only think of one reasonable way
      to write this algorithm:
    '')

    (code {} ''
      function choose(input)
          i = 1
          chosen := nil
          for x in input
              if (true with probability f_i)
                  chosen := x
              i := i + 1
          return chosen
    '')

    (p ''
      It holds onto a single entry (*chosen*) at a time. Each time new entry
      *x* is read, it becomes the chosen entry with some probability dependent
      only on *i* (because the value of *i* is the only information available).
    '')

    (p ''
      First, an expression for *p<sub>i</sub>*, the probability that the
      algorithm chooses *x<sub>i</sub>*. This event occurs when
      *x<sub>i</sub>* is swapped in, and no subsequent elements are swapped
      in to replace it:
    '')

    (latex "\[ p_i = f_i \prod_{k=i+1}^n (1 - f_k) \]")

    (p ''
      *f* needs to be defined such that *p<sub>i</sub> = 1 / n* for all
      *1 &le; i &le; n*. An equivalent statement is that
      *p<sub>i</sub> = p<sub>i+1</sub>* for all *1 &le; i &lt; n*.
      This information is enough to construct a recurrence relation for *f*.
    '')

    (latex ''
      \[
      \begin{align*}
          p_i               & = p_{i+1}
          \\
          f_i \prod_{k=i+1}^n (1 - f_k)
                            & = f_{i+1} \prod_{k=i+2}^n (1 - f_k)
          \\
          f_i (1 - f_{i+1}) \prod_{k=i+2}^n (1 - f_k)
                            & = f_{i+1} \prod_{k=i+2}^n (1 - f_k)
          \\
          f_i (1 - f_{i+1}) & = f_{i+1}
          \\
          f_{i+1}           & = \frac{f_i}{f_i + 1}
      \end{align*}
      \]
    '')

    (p ''
      The first entry needs to be recorded no matter what, so
      *f<sub>1</sub> = 1*. Solving the recurrence for *f* gives
      *f<sub>i</sub> = 1 / i*.
    '')

    #####################################################################

    (h2 "Bash Implementation")

    (bash (file-string ./choose-one.sh))

    (p "Tested it with a cute little histogram script called [bars](${bars}):")

    (bash ''
      (for i in {1..10000}; do (echo `seq 1 5 | ./choose`); done) | ./bars -r 0 -3 30
    '')

    br

    (code {} (file-string ./histogram-one.txt))

    #####################################################################

    (h2 "More Random Elements")

    (p ''
      So the next logical question is: Can this be generalized to choose some
      *r* elements instead of just one?
    '')

    (code {} ''
      function choose(input, r)
          i = 1
          chosen := collection of size r
          for x in input
              if (true with probability f_i))
                  chosen.add(x)
              i := i + 1
          return chosen
    '')

    (p ''
      The new algorithm is strikingly similar to the first, but *chosen* now
      needs to be some sort of data structure which holds up to *r* elements.
      For the same reason that *f<sub>1</sub> = 1* in the previous version,
      in this case *f<sub>1</sub> ... f<sub>r</sub>* must all be *1* (the
      first *r* elements must all be saved).
    '')

    (p ''
      Once full, however, this data stucture needs to make some decision
      about which element to evict when a new one is added. There are two
      reasonable choices - it could choose randomly, or behave as a FIFO
      queue. The latter solution seemed to be more elegant, but the math
      involved is not pretty (just trying to write an expression for
        *p<sub>i</sub>* is a dreadful mess).
    '')

    (p ''
      When the replacement occurs randomly, however, determining
      *p<sub>i</sub>* is just as easy as it was last time. The only
      difference is that now the probability that an item with be evicted
      by a subsequent item *x<sub>k</sub>*, which has changed from
      *f<sub>k</sub>* to *f<sub>k</sub> / r*.
    '')

    (latex ''
      \[
      \begin{align*}
          p_i & = f_i \prod_{k=i+1}^n \left(1 - \frac{f_k}{r}\right) \\
              & = f_i \, (r^{i-n}) \prod_{k=i+1}^n (r - f_k)
      \end{align*}
      \]
    '')

    (p "A recurrence for *f<sub>i</sub>* also follows in the same manner.")

    (latex ''
      \[
      \begin{align*}
          p_i               & = p_{i+1} \\
          f_i (r^{i-n}) \prod_{k=i+1}^n (r - f_k)
                            & = f_{i+1} (r^{(i+1)-n}) \prod_{k=i+2}^n (r - f_k) \\
          f_i (r^{i-n}) (r - f_{i+1}) \prod_{k=i+2}^n (r - f_k)
                            & = f_{i+1} (r) (r^{i-n}) \prod_{k=i+2}^n (r - f_k) \\
          f_i (r - f_{i+1}) & = f_{i+1} (r) \\
          f_{i+1}           & = \frac{f_i r}{f_i + r}
      \end{align*}
      \]
    '')

    (p ''
      *f<sub>r</sub> = 1* as stated earlier. So for *i &le; r*, this
      recurrence tells us that *f<sub>i</sub> = r / i*.
    '')

    #####################################################################

    (h2 "More Bash")

    (bash (file-string ./choose-many.sh))

    (p "This histogram is for the selection of 2 elements from 1 to 5:")

    (bash ''
      (for i in {1..10000}; do (echo `seq 1 5 | ./choose 2 | sort | tr "\n" " "`); done) | ./bars -r 0
    '')

    br

    (code {} (file-string histogram-many.txt))

    #####################################################################

    (h2 "A Few Notes")

    (p ''
      Much of this math lacks rigor. Specifically, I sort of made up a
      definition of a uniformly random subset. This text only considers the
      criterion that each element be included with the correct probability,
      but it ignores any notion of independence. For instance, a poorly
      designed array shuffling algorithm that merely shifts indices (moves
      each *x<sub>i</sub>* to position *(i + random) % n*) would satisfy the
      former but not the latter requirement.
    '')

    (p ''
      This implementation is not good for large sets, because
      [$RANDOM](${random}) is limited to 32767. It won’t fail, but as the set
      number approaches this magnitude, probabilities will be off.
    '')

    (p ''
      My testing has not shown this algorithm to provide any speedup over the
      simple version. Picking a random number from `seq 10000` takes about twice
      as long.
    '')

    (p ''
      I did not design this with much mind to efficiency. For example, if
      *r = 1*, there is no need to generate a random number to determine an
      element’s place in the `chosen` array.
    '')

    (p ''
      The script generates a lot of random numbers - two for each element. If
      "true" randomness is important, this algorithm requires a great deal of
      entropy.
    '')

    #####################################################################

    (h2 "Conclusion")

    (p ''
      Hopefully something here was useful aside from learning some new Bash
      tricks.
    '')

    (p ''
      I think this is a good strategy if you ever need to pick something random
      from a set elements read from IO in a situation where space is a concern.
    '')

    (p ''
      So, like all clever tricks, you will likely never have good reason to use
      it.
    '')

  ];
}
