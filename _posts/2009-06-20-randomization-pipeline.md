---
layout: post
title: "Randomization pipeline"
date_granularity: day
thumbnail: /assets/posts/2009-06-20-randomization-pipeline/thumbnail.png
tags: algorithms randomness bash
---

One disappointing aspect of my CS experience is that I don't get to write many
interesting algorithms for real life. I think I came into this field expecting
to see a lot of abstract computing problems requiring clever solutions.  What
I've experienced, however, is that most computing exercises tends to lean
heavily to one side of the theory-practice spectrum. A math-centric discussion
of a clever algorithm may deal with proofs of correctness or asymptotic runtime,
but will rarely consider its practical application.

In the software engineering world, we focus on higher-level architecture, and
sometimes use the term *clever* pejoratively. The software goal is to abstract
out the algorithms entirely so we can leave those pesky *implementation details*
to libraries written by guys with smaller [Erdős numbers][erdos] and larger
[beards][rms].

There is certainly nothing wrong with this mentality. Writing javascript for the
web without a framework may be more fun for a while, but it is ultimately
foolish. You won't be able to address nearly as many browser bugs within the
scope of your project as the library maintainers have, and you don't want to
waste time replicating that effort anyway. Although it can be temptingly
enjoyable to delve into the murky depths of writing low-level code and
implementing your own generic data structures, I've had a hard time finding
opportunities to get my hands dirty with anything outside of purely academic
endeavours.

So I get excited when I encounter a real problem with a nontrivial solution
involving math and progamming. I want to select a random file from some part of
the filesystem, so I need a script (let's call it `choose`) that can pick a
random line from its input stream, so that I can get a random file with:

{% highlight bash %}
find -type f | choose
{% endhighlight %}

This problem poses no actual difficulty, of course - the obvious solution is to
read all *n* lines, pick a random number *i* uniformly on *[1, n]*, and print
the *i*<sup>th</sup> line:

{% highlight bash %}
#!/bin/bash

all=()
i=0

while read x; do
  all[$i]="$x"
  i=$((i + 1))
done

echo "${all[$((RANDOM % i))]}"
{% endhighlight %}

But this just doesn't feel like it follows the [Way of Unix][koans], because it
requires holding the entire list in memory just to eventually write a single
line. We're piping streams without using the pipelining to our advantage.  I'd
like to be able to do this using constant space.

## The Shuffle

My initial reaction is that it probably is not possible, but I think back to a
neat little in-place array randomizer introduced in algorithms class. This is
not all that related my current problem, but I'd mention it as sort of a source
of inspiration. It's a simple array shuffle:

    function shuffle(S)
        for a in 1..|S|
            b := randomly select integer from [i, n]
            swap(S, a, b)

It's not a difficult exercise to show that this produces a uniformly random
distribution of array permutations. Let *pr(x<sub>i</sub>, j)* be the
probability that element starting at position *i* ends up at position *j*. For
the distribution to be uniform, this probability must be *1 / n* for all *i* and
*j*.

First, an element can only end up in position *1* if it is selected on the first
iteration, with probability *1 / n*. Then with strong induction we can show:

`\[
\begin{align*}
    \textrm{pr}(x,j)
    & = \left( 1 - \sum_{i=1}^{j-1}(\textrm{pr}(x,i)) \right)
        \left( \frac{1}{n-j+1} \right) \\
    & = \left( 1 - \frac{j-1}{n} \right)
        \left( \frac{1}{n-j+1} \right)
      = \frac{1}{n}
\end{align*}
\]`

The math wasn't really necessary there, because this process is fairly
intuitive. You can think of it as a random version of an in-place insertion
sort, wherein the array is divided into *randomized* and *unrandomized* segments
(instead of *sorted* and *unsorted*.)

This is also useful for generating a random subset of size *r*:

    function subset(S, r)
        for a in 1..r
            b := randomly select integer from [i, n]
            swap(S, a, b)
        return S[1..r]

That was quite a digression. But the point was, you can generate uniform
randomness in some odd, unobvious ways.

## One Random Element

But to get back to the task at hand: picking a random element from an input
stream in constant space. I can only think of one reasonable way to write this
algorithm:

    function choose(input)
        i = 1
        chosen := nil
        for x in input
            if (true with probability f_i)
                chosen := x
            i := i + 1
        return chosen

It holds onto a single entry (*chosen*) at a time. Each time new entry *x* is
read, it becomes the chosen entry with some probability dependent only on *i*
(because the value of *i* is the only information available).

First, an expression for *p<sub>i</sub>*, the probability that the algorithm
chooses *x<sub>i</sub>*. This event occurs when *x<sub>i</sub>* is swapped in,
and no subsequent elements are swapped in to replace it:

`\[ p_i = f_i \prod_{k=i+1}^n (1 - f_k) \]`

*f* needs to be defined such that *p<sub>i</sub> = 1 / n* for all
*1 &le; i &le; n*. An equivalent statement is that
*p<sub>i</sub> = p<sub>i+1</sub>* for all *1 &le; i &lt; n*. This information is
enough to construct a recurrence relation for *f*.

`\[
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
\]`

The first entry needs to be recorded no matter what, so *f<sub>1</sub> = 1*.
Solving the recurrence for *f* gives *f<sub>i</sub> = 1 / i*.

## Bash Implementation

{% highlight bash %}
#!/bin/bash
RANDOM=$$   # seed the rng with the pid
chosen=''   # will hold selected element
i=1         # initialize loop counter

# read one line at a time
while read x; do

  # select x with probability 1/i
  if [ "$(($RANDOM % $i))" = "0" ]; then
    chosen="$x"
  fi

  # increment loop counter
  i=$((i + 1))

done

echo $chosen
{% endhighlight %}

Tested it with a cute little histogram script called [bars][bars]:

{% highlight bash %}
(for i in {1..10000}; do (echo `seq 1 5 | ./choose`); done) | ./bars -r 0 -3 30
{% endhighlight %}

        1| 1047| ******************************
        2| 1002| *****************************
        3|  988| ****************************
        4| 1035| ******************************
        5|  977| ****************************
        6|  982| ****************************
        7|  988| ****************************
        8|  976| ****************************
        9|  996| *****************************
       10| 1009| *****************************

## More Random Elements

So the next logical question is: Can this be generalized to choose some *r*
elements instead of just one?

    function choose(input, r)
        i = 1
        chosen := collection of size r
        for x in input
            if (true with probability f_i))
                chosen.add(x)
            i := i + 1
        return chosen

The new algorithm is strikingly similar to the first, but *chosen* now needs to
be some sort of data structure which holds up to *r* elements. For the same
reason that *f<sub>1</sub> = 1* in the previous version, in this case
*f<sub>1</sub> ... f<sub>r</sub>* must all be *1* (the first *r* elements must
all be saved).

Once full, however, this data stucture needs to make some decision about which
element to evict when a new one is added. There are two reasonable choices - it
could choose randomly, or behave as a FIFO queue. The latter solution seemed to
be more elegant, but the math involved is not pretty (just trying to write an
expression for *p<sub>i</sub>* is a dreadful mess).

When the replacement occurs randomly, however, determining *p<sub>i</sub>* is
just as easy as it was last time. The only difference is that now the
probability that an item with be evicted by a subsequent item *x<sub>k</sub>*,
which has changed from *f<sub>k</sub>* to *f<sub>k</sub> / r*.

`\[
\begin{align*}
    p_i & = f_i \prod_{k=i+1}^n \left(1 - \frac{f_k}{r}\right) \\
        & = f_i \, (r^{i-n}) \prod_{k=i+1}^n (r - f_k)
\end{align*}
\]`

A recurrence for *f<sub>i</sub>* also follows in the same manner.

`\[
\begin{align*}
    p_i               & = p_{i+1} \\
    f_i (r^{i-n}) \prod_{k=i+1}^n (r - f_k)
                      & = f_{i+1} (r^{(i+1)-n}) \prod_{k=i+2}^n (r - f_k) \\
    f_i (r^{i-n}) (r - f_{i+1}) \prod_{k=i+2}^n (r - f_k)
                      & = f_{i+1} (r) (r^{i-n}) \prod_{k=i+2}^n (r - f_k) \\
    f_i (r - f_{i+1}) & = f_{i+1} (r) \\
    f_{i+1}           & = \frac{f_i r}{f_i + r}
\end{align*}
\]`

*f<sub>r</sub> = 1* as stated earlier. So for *i &le; r*, this recurrence tells
us that *f<sub>i</sub> = r / i*.

## More Bash

{% highlight bash %}
#!/bin/bash
r=${1:-1}   # number of items (default 1)
RANDOM=$$   # seed the rng with the pid
chosen=()   # will contain selected elements
i=1         # initialize loop counter

# read one line at a time
while read x; do

  # add 1..r sequentially
  if [ $i -le $r ]; then
    chosen[$((i-1))]="$x"

  # add the rest with probability r/i
  elif [ "$(($RANDOM % $i))" -lt "$r" ]; then
    chosen[$(($RANDOM % $r))]="$x"

  fi

  # increment loop counter
  i=$((i + 1))

done

for ((i = 0; i < ${#chosen[@]}; i++)); do
  echo "${chosen[i]}"
done
{% endhighlight %}

This histogram is for the selection of 2 elements from 1 to 5:

{% highlight bash %}
(for i in {1..10000}; do (echo `seq 1 5 | ./choose 2 | sort | tr "\n" " "`); done) | ./bars -r 0
{% endhighlight %}

      1 2| 1016| ******************************
      1 3| 1007| *****************************
      1 4|  998| *****************************
      1 5|  997| *****************************
      2 3|  981| *****************************
      2 4| 1015| ******************************
      2 5|  987| *****************************
      3 4|  994| *****************************
      3 5|  975| ****************************
      4 5| 1030| ******************************

## A Few Notes

Much of this math lacks rigor. Specifically, I sort of made up a definition of a
uniformly random subset. This text only considers the criterion that each
element be included with the correct probability, but it ignores any notion of
independence. For instance, a poorly designed array shuffling algorithm that
merely shifts indices (moves each *x<sub>i</sub>* to position
*(i + random) % n*) would satisfy the former but not the latter requirement.

This implementation is not good for large sets, because [$RANDOM][bash-random]
is limited to 32767. It won't fail, but as the set number approaches this
magnitude, probabilities will be off.

My testing has not shown this algorithm to provide any speedup over the simple
version. Picking a random number from <code>`seq 10000`</code> takes about twice
as long.

I did not design this with much mind to efficiency. For example, if *r = 1*,
there is no need to generate a random number to determine an element's place in
the `chosen` array.

The script generates a lot of random numbers - two for each element. If "true"
randomness is important, this algorithm requires a great deal of entropy.

## Conclusion

Hopefully something here was useful aside from learning some new Bash tricks.

I think this is a good strategy if you ever need to pick something random from a
set elements read from IO in a situation where space is a concern.

So, like all clever tricks, you will likely never have good reason to use it.

[bars]: https://github.com/chris-martin/bars
[bash-random]: http://tldp.org/LDP/abs/html/randomvar.html
[erdos]: http://en.wikipedia.org/wiki/Erd%C5%91s_number
[koans]: http://www.catb.org/~esr/writings/unix-koans/
[rms]: http://en.wikipedia.org/wiki/Richard_Stallman

<script src="//cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"></script>

<script>
  MathJax.Hub.Config({
    tex2jax: {
      skipTags: ['script', 'noscript', 'style', 'textarea', 'pre']
    }
  });
  MathJax.Hub.Queue(function() {
    var all = MathJax.Hub.getAllJax(), i;
    for (i=0; i < all.length; i += 1) {
      all[i].SourceElement().parentNode.className += ' has-jax';
    }
  });
</script>

<style>
  code.has-jax {
    font: inherit;
    font-size: 100%;
    background: inherit;
    border: inherit;
  }
</style>
