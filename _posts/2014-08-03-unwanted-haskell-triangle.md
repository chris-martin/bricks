---
layout: post
title: "Unwanted Haskell triangle"
date_granularity: day
tags: haskell java stackoverflow
thumbnail: /assets/posts/2014-08-03-unwanted-haskell-triangle/triangle.png
redirect_from: /haskell/java/stackoverflow/2014/08/03/unwanted-haskell-triangle/
---

<blockquote class="twitter-tweet" lang="en"><p>Sometimes I see a Java SO
question, write myself a haskell solution, then sadly read over the thread of
people who will never know about it.</p>&mdash; Chris Martin (@chris__martin)
<a href="https://twitter.com/chris__martin/statuses/495434666623909889">August
2, 2014</a></blockquote>

<script async src="//platform.twitter.com/widgets.js" charset="utf-8"></script>

It's getting increasingly painful to read Stack Overflow newb questions,
because imperative languages are so unnecessarily difficult.

## How do I do \_\_\_\_ in Java?

[This question on Stack Overflow][stackoverflow] is about producing this string
(using Java):

    1
    0 1
    1 0 1
    0 1 0 1

The answers given are okay as far as Java goes (although they're both subtly
incorrect because they include an extra space at the end of each line).

{% highlight java %}
int i, j, a = 1;
for (i = 1; i <= 4; i++){
    a = i % 2;
    for(j=1; j<=i; j++) {
        System.out.print(a+ " ");
        a = a ^ 1;
    }
    System.out.println();
}
{% endhighlight %}

This one's rather clever.

{% highlight java %}
String str = "";
for (int i = 1; i <= 4; i++) {
    str = (i % 2) + " " + str;
    System.out.println(str);
}
{% endhighlight %}

But the thread is frustrating to me because it doesn't seem like the asker
could have learned anything about algorithm construction. None of the answers
break the problem into smaller logical components. Their correctness isn't
constructive; you kind of just have to look at it, convince yourself that it's
correct, and wonder how you could have come up with it on your own.

## Here's how you'd do it in Haskell.

I want to write a more instructive answer. I turn to my latest pet language. A
few weeks ago it would've been Scala. At the moment it's Haskell.

### Alternation

For starters, at the core of the problem is an alternating sequence of `1` and
`0`.

{% highlight haskell %}
> let alternation = map (`mod` 2) [1..]
> take 10 alternation
[1,0,1,0,1,0,1,0,1,0]
{% endhighlight %}

Sometimes we need the sequence to start with `0`, so let's parameterize it.

{% highlight haskell %}
> let alternation start = map (`mod` 2) [start..]
> take 10 $ alternation 0
[0,1,0,1,0,1,0,1,0,1]
> take 10 $ alternation 1
[1,0,1,0,1,0,1,0,1,0]
{% endhighlight %}

Already we have something here that the other answers don't: A very small piece
of the puzzle solved in a way that we can analyze and test in isolation.

### Lines

We're going to be putting these sequences together in space-separated strings,
so we import `intercalate :: [a] -> [[a]] -> [a]` , which (for `Char` lists)
resembles Scala's `Iterable.mkString` or Python's `str.join`.

{% highlight haskell %}
> import Data.List (intercalate)
{% endhighlight %}

Now we can define each line as a function of its index.

{% highlight haskell %}
> let line i = intercalate " " $ map show $ take i $ alternation i
> line 3
"1 0 1"
> line 4
"0 1 0 1"
{% endhighlight %}

Note how pleasant it is that it's trivial to test the program at every step of
the design.

### Triangles

A triangle of size `n` consists of the first `n` lines joined with a newline.

{% highlight haskell %}
let triangle n = intercalate "\n" $ map line [1..n]
> putStrLn $ triangle 4
1
0 1
1 0 1
0 1 0 1
{% endhighlight %}

### In summation

{% highlight haskell %}
import Data.List (intercalate)

main :: IO()
main = putStrLn $ triangle 4
  where
    -- a triangle with n rows
    triangle n = intercalate "\n" $ map line [1..n]
    -- the i^th line of a triangle
    line i = intercalate " " $ map show $ take i $ alternation i
    -- list of 0 and 1, starting with i mod 2
    alternation i = map (`mod` 2) [i..]
{% endhighlight %}

I find it amusing here that I actually wrote as much (if not *more*) code here
than the Java implementations. I attribute a lot of that to our ability to make
concepts like `alternation`, `line`, and `triangle` explicit and therefore to
give them names. Contrary to stereotype, the Java is terse and symbol-heavy,
and the Haskell uses more human words.

## Do you have a moment to talk about our Lord and Savior Jesus Christ?

<blockquote class="twitter-tweet" lang="en"><p>you could post it, just for
curiosity&#39;s sake, but it&#39;d only cause negative emotions</p>&mdash;
Chris Martin (@chris__martin) <a
href="https://twitter.com/chris__martin/statuses/495434954399285248">August 2,
2014</a></blockquote>

I wish we'd do this more often. I wish we'd look at these Java questions and 
say: "Look, first you need to *understand* the problem on a deeper level - So
let's implement it with more tractable mathematics ... Now that you understand
the basics, we can get into more difficult coding topics like control flow and
Java."

Teaching, trolling, or proselytizing?

## Addendum

Here's a slightly revised program thanks to some feedback.

<blockquote class="twitter-tweet" lang="en"><p>
<a href="https://twitter.com/chris__martin">@chris__martin</a> Nice
understandable solution. My similar would use
&quot;zeroOnes = &#39;0&#39;:&#39;1&#39;:zeroOnes&quot; and &quot;take i $
drop (i `mod` 2) $ zeroOnes&quot;.</p>&mdash; Franklin Chen (@franklinchen)
<a href="https://twitter.com/franklinchen/statuses/496275930667843584">August
4, 2014</a></blockquote>

{% highlight haskell %}
import Data.List (intercalate, intersperse)

main :: IO()
main = putStrLn $ triangle 4
  where
    triangle n = intercalate "\n" $ map line [1..n]
    line i = intersperse ' ' $ take i $ drop (i `mod` 2) $ zeroOnes
    zeroOnes = '0' : '1' : zeroOnes
{% endhighlight %}

I like how it expresses the zero-one sequence in more straightforward manner
without being tricky with arithmetic.

[stackoverflow]: https://stackoverflow.com/questions/25091218/print-triangle-pattern-using-java
