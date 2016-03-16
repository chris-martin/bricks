---
layout: post
title: Where do the little libraries belong?
date_granularity: day
tags: java
---

**There is a program we ask** of Java programmers in interviews sometimes, just
as an initial sanity check to make sure the candidate really knows the
language:

"Write a method that accepts a `List` and returns a copy of the list with
duplicate elements removed."

It's mostly just a test of whether they know how to use a hash set. Ignoring
order, all we're looking for is:

{% highlight java %}
return new ArrayList<>(new HashSet<>(list));
{% endhighlight %}

I work in what can be best described as a Java shop, but I am at heart a Scala
developer (for the moment), and this interview question saddens me. You
wouldn't ask it of a Scala programmer, unless you were merely quizzing them on
the standard library, because the answer is trivial:

{% highlight scala %}
list.distinct
{% endhighlight %}

---

**Why is life harder in Java?** The Scala coder has a library method for this
little task. Why doesn't the Java coder?

As a Java programmer, you have a several options that are all bad.

* Inline the method, potentially cluttering up your business logic. If you do
  it more than once, be prepared for the peer review comment "Refactor this and
  create a static `distinct` method."

* Implement `distinct` as a private method in whatever file you happen to be
  editing. But this won't help you or anyone else next time.

* Add the `distinct` method to your organization's "util" (a.k.a. "pile of
  miscellaneous shit") project. Without any coherency to its surroundings, the
  code will proceed to suffer the worst manner of bit rot.

* Publish `distinct` in a jar to Sonatype's repository. Hopefully by the end of
  the day it will be synced to Maven Central. No one else will ever use it, but
  you can pretend you contributed to open source.

* Try to find some Apache or Guava library that already includes this. I don't
  know of one, but I wouldn't be surprised if there is one that I just couldn't
  find. Peer review will ask: "Did you have to add a dependency just to avoid
  writing a one-line method?"

---

**Is this really important?** I don't know if I can explain to someone who has
not experienced the drudgery of a programming job why `distinct` is an example
of a problem that we need to worry about. I'm not sure what Emily Dickinson was
writing about here, but it feels appropriate:

> It's such a little thing to weep,<br/>
> So short a thing to sigh;<br/>
> And yet by trades the size of these<br/>
> We men and women die!

I find myself saying this a lot lately: The biggest reason we have for failing
to solve the hard problems is that we're too busy perpetually solving small
ones. Day-to-day programming is so full of these trivialities that I often
believe we've managed to foil the prediction of Alan Turing:

> Instruction tables will have to be made up by mathematicians with computing
> experience and perhaps a certain puzzle-solving ability. There need be no
> real danger of it ever becoming a drudge, for any processes that are quite
> mechanical may be turned over to the machine itself.

---

Scala shows up Java in the case study of `distinct` not because the language is
better, but because its standard library is more extensive. `Iterable` has a
fantastic assortment of features. I can speculate on why that came to be in
Scala but not Java, but I don't think it's relevant. I don't even mean to pick
on Java here. The question, as far as I know, applies universally:

---

**Why are we at the mercy** of languages' core libraries? Eight years ago I was
in my first computer science class, hearing about the virtues of code reuse,
imagining the open source Java ecosystem as a utopia of shared code. Reality
has brought complete disillusionment. It's too hard to publish code, it's too
hard to find code, and it's too hard to add dependencies.

The result is a situation wherein you have the very basics covered by the core
library, some particular tasks covered by third-party libraries only if the
problems they solve are hard enough both "to justify releasing a library" and
"to justify adding a dependency", and a large void in between where developers
poorly re-implement the same libraries over and over.

---

It's the easiest interview question we have, but I don't know the answer
anymore. How do you remove duplicate values from a Java `List`?
