---
layout: post
title: Correctness.scala
date_granularity: day
tags: scala
---

{% highlight scala %}
trait Correctness {
  
  // Let's talk about programs.
  type P
  
  // A specification is a predicate on a program.
  trait S { def apply (p: P): Boolean }
  
  // We say "p implements s" when a program meets a specification.
  def `p implements s` (p: P, s: S) = s (p)
  
  // We shorten this to "p is correct" when a specification is contextually implied.
  def `p is correct` (p: P)(implicit s: S) = `p implements s` (p, s)
  
  // This abbreviation can lead us to forget about specification.
  // But without it, there can be no notion of correctness.
  def `p is intrinsically correct` (p: P): Boolean = ???
  
  // Acceptability is also a predicate on a program.
  trait A { def apply (p: P): Boolean }
  
  // This makes it easy to confuse acceptance criteria with specification,
  // but "p is acceptable" and "p is correct" are not the same.
  def `p satisfies a` (p: P, a: A) = a (p)
  def `p is acceptable` (p: P)(implicit a: A) = `p satisfies a` (p, a)
  
  // The specification is correct iff all programs that implement it are acceptable.
  def `s correctly reflects a` (s: S, a: A): Boolean
  def `s is correct` (s: S)(implicit a: A) = `s correctly reflects a` (s, a)
  
  // From this we can derive an alternative definition of program acceptability.
  def `p is acceptable` (p: P)(implicit s: S, a: A) =
    `p is correct` (p) && `s is correct` (s)
  
  // The purpose of a specification is to be used in this way.
  def `specification used as an acceptance test` (s: S)(implicit a: A): A = {
    assert ( `s is correct` (s) )
    new A { def apply (p: P) = s (p) }
  }
}
{% endhighlight %}
