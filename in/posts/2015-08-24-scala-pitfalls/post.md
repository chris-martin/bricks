--------------------------------------------------------------------------------
title:    Intro Scala pitfalls
date:     2015 Aug 24
slug:     intro-scala-pitfalls
abstract: There are a handful of mistakes that most people
          (who aren’t coming from a Haskell background) tend to fall into.
--------------------------------------------------------------------------------

This is an abridged summary of what I’ve learned from watching a few people
get started with Scala. There are a handful of mistakes that most people
(who aren’t coming from a Haskell background) tend to fall into.

My advice for Scala beginners:

1. [Don’t use collections methods that throw.](#collection-exceptions)
2. [Don’t use `Try` (often).](#try)
3. [Don’t use `Option.get`.](#option-get)
4. [Reduce block depth with `for` comprehensions.](#block-depth)
5. [Use implicit Java conversions.](#java-conversions)

I believe this is all pretty uncontroversial among Scala mavens, though
the unindoctrinated may require some discussion of functional programming
to justify it (which I make no attempt to do here).

## <a name="collection-exceptions"></a> Collection exceptions

These methods are killers:

```scala
(_: Seq[A].head)       :     => A

(_: Seq[A].apply)      : Int => A

(_: Map[A, B]).apply   : A   => B
```

They’re innocuously-named and look like the right methods to call, but in most
cases you need something that returns an `Option` instead:

```scala
(_: Seq[A].headOption) :     => Option[A]

(_: Seq[A].lift)       : Int => Option[A]

(_: Map[A, B].get)     : A   => Option[B]
```

The correct methods are all slightly longer (when we elide `apply`) than their
improperly typed counterparts, so coders exploring the API on their own tend to
find the wrong methods first.

## <a name="try"></a> `Try`

If one then discovers `Try` before correcting those habits, you end up a lot of
expressions like

```scala
Try(xs.head).toOption
```

instead of

```scala
xs.headOption
```

This can be tricky because there are some places where exceptions are
unavoidable. For example, I write this function often:

```scala
def parseDecimal(x: String): Option[BigDecimal] =
  Try(BigDecimal(x)).toOption
```

It’s important to explain that this is just a workaround for a deficiency in
the library, not an idiom to emulate.

## <a name="option-get"></a> `Option.get`

These examples are at least careful enough to check the `Option` before
unwrapping it, similar to how one would guard against `null` dereferences
in other languages. 

```scala
if (xOption.isDefined) f(xOption.get)

if (xOption.isDefined) f(xOption.get) else y

xOption.isEmpty || (xOption.isDefined && !f(xOption.get))
```

In Scala we can do better and preserve type safety all the way through.
The easy go-to solution for any `Option` problem is a pattern match:

```scala
xOption match { 
  case Some(x) => f(x)
  case None    =>
}

xOption match { 
  case Some(x) => f(x)
  case None    => y
}

xOption match { 
  case Some(x) => !f(x)
  case None    => true
}
```
 
And then with a little more API familiarity these expressions can be reduced:

```scala
xOption.foreach(f)

xOption.map(f).getOrElse(y)

!xOption.exists(f)
```

## <a name="block-depth"></a> Block depth

You then start to encounter the nauseating block nesting known in some
circles as "callback hell":

```scala
xs.foreach(x =>
  f(x).headOption.foreach(y =>
    map.get(y).foreach(z =>
      g(z, x)
    )
  )
)
```

```scala
w.toRight("a").flatMap(x =>
  f(x).toRight("b").flatMap(y =>
    g(y).toRight("c")
  )
)
```

So at this point you have to introduce `for` comprehensions.

```scala
for {
  x <- xs
  y <- f(x).headOption
  z <- map.get(y)
} g(z, x)
```

```scala
for {
  x <- w.toRight("a")
  y <- f(x).toRight("b")
  z <- g(y).toRight("c")
} yield z
```

Once someone is comfortable with `for` expressions, I think they’ve got the
Scala essentials pretty well under control.

## <a name="java-conversions"></a> Java conversions

There’s one more thing that will only come up if you’re doing Java interop.
There are several options for conversions between Java and Scala collections:

* `scala.collection.convert`
* `scala.collection.JavaConversions`
* `scala.collection.JavaConverters`

Unfortunately these names are all very similar, and the documentation doesn’t
explain how to choose.

I find this one easiest in most situations: 

```scala
import scala.collection.JavaConversions._
```

This import provides implicit conversions between the two collections libraries,
which usually works without thinking about it at all.
