---
layout: post
title: Javac bug report
date_granularity: day
tags: java
---

I have been cursing at Java a lot lately. Over this past week, it's because
I've been experiencing inexplicable compilation failures. I finally tracked
down the cause and submitted this bug report.

## Steps to reproduce

Create files One.java and Two.java, and run \`javac *.java\`.

{% highlight java %}
/* One.java */
package one;
import static one.Two.three;
public class One<A> { }
{% endhighlight %}

{% highlight java %}
/* Two.java */
package one;
public class Two<B> extends One<B> {
    public static Object three;
}
{% endhighlight %}

## Error message

    An exception has occurred in the compiler (1.6.0_21). Please file a bug at
    the Java Developer Connection (http://java.sun.com/webapps/bugreport) after
    checking the Bug Parade for duplicates. Include your program and the
    following diagnostic in your report. Thank you.

    java.lang.NullPointerException
      at com.sun.tools.javac.comp.Check.checkCompatibleConcretes(Check.java:1215)
      at com.sun.tools.javac.comp.Check.checkCompatibleSupertypes(Check.java:1567)
      at com.sun.tools.javac.comp.Attr.attribClassBody(Attr.java:2674)
      at com.sun.tools.javac.comp.Attr.attribClass(Attr.java:2628)
      at com.sun.tools.javac.comp.Attr.attribClass(Attr.java:2564)
      at com.sun.tools.javac.main.JavaCompiler.attribute(JavaCompiler.java:1036)
      at com.sun.tools.javac.main.JavaCompiler.compile2(JavaCompiler.java:765)
      at com.sun.tools.javac.main.JavaCompiler.compile(JavaCompiler.java:730)
      at com.sun.tools.javac.main.Main.compile(Main.java:353)
      at com.sun.tools.javac.main.Main.compile(Main.java:279)
      at com.sun.tools.javac.main.Main.compile(Main.java:270)
      at com.sun.tools.javac.Main.compile(Main.java:69)
      at com.sun.tools.javac.Main.main(Main.java:54)

## Workaround

From what I can tell, the problem is only encountered when using static
imports, and when a class has the same (case-insensitive) name as a package in
which it resides. Avoiding either of these situations is sufficient to work
around this bug.

## Addendum

I don't understand why no one else seems to be complaining about this bug,
because lately I can't seem to avoid it.

Perhaps I was too hasty in posting the bug report a few days ago, because I've
discovered now that it happens in more cases than I thought. A namespace clash
is not even necessary. You can change the package name and get the same error:

{% highlight java %}
/* One.java */
package abc;
import static abc.Two.three;
public class One<A> { }
{% endhighlight %}

{% highlight java %}
/* Two.java */
package abc;
public class Two<B> extends One<B> {
    public static Object three;
}
{% endhighlight %}

This is absurd.
