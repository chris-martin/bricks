--------------------------------------------------------------------------------
title:     Null as a sort of bottom
date:      2017 Apr 29
slug:      null-as-bottom
abstract:  When bottoms are tagged, we may view bottom more inclusively.
           This could reasonably include null.
--------------------------------------------------------------------------------

> “It uses the faculty of the imagination. But that does not mean *making things
> up*. It is a form of seeing.”
>
> “Not *real* traveling, then,” said Lyra, “Just pretend...”
>
> “No,” said Xaphania, “nothing like pretend. Pretending is easy. This way is
> hard, but much truer.”
>
> — Philip Pullman, *The Amber Spyglass*

I struggle to talk about programming languages that aren’t based in some
mathematical formalism. I have to choose ways of reasoning about them. I find
it’s often helpful to choose to mentally declare null to be a kind of bottom
when reasoning about languages that don’t provide their own sufficient system of
reason.

I’m aware that this is blasphemy. The official Haskell party line goes something
like this:

> *Novice*: “So is *bottom* sort of like *null*?”
>
> *Master*: “No, they couldn’t be more different! Null is a value; bottom is
  not. There is one null value; there are no bottom values.”

## The real bottom

Here are some bona fide exemplary bottoms.

Haskell:

```haskell
bottom :: forall a. a
bottom =
  let x = x
  in  x
```

Scala:

```scala
def bottom[A]: A = {
  while (true) {}
  ???
}
```

Non-terminating evaluation. The infinite loop. That possibility lurking in all
Turing-complete systems, that exciting consequence of the undecidability of the
*Entscheidungsproblem*. It is a fascinating and irksome reality. If you
understand what this is and why it must be, be proud of your knowledge. But keep
going.

### The real bottom is annoying.

Here are some partial functions, defined using the bottoms I showed above.

Haskell:

```haskell
fromJust :: forall a. Maybe a -> a
fromJust =
  \case
    Just a  -> a
    Nothing -> let x = x
               in  x
```

Scala:

```scala
def fromSome[A](o: Option[A]): A =
  o match {
    case Some(a) => a
    case None    => while (true) {}
                    ???
  }
```

This approach works, but it is a harsh way to treat the poor evaluator. Remember
there’s no general way to know that you’ve hit nonterminating recursion. So all
we can do with `fromJust Nothing` when executing this sort of program is spin
indefinitely trying to evaluate it.

## Tagged bottoms

Once you’re in a situation with nonterminating recursion, it’s impossible to
either finish evaluating or know that you’ll never finish evaluating. It’s a
dreadful state, so it seems unnecessarily cruel to ever *intentionally* produce
it for the evaluator to either puzzle out or chew on forever. If we *know* that
an expression will be bottom, let’s state that explicitly in our code.

```haskell
fromJust =
  \case
    Just a  -> a
    Nothing -> undefined
```

`fromJust Nothing` is still bottom — it’s still an expression of type `a` that
does not evaluate to any value of type `a` — but we’ve added a little metadata
that lets us fail quickly when attempting to evaluate it.

We can go further than that, though, and provide not only a proclamation of
bottomness, but an *explanation* for *why* the expression is bottom.

```haskell
fromJust =
  \case
    Just a  -> a
    Nothing -> error "fromJust Nothing"
```

Don’t be fooled: the expression `fromJust Nothing` is still bottom. But when
that expression is evaluated, the fact that it is bottom is *known*, and that
fact *comes with some additional information*.

“Tagged bottom” is a term I just made up, for lack of a better phrase. It refers
to *the result of determining that an expression cannot be evaluated*, including
some justification if available, like “this expression can’t be evaluated
because it includes a division by zero”. In the case of `undefined`, the tag
merely asserts that the expression *is* bottom.

*Bottom is nothing. It has no values. It is the epitome of emptiness.* — These
are half truths. In Haskell, bottoms can carry information, and we can even
inspect them (only in IO):

```haskell
try   :: Exception e => IO a -> IO (Either e a)
catch :: Exception e => IO a -> (e -> IO a) -> IO a
```

## Non-null reasoning is morally correct

Officially, `null` is a value that inhabits all `Nullable` types in Scala (which
is most of them).

But we do not operate in this mindset on a regular basis. Like bottom, we ignore
it, and allow it to propagate upward — often implicitly converted to
another form of bottom:

```scala
throw new NullPointerException()
```

Consider this Scala function that adds one to an integer:

```scala
def succ1(x: BigInt): BigInt =
  x + 1
```

Technically, this function is partial. Because when `x` is `null`, it throws
`NullPointerException`.

You could diligently handle nulls throughout your code:

```scala
def succ2(x: BigInt): BigInt =
  if (x == null) null
  else           x + 1
```

Or you can, as we did in the first version, choose to interpret null as a bottom
value — not an inhabitant of any type, but merely one of the many forms of
tagged bottom. In that case, we do not have to consider `succ1` to be a partial
function, because `null` no longer belongs to its domain.

When we choose *not* to ignore null — when we check `if (x == null)` or use
`try`/`catch` — we are writing meta-code that reasons about whether our program
has been reasoning correctly.

## The dark path

From a software design perspective, bottoms are characterized by their use as a
*last resort*, when writing a total function is not possible or would be
particularly inconvenient.

Sometimes the temptation to use bottom tags in unextraordinary circumstances
leads us to dark places. We can do this by overusing `throw` and `catch` in
Haskell, and we can do this by overusing `null` as idiomatic Java does.

I do not believe null was Java’s billion-dollar mistake. It is no more
inherently harmful than `???` in Scala or `undefined` in Haskell. The
billion-dollar mistakes were more serious fundamental flaws — notably, its
omission of sum types and pattern matching — that drive Java’s users to abuse
tagged bottoms for lack of sufficient means to express themselves properly
within the normal confines of the type system. We can see from Scala that, with
this limitation removed, programmers typically choose to relegate null-tagged
bottom to a footnote along with the rest of the bottoms.
