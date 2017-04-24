--------------------------------------------------------------------------------
title:     What is a leak?
date:      2017 Apr 24
slug:      what-is-a-leak
abstract:  “Leak” is one of those technical words we often forget to explain.
--------------------------------------------------------------------------------

“Leak” is one of those technical words we often forget to explain.

A *leak* refers to something that consumes some resource in a way that you can't
get it back, like water leaking out of a hole in a bucket and falling onto the
ground.

Regarding a *memory leak*, it's natural to ask “Where does the memory go?”
There's no good answer to that question, because the analogy just doesn't extend
that far.

Memory is a sort of renewable resource. We use some of it every time we make a
thunk, and it gets released when we're done with it. You take a little water out
of the bucket, use it for a while, then the garbage collector puts it back.

If your program is accidentally written such that it continues to hold
references to things it doesn't need anymore, then those things don't get
garbage collected, and you continually consume more and more memory.

Or in other words — if you look at it from the perspective that you only have a
finite amount of memory in your computer — you continually *lose* memory. More
precisely, you lose *available* memory — free space in which to make more thunks
and whatnot. If that program runs long enough, this bucket will eventually end
up empty.

We also talk about leaking other resources, like *file handles*. The OS keeps
track of all the files that all the processes have open, and there's only some
finite number of files your OS can keep open at one time. This is in part
because keeping a file open requires keeping some information about it in
memory, so in some sense a file handle leak is just another kind of memory leak.

So don't take the “leaking” metaphor too seriously; it just refers to anything
that unnecessarily causes the dwindling availability of some limited resource.
