# ImpFun - INF222 interpreter extension project (A8)

## Overview

### Tasks solved:
- 4.2
- 4.3
  - NOTE: Both try and catch expect a following block, not just any statement
- 4.4
- 4.5
- 4.6

### Not solved:

I have not attempted to solve task 4.7 due to time constraints and the
realization that this might be a greater endeavor than I'm prepared to deal with
on the last day before the deadline.

## Caveats

### Threads are weird

I've implemented threads something like 3 times, finally opting to let each
thread keep track of its own TID and its parent's, which allows me to handle one
thread at a time in the steps function while keeping a reference to all of them
at hand. The TID of the main thread is 0, and each new TID is set as one more
than the maximum of all currently existing TIDs.  Detached threads are currently
not really treated differently from regular child threads - you can still join
with a detached thread. Adding an error message if the programmer tries, however,
would be trivial.

The only real difference between detached threads and normal threads is actually
that they have no parent TID. This was an elaborate decision which allows me to
differentiate between them and other threads in the future - but I could also
let them not have a tid, and instead let them be the children of the main
thread. This would also mean that the main thread would either have to have a
special TID, or that both the TID and parent TID fields would be Maybe values,
which I wanted to avoid. However, I could have gone a different route, and the
complexity of both are probably comparable, but it's possible that the other
route would have followed the metaphor of detached thread being considered the
children of the main thread more clearly.

There is nothing that stops threads from mutually waiting for each other, but it
wouldn't be hard to add a check for that.

### Continuations are weird

I spent way too long ripping my hair out over continuations, before it finally
loosened up a little bit. I haven't really tested it a lot, because I'm afraid
I'll find a problem with it that I won't be able to fix, but it works for the
example program provided.

Ideally I'd like to only consume a single context item every step, but by the
time I figured out how to solve the final problem of `return k` in the example
program, I was kind of exhausted and just wanted to be done with it, so I
skipped ahead and dropped a bunch of context items all the way down to the final
argument of the continuation.

### New features with undefined behavior

I *think* the added features should work as expected when they are used as
they're supposed to be. I *also* think they will not work as expected (or in any
way reasonable) if they're used in a different way from the one way they're
supposed to be used. I should probably have written more tests for these
eventualities, and added error output for whenever unexpected behavior arises,
but there would be no telling what stuff I might break. The tests I have written
thus far all work, and I expect the features to work as intented if properly
employed by the programmer, but I also expect them to fail in a big way if they
are not.

## Bugfixes(?)

Fixed the != bug mentioned in the announcement. Also added unary negation, since
there is obviously support for negative values in the language (as an example,
try evaluating 0-1), so it would be silly not to support negative literals as
well, or to allow negating a positive expression.

## Language extensions

### String concatenation

I made it so that the `+` operator is treated as string concatenation between any
two values that are not both integers.

NOTE: This also means that two non-strings which are added will be type-coerced
to strings as long as they are not integers. A potential improvement would be to
check that at least one of the values are strings, and otherwise give the user
an error (if, for example, attempting to add two closures).

It's also relatively easy to extend this behavior to work differently for
different operands, which could be desirable - for instance if implementing a
list type.

### Optional finally clause in try-catch

Made it so that the try-catch statement accepts an optional "finally" clause
that is run after the try/catch statements no matter whether an exception is
thrown or not. If there is no finally clause specified, the finally clause
defaults to a SSkip statement.

### Optional else clause in if-else statements

I made the "else" part of the if/else statements optional. Else statements that
are missing default to SSkip statements.

### Break and continue in loops

Implemented the break and continue statements in loops. Break will unwind
everything until it encounters a while statement, then drops the while from the
context; continue will unwind until it encounters the while statement and then
evaluates the statement. (In practice, the while statements are arguments to a
sequence statement.)

NOTE: Results in undefined behavior if used outside of a loop.

## Planned features (dropped for this release due to limited time)

### Function composition

Composition of functions using the . (dot) operator. If `f` and `g` are functions,
`f . g` is a function `h(...)` such that it returns returns `f(g(...))`. This
should not be too hard to implement.

### Include statement

I wanted to let files include other files, and then behave as if the included file
was executed first. This would probably be a larger and more complex feature
than I'd be able to implement in a short amount of time.

### Objects (or records)

Basic object syntax:

```
var record = {
  // constants and modifiable variables!
  var fieldA = 10;
  var fieldB = ref 20;

  // methods!
  var func = fun(x) {
    fieldB = *fieldB - x; // local field access without fully qualified name!
    return fieldA + fieldB;
  };

  // nested records!
  var subrecord = {
    var a = 10;
  }
}

// access syntax:
record.func(record.subrecord.a);
```

A simplified version of this might be easily accomplishable, but I'm still too
constrained by time to really go for it.

### Lists (or arrays)

Basic list syntax:

```
var x = [10, fun() {return 5;}, true]; // No uniform type enforced
println(x[1]()); // incices are 0-based
```

Since this was just something I thought up offhandedly just now, I haven't really
considered how complicated it would be, but I'm guessing it would be less
complicated than objects, yet probably more complicated than, say, function
composition (which should be easy enough).

### Incrementation/decrementation and op-gets operators

I would have liked variable incrementation and decrementation of references
(with the `++` and `--` operators) for both post-incrementation and
pre-incrementation. Similarly, I'd like to have op-gets operators like for
instance `+=` or `*=`, which I consider to be in the same vein. However, as far
as I can tell, variable assignment does not evaluate to a value, and I would
either have to change this fundamental behavior of the language, or work around
it. For this reason, I decided not to implement these features.

### For loop syntax

It should be achievable to create for loops (with normal for loop syntax) in
terms of the while loops that already exists. It would probably be a
surprisingly easy thing to implement, and it would probably take me a lot of
time trying to find and fix the simple little error causing the problem.
