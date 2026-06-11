# The Stak calling convention

This is how the Stak compiler turns function calls into vm0 stack
instructions. It is, in the words of the README, made of duct tape and
determination, but it is also completely regular once you see the
frame layout. Everything below was traced out of the running VM, not
guessed.

There are actually two calling conventions in vm0: this one (what the
compiler emits) and a simpler hand-rolled one used in the assembly
examples (`:fn-args-N` / `:retv`). This document is only about the
compiler's.

## Two things called "offset"

Keep these separate or nothing will make sense:

- **`fp`** and **`sp`** are runtime registers, both indices into the
  stack array: `fp` marks the base of the current frame, `sp` the next
  free slot at the top. The VM addresses variables relative to `fp`.
- **`offset`** is a compile-time number the compiler threads through every
  `compile-*` function. It is the height of the working stack *above `fp`*,
  i.e. the index of the next free slot relative to `fp`.  At runtime that slot
  is `sp`, so `offset = sp - fp`; the compiler tracks `offset` statically.  The
  golden rule of the compiler is that **every expression pushes exactly one net
  value**, so compiling an expression always advances `offset`
  by 1. `ensure-one-value` enforces this everywhere.

The registers are what the machine uses at runtime; `offset` is how the
compiler keeps track of where everything will be so it can emit the
right `:pick`/`:roll` depths.

## The frame

When the caller invokes a function it first pushes the arguments, then
the argument count, then executes `:call`. The `:call` instruction
(see [`instructions.lisp`](../instructions.lisp)) pushes the current
frame pointer and the return address, and sets `fp` to point at the
saved frame pointer.

For a call to a two-argument function, the frame looks like this
(`fp` shown on the left; stack index increases downward, so the top of
stack is at the bottom):

```
index       contents
-----       --------
fp-3        arg0
fp-2        arg1
fp-1        n            argument count (here 2)
fp+0  <-fp  saved fp     pushed by :call
fp+1        retaddr      pushed by :call
fp+2        ...          working area: locals and temporaries
```

In general, for an `n`-argument function:

- `arg0 .. arg(n-1)` live at `fp-(n+1) .. fp-2`
- the argument count `n` lives at `fp-1`
- the saved `fp` is at `fp+0`, the return address at `fp+1`
- the body's working stack starts at `fp+2`

That `fp+2` is why `compile-fn` starts the body with `offset = 2`.

## Reading and writing variables

A variable reference compiles to `(:push depth) (:pick)`, which copies
`stack[fp + depth]` to the top of the stack. The depth comes from
`get-depth`, which adds the variable's index within its frame to the
frame's base offset.

- **Arguments** are given negative indices. `compile-fn` creates the
  function's frame with `start-var-index = -(n+1)`, so `arg0` resolves
  to depth `-(n+1)`, `arg1` to `-n`, and so on. Those are exactly the
  `fp-(n+1) ...` slots above.
- **Locals** (from `let`) are given non-negative indices starting at
  the `offset` in effect when the `let` was entered, so they land in
  the working area at `fp+2` and up.

Assignment (`set`) compiles to `(:push depth) (:set)`, writing the top
of stack back into the variable's slot.

## A normal call, start to finish

Take `(add 7 5)` where `add` is `(fn add (x y) (+ x y))`. Traced:

```
(PUSH 7)         #(7)               caller pushes arg0
(PUSH 5)         #(7 5)             caller pushes arg1
(PUSH 2)         #(7 5 2)           caller pushes argument count
(CALL 17)  fp=3  #(7 5 2 0 4)       :call pushes saved-fp(0) and retaddr(4); fp=3
(PUSH -3)(PICK)  #(7 5 2 0 4 7)     read x  (fp-3)
(PUSH -2)(PICK)  #(7 5 2 0 4 7 5)   read y  (fp-2)
(ADD)            #(7 5 2 0 4 12)    body result sits at fp+2
```

The body is done and has left exactly one value (`12`) at `fp+2`. Now
the **epilogue** (emitted by `compile-fn`) rotates that return value
*below* the saved `fp` and return address so that `:ret` can consume
them and leave the value behind:

```
(PUSH 2)(ROLL)   #(7 5 2 4 12 0)
(PUSH 2)(ROLL)   #(7 5 2 12 0 4)    now: ... v saved-fp retaddr
(RET)      fp=0  #(7 5 2 12)        :ret pops retaddr+fp, restores fp, jumps back
```

Control is back in the caller, which now sees its original args plus
the return value on top: `[arg0 arg1 n v]`. The caller's **cleanup**
(emitted by `normal-call`) uses the argument count `n` as a loop
counter to pop all the arguments, leaving just the return value:

```
(PUSH 1)(ROLL)   #(7 5 12 2)        bring n above v
... loop n times, popping one arg each pass ...
                 #(7 12 2) -> #(12 1) -> ...
(POP)            #(12)              only the return value remains
```

Net effect on the working stack: the arguments are replaced by a
single return value, so `offset` goes up by exactly 1, just like every
other expression. `compile-call` returns `(1+ offset)` accordingly.

## Tail calls

In tail position the compiler emits `tail-call` instead of
`normal-call`. The idea: instead of pushing a fresh frame on top of
the current one (which would grow the stack on every recursive call),
**reuse the current frame in place**. The new arguments overwrite the
old ones, `fp` is adjusted, and we `:jmp` straight to the function
label without ever pushing a new return address or running `:ret`.

The fiddly part is that the new argument count may differ from the
old, so the frame can shift. `tail-call` recomputes the frame pointer
as:

```
new-fp = current-fp - old-arg-count + new-arg-count
```

and writes it with `:setfp` before jumping. (For self-recursion the
arity is the same, so `new-fp = current-fp` and the frame stays
exactly where it is.)

You can watch it work with `(fn cd (n) (if (= n 0) 99 (cd (- n 1))))`:
across every recursive call, `fp` stays put and the stack never grows
beyond the single frame. That is the whole point. A non-tail recursion
would stack a new frame each time; this one runs in constant space.

## The known limitation: no TCO inside `let`

Tail-call optimization only fires for calls in true tail position as
the compiler sees it. A call that is syntactically the last thing in a
`let` body is **not** optimized, because the `let` still has locals on
the working stack that have to be cleaned up after the call returns,
so the call cannot simply reuse the frame and jump away.

This is why the example file has a program named
`+does-not-break-horribly+`: it confirms that such a call still
produces the correct answer (via a normal call), it just does not get
the constant-space treatment. Fixing it properly is hard and out of
scope for the project.

## Where this lives in the code

- [`compiler.lisp`](../compiler.lisp): `compile-fn` (frame setup +
  epilogue), `normal-call`, `tail-call`, `compile-call`,
  `compile-arg-list`, `get-depth`, and the `:symbol` / `:set` rules.
- [`instructions.lisp`](../instructions.lisp): `:call`, `:ret`,
  `:getfp`, `:setfp`, `:pick`, `:set`, `:roll`, `:iroll`.
