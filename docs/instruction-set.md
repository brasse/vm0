# The vm0 instruction set

vm0 is a stack machine. There is a single operand stack of integers,
and three registers:

- **`sp`** stack pointer, the index of the next free slot (the live top
  is `sp - 1`).
- **`fp`** frame pointer, the base of the current call frame.
- **`pc`** program counter, the index of the instruction being run.

The only data type is the integer. There is no separate boolean: false
is `0` and true is anything non-zero, and the comparison instructions
produce `1` or `0`. Integers are host Common Lisp integers, so they are
arbitrary precision (this is why factorial of 1000 just works).

The instruction set is tiny. Everything below is defined in
[`instructions.lisp`](../instructions.lisp); the semantics of each
instruction are written in a small Lisp DSL, so that file reads like
this document with the prose removed.

## Notation

Stack effects are written with the **top of stack on the right**:

```
( before -- after )
```

So `( a b -- a b a )` means "with `b` on top and `a` below it, leave
`a`, `b`, and a copy of `a` on top".

Two addressing anchors show up in the descriptions (see the
[calling convention](./calling-convention.md) for the full story):

- **frame-relative** access is measured from `fp`: slot `fp + index`.
  Used for arguments and locals. The index may be negative (arguments
  live below `fp`).
- **stack-relative** access is measured from the top: depth `0` is the
  top, depth `1` the value under it, and so on.

Any access outside the live stack `[0, sp)` traps rather than reading
or writing a junk slot. The traps are listed at the end.

## Stack manipulation

### `:push n`
```
( -- n )
```
Push the integer literal `n` (the instruction's argument). Traps
`:stack-overflow` if the stack is full.

### `:pop`
```
( a -- )
```
Discard the top value. Traps `:stack-underflow` if the stack is empty.

### `:dup`
```
( a -- a a )
```
Copy the top value (stack-relative depth `0`).

### `:over`
```
( a b -- a b a )
```
Copy the second value from the top (stack-relative depth `1`).

### `:pick`
```
( index -- value )
```
Pop `index`, then push the value at the frame-relative slot
`fp + index`. This is how variables are read: arguments sit at negative
indices, locals at non-negative ones.

### `:set`
```
( value index -- )
```
Pop `index` and `value`, then write `value` into the frame-relative
slot `fp + index`. This is how variables are assigned.

### `:roll`
```
( x_d ... x_1 x_0 depth -- x_{d-1} ... x_0 x_d )
```
Pop `depth`, then bring the value at stack-relative depth `depth` up to
the top, shifting the values above it down by one. `(:push 1) (:roll)`
swaps the top two.

### `:iroll`
```
( x_d ... x_1 x_0 depth -- x_0 x_d ... x_1 )
```
The inverse of `:roll`. Pop `depth`, then send the top value down to
stack-relative depth `depth`, shifting the rest up by one.

## Arithmetic

All take two operands with the top of stack as the right-hand side, so
`( a b -- ... )` computes `a OP b`.

### `:add`
```
( a b -- a+b )
```

### `:sub`
```
( a b -- a-b )
```

### `:mul`
```
( a b -- a*b )
```

### `:div`
```
( a b -- a/b )
```
Integer division truncated toward zero. Traps `:divide-by-zero` if
`b` is `0`.

### `:mod`
```
( a b -- a mod b )
```
Remainder matching the truncating division above. Traps
`:divide-by-zero` if `b` is `0`.

## Comparison and logic

Each pushes `1` for true and `0` for false.

### `:eq`
```
( a b -- a=b )
```

### `:lt`
```
( a b -- a<b )
```

### `:gt`
```
( a b -- a>b )
```

### `:not`
```
( a -- r )
```
`r` is `1` if `a` is `0`, otherwise `0`.

## Control flow

Jump targets are written as labels in source (for example `:loop`); the
assembler resolves them to instruction indices before the program runs.

### `:jmp target`
```
( -- )
```
Unconditional jump to `target`.

### `:jz target`
```
( a -- )
```
Pop `a`; jump to `target` if `a` is `0`, otherwise fall through.

### `:jnz target`
```
( a -- )
```
Pop `a`; jump to `target` if `a` is non-zero, otherwise fall through.

### `:halt`
```
( -- )
```
Stop execution.

## Functions and the frame

These four implement the calling convention. The full mechanics, frame
layout, and worked traces are in the
[calling convention](./calling-convention.md) document; the summaries
here are just the per-instruction effects.

### `:call target`
```
( -- saved-fp retaddr )
```
Push the current `fp` and the return address, set `fp` to point at the
saved-`fp` slot, and jump to `target`. Arguments are expected to be on
the stack already, below these two control words.

### `:ret`
```
( saved-fp retaddr -- )
```
Pop the return address and the saved `fp` from the top, restore `fp`,
and jump back to the return address.

### `:getfp`
```
( -- fp )
```
Push the current frame pointer.

### `:setfp`
```
( n -- )
```
Pop `n` and set the frame pointer to it.

## I/O

### `:print`
```
( a -- )
```
Pop `a` and print it followed by a newline.

## Assembler directives

### `:label name`
Not a runtime instruction. It marks a position in the program so that
`:jmp`, `:jz`, `:jnz`, and `:call` can refer to it by name. The
assembler records its address and removes it before execution.

## Macros

The assembler also expands a fixed set of convenience macros into
primitive instructions. They are defined alongside the instructions in
[`instructions.lisp`](../instructions.lisp).

| Macro | Expands to | Effect |
|-------|------------|--------|
| `:swap` | `(:push 1) (:roll)` | swap the top two values |
| `:rot` | `(:push 2) (:roll)` | bring the third value to the top |
| `:inc` | `(:push 1) (:add)` | add 1 to the top |
| `:dec` | `(:push 1) (:sub)` | subtract 1 from the top |
| `:lte` | `(:gt) (:not)` | `( a b -- a<=b )` |
| `:gte` | `(:lt) (:not)` | `( a b -- a>=b )` |
| `:fn-args-1` | `(:push 2) (:iroll) (:push 2) (:iroll)` | move the saved `fp`/return address below 1 argument |
| `:fn-args-2` | `(:push 3) (:iroll) (:push 3) (:iroll)` | same, below 2 arguments |
| `:fn-args-3` | `(:push 4) (:iroll) (:push 4) (:iroll)` | same, below 3 arguments |
| `:retv` | `(:push 2) (:iroll) (:ret)` | return the top value to the caller |

The `:fn-args-N` and `:retv` macros belong to the simpler hand-written
assembly calling convention, not the one the compiler emits. See the
[calling convention](./calling-convention.md) document for that
distinction.

## Traps

When an instruction cannot proceed it returns a trap instead of
corrupting state. The driver prints `trap: (<reason>)` and stops.

- `:stack-underflow` popping from an empty stack
- `:stack-overflow` pushing onto a full stack
- `:stack-out-of-bounds` reading or writing a slot outside `[0, sp)`
- `:divide-by-zero` `:div` or `:mod` with a zero divisor
- `:unknown-instruction` the head of the instruction is not recognized
