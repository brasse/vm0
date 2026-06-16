# vm0

**vm0** - a tiny Lisp-powered stack machine that compiles a real
language from scratch.

**vm0** is a tiny stack-based virtual machine with big dreams. And it
actually pulled them off. It runs code. It prints numbers. It handles
recursion. It even does tail calls (some of the time).

At its core, vm0 is a minimal stack machine: no heap, no types, no
objects. Just a disciplined stack, with the whole thing (machine,
assembler and compiler) written in Common Lisp. The [instruction
set](./docs/instruction-set.md) is tiny yet capable, and the semantics
of each instruction are written in a little [Lisp
DSL](./instructions.lisp), so the whole thing fits on one screen. And when something goes wrong, like a stack
underflow, a division by zero, or a bogus instruction, it traps
cleanly instead of taking the whole process down with it.

Built on top is Stak, a small Lisp-like language that compiles down to
vm0. It supports `let`, `set`, `if`, `while`, `progn`, function
definitions and proper tail call optimization, plus the usual
arithmetic, comparisons, `not`, and short-circuiting `and`/`or`
(lowered by a little AST-rewriting pass before compilation). You write
in Stak like a normal language. Under the hood, it carefully rewrites
and lowers your code onto the VM's humble instruction set, wiring it
all together with a [DIY calling
convention](./docs/calling-convention.md) made of duct tape and
determination.

It’s simple, self-contained, and fun! Like a tiny CPU emulator that
learned just enough Lisp to build a real language.

# Stak

This is real Stak code:

```lisp
(fn gcd (a b)
     (if (= b 0)
         a
         (gcd b (% a b))))
(print (gcd 48 18))
```

If you squint this might look like Lisp or Scheme, but it isn't. It's
Stak. And it compiles to this beatiful mess:

```
(:PUSH 48) (:PUSH 18) (:PUSH 2) (:CALL :|fn-GCD|) (:PUSH 1) (:ROLL)
(:LABEL :|pop-start-295|) (:DUP) (:JZ :|pop-end-296|) (:PUSH 2) (:ROLL) (:POP)
(:DEC) (:JMP :|pop-start-295|) (:LABEL :|pop-end-296|) (:POP) (:PRINT) (:HALT)
(:LABEL :|fn-GCD|) (:PUSH -2) (:PICK) (:PUSH 0) (:EQ) (:JZ :|else-293|)
(:PUSH -3) (:PICK) (:JMP :|end-294|) (:LABEL :|else-293|) (:PUSH -2) (:PICK)
(:PUSH -3) (:PICK) (:PUSH -2) (:PICK) (:MOD) (:PUSH 4) (:ROLL) (:DUP)
(:LABEL :|pop-old-start-291|) (:DUP) (:JZ :|pop-old-end-292|) (:PUSH 2)
(:PUSH 4) (:ADD) (:ROLL) (:POP) (:DEC) (:JMP :|pop-old-start-291|)
(:LABEL :|pop-old-end-292|) (:POP) (:PUSH 4) (:ROLL) (:PUSH 4) (:ROLL)
(:PUSH 2) (:ROLL) (:GETFP) (:SWAP) (:SUB) (:PUSH 2) (:ADD) (:SETFP) (:PUSH 2)
(:PUSH 2) (:ROLL) (:PUSH 2) (:ROLL) (:JMP :|fn-GCD|) (:LABEL :|end-294|)
(:PUSH 2) (:ROLL) (:PUSH 2) (:ROLL) (:RET)
```

There's tail call optimization in there and it works! Look at the
`(:JMP :|fn-GCD|)` instruction. That's the tail call to `gcd`. Pretty
cool!

## Running it

vm0 is Common Lisp, built with ASDF. You'll need a Lisp to run it
(SBCL via [Roswell](https://github.com/roswell/roswell) works nicely):

```lisp
;; load it
(asdf:load-system :vm0)
(in-package :vm0)

;; run raw stack assembly
(assemble-and-run :program +factorial-asm+)   ; => 120

;; run Stak: compile -> expand macros -> assemble -> run, in one call
(compile-and-run +gcd+)                        ; => 6
```

Both helpers take a `:trace t` keyword if you want to watch the stack
evolve instruction by instruction.

To run the tests (needs `fiveam` and `split-sequence`, both on
Quicklisp):

```lisp
(asdf:load-system :vm0/tests)
(fiveam:run-all-tests)
```

## More examples

Poke around the `examples/` directory for more:

- [`examples/examples-stak.lisp`](./examples/examples-stak.lisp): Stak
  programs. Factorial (of *1000*, thanks to bignums and tail calls),
  Fibonacci, `gcd`, mutually-recursive even/odd, and a counting loop.
- [`examples/examples-asm.lisp`](./examples/examples-asm.lisp):
  hand-written stack assembly for the brave.

## License

This project is licensed under the MIT License. See the
[LICENSE](./LICENSE) file for details.
