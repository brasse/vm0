# vm0

**vm0** - a tiny Lisp-powered stack machine that compiles a real
language from scratch.

**vm0** is a tiny stack-based virtual machine with big dreams. And it
actually pulled them off. It runs code. It prints numbers. It handles
recursion. It even does tail calls (most of the time).

At its core, vm0 is a minimal stack machine: no heap, no types, no
objects. Just a disciplined stack and a Lisp-powered brain. The
instruction set is tiny, but the assembler supports macros to make raw
stack programs slightly less painful to write (and read).

Built on top is Stak, a small Lisp-like language that compiles down to
vm0. It supports `let`, `set`, `if`, `while`, function definitions
and proper tail call optimization. You write in Stak like a normal
language. Under the hood, it carefully rewrites and lowers your code
into stack instructions using pick, roll, and a DIY calling convention
made of duct tape and determination.

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
