# vm0

**vm0** is a tiny stack-based virtual machine with big dreams. And it
actually pulled them off. It runs code. It prints numbers. It handles
recursion. It even does tail calls (most of the time).

At its core, vm0 is a minimal stack machine: no heap, no types, no
objects. Just a disciplined stack and a Lisp-powered brain. The
instruction set is tiny, and the assembler supports macros to make
writing raw programs slightly less painful.

Built on top is Stak, a small Lisp-like language that compiles down to
vm0. It supports `let`, `set`, `if`, `while`, function definitions,
and proper tail call optimization. You write in Stak like a normal
language. Under the hood, it carefully rewrites and lowers your code
into stack instructions using pick, roll, and a DIY calling convention
made of duct tape and determination.

It’s simple, self-contained, and fun! Like a tiny CPU emulator that
learned just enough Lisp to build a real language.
