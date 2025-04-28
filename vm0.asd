(asdf:defsystem "vm0"
  :description "A small stack-based virtual machine in Common Lisp"
  :author "Mattias Brändström"
  :license "MIT"
  :depends-on ()
  :serial t
  :components ((:file "package")
               (:file "defvm-macros")
               (:file "instructions")
               (:file "assembler")
               (:file "compiler")
               (:file "vm")
               (:file "examples/examples-asm")
               (:file "examples/examples-stak")))

(asdf:defsystem "vm0/tests"
  :depends-on ("vm0" "fiveam" "split-sequence")
  :serial t
  :components ((:file "tests/package")
               (:file "tests/util")
               (:file "tests/examples")
               (:file "tests/core-instructions")
               (:file "tests/stack-frames")
               (:file "tests/macros")))
