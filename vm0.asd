(asdf:defsystem "vm0"
  :description "A small stack-based virtual machine in Common Lisp"
  :author "Mattias Brändström"
  :license "MIT"
  :depends-on ()
  :serial t
  :components ((:file "package")
               (:file "macros")
               (:file "instructions")
               (:file "assembler")
               (:file "vm")
               (:file "programs")))

(asdf:defsystem "vm0/tests"
  :depends-on ("vm0" "fiveam")
  :components ((:file "tests")))
