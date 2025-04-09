(defpackage :vm0/examples
  (:use :cl)
  (:export :+factorial+
           :+fibonacci+
           :+sum-0-n+
           :+max-a-b+
           :+function-return-void+
           :+function-return-int+))

(in-package :vm0/examples)

(defparameter +factorial+
  '((:push 1)                           ; product
    (:push 5)                           ; counter, factorial to compute

    (:label :loop)
    (:dup)                              ; copy counter for check
    (:push 1)
    (:eq)
    (:jnz :done)

    (:dup)                              ; copy counter
    (:rot)                              ; now: counter product counter
    (:mul)                              ; counter * product
    (:swap)                             ; product result under counter
    (:dec)                              ; dec counter
    (:jmp :loop)

    (:label :done)
    (:pop)                              ; discard counter
    (:print)
    (:halt)))

(defparameter +fibonacci+
  '((:push 0)                           ; a fib(n - 2)
    (:push 1)                           ; b fib(n - 1)
    (:push 10)                          ; n

    (:label :loop)
    (:dup)                              ; n
    (:push 0)
    (:eq)
    (:jnz :done)

    ;; next = a + b
    (:push 2) (:pick)                   ; a
    (:push 2) (:pick)                   ; b
    (:add)                              ; next

    ;; update a and b
    (:push 3) (:roll)                   ; b → a
    (:push 2) (:roll)                   ; next → b

    (:dec)                              ; n = n - 1

    ;; clean up old a
    (:swap)                             ; bring a to TOS
    (:pop)                              ; discard it

    (:jmp :loop)

    (:label :done)
    (:pop)                              ; discard n
    (:print)                            ; print b
    (:halt)))

(defparameter +sum-0-n+
  '((:push 0)                           ; sum = 0
    (:push 5)                           ; n = 5

    (:label :loop)
    (:dup)                              ; copy n
    (:push 0)
    (:eq)
    (:jnz :done)

    (:over)                             ; copy sum
    (:over)                             ; copy n
    (:add)                              ; sum + n
    (:rot)                              ; n result sum
    (:pop)                              ; drop old sum
    (:swap)                             ; result n
    (:dec)                              ; n = n - 1

    (:jmp :loop)

    (:label :done)
    (:pop)                              ; remove counter
    (:print)                            ; print sum
    (:halt)))

(defparameter +max-a-b+
  '((:push 10)                          ; a
    (:push 42)                          ; b

    (:over)
    (:over)
    (:gt)
    (:jnz :a-is-bigger)

    (:swap)

    (:label :a-is-bigger)
    (:pop)

    (:label :done)
    (:print)
    (:halt)))

(defparameter +function-return-void+
  '((:push 10)
    (:push 42)
    (:call :func)

    (:push 5)
    (:push 6)
    (:call :func)
    (:halt)

    (:label :func)
    (:push 2)
    (:roll)
    (:push 2)
    (:roll)                   ; move return address below args
    (:add)
    (:print)
    (:ret)))

(defparameter +function-return-int+
  '((:push 2)
    (:push 3)
    (:call :func)

    (:push 60)
    (:push 40)
    (:call :func)

    (:call :func)              ; TOS = (2 + 5) + (60 + 40) = 105

    (:print)

    (:halt)

    (:label :func)
    (:push 2)
    (:roll)
    (:push 2)
    (:roll)                   ; move return address below args
    (:add)
    (:swap)                   ; move return value below return address
    (:ret)))
