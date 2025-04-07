(in-package :vm0)

;;; factorial
(assemble-and-run
 :program '((:push 1)                      ; product
            (:push 5)                      ; counter, factorial to compute

            (:label :loop)
            (:dup)                         ; copy counter for check
            (:push 1)
            (:eq)
            (:jnz :done)

            (:dup)                         ; copy counter
            (:rot)                         ; now: counter product counter
            (:mul)                         ; counter * product
            (:swap)                        ; product result under counter
            (:dec)                         ; dec counter
            (:jmp :loop)

            (:label :done)
            (:pop)                         ; discard counter
            (:print)
            (:halt)))

;;; fibonacci
(assemble-and-run
 :program '((:push 0)                            ; a fib(n - 2)
            (:push 1)                            ; b fib(n - 1)
            (:push 10)                           ; n

            (:label :loop)
            (:dup)                               ; n
            (:push 0)
            (:eq)
            (:jnz :done)

            ;; next = a + b
            (:push 2) (:pick)                    ; a
            (:push 2) (:pick)                    ; b
            (:add)                               ; next

            ;; update a and b
            (:push 3) (:roll)                    ; b → a
            (:push 2) (:roll)                    ; next → b

            (:dec)                               ; n = n - 1

            ;; clean up old a
            (:swap)                              ; bring a to TOS
            (:pop)                               ; discard it

            (:jmp :loop)

            (:label :done)
            (:pop)                               ; discard n
            (:print)                             ; print b
            (:halt)))

;;; sum [0 .. n]
(assemble-and-run
 :program '((:push 0)                            ; sum = 0
            (:push 5)                            ; n = 5

            (:label :loop)
            (:dup)                               ; copy n
            (:push 0)
            (:eq)
            (:jnz :done)

            (:over)                              ; copy sum
            (:over)                              ; copy n
            (:add)                               ; sum + n
            (:rot)                               ; n result sum
            (:pop)                               ; drop old sum
            (:swap)                              ; result n
            (:dec)                               ; n = n - 1

            (:jmp :loop)

            (:label :done)
            (:pop)                               ; remove counter
            (:print)                             ; print sum
            (:halt)))

;;; max(a, b)
(assemble-and-run
 :program '((:push 72)                   ; a
            (:push 42)                   ; b

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
