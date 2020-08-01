(load "prelude.scm")
(load "syntax.scm")
(load "register-simulator.scm")
(load "ec-eval-support.scm")
(load "ec-eval.scm")
(load "compiler.scm")
(load "tests.scm")

(set! log? #t)

(define (display-code code)
  (for-each (lambda (stmt)
              (display stmt)
              (newline))
            (statements code)))

(define code
  '(define (factorial n)
     (if (= n 1)
         1
         (* n (factorial (- n 1))))))
