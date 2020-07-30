(load "prelude.scm")
(load "syntax.scm")
(load "register-simulator.scm")
(load "ec-eval-support.scm")
(load "ec-eval.scm")
(load "compiler.scm")
;; (load "tests.scm")

(set! log? #t)

(for-each
 log-line
 (statements
  (compile '(+ 1 2) 'val 'next (make-make-label))))
