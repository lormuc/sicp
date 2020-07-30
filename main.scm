(load "prelude.scm")
(load "syntax.scm")
(load "register-simulator.scm")
(load "ec-eval-support.scm")
(load "ec-eval.scm")
(load "compiler.scm")
(load "tests.scm")

(set! log? #t)

(define machine-operations
  (list
   (list 'true? true?)
   (list 'make-procedure make-procedure)
   (list 'compound-procedure? compound-procedure?)
   (list 'procedure-parameters procedure-parameters)
   (list 'procedure-body procedure-body)
   (list 'procedure-environment procedure-environment)
   (list 'extend-environment extend-environment)
   (list 'lookup-variable-value lookup-variable-value)
   (list 'set-variable-value! set-variable-value!)
   (list 'define-variable! define-variable!)
   (list 'primitive-procedure? primitive-procedure?)
   (list 'apply-primitive-procedure apply-primitive-procedure)
   (list 'empty-arglist empty-arglist)
   (list 'adjoin-arg adjoin-arg)
   (list 'last-operand? last-operand?)

   (list 'null? null?)
   (list 'car car)
   (list 'cdr cdr)
   (list 'list list)
   (list 'cons cons)

   (list 'make-compiled-procedure make-compiled-procedure)
   (list 'compiled-procedure-env compiled-procedure-env)
   (list 'compiled-procedure-entry compiled-procedure-entry)
   (list 'false? false?)))

(let ((machine
       (make-machine
        '(exp env val proc argl continue unev)
        machine-operations
        (statements
         (compile
          '(begin (define (factorial n)
                    (if (= n 1)
                        1
                        (* (factorial (- n 1)) n)))
                  (factorial 3))
          'val
          'next
          (make-make-label))))))
  (set-register-contents! machine 'env (setup-environment))
  (start machine)
  (get-register-contents machine 'val))

(for-each
 log-line
 (statements
  (compile
   '(define f
      (lambda (x)
        (+ x (g (+ x 2)))))
   'val
   'next
   (make-make-label))))
