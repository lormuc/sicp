(import trace)
(import (chicken random))
(load "prelude.scm")

;;; begin metacircular evaluator

(define apply-in-underlying-scheme apply)

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "unknown expression type -- eval" exp))))

(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           arguments
           (procedure-environment procedure))))
        (else
         (error
          "unknown procedure type -- apply" procedure))))


(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)

;;;section 4.1.2

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (variable? exp) (symbol? exp))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))


(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))

(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))


(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))


(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))


(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))


(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "else clause isn't last -- cond->if"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

;;;section 4.1.3

(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))


(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))


(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))


(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "too many arguments supplied" vars vals)
          (error "too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "unbound variable -- set!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

;;;section 4.1.4

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

;;; end metacircular evaluator

;;; begin amb evaluator

(define (amb? exp) (tagged-list? exp 'amb))
(define (amb-choices exp) (cdr exp))

(define (ramb? exp) (tagged-list? exp 'ramb))
(define (permanent-assignment? exp)
  (tagged-list? exp 'permanent-set!))

;; analyze from 4.1.6, with clause from 4.3.3 added
;; and also support for let
(define (analyze exp)
  (cond ((self-evaluating? exp)
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((let? exp) (analyze (let->combination exp))) ;**
        ((amb? exp) (analyze-amb exp))                ;**
        ((ramb? exp) (analyze-ramb exp))
        ((permanent-assignment? exp)
         (analyze-permanent-assignment exp))
        ((application? exp) (analyze-application exp))
        (else
         (error "unknown expression type -- analyze" exp))))

(define (amb-eval exp env succeed fail)
  ((analyze exp) env succeed fail))

;;;simple expressions

(define (analyze-self-evaluating exp)
  (lambda (env succeed fail)
    (succeed exp fail)))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env succeed fail)
      (succeed qval fail))))

(define (analyze-variable exp)
  (lambda (env succeed fail)
    (succeed (lookup-variable-value exp env)
             fail)))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env succeed fail)
      (succeed (make-procedure vars bproc env)
               fail))))

;;;conditionals and sequences

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env succeed fail)
      (pproc env
             ;; success continuation for evaluating the predicate
             ;; to obtain pred-value
             (lambda (pred-value fail-2)
               (if (true? pred-value)
                   (cproc env succeed fail-2)
                   (aproc env succeed fail-2)))
             ;; failure continuation for evaluating the predicate
             fail))))

(define (analyze-sequence exps)
  (define (sequentially a b)
    (lambda (env succeed fail)
      (a env
         ;; success continuation for calling a
         (lambda (a-value fail-2)
           (b env succeed fail-2))
         ;; failure continuation for calling a
         fail)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "empty sequence -- analyze"))
    (loop (car procs) (cdr procs))))

;;;definitions and assignments

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail-2)
               (define-variable! var val env)
               (succeed 'ok fail-2))
             fail))))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail-2)       ; *1*
               (let ((old-value
                      (lookup-variable-value var env)))
                 (set-variable-value! var val env)
                 (succeed 'ok
                          (lambda ()    ; *2*
                            (set-variable-value! var
                                                 old-value
                                                 env)
                            (fail-2)))))
             fail))))

;;;procedure applications

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env succeed fail)
      (fproc env
             (lambda (proc fail-2)
               (get-args aprocs
                         env
                         (lambda (args fail-3)
                           (execute-application
                            proc args succeed fail-3))
                         fail-2))
             fail))))

(define (get-args aprocs env succeed fail)
  (if (null? aprocs)
      (succeed '() fail)
      ((car aprocs) env
       ;; success continuation for this aproc
       (lambda (arg fail-2)
         (get-args (cdr aprocs)
                   env
                   ;; success continuation for recursive
                   ;; call to get-args
                   (lambda (args fail-3)
                     (succeed (cons arg args)
                              fail-3))
                   fail-2))
       fail)))

(define (execute-application proc args succeed fail)
  (cond ((primitive-procedure? proc)
         (succeed (apply-primitive-procedure proc args)
                  fail))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc))
          succeed
          fail))
        (else
         (error
          "unknown procedure type -- execute-application"
          proc))))

;;;amb expressions

(define (analyze-amb exp)
  (let ((cprocs (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            ((car choices) env
             succeed
             (lambda ()
               (try-next (cdr choices))))))
      (try-next cprocs))))

(define (analyze-ramb exp)
  (analyze-amb
   (cons 'amb
         (shuffle (cdr exp)
                  pseudo-random-integer))))

(define (analyze-permanent-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail-2)
               (set-variable-value! var val env)
               (succeed 'ok fail-2))
             fail))))

;;;driver loop

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

(define (make-exp-reader)
  (let ((exps '()))
    (lambda (message)
      (cond ((eq? message 'put)
             (lambda (new-exps)
               (set! exps new-exps)
               exps))
            ((eq? message 'read)
             (let ((result (car exps)))
               (set! exps (cdr exps))
               result))
            ((eq? message 'empty?)
             (null? exps))))))

(define exp-reader (make-exp-reader))

(define (announce-output string)
  (display string) (newline))

(define output-prompt ";;; amb-eval value:")

(define (driver-loop)
  (define (internal-loop try-again)
    (if (not (exp-reader 'empty?))
        (let ((input (exp-reader 'read)))
          (if (eq? input 'try-again)
              (try-again)
              (begin
                (display ";;; starting a new problem")
                (newline)
                (amb-eval input
                          the-global-environment
                          ;; amb-eval success
                          (lambda (val next-alternative)
                            (announce-output output-prompt)
                            (user-print val)
                            (newline)
                            (internal-loop next-alternative))
                          ;; amb-eval failure
                          (lambda ()
                            (announce-output
                             ";;; there are no more values of")
                            (user-print input)
                            (newline)
                            (driver-loop))))))))
  (internal-loop
   (lambda ()
     (display ";;; there is no current problem")
     (newline)
     (driver-loop))))

;;; support for let (as noted in footnote 56, p.428)

(define (let? exp) (tagged-list? exp 'let))
(define (let-bindings exp) (cadr exp))
(define (let-body exp) (cddr exp))

(define (let-var binding) (car binding))
(define (let-val binding) (cadr binding))

(define (make-combination operator operands) (cons operator operands))

(define (let->combination exp)
  ;;make-combination defined in earlier exercise
  (let ((bindings (let-bindings exp)))
    (make-combination (make-lambda (map let-var bindings)
                                   (let-body exp))
                      (map let-val bindings))))

;; a longer list of primitives -- suitable for running everything in 4.3
;; overrides the list in ch4-mceval.scm
;; has not to support require; various stuff for code in text (including
;;  support for prime?); integer? and sqrt for exercise code;
;;  eq? for ex. solution

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'list list)
        (list 'memq memq)
        (list 'member member)
        (list 'not not)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '= =)
        (list '> >)
        (list '>= >=)
        (list '<= <=)
        (list 'abs abs)
        (list 'remainder remainder)
        (list 'integer? integer?)
        (list 'sqrt sqrt)
        (list 'eq? eq?)))

(define the-global-environment (setup-environment))

;;; end amb evaluator

(define (list-remove items i)
  (if (= i 0)
      (cdr items)
      (cons (car items)
            (list-remove (cdr items) (- i 1)))))

(define (shuffle items picker)
  (if (null? items)
      '()
      (let ((i (picker (length items))))
        (cons (list-ref items i)
              (shuffle (list-remove items i) picker)))))

(test-group
 "list-remove"
 (test
  '(1 2 3)
  (list-remove '(0 1 2 3) 0))
 (test
  '(0 1 2)
  (list-remove '(0 1 2 3) 3))
 (test
  '(0 1 3)
  (list-remove '(0 1 2 3) 2)))

(test-group
 "shuffle"
 (test
  '()
  (shuffle '() '()))
 (test
  '(0 1 2 3)
  (shuffle '(0 1 2 3)
           (lambda (n) 0)))
 (test
  '(3 2 1 0)
  (shuffle '(0 1 2 3)
           (lambda (n) (- n 1)))))

(define debug #t)

(log-line (pseudo-random-integer 4))

((exp-reader 'put)
 '((define (require p) (if (not p) (amb)))
   (define (an-element-of items)
     (require (not (null? items)))
     (amb (car items) (an-element-of (cdr items))))
   (define (an-integer-between low high)
     (require (<= low high))
     (amb low (an-integer-between (+ low 1) high)))
   (define (distinct? items)
     (cond ((null? items) true)
           ((null? (cdr items)) true)
           ((member (car items) (cdr items)) false)
           (else (distinct? (cdr items)))))
   (define count 0)
   (let ((x (an-element-of '(a b c)))
         (y (an-element-of '(a b c))))
     (permanent-set! count (+ count 1))
     (require (not (eq? x y)))
     (list x y count))
   try-again))
(driver-loop)
