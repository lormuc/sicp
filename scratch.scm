(load "prelude.scm")

;;;from section 4.1.4 -- must precede def of metacircular apply
(define apply-in-underlying-scheme apply)

;;;section 4.1.1

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        (else
         (let ((proc (get 'eval (car exp))))
           (if proc
               (proc exp env)
               (if (application? exp)
                   (apply (eval (operator exp) env)
                          (list-of-values (operands exp) env))
                   (error "unknown expression type -- eval" exp)))))))

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

(put 'eval 'quote
     (lambda (exp env) (text-of-quotation exp)))
(put 'eval 'set! eval-assignment)
(put 'eval 'define eval-definition)
(put 'eval 'if eval-if)
(put 'eval 'lambda
     (lambda (exp env)
       (make-procedure (lambda-parameters exp)
                       (lambda-body exp)
                       env)))
(put 'eval 'begin
     (lambda (exp env)
       (eval-sequence (begin-actions exp) env)))

(define (eval-and exp env)
  (define (loop args)
    (if (null? args)
        #t
        (let ((value (eval (car args) env)))
          (if (true? value)
              (if (null? (cdr args))
                  value
                  (loop (cdr args)))
              #f))))
  (loop (cdr exp)))

(put 'eval 'and eval-and)

(define (expand-and exp)
  (define (loop args)
    (if (null? args)
        #t
        (if (null? (cdr args))
            (car args)
            (list 'if (car args)
                  (loop (cdr args))
                  #f))))
  (loop (cdr exp)))

(put 'eval 'and
     (lambda (exp env)
       (eval (expand-and exp) env)))

(define (eval-or exp env)
  (define (loop args)
    (if (null? args)
        #f
        (let ((value (eval (car args) env)))
          (if (true? value)
              value
              (loop (cdr args))))))
  (loop (cdr exp)))
(put 'eval 'or eval-or)

;;;section 4.1.2

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        ((boolean? exp) true)
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

(define (eval-cond exp env)
  (define (arrow-clause? clause)
    (eq? (cadr clause) '=>))
  (define (arrow-clause-recipient clause)
    (caddr clause))
  (define (loop clauses)
    (if (null? clauses)
        (eval 'false env)
        (let ((first (car clauses))
              (rest (cdr clauses)))
          (let ((predicate-value
                 (eval (if (cond-else-clause? first)
                           'true
                           (cond-predicate first))
                       env)))
            (if (true? predicate-value)
                (if (arrow-clause? first)
                    (let ((recipient
                           (eval (arrow-clause-recipient first) env)))
                      (apply recipient (list predicate-value)))
                    (eval (sequence->exp (cond-actions first)) env))
                (loop rest))))))
  (loop (cond-clauses exp)))

(put 'eval 'cond eval-cond)

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

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list '+ +)
        (list '* *)))

(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

(define the-global-environment (setup-environment))

(define (application? exp) (tagged-list? exp 'call))
(define (operator exp) (cadr exp))
(define (operands exp) (cddr exp))

(test-group
 "procedure-call-begins-with-call"
 (test-error (eval '(+ 1 2) (setup-environment)))
 (test 3 (eval '(call + 1 2) (setup-environment)))
 (test 'a (eval '(call car (call cons 'a 'b)) (setup-environment))))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(test-group
 "eval"
 (test 29 (eval '(+ (* 1 2) (* 3 (+ 4 5)))
                (setup-environment)))
 (test 2 (eval '(if (null? 0) 1 2) (setup-environment)))
 (test 3 (eval '(begin 0 1 2 3) (setup-environment))))

(test-group
 "and"
 (test #t (eval '(and) (setup-environment)))
 (test #f (eval '(and #f) (setup-environment)))
 (test 1 (eval '(and 1) (setup-environment)))
 (test 3 (eval '(and #t 3) (setup-environment)))
 (test 0 (eval '(begin (define x 0)
                       (and #f (set! x 1))
                       x) (setup-environment))))

(test-group
 "or"
 (test #f (eval '(or) (setup-environment)))
 (test #f (eval '(or #f) (setup-environment)))
 (test 1 (eval '(or 1) (setup-environment)))
 (test #t (eval '(or #t 3) (setup-environment)))
 (test 0 (eval '(begin (define x 0)
                       (or #t (set! x 1))
                       x) (setup-environment))))

(test-group
 "expand-and"
 (test #t (expand-and '(and)))
 (test
  '(if 0 1 #f)
  (expand-and '(and 0 1)))
 (test
  '(if 0 (if 1 2 #f) #f)
  (expand-and '(and 0 1 2))))

(test-group
 "cond"
 (test
  3
  (eval '(cond (#f 1)
               (#f 2)
               (0 3))
        (setup-environment)))
 (test
  2
  (eval '(cond (#f x)
               (#f y)
               (else 2))
        (setup-environment)))
 (test
  0
  (eval '(cond ((+ 2 3) 0)
               (else 1))
        (setup-environment))))

(test-group
 "cond =>"
 (test
  0
  (eval '(cond ((cons 0 1) => car))
        (setup-environment)))
 (test
  3
  (eval '(cond (#f 0)
               (#f => 1)
               ((cons 2 3) => cdr)
               (else 2))
        (setup-environment))))

(define debug #t)
