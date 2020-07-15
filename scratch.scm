(import trace)
(load "prelude.scm")

;;;from section 4.1.4 -- must precede def of metacircular apply
(define apply-in-underlying-scheme apply)

;;;section 4.1.1

(define (eval exp env)
  (log-line "eval" exp)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        (else
         (let ((proc (get 'eval (car exp))))
           (if proc
               (proc exp env)
               (if (application? exp)
                   (let ((op-val (actual-value (operator exp) env)))
                     (apply op-val
                            (prepare-arguments op-val
                                               (operands exp)
                                               env)))
                   (error "unknown expression type -- eval" exp)))))))

(define (prepare-arguments operator operands env)
  (define (helper params args)
    (if (no-operands? args)
        '()
        (cons
         (let ((arg (first-operand args)))
           (cond ((parameter-lazy? (car params))
                  (delay-it arg env))
                 ((parameter-lazy-memo? (car params))
                  (delay-it-memo arg env))
                 (else (eval arg env))))
         (helper (cdr params) (rest-operands args)))))
  (cond ((primitive-procedure? operator)
         (map (lambda (exp) (actual-value exp env))
              operands))
        (else
         (helper (procedure-parameters-x operator) operands))))

;; thunks

(define (delay-it exp env)
  (list 'thunk exp env))
(define (delay-it-memo exp env)
  (list 'thunk-memo exp env))

(define (thunk? obj)
  (tagged-list? obj 'thunk))

(define (thunk-memo? obj)
  (tagged-list? obj 'thunk-memo))

(define (thunk-exp thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))

(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))

(define (thunk-value evaluated-thunk) (cadr evaluated-thunk))

(define (force-it obj)
  (cond ((thunk? obj)
         (actual-value (thunk-exp obj) (thunk-env obj)))
        ((thunk-memo? obj)
         (let ((result (actual-value
                        (thunk-exp obj)
                        (thunk-env obj))))
           (set-car! obj 'evaluated-thunk)
           (set-car! (cdr obj) result)  ; replace exp with its value
           (set-cdr! (cdr obj) '())     ; forget unneeded env
           result))
        ((evaluated-thunk? obj)
         (thunk-value obj))
        (else obj)))

(define (actual-value exp env)
  (force-it (eval exp env)))

(define (parameter-variable param)
  (if (pair? param)
      (car param)
      param))

(define (parameter-lazy? param)
  (and (pair? param) (eq? (cadr param) 'lazy)))
(define (parameter-lazy-memo? param)
  (and (pair? param) (eq? (cadr param) 'lazy-memo)))

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
                 (actual-value (if (cond-else-clause? first)
                                   'true
                                   (cond-predicate first))
                               env)))
            (if (true? predicate-value)
                (if (arrow-clause? first)
                    (let ((recipient
                           (actual-value (arrow-clause-recipient first)
                                         env)))
                      (apply recipient (list predicate-value)))
                    (eval (sequence->exp (cond-actions first)) env))
                (loop rest))))))
  (loop (cond-clauses exp)))

(put 'eval 'cond eval-cond)

(define (let-named? exp)
  (symbol? (cadr exp)))

(define (let-body exp)
  (if (let-named? exp)
      (cdddr exp)
      (cddr exp)))

(define (let-bindings exp)
  (if (let-named? exp)
      (caddr exp)
      (cadr exp)))

(define (let-name exp)
  (cadr exp))

(define (make-application proc args)
  (cons proc args))

(define (make-define name value)
  (list 'define name value))

(define (let->combination exp)
  (let ((bindings (let-bindings exp))
        (body (let-body exp)))
    (let ((vars (map car bindings))
          (exps (map cadr bindings)))
      (if (let-named? exp)
          (let ((name (let-name exp)))
            `((lambda ()
                (define ,name (lambda ,vars ,@body))
                (,name ,@exps))))
          `((lambda ,vars ,@body) ,@exps)))))

(put 'eval 'let
     (lambda (exp env)
       (eval (let->combination exp) env)))

(define (make-let* bindings body)
  (cons 'let* (cons bindings body)))

(define (make-let bindings body)
  (cons 'let (cons bindings body)))

(define (let*->nested-lets exp)
  (let ((bindings (cadr exp))
        (body (cddr exp)))
    (if (null? (cdr bindings))
        (make-let bindings body)
        (make-let (list (car bindings))
                  (list (let*->nested-lets
                         (make-let* (cdr bindings)
                                    body)))))))

(put 'eval 'let*
     (lambda (exp env) (eval (let*->nested-lets exp) env)))

(define (expand-for exp)
  (assert (eq? 'do (cadddr exp)))
  (let ((var (cadr exp))
        (limit (caddr exp))
        (body (cddddr exp)))
    `(let ((body (lambda (,var) ,@body)))
       (define (loop i)
         (if (= i ,limit)
             #f
             (begin (body i)
                    (loop (+ i 1)))))
       (loop 0))))

(put 'eval 'for
     (lambda (exp env) (eval (expand-for exp) env)))

(define (expand-while exp)
  (assert (eq? 'do (caddr exp)))
  (let ((condition (cadr exp))
        (body (cdddr exp)))
    `(let ((body (lambda () ,@body))
           (condition (lambda () ,condition)))
       (define (loop)
         (if (condition)
             (begin (body)
                    (loop))))
       (loop))))

(put 'eval 'while
     (lambda (exp env) (eval (expand-while exp) env)))

(put 'eval 'make-unbound!
     (lambda (exp env)
       (frame-delete-binding! (first-frame env)
                              (cadr exp))))

(define (expand-letrec exp)
  (let ((bindings (cadr exp))
        (body (cddr exp)))
    (make-let
     (map (lambda (binding)
            (list (car binding) ''*unassigned*))
          bindings)
     (append
      (map (lambda (binding)
             (list 'set! (car binding) (cadr binding)))
           bindings)
      body))))

(put 'eval 'letrec
     (lambda (exp env) (eval (expand-letrec exp) env)))

(define (expand-unless exp)
  (make-if (cadr exp) (cadddr exp) (caddr exp)))

(put 'eval 'unless
     (lambda (exp env) (eval (expand-unless exp) env)))

;;;section 4.1.3

(define (true? x)
  (not (eq? (force-it x) false)))

(define (false? x)
  (eq? (force-it x) false))


(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))


(define (procedure-parameters p)
  (map parameter-variable (cadr p)))
(define (procedure-parameters-x p) (cadr p))
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

(define (scan-frame frame var)
  (define (scan vars vals)
    (cond ((null? vars) '())
          ((eq? (car vars) var) vals)
          (else (scan (cdr vars) (cdr vals)))))
  (scan (frame-variables frame) (frame-values frame)))

(define (scan-env env var)
  (define (loop env)
    (if (eq? env the-empty-environment)
        '()
        (let ((vals (scan-frame (first-frame env) var)))
          (if (null? vals)
              (loop (enclosing-environment env))
              vals))))
  (loop env))

(define (lookup-variable-value var env)
  (let ((vals (scan-env env var)))
    (if (null? vals)
        (error "unbound variable" var)
        (car vals))))

(define (set-variable-value! var val env)
  (let ((vals (scan-env env var)))
    (if (null? vals)
        (error "unbound variable -- set!" var)
        (set-car! vals val))))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (let ((vals (scan-frame frame var)))
      (if (null? vals)
          (add-binding-to-frame! var val frame)
          (set-car! vals val)))))

(define (frame-delete-binding! frame var)
  (define (loop vars vals)
    (cond ((null? (cdr vars)) #f)
          ((eq? (cadr vars) var)
           (set-cdr! vars (cddr vars))
           (set-cdr! vals (cddr vals))
           #t)
          (else (loop (cdr vars) (cdr vals)))))
  (let ((vars (car frame))
        (vals (cdr frame)))
    (cond ((null? vars) #f)
          ((eq? (car vars) var)
           (set-car! frame (cdr vars))
           (set-cdr! frame (cdr vals))
           #t)
          (else (loop vars vals)))))

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
        (list 'list list)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '= =)
        (list 'newline newline)
        (list 'display display)
        (list '> >)
        (list '< <)
        (list 'strict-cadr cadr)
        (list 'strict-list list)))

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

(test-group
 "let->combination"
 (test
  '((lambda ()))
  (let->combination '(let ())))
 (test
  '((lambda (a) c d) b)
  (let->combination '(let ((a b)) c d)))
 (test
  '((lambda (a) c d) b)
  (let->combination '(let ((a b)) c d)))
 (test
  '((lambda (a c) e f) b d)
  (let->combination '(let ((a b) (c d)) e f))))

(test-group
 "let"
 (test
  0
  (eval '(let ((x 0)) x)
        (setup-environment)))
 (test
  4
  (eval '(let ((x 0) (y (+ 1 3))) (+ x y))
        (setup-environment)))
 (test
  0
  (eval '(begin (define x 0)
                (let () (define x 1))
                x)
        (setup-environment))))

(test-group
 "let*->nested-lets"
 (test
  '(let ((x 3))
     (let ((y (+ x 2)))
       (let ((z (+ x y 5)))
         (* x z))))
  (let*->nested-lets
   '(let* ((x 3)
           (y (+ x 2))
           (z (+ x y 5)))
      (* x z)))))

(test-group
 "let*"
 (test
  39
  (eval '(let* ((x 3)
                (y (+ x 2))
                (z (+ x y 5)))
           (* x z))
        (setup-environment))))

(test-group
 "let->combination named"
 (test
  '((lambda ()
      (define f (lambda (v0 v1) body))
      (f e0 e1)))
  (let->combination '(let f ((v0 e0) (v1 e1)) body))))

(test-group
 "let named"
 (test
  3
  (eval
   '(let g ((n 2))
      (if (= n 0)
          3
          (g (- n 1))))
   (setup-environment)))
 (test
  8
  (eval
   '(let fib-iter ((a 1) (b 0) (count 6))
      (if (= count 0)
          b
          (fib-iter (+ a b)
                    a
                    (- count 1))))
   (setup-environment)))
 (test
  0
  (eval
   '(let ((x 0))
      (let x ((y 0)) 0)
      (+ x 0))
   (setup-environment))))

(test-group
 "expand-for"
 (test
  '(let ((body (lambda (i) x y)))
     (define (loop i)
       (if (= i 10)
           #f
           (begin (body i)
                  (loop (+ i 1)))))
     (loop 0))
  (expand-for '(for i 10 do
                    x y)))
 (test
  '(let ((body (lambda (x) #f)))
     (define (loop i)
       (if (= i y)
           #f
           (begin (body i)
                  (loop (+ i 1)))))
     (loop 0))
  (expand-for '(for x y do #f))))

(test-group
 "for"
 (test
  0
  (eval
   '(begin (define x 0)
           (for x 10 do #f)
           x)
   (setup-environment)))
 (test
  45
  (eval
   '(let ((s 0))
      (for i 10 do (set! s (+ s i)))
      s)
   (setup-environment))))

(test-group
 "expand-while"
 (test
  '(let ((body (lambda () b c))
         (condition (lambda () a)))
     (define (loop)
       (if (condition)
           (begin (body)
                  (loop))))
     (loop))
  (expand-while '(while a do b c))))

(test-group
 "while"
 (test
  45
  (eval
   '(begin (define i 0)
           (define s 0)
           (while (< i 10) do
                  (set! s (+ s i))
                  (set! i (+ i 1)))
           s)
   (setup-environment))))

(test-group
 "frame-delete-binding"
 (test
  #f
  (let ((frame (make-frame '() '())))
    (frame-delete-binding! frame 'a)))
 (test
  '(() ())
  (let ((frame (make-frame '(x) '(0))))
    (frame-delete-binding! frame 'x)
    (list (frame-variables frame) (frame-values frame))))
 (test
  '((y z) (4 5))
  (let ((frame (make-frame '(x y z) '(3 4 5))))
    (frame-delete-binding! frame 'x)
    (list (frame-variables frame) (frame-values frame))))
 (test
  '((x z) (3 5))
  (let ((frame (make-frame '(x y z) '(3 4 5))))
    (frame-delete-binding! frame 'y)
    (list (frame-variables frame) (frame-values frame))))
 (test
  '((x y) (3 4))
  (let ((frame (make-frame '(x y z) '(3 4 5))))
    (frame-delete-binding! frame 'z)
    (list (frame-variables frame) (frame-values frame)))))

(test-group
 "make-unbound!"
 (test
  0
  (eval
   '(let ((x 0))
      (let ((x 1))
        (make-unbound! x)
        x))
   (setup-environment)))
 (test-error
  (eval
   '(let ((a 3))
      (make-unbound! a)
      a)
   (setup-environment))))

(test-group
 "expand-letrec"
 (test
  '(let () x)
  (expand-letrec
   '(letrec () x)))
 (test
  '(let ((v '*unassigned*))
     (set! v e)
     b)
  (expand-letrec
   '(letrec ((v e)) b)))
 (test
  '(let ((v '*unassigned*)
         (v1 '*unassigned*))
     (set! v e)
     (set! v1 e1)
     x y)
  (expand-letrec
   '(letrec ((v e) (v1 e1)) x y))))

(test-group
 "letrec"
 (test
  120
  (eval
   '(letrec
        ((fact
          (lambda (n)
            (if (= n 1)
                1
                (* n (fact (- n 1)))))))
      (fact 5))
   (setup-environment)))
 (test
  '(#t #f #t)
  (eval
   '(letrec
        ((even?
          (lambda (n)
            (if (= n 0)
                true
                (odd? (- n 1)))))
         (odd?
          (lambda (n)
            (if (= n 0)
                false
                (even? (- n 1))))))
      (list (even? 0) (even? 1) (even? 2)))
   (setup-environment))))

(test-group
 "lazy"
 (test
  3
  (actual-value '((lambda ((x lazy)) x) 3)
                (setup-environment)))
 (test
  1
  (actual-value '((lambda ((x lazy) (y lazy)) y) (/ 1 0) 1)
                (setup-environment)))
 (test
  4
  (eval '((lambda ((x lazy) (y lazy) z) z) (/ 1 0) (/ 1 0) 4)
        (setup-environment))))

(test-group
 "lazy-memo"
 (test
  3
  (force-it (eval '((lambda ((x lazy-memo)) x) 3)
                  (setup-environment))))
 (test
  1
  (force-it (eval '(let ((y 0))
                     ((lambda ((x lazy-memo)) (null? x) (null? x))
                      (set! y (+ y 1)))
                     y)
                  (setup-environment))))
 (test
  4
  (eval '((lambda (a b (c lazy-memo) d) (+ a b c d))
          1 1 1 1)
        (setup-environment))))

(define (eval-a exp env)
  ((analyze exp) env))

(define (let? exp)
  (tagged-list? exp 'let))

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
        ((let? exp) (analyze (let->combination exp)))
        ((application? exp) (analyze-application exp))
        (else
         (error "unknown expression type -- analyze" exp))))

(define (analyze-self-evaluating exp)
  (lambda (env) exp))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env) qval)))

(define (analyze-variable exp)
  (lambda (env) (lookup-variable-value exp env)))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env)
      (set-variable-value! var (vproc env) env)
      'ok)))

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env)
      (define-variable! var (vproc env) env)
      'ok)))

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env)
      (if (true? (pproc env))
          (cproc env)
          (aproc env)))))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env) (make-procedure vars bproc env))))

(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "empty sequence -- analyze"))
    (loop (car procs) (cdr procs))))

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env)
      (execute-application (fproc env)
                           (map (lambda (aproc) (aproc env))
                                aprocs)))))

(define (execute-application proc args)
  (cond ((primitive-procedure? proc)
         (apply-primitive-procedure proc args))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc))))
        (else
         (error
          "unknown procedure type -- execute-application"
          proc))))


(let ((eval eval-a))
  (test-group
   "eval-a let"
   (test
    0
    (eval '(let ((x 0)) x)
          (setup-environment)))
   (test
    4
    (eval '(let ((x 0) (y (+ 1 3))) (+ x y))
          (setup-environment)))
   (test
    0
    (eval '(begin (define x 0)
                  (let () (define x 1))
                  x)
          (setup-environment)))))

(test-group
 "unless"
 (test 0 (eval '(unless true (/ 1 0) 0)
               (setup-environment)))
 (test 1 (eval '(unless false 1 (/ 1 0))
               (setup-environment))))

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp)
         (let ((text (text-of-quotation exp)))
           (if (pair? text)
               (eval `(cons (quote ,(car text))
                            (quote ,(cdr text)))
                     env)
               text)))
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
        ((application? exp)             ; clause from book
         (apply (actual-value (operator exp) env)
                (operands exp)
                env))
        (else
         (error "unknown expression type -- eval" exp))))

(define (actual-value exp env)
  (force-it (eval exp env)))

(define (apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
          procedure
          (list-of-arg-values arguments env))) ; changed
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           (list-of-delayed-args arguments env) ; changed
           (procedure-environment procedure))))
        (else
         (error
          "unknown procedure type -- apply" procedure))))

(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value (first-operand exps) env)
            (list-of-arg-values (rest-operands exps)
                                env))))

(define (list-of-delayed-args exps env)
  (if (no-operands? exps)
      '()
      (cons (delay-it (first-operand exps) env)
            (list-of-delayed-args (rest-operands exps)
                                  env))))

(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

;;; representing thunks

;; non-memoizing version of force-it

(define (force-it obj)
  (if (thunk? obj)
      (actual-value (thunk-exp obj) (thunk-env obj))
      obj))

;; thunks

(define (delay-it exp env)
  (list 'thunk exp env))

(define (thunk? obj)
  (tagged-list? obj 'thunk))

(define (thunk-exp thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))

;; "thunk" that has been forced and is storing its (memoized) value
(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))

(define (thunk-value evaluated-thunk) (cadr evaluated-thunk))

(define (force-it obj)
  (cond ((thunk? obj)
         (let ((result (actual-value
                        (thunk-exp obj)
                        (thunk-env obj))))
           (set-car! obj 'evaluated-thunk)
           (set-car! (cdr obj) result)  ; replace exp with its value
           (set-cdr! (cdr obj) '())     ; forget unneeded env
           result))
        ((evaluated-thunk? obj)
         (thunk-value obj))
        (else obj)))

(define (eval-program exps)
  (let ((env (setup-environment)))
    (for-each
     (lambda (exp)
       (log-line (actual-value exp env)))
     exps)))

(define (lazy-pair? x)
  (tagged-list? x 'lazy-pair))

(define (lazy-list-setup-environment)
  (let ((env (setup-environment)))
    (eval '(define (cons x y)
             (strict-list 'lazy-pair (lambda (m) (m x y))))
          env)
    (eval '(define (car z) ((strict-cadr z) (lambda (x y) x)))
          env)
    (eval '(define (cdr z) ((strict-cadr z) (lambda (x y) y)))
          env)
    env))

(test-group
 "quoted pairs are lazy"
 (test
  'a
  (actual-value '(car '(a b c))
                (lazy-list-setup-environment)))
 (test
  'b
  (actual-value '(car (car (cdr '(a (b c)))))
                (lazy-list-setup-environment))))

(define (call operator arguments env)
  (let ((procedure (actual-value operator env)))
    (eval-sequence
     (procedure-body procedure)
     (extend-environment
      (procedure-parameters procedure)
      arguments
      (procedure-environment procedure)))))

(define (my-user-print value env)
  (let ((count 10))
    (define (show obj)
      (if (= count 0)
          (display "...")
          (cond ((thunk? obj) (show (force-it obj)))
                ((evaluated-thunk? obj)
                 (show (thunk-value obj)))
                ((lazy-pair? obj)
                 (display "(")
                 (show (call 'car (list obj) env))
                 (display " . ")
                 (show (call 'cdr (list obj) env))
                 (display ")"))
                (else (display obj)
                      (set! count (- count 1))))))
    (show value)))

(define (amb? exp) (tagged-list? exp 'amb))
(define (amb-choices exp) (cdr exp))

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
             (lambda (val fail-2)        ; *1*
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

;;;driver loop

(define input-prompt ";;; amb-eval input:")
(define output-prompt ";;; amb-eval value:")

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

(define (make-reader)
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

(define reader (make-reader))

(define (driver-loop)
  (define (internal-loop try-again)
    (if (not (reader 'empty?))
        (let ((input (reader 'read)))
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

(test-group
 "make-reader"
 (test
  '(#f 0 1 2 #t)
  (let ((r (make-reader)))
    ((r 'put) '(0 1 2))
    (list (r 'empty?) (r 'read) (r 'read) (r 'read) (r 'empty?)))))

(define the-global-environment (setup-environment))
(define debug #t)

((reader 'put)
 '((define (require p) (if (not p) (amb)))
   (define (an-integer-between low high)
     (require (<= low high))
     (amb low (an-integer-between (+ low 1) high)))
   (define (an-integer-starting-from low)
     (amb low (an-integer-starting-from (+ 1 low))))
   (define (distinct? items)
     (cond ((null? items) true)
           ((null? (cdr items)) true)
           ((member (car items) (cdr items)) false)
           (else (distinct? (cdr items)))))
   (define (xor a b)
     (if a (not b) b))
   (define (solve)
     (define (select-daughter)
       (amb 'meri 'rosalinda 'lorna 'gabriella 'melissa))
     (let ((mur-y (select-daughter))
           (dauning-y (select-daughter))
           (holl-y (select-daughter))
           (barnakl-y (select-daughter))
           (parker-y (select-daughter)))
       (require (eq? barnakl-y 'gabriella))
       (require (eq? mur-y 'lorna))
       (require (eq? holl-y 'rosalinda))
       (require (eq? dauning-y 'melissa))
       (require (distinct?
                 (list mur-y dauning-y holl-y barnakl-y parker-y)))
       (let ((mur-d (select-daughter))
             (dauning-d (select-daughter))
             (holl-d (select-daughter))
             (barnakl-d (select-daughter))
             (parker-d (select-daughter)))
         (require (eq? barnakl-d 'melissa))
         ;; (require (eq? mur-d 'meri))
         (require (not (eq? mur-d mur-y)))
         (require (not (eq? dauning-d dauning-y)))
         (require (not (eq? holl-d holl-y)))
         (require (not (eq? barnakl-d barnakl-y)))
         (require (not (eq? parker-d parker-y)))
         (require (distinct?
                   (list mur-d dauning-d holl-d barnakl-d parker-d)))
         (let ((father-of-gabriella-yacht
                (cond ((eq? mur-d 'gabriella) mur-y)
                      ((eq? dauning-d 'gabriella) dauning-d)
                      ((eq? holl-d 'gabriella) holl-y)
                      ((eq? barnakl-d 'gabriella) barnakl-y)
                      ((eq? parker-d 'gabriella) parker-y))))
           (require (eq? father-of-gabriella-yacht parker-d)))
         (list (list 'mur mur-d mur-y)
               (list 'dauning dauning-d dauning-y)
               (list 'holl holl-d holl-y)
               (list 'barnakl barnakl-d barnakl-y)
               (list 'parker parker-d parker-y)))))
   (solve)
   try-again
   try-again
   try-again
   try-again
   try-again))
(time (driver-loop))
