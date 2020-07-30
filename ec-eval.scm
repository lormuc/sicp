(define ec-eval-operations
  (list
   (list 'self-evaluating? self-evaluating?)
   (list 'quoted? quoted?)
   (list 'text-of-quotation text-of-quotation)
   (list 'variable? variable?)
   (list 'assignment? assignment?)
   (list 'assignment-variable assignment-variable)
   (list 'assignment-value assignment-value)
   (list 'definition? definition?)
   (list 'definition-variable definition-variable)
   (list 'definition-value definition-value)
   (list 'lambda? lambda?)
   (list 'lambda-parameters lambda-parameters)
   (list 'lambda-body lambda-body)
   (list 'if? if?)
   (list 'if-predicate if-predicate)
   (list 'if-consequent if-consequent)
   (list 'if-alternative if-alternative)
   (list 'begin? begin?)
   (list 'begin-actions begin-actions)
   (list 'last-exp? last-exp?)
   (list 'first-exp first-exp)
   (list 'rest-exps rest-exps)
   (list 'application? application?)
   (list 'operator operator)
   (list 'operands operands)
   (list 'no-operands? no-operands?)
   (list 'first-operand first-operand)
   (list 'rest-operands rest-operands)

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
   (list 'no-more-exps? no-more-exps?)

   (list 'make-error list)
   (list 'cond? cond?)
   (list 'let? let?)
   (list 'let->combination let->combination)
   (list 'cond-clauses cond-clauses)
   (list 'cond-predicate cond-predicate)
   (list 'cond-actions cond-actions)
   (list 'cond-else-clause? cond-else-clause?)
   (list 'null? null?)
   (list 'car car)
   (list 'cdr cdr)
   (list 'symbol? symbol?)))

(define (make-ec-eval-machine)
  (make-machine
   '(exp env val proc argl continue unev)
   ec-eval-operations
   '((perform (op initialize-stack))
     (assign continue (label done))
     (goto (label eval-dispatch))

     unknown-procedure-type
     (restore continue)
     (assign val
             (op make-error)
             (const unknown-procedure-type-error)
             (reg proc))
     (goto (label done))

     eval-dispatch
     (test (op self-evaluating?) (reg exp))
     (branch (label ev-self-eval))
     (test (op variable?) (reg exp))
     (branch (label ev-variable))
     (test (op quoted?) (reg exp))
     (branch (label ev-quoted))
     (test (op assignment?) (reg exp))
     (branch (label ev-assignment))
     (test (op definition?) (reg exp))
     (branch (label ev-definition))
     (test (op if?) (reg exp))
     (branch (label ev-if))
     (test (op lambda?) (reg exp))
     (branch (label ev-lambda))
     (test (op begin?) (reg exp))
     (branch (label ev-begin))
     (test (op cond?) (reg exp))
     (branch (label ev-cond))
     (test (op let?) (reg exp))
     (branch (label ev-let))
     (test (op application?) (reg exp))
     (branch (label ev-application))

     ev-self-eval
     (assign val (reg exp))
     (goto (reg continue))
     ev-variable
     (assign val (op lookup-variable-value) (reg exp) (reg env))
     (goto (reg continue))
     ev-quoted
     (assign val (op text-of-quotation) (reg exp))
     (goto (reg continue))
     ev-lambda
     (assign unev (op lambda-parameters) (reg exp))
     (assign exp (op lambda-body) (reg exp))
     (assign val (op make-procedure)
             (reg unev) (reg exp) (reg env))
     (goto (reg continue))

     ev-application
     (save continue)
     (assign unev (op operands) (reg exp))
     (save unev)
     (assign exp (op operator) (reg exp))
     (test (op symbol?) (reg exp))
     (branch (label ev-appl-op-no-save-env))
     (save env)
     (assign continue (label ev-appl-did-operator))
     (goto (label eval-dispatch))
     ev-appl-did-operator
     (restore env)
     (goto (label ev-appl-did-operator-no-save))
     ev-appl-op-no-save-env
     (assign continue (label ev-appl-did-operator-no-save))
     (goto (label eval-dispatch))
     ev-appl-did-operator-no-save
     (restore unev)
     (assign argl (op empty-arglist))
     (assign proc (reg val))
     (test (op no-operands?) (reg unev))
     (branch (label apply-dispatch))
     (save proc)
     ev-appl-operand-loop
     (save argl)
     (assign exp (op first-operand) (reg unev))
     (test (op last-operand?) (reg unev))
     (branch (label ev-appl-last-arg))
     (save env)
     (save unev)
     (assign continue (label ev-appl-accumulate-arg))
     (goto (label eval-dispatch))
     ev-appl-accumulate-arg
     (restore unev)
     (restore env)
     (restore argl)
     (assign argl (op adjoin-arg) (reg val) (reg argl))
     (assign unev (op rest-operands) (reg unev))
     (goto (label ev-appl-operand-loop))
     ev-appl-last-arg
     (assign continue (label ev-appl-accum-last-arg))
     (goto (label eval-dispatch))
     ev-appl-accum-last-arg
     (restore argl)
     (assign argl (op adjoin-arg) (reg val) (reg argl))
     (restore proc)
     (goto (label apply-dispatch))
     apply-dispatch
     (test (op primitive-procedure?) (reg proc))
     (branch (label primitive-apply))
     (test (op compound-procedure?) (reg proc))
     (branch (label compound-apply))
     (goto (label unknown-procedure-type))

     primitive-apply
     (assign val (op apply-primitive-procedure)
             (reg proc)
             (reg argl))
     (restore continue)
     (goto (reg continue))

     compound-apply
     (assign unev (op procedure-parameters) (reg proc))
     (assign env (op procedure-environment) (reg proc))
     (assign env (op extend-environment)
             (reg unev) (reg argl) (reg env))
     (assign unev (op procedure-body) (reg proc))
     (goto (label ev-sequence))

     ev-begin
     (assign unev (op begin-actions) (reg exp))
     (save continue)
     (goto (label ev-sequence))

     ev-sequence
     (assign exp (op first-exp) (reg unev))
     (test (op last-exp?) (reg unev))
     (branch (label ev-sequence-last-exp))
     (save unev)
     (save env)
     (assign continue (label ev-sequence-continue))
     (goto (label eval-dispatch))
     ev-sequence-continue
     (restore env)
     (restore unev)
     (assign unev (op rest-exps) (reg unev))
     (goto (label ev-sequence))
     ev-sequence-last-exp
     (restore continue)
     (goto (label eval-dispatch))

     ev-if
     (save exp)
     (save env)
     (save continue)
     (assign continue (label ev-if-decide))
     (assign exp (op if-predicate) (reg exp))
     (goto (label eval-dispatch))
     ev-if-decide
     (restore continue)
     (restore env)
     (restore exp)
     (test (op true?) (reg val))
     (branch (label ev-if-consequent))
     ev-if-alternative
     (assign exp (op if-alternative) (reg exp))
     (goto (label eval-dispatch))
     ev-if-consequent
     (assign exp (op if-consequent) (reg exp))
     (goto (label eval-dispatch))

     ev-assignment
     (assign unev (op assignment-variable) (reg exp))
     (save unev)
     (assign exp (op assignment-value) (reg exp))
     (save env)
     (save continue)
     (assign continue (label ev-assignment-1))
     (goto (label eval-dispatch))
     ev-assignment-1
     (restore continue)
     (restore env)
     (restore unev)
     (perform
      (op set-variable-value!) (reg unev) (reg val) (reg env))
     (assign val (const ok))
     (goto (reg continue))

     ev-definition
     (assign unev (op definition-variable) (reg exp))
     (save unev)
     (assign exp (op definition-value) (reg exp))
     (save env)
     (save continue)
     (assign continue (label ev-definition-1))
     (goto (label eval-dispatch))
     ev-definition-1
     (restore continue)
     (restore env)
     (restore unev)
     (perform
      (op define-variable!) (reg unev) (reg val) (reg env))
     (assign val (const ok))
     (goto (reg continue))

     ev-cond
     (save continue)
     (assign unev (op cond-clauses) (reg exp))
     ev-cond-loop
     (test (op null?) (reg unev))
     (branch (label ev-cond-clauses-null))
     (assign exp (op car) (reg unev))
     (test (op cond-else-clause?) (reg exp))
     (assign exp (op cond-predicate) (reg exp))
     (branch (label ev-cond-true))
     (save env)
     (save unev)
     (assign continue (label ev-cond-0))
     (goto (label eval-dispatch))
     ev-cond-0
     (restore unev)
     (restore env)
     (test (op true?) (reg val))
     (branch (label ev-cond-true))
     ev-cond-false
     (assign unev (op cdr) (reg unev))
     (goto (label ev-cond-loop))
     ev-cond-true
     (assign unev (op car) (reg unev))
     (assign unev (op cond-actions) (reg unev))
     (goto (label ev-sequence))
     ev-cond-clauses-null
     (assign val (const #f))
     (restore continue)
     (goto (reg continue))

     ev-let
     (assign exp (op let->combination) (reg exp))
     (goto (label eval-dispatch))

     done)))

(define (ec-eval exp)
  (car (machine-eval (make-ec-eval-machine)
                     exp
                     (setup-environment))))

(define (machine-eval machine exp env)
  (set-register-contents! machine 'env env)
  (set-register-contents! machine 'exp exp)
  (start machine)
  (list (get-register-contents machine 'val)
        ((machine 'stack) 'get-push-count)
        ((machine 'stack) 'get-max-depth)))

(define (ec-eval-program exps)
  (let ((machine (make-ec-eval-machine))
        (env (setup-environment)))
    (define (loop exps)
      (if (null? exps)
          '()
          (cons (machine-eval machine (car exps) env)
                (loop (cdr exps)))))
    (loop exps)))
