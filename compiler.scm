(define (minus? exp)
  (tagged-list? exp '-))

(define (equality? exp)
  (tagged-list? exp '=))

(define (plus? exp)
  (tagged-list? exp '+))

(define (star? exp)
  (tagged-list? exp '*))

(define (compile exp target linkage make-label)
  (cond ((self-evaluating? exp)
         (compile-self-evaluating exp target linkage make-label))
        ((quoted? exp)
         (compile-quoted exp target linkage make-label))
        ((variable? exp)
         (compile-variable exp target linkage make-label))
        ((assignment? exp)
         (compile-assignment exp target linkage make-label))
        ((definition? exp)
         (compile-definition exp target linkage make-label))
        ((if? exp)
         (compile-if exp target linkage make-label))
        ((lambda? exp)
         (compile-lambda exp target linkage make-label))
        ((begin? exp)
         (compile-sequence
          (begin-actions exp) target linkage make-label))
        ((cond? exp)
         (compile (cond->if exp) target linkage make-label))
        ((let? exp)
         (compile (let->combination exp)
                  target linkage make-label))
        ((or (plus? exp) (star? exp))
         (compile-open-coded-primitive exp target linkage make-label))
        ((or (equality? exp) (minus? exp))
         (compile-open-coded-primitive-2 exp target linkage make-label))
        ((application? exp)
         (compile-application exp target linkage make-label))
        (else
         (error "unknown expression type -- compile" exp))))


(define (make-instruction-sequence
         needs modifies statements)
  (list needs modifies statements))

(define (empty-instruction-sequence)
  (make-instruction-sequence '() '() '()))


(define (compile-linkage linkage)
  (cond ((eq? linkage 'next)
         (empty-instruction-sequence))
        ((eq? linkage 'return)
         (make-instruction-sequence
          '(continue) '() '((goto (reg continue)))))
        (else
         (make-instruction-sequence
          '() '() `((goto (label ,linkage)))))))

(define (end-with-linkage linkage instruction-sequence)
  (preserving '(continue)
              instruction-sequence
              (compile-linkage linkage)))


(define (compile-self-evaluating exp target linkage make-label)
  (end-with-linkage
   linkage
   (make-instruction-sequence
    '() (list target)
    `((assign ,target (const ,exp))))))

(define (compile-quoted exp target linkage make-label)
  (end-with-linkage
   linkage
   (make-instruction-sequence
    '() (list target)
    `((assign ,target (const ,(cadr exp)))))))

(define (compile-variable exp target linkage make-label)
  (end-with-linkage
   linkage
   (make-instruction-sequence
    '(env) (list target)
    `((assign ,target
              (op lookup-variable-value)
              (const ,exp) (reg env))))))

(define (compile-assignment exp target linkage make-label)
  (let ((var (assignment-variable exp))
        (get-value-code
         (compile (assignment-value exp)
                  'val
                  'next
                  make-label)))
    (end-with-linkage
     linkage
     (preserving
      '(env)
      get-value-code
      (make-instruction-sequence
       '(env val) (list target)
       `((perform (op set-variable-value!)
                  (const ,var)
                  (reg val)
                  (reg env))
         (assign ,target (const ok))))))))

(define (compile-definition exp target linkage make-label)
  (let ((var (definition-variable exp))
        (get-value-code
         (compile (definition-value exp) 'val 'next make-label)))
    (end-with-linkage
     linkage
     (preserving
      '(env)
      get-value-code
      (make-instruction-sequence
       '(env val) (list target)
       `((perform (op define-variable!)
                  (const ,var)
                  (reg val)
                  (reg env))
         (assign ,target (const ok))))))))


(define (make-make-label)
  (let ((counter -1))
    (lambda (name)
      (set! counter (+ 1 counter))
      (string->symbol
       (string-append (symbol->string name)
                      "-"
                      (number->string counter))))))

(define (compile-if exp target linkage make-label)
  (let ((t-branch (make-label 'true-branch))
        (f-branch (make-label 'false-branch))
        (after-if (make-label 'after-if)))
    (let ((consequent-linkage
           (if (eq? linkage 'next)
               after-if
               linkage)))
      (let ((p-code
             (compile (if-predicate exp)
                      'val
                      'next
                      make-label))
            (c-code
             (compile (if-consequent exp)
                      target
                      consequent-linkage
                      make-label))
            (a-code
             (compile (if-alternative exp)
                      target
                      linkage
                      make-label)))
        (preserving
         '(env continue)
         p-code
         (append-instruction-sequences
          (make-instruction-sequence
           '(val)
           '()
           `((test (op false?) (reg val))
             (branch (label ,f-branch))))
          (parallel-instruction-sequences
           (append-instruction-sequences
            t-branch c-code)
           (append-instruction-sequences
            f-branch a-code))
          after-if))))))

(define (compile-sequence seq target linkage make-label)
  (if (null? (cdr seq))
      (compile (car seq) target linkage make-label)
      (preserving '(continue env)
                  (compile (car seq)
                           'val
                           'next
                           make-label)
                  (compile-sequence (cdr seq)
                                    target
                                    linkage
                                    make-label))))


(define (compile-lambda exp target linkage make-label)
  (let ((proc-entry
         (make-label 'entry))
        (after-lambda
         (make-label 'after-lambda)))
    (let ((lambda-linkage
           (if (eq? linkage 'next)
               after-lambda
               linkage)))
      (append-instruction-sequences
       (tack-on-instruction-sequence
        (end-with-linkage
         lambda-linkage
         (make-instruction-sequence
          '(env)
          (list target)
          `((assign
             ,target
             (op make-compiled-procedure)
             (label ,proc-entry)
             (reg env)))))
        (compile-lambda-body exp proc-entry make-label))
       after-lambda))))

(define (compile-lambda-body exp proc-entry make-label)
  (let ((formals (lambda-parameters exp)))
    (append-instruction-sequences
     (make-instruction-sequence
      '(env proc argl)
      '(env)
      `(,proc-entry
        (assign env
                (op compiled-procedure-env)
                (reg proc))
        (assign env
                (op extend-environment)
                (const ,formals)
                (reg argl)
                (reg env))))
     (compile-sequence (scan-out-defines (lambda-body exp))
                       'val
                       'return
                       make-label))))


(define (compile-application exp target linkage make-label)
  (let ((proc-code
         (compile (operator exp) 'proc 'next make-label))
        (operand-codes
         (map (lambda (operand)
                (compile operand 'val 'next make-label))
              (operands exp))))
    (preserving
     '(env continue)
     proc-code
     (preserving
      '(proc continue)
      (construct-arglist operand-codes)
      (compile-procedure-call target linkage make-label)))))

(define (construct-arglist operand-codes)
  (let ((operand-codes (reverse operand-codes)))
    (if (null? operand-codes)
        (make-instruction-sequence
         '() '(argl)
         '((assign argl (const ()))))
        (let ((code-to-get-last-arg
               (append-instruction-sequences
                (car operand-codes)
                (make-instruction-sequence
                 '(val) '(argl)
                 '((assign argl (op list) (reg val)))))))
          (if (null? (cdr operand-codes))
              code-to-get-last-arg
              (preserving '(env)
                          code-to-get-last-arg
                          (code-to-get-rest-args
                           (cdr operand-codes))))))))

(define (code-to-get-rest-args operand-codes)
  (let ((code-for-next-arg
         (preserving '(argl)
                     (car operand-codes)
                     (make-instruction-sequence
                      '(val argl) '(argl)
                      '((assign argl
                                (op cons) (reg val) (reg argl)))))))
    (if (null? (cdr operand-codes))
        code-for-next-arg
        (preserving '(env)
                    code-for-next-arg
                    (code-to-get-rest-args (cdr operand-codes))))))

(define (compile-procedure-call target linkage make-label)
  (let ((primitive-branch (make-label 'primitive-branch))
        (compiled-branch (make-label 'compiled-branch))
        (after-call (make-label 'after-call)))
    (let ((compiled-linkage
           (if (eq? linkage 'next) after-call linkage)))
      (append-instruction-sequences
       (make-instruction-sequence
        '(proc) '()
        `((test (op primitive-procedure?) (reg proc))
          (branch (label ,primitive-branch))))
       (parallel-instruction-sequences
        (append-instruction-sequences
         compiled-branch
         (compile-proc-appl target compiled-linkage make-label))
        (append-instruction-sequences
         primitive-branch
         (end-with-linkage
          linkage
          (make-instruction-sequence
           '(proc argl)
           (list target)
           `((assign ,target
                     (op apply-primitive-procedure)
                     (reg proc)
                     (reg argl)))))))
       after-call))))

(define (compile-proc-appl target linkage make-label)
  (cond ((and (eq? target 'val) (not (eq? linkage 'return)))
         (make-instruction-sequence
          '(proc) all-regs
          `((assign continue (label ,linkage))
            (assign val
                    (op compiled-procedure-entry)
                    (reg proc))
            (goto (reg val)))))
        ((and (not (eq? target 'val))
              (not (eq? linkage 'return)))
         (let ((proc-return (make-label 'proc-return)))
           (make-instruction-sequence
            '(proc) all-regs
            `((assign continue (label ,proc-return))
              (assign val
                      (op compiled-procedure-entry)
                      (reg proc))
              (goto (reg val))
              ,proc-return
              (assign ,target (reg val))
              (goto (label ,linkage))))))
        ((and (eq? target 'val) (eq? linkage 'return))
         (make-instruction-sequence
          '(proc continue) all-regs
          '((assign val
                    (op compiled-procedure-entry)
                    (reg proc))
            (goto (reg val)))))
        ((and (not (eq? target 'val)) (eq? linkage 'return))
         (error "return linkage, target not val -- compile"
                target))))

(define all-regs '(env proc val argl continue arg1 arg2))

(define (registers-needed s)
  (if (symbol? s) '() (car s)))

(define (registers-modified s)
  (if (symbol? s) '() (cadr s)))

(define (statements s)
  (if (symbol? s) (list s) (caddr s)))

(define (needs-register? seq reg)
  (memq reg (registers-needed seq)))

(define (modifies-register? seq reg)
  (memq reg (registers-modified seq)))


(define (append-instruction-sequences . seqs)
  (define (append-2-sequences s1 s2)
    (make-instruction-sequence
     (list-union (registers-needed s1)
                 (list-difference (registers-needed s2)
                                  (registers-modified s1)))
     (list-union (registers-modified s1)
                 (registers-modified s2))
     (append (statements s1) (statements s2))))
  (if (null? seqs)
      (empty-instruction-sequence)
      (append-2-sequences
       (car seqs)
       (apply append-instruction-sequences
              (cdr seqs)))))

(define (list-union s1 s2)
  (cond ((null? s1) s2)
        ((memq (car s1) s2) (list-union (cdr s1) s2))
        (else (cons (car s1) (list-union (cdr s1) s2)))))

(define (list-difference s1 s2)
  (cond ((null? s1) s1)
        ((memq (car s1) s2)
         (list-difference (cdr s1) s2))
        (else (cons (car s1)
                    (list-difference (cdr s1) s2)))))

(define (preserving registers s1 s2)
  (if (null? registers)
      (append-instruction-sequences s1 s2)
      (let ((register (car registers)))
        (if (and (modifies-register? s1 register)
                 (needs-register? s2 register))
            (preserving
             (cdr registers)
             (make-instruction-sequence
              (list-union (registers-needed s1)
                          (list register))
              (list-difference (registers-modified s1)
                               (list register))
              (append `((save ,register))
                      (statements s1)
                      `((restore ,register))))
             s2)
            (preserving (cdr registers) s1 s2)))))

(define (tack-on-instruction-sequence seq body-seq)
  (make-instruction-sequence
   (registers-needed seq)
   (registers-modified seq)
   (append (statements seq) (statements body-seq))))

(define (parallel-instruction-sequences s1 s2)
  (make-instruction-sequence
   (list-union (registers-needed s1)
               (registers-needed s2))
   (list-union (registers-modified s1)
               (registers-modified s2))
   (append (statements s1) (statements s2))))


(define (spread-arguments args make-label)
  (assert (equal? 2 (length args)))
  (preserving
   '(env)
   (compile (car args) 'arg1 'next make-label)
   (preserving
    '(arg1)
    (compile (cadr args) 'arg2 'next make-label)
    (make-instruction-sequence '(arg1) '() '()))))

(define (compile-open-coded-primitive-2 exp target linkage make-label)
  (end-with-linkage
   linkage
   (append-instruction-sequences
    (spread-arguments (cdr exp) make-label)
    (make-instruction-sequence
     '(arg1 arg2) (list target)
     `((assign ,target (op ,(car exp)) (reg arg1) (reg arg2)))))))

(define (compile-open-coded-primitive exp target linkage make-label)
  (let ((operands (cdr exp))
        (operation (car exp)))
    (cond ((null? operands)
           (compile (if (eq? operation '*) 1 0)
                    target linkage make-label))
          ((= (length operands) 2)
           (compile-open-coded-primitive-2
            exp target linkage make-label))
          (else
           (compile (list operation
                          (cons (car exp) (cdr operands))
                          (car operands))
                    target
                    linkage
                    make-label)))))


(define (lexical-address-lookup adr env)
  (let ((frame-number (car adr))
        (displacement-number (cadr adr)))
    (if (= frame-number 0)
        (let ((val
               (list-ref (frame-values (first-frame env))
                         displacement-number)))
          (if (eq? val '*unassigned*)
              (error "unassigned variable -- lexical-address-lookup")
              val))
        (lexical-address-lookup
         (list (- frame-number 1) displacement-number)
         (enclosing-environment env)))))

(define (lexical-address-set! adr val env)
  (let ((frame-number (car adr))
        (displacement-number (cadr adr)))
    (if (= frame-number 0)
        (let ((vals
               (list-tail (frame-values (first-frame env))
                          displacement-number)))
          (set-car! vals val))
        (lexical-address-set!
         (list (- frame-number 1) displacement-number)
         val
         (enclosing-environment env)))))

(define (item-index item items)
  (define (iter index items)
    (cond ((null? items) 'not-found)
          ((equal? (car items) item)
           index)
          (else (iter (+ 1 index) (cdr items)))))
  (iter 0 items))

(define (find-variable var lex-env)
  (define (iter frame-number lex-env)
    (if (null? lex-env)
        'not-found
        (let ((offset (item-index var (car lex-env))))
          (if (eq? 'not-found offset)
              (iter (+ 1 frame-number) (cdr lex-env))
              (list frame-number offset)))))
  (iter 0 lex-env))

(define (lex-compile-variable
         exp target linkage make-label env)
  (let ((lex-adr (find-variable exp env)))
    (end-with-linkage
     linkage
     (make-instruction-sequence
      '(env) (list target)
      (if (eq? 'not-found lex-adr)
          `((assign ,target
                    (op lookup-variable-value)
                    (const ,exp) (reg env)))
          `((assign ,target
                    (op lexical-address-lookup)
                    (const ,lex-adr)
                    (reg env))))))))

(define (lex-compile-assignment-helper
         exp target linkage env get-value-code)
  (end-with-linkage
   linkage
   (preserving
    '(env)
    get-value-code
    (make-instruction-sequence
     '(env val) (list target)
     (let* ((var (assignment-variable exp))
            (lex-adr (find-variable var env)))
       (if (eq? 'not-found lex-adr)
           `((perform (op set-variable-value!)
                      (const ,var)
                      (reg val)
                      (reg env))
             (assign ,target (const ok)))
           `((perform (op lexical-address-set!)
                      (const ,lex-adr)
                      (reg val)
                      (reg env))
             (assign ,target (const ok)))))))))

(define (make-let bindings body)
  (cons 'let (cons bindings body)))

(define (filter predicate sequence)
  (cond ((null? sequence) '())
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (scan-out-defines body)
  (let ((bindings
         (map (lambda (definition)
                (list (definition-variable definition)
                      ''*unassigned*))
              (filter definition? body))))
    (if (null? bindings)
        body
        (list (make-let
               bindings
               (map (lambda (exp)
                      (if (definition? exp)
                          `(set! ,(definition-variable exp)
                                 ,(definition-value exp))
                          exp))
                    body))))))


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
   (list 'false? false?)
   (list '+ +)
   (list '- -)
   (list '* *)
   (list '= =)))

(define (eval-compiled exp)
  (let ((machine
         (make-machine
          all-regs
          machine-operations
          (statements
           (compile exp 'val 'next (make-make-label))))))
    (set-register-contents! machine 'env (setup-environment))
    (start machine)
    (get-register-contents machine 'val)))
