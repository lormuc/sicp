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
     (compile-sequence (lambda-body exp)
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
         (compile-proc-appl target compiled-linkage))
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

(define (compile-proc-appl target linkage)
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

(define all-regs '(env proc val argl continue))

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
