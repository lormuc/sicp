(test-begin)

(test
 5
 (ec-eval '(+ 2 3)))

(test '(unknown-procedure-type-error 3)
      (ec-eval '(3)))

(test 0 (ec-eval '(cond (#t 0))))
(test 2 (ec-eval '(cond (#f 0) (#f 1) (#t 2) (#t 3))))
(test 3 (ec-eval '(cond (#f 1) (else 3))))
(test #f (ec-eval '(cond)))
(test 5 (ec-eval '(cond (#f #f)
                        (#t (cond (#f #f)
                                  (else 5))))))

(test 1 (ec-eval '(let () 1)))
(test 0 (ec-eval '(let ((x 0)) x)))
(test 3 (ec-eval '(let ((y 1) (z 2)) (+ z 1))))

(test 0 ((make-stack) 'get-push-count))
(test 0 ((make-stack) 'get-max-depth))
(test 2 (let ((s (make-stack)))
          ((s 'push) 0)
          ((s 'push) 0)
          (s 'pop)
          (s 'get-push-count)))
(test 3 (let ((s (make-stack)))
          ((s 'push) 0)
          ((s 'push) 0)
          ((s 'push) 0)
          (s 'pop)
          ((s 'push) 0)
          (s 'get-max-depth)))

(test '(0)
      (map car (ec-eval-program '(0))))
(test '(ok 3)
      (map car (ec-eval-program '((define x 3) x))))

(test 0 (ec-eval '(let ((x 0))
                    ((let ((x 1)) (lambda (y) y)) x))))

(test
 '((assign a (const 0))
   (assign b (const 1)))
 (statements (make-instruction-sequence
              '() '(a b)
              '((assign a (const 0))
                (assign b (const 1))))))

(test 'x-0 ((make-make-label) 'x))
(test '(label-0 label-1)
      (let* ((make-label (make-make-label))
             (l-0 (make-label 'label))
             (l-1 (make-label 'label)))
        (list l-0 l-1)))

(test
 (make-instruction-sequence
  '() '(val) '((assign val (const 5))))
 (compile 5 'val 'next (make-make-label)))
(test (make-instruction-sequence
       '(continue) '(exp) '((assign exp (const 3))
                            (goto (reg continue))))
      (compile 3 'exp 'return (make-make-label)))

(test
 (make-instruction-sequence
  '() '() '((goto (label x))))
 (compile-linkage 'x))
(test
 (make-instruction-sequence
  '(continue) '() '((goto (reg continue))))
 (compile-linkage 'return))
(test (empty-instruction-sequence)
      (compile-linkage 'next))

(test
 (make-instruction-sequence
  '(continue) '()
  '((save continue)
    (assign continue (const 0))
    (restore continue)
    (goto (reg continue))))
 (end-with-linkage
  'return
  (make-instruction-sequence
   '() '(continue) '((assign continue (const 0))))))

(test
 (list
  '(env continue) '(val)
  '((assign val
            (op lookup-variable-value)
            (const x) (reg env))
    (goto (reg continue))))
 (make-instruction-sequence
  '(env continue) '(val)
  '((assign val
            (op lookup-variable-value)
            (const x) (reg env))
    (goto (reg continue)))))

(test '(a c e) (list-difference '(a b c d e f) '(b d f)))

(test (list '() '() '()) (empty-instruction-sequence))

(test (empty-instruction-sequence)
      (append-instruction-sequences))
(test
 (make-instruction-sequence
  (list-union '(a b) (list-difference '(a c d) '(c d)))
  (list-union '(c d) '(d e f))
  '((assign c (reg a))
    (assign d (reg a))))
 (append-instruction-sequences
  (make-instruction-sequence '(a b) '(c d)
                             '((assign c (reg a))))
  (make-instruction-sequence '(a c d) '(d e f)
                             '((assign d (reg a))))))

(test
 '((assign x (const 0))
   (assign v (reg w)))
 (statements
  (preserving
   '(x y)
   (make-instruction-sequence '() '(x)
                              '((assign x (const 0))))
   (make-instruction-sequence '(w) '(v)
                              '((assign v (reg w)))))))

(test
 '((save b)
   (save a)
   (assign a (const 0))
   (assign b (const 1))
   (restore a)
   (restore b)
   (assign c (reg a)))
 (statements
  (preserving
   '(a b)
   (make-instruction-sequence '() '(a b)
                              '((assign a (const 0))
                                (assign b (const 1))))
   (make-instruction-sequence '(a b) '(c d)
                              '((assign c (reg a)))))))

(test (make-instruction-sequence
       '(continue) '(exp) '((assign exp (const 3))
                            (goto (reg continue))))
      (compile-self-evaluating 3 'exp 'return (make-make-label)))

(test (make-instruction-sequence
       '() '(val) '((assign val (const a))))
      (compile-quoted '(quote a) 'val 'next (make-make-label)))

(test (make-instruction-sequence
       '(env) '(val)
       '((assign val
                 (op lookup-variable-value)
                 (const x) (reg env))))
      (compile-variable 'x 'val 'next (make-make-label)))

(test (make-instruction-sequence
       '(env) '(val)
       '((assign val (const 0))
         (perform (op set-variable-value!)
                  (const x) (reg val) (reg env))
         (assign val (const ok))))
      (compile-assignment '(set! x 0) 'val 'next (make-make-label)))

(test (make-instruction-sequence
       '(env) '(val)
       '((assign val (const 1))
         (perform (op define-variable!)
                  (const y) (reg val) (reg env))
         (assign val (const ok))))
      (compile-definition '(define y 1) 'val 'next (make-make-label)))

(test
 (empty-instruction-sequence)
 (parallel-instruction-sequences
  (empty-instruction-sequence) (empty-instruction-sequence)))

(test
 (make-instruction-sequence
  '(a b c d) '(c d e)
  '((assign c (op +) (reg a) (reg b))
    (assign d (op +) (reg a) (reg b))
    (goto (label after))
    (assign e (op +) (reg c) (reg d))))
 (parallel-instruction-sequences
  (make-instruction-sequence '(a b) '(c d)
                             '((assign c (op +) (reg a) (reg b))
                               (assign d (op +) (reg a) (reg b))
                               (goto (label after))))
  (make-instruction-sequence '(c d) '(e)
                             '((assign e (op +) (reg c) (reg d))))))

(test (compile 'a 'val 'next (make-make-label))
      (compile-sequence
       '(a) 'val 'next (make-make-label)))

(test
 (let ((make-label (make-make-label)))
   (preserving '(continue env)
               (compile '(set! a 0) 'val 'next make-label)
               (preserving '(continue env)
                           (compile '(set! a 1) 'val 'next make-label)
                           (compile 'a 'val 'return make-label))))
 (compile-sequence '((set! a 0) (set! a 1) a)
                   'val 'return (make-make-label)))

(test 3 (eval-compiled 3))
(test 5 (eval-compiled '((lambda (x y) (+ x y)) 3 2)))

(test
 (append-instruction-sequences
  (compile '0 'arg1 'next (make-make-label))
  (compile '1 'arg2 'next (make-make-label)))
 (spread-arguments '(0 1) (make-make-label)))

(test
 (append-instruction-sequences
  (compile '0 'arg1 'next (make-make-label))
  (preserving
   '(arg1)
   (compile '(+ 2 2) 'arg2 'next (make-make-label))
   (make-instruction-sequence '(arg1) '() '())))
 (spread-arguments '(0 (+ 2 2)) (make-make-label)))

(test
 (append-instruction-sequences
  (spread-arguments '(1 2) (make-make-label))
  (make-instruction-sequence
   '(arg1 arg2) '(val)
   '((assign val (op *) (reg arg1) (reg arg2)))))
 (compile-open-coded-primitive
  '(* 1 2) 'val 'next (make-make-label)))

(test
 (compile-open-coded-primitive
  '(+ 2 3) 'val 'next (make-make-label))
 (compile '(+ 2 3) 'val 'next (make-make-label)))

(test 4 (eval-compiled '(+ 0 (+ 2 2))))

(test
 (compile '(+ (+ 2 3) 1) 'val 'next (make-make-label))
 (compile-open-coded-primitive '(+ 1 2 3) 'val 'next (make-make-label)))

(test
 (compile '(* (* 3 4) 2) 'val 'next (make-make-label))
 (compile-open-coded-primitive '(* 2 3 4) 'val 'next (make-make-label)))

(test
 (compile '(* (* (* 7 8) 6) 5) 'arg1 'label-0 (make-make-label))
 (compile-open-coded-primitive '(* 5 6 7 8) 'arg1 'label-0 (make-make-label)))

(test
 (compile '1 'val 'next (make-make-label))
 (compile-open-coded-primitive '(*) 'val 'next (make-make-label)))

(test
 (compile '(- 4 3) 'val 'next (make-make-label))
 (compile-open-coded-primitive '(- 4 3) 'val 'next (make-make-label)))

(test
 (compile '(= x y) 'val 'next (make-make-label))
 (compile-open-coded-primitive '(= x y) 'val 'next (make-make-label)))

(test 0 (eval-compiled '(+)))
(test 2 (eval-compiled '(+ (+ 1 1))))
(test 10 (eval-compiled '(+ 1 2 3 4)))
(test 11 (eval-compiled '(+ (+ 1 2 1) (+ 1 1 1) (+ 2 1 1))))
(test 6 (eval-compiled '(+ (let ((x 3)) x) (let ((x 1)) x) 2)))

(test 6 (eval-compiled '(* 1 2 3)))
(test 16 (eval-compiled '(* (* 1 2) (* 2 1 2) (* 1 2))))
(test 1 (eval-compiled '(*)))
(test 3 (eval-compiled '(* (+ 1 2))))

(test (eval-compiled 'false) (eval-compiled '(= 3 4)))
(test (eval-compiled 'true) (eval-compiled '(= 5 5)))

(test 3 (eval-compiled '(- 6 3)))
(test 0 (eval-compiled '(- (+ 1 2) 3)))

(test 24 (eval-compiled (quote (+ (*) (let ((x 2) (y 3)) (* x y 2))
                                  (let ((x 5) (y 2)) (+ x (* 2 y 1) 1))
                                  (- 4 4)
                                  (- 5 4)))))

(test 3 (eval-compiled '(let ((x 2)) (+ (let ((x 1)) 1) x))))

(test-end)
