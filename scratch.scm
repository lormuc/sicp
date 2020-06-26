(require-extension test)

(define-syntax assert-equal
  (syntax-rules ()
    ((_ a b)
     (let ((b-value b))
       (let ((comparator
              (if (and (number? a)
                       (not (integer? a))
                       (number? b))
                  (lambda (x y) (< (abs (- x y)) 0.001))
                  equal?)))
         (if (not (comparator a b-value))
             (begin (error (list (quote b) 'is b-value 'not a)))))))))


(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "unknown operation -- table" m))))
    dispatch))

(define false #f)

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(define coercion-table (make-table))
(define get-coercion (coercion-table 'lookup-proc))
(define put-coercion (coercion-table 'insert-proc!))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
           "no method for these types -- apply-generic"
           (list op type-tags))))))

(define (attach-tag type-tag contents)
  (if (eq? type-tag 'scheme-number)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (if (number? datum)
      'scheme-number
      (if (pair? datum)
          (car datum)
          'no-tag)))

(define (contents datum)
  (if (number? datum)
      datum
      (if (pair? datum)
          (cdr datum)
          (error "bad tagged datum -- contents" datum))))

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

;; generic selectors

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))


;; constructors for complex numbers

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))

(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))


(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'neg '(scheme-number)
       (lambda (x) (tag (- x))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  (put 'equ '(scheme-number scheme-number) =)
  (put 'is-zero '(scheme-number)
       (lambda (x) (= x 0)))
  (put 'exp '(scheme-number scheme-number)
       (lambda (x y) (tag (expt x y))))
  (put 'raise 'scheme-number
       (lambda (x)
         (make-rational (contents x) 1)))
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  (define (neg-rat x)
    (make-rat (- (numer x)) (denom x)))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'neg '(rational)
       (lambda (x) (tag (neg-rat x))))

  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))

  (put 'equ '(rational rational)
       (lambda (x y)
         (and (= (numer x) (numer y))
              (= (denom x) (denom y)))))
  (put 'is-zero '(rational)
       (lambda (x) (= (numer x) 0)))
  (put 'raise 'rational
       (lambda (x)
         (let ((x (contents x)))
           (make-complex-from-real-imag
            (/ (numer x) (denom x))
            0))))
  (put 'project 'rational
       (lambda (x)
         (let ((x (contents x)))
           (make-scheme-number
            (quotient (numer x)
                      (denom x))))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  (define (neg-complex z)
    (make-from-real-imag (- (real-part z))
                         (- (imag-part z))))

  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'neg '(complex)
       (lambda (z) (tag (neg-complex z))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'equ '(complex complex)
       (lambda (x y)
         (and (= (real-part x) (real-part y))
              (= (imag-part x) (imag-part y)))))
  (put 'is-zero '(complex)
       (lambda (x)
         (and (= (real-part x) 0)
              (= (imag-part x) 0))))
  (put 'project 'complex
       (lambda (x)
         (let ((x (real-part (contents x))))
           (make-rational (numerator x)
                          (denominator x)))))
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(install-rectangular-package)
(install-polar-package)
(install-scheme-number-package)
(install-rational-package)
(install-complex-package)

(put 'real-part '(complex) real-part)
(put 'imag-part '(complex) imag-part)
(put 'magnitude '(complex) magnitude)
(put 'angle '(complex) angle)

(define (equ x y) (apply-generic 'equ x y))

(define (test-equ)
  (assert (equ (make-scheme-number 0)
               (make-scheme-number 0)))
  (assert (not (equ (make-scheme-number 0)
                    (make-scheme-number 2))))
  (assert (equ (make-complex-from-mag-ang 1 1)
               (make-complex-from-mag-ang 1 1)))
  (assert (not (equ (make-complex-from-mag-ang 1 1)
                    (make-complex-from-mag-ang 1 2))))
  (assert (equ (make-rational 2 3)
               (make-rational 2 3)))
  (assert (not (equ (make-rational 1 1)
                    (make-rational 1 2)))))
(test-assert (test-equ))

(define (is-zero x) (apply-generic 'is-zero x))

(define (test-is-zero)
  (assert (is-zero (make-scheme-number 0)))
  (assert (not (is-zero (make-scheme-number 1))))
  (assert (is-zero (make-rational 0 1)))
  (assert (not (is-zero (make-rational 2 3))))
  (assert (is-zero (make-complex-from-real-imag 0 0)))
  (assert (not (is-zero (make-complex-from-real-imag 2 3)))))
(test-assert (test-is-zero))

(define (coerce target-type args)
  (define (loop result loop-args)
    (if (null? loop-args)
        result
        (let ((arg (car loop-args)))
          (if (equal? (type-tag arg) target-type)
              (loop (cons arg result) (cdr loop-args))
              (let ((coercion
                     (get-coercion (type-tag arg)
                                   target-type)))
                (if coercion
                    (loop (cons (coercion arg) result)
                          (cdr loop-args))
                    #f))))))
  (loop '() args))

(define (try-coercions op args)
  (define (loop loop-args)
    (if (null? loop-args)
        #f
        (let ((coerced-args
               (coerce (type-tag (car loop-args))
                       args)))
          (if coerced-args
              (apply apply-generic (cons op coerced-args))
              (loop (cdr loop-args))))))
  (loop args))

(define (multi-raise arg target-type)
  (define (loop loop-arg)
    (if (equal? (type-tag loop-arg) target-type)
        loop-arg
        (let ((raise-proc
               (get 'raise (type-tag loop-arg))))
          (if raise-proc
              (loop (raise-proc loop-arg))
              #f))))
  (and (not (equal? (type-tag arg) target-type))
       (loop arg)))

(define (drop x)
  (let ((project-proc (get 'project (type-tag x))))
    (if project-proc
        (let ((projection (project-proc x)))
          (if (equ (raise projection) x)
              (drop projection)
              x))
        x)))

(define (apply-generic op . args)
  (define (error-no-method op type-tags)
    (error "no method for these types"
           (list op type-tags)))
  (define (coerce-apply-2 op arg-1 arg-2)
    (let ((raise-1-2
           (multi-raise arg-1 (type-tag arg-2))))
      (if raise-1-2
          (apply-generic op raise-1-2 arg-2)
          (let ((raise-2-1
                 (multi-raise arg-2 (type-tag arg-1))))
            (if raise-2-1
                (apply-generic op arg-1 raise-2-1)
                (error-no-method op (list (type-tag arg-1)
                                          (type-tag arg-2))))))))
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (drop (apply proc (map contents args)))
          (if (= (length args) 2)
              (coerce-apply-2 op (car args) (cadr args))
              (error-no-method op type-tags))))))

(put-coercion 'scheme-number 'rational
              (lambda (n)
                (make-rational (contents n) 1)))

(define (raise x)
  ((get 'raise (type-tag x)) x))

(define (test-raise)
  (assert-equal (make-rational 1 1) (raise 1))
  (assert-equal (make-rational 3 1) (raise 3))
  (assert-equal (make-complex-from-real-imag 1 0)
                (raise (make-rational 1 1)))
  (assert-equal (make-complex-from-real-imag 3 0)
                (raise (make-rational 6 2))))
(test-assert (test-raise))

(define (test-apply-generic-raise)
  (assert (equ 0
               (add (make-scheme-number 0)
                    (make-complex-from-real-imag 0 0))))
  (assert (equ (make-complex-from-real-imag 5 1)
               (add (make-scheme-number 2)
                    (make-complex-from-real-imag 3 1))))
  (assert (equ (make-complex-from-real-imag 5 1)
               (add (make-complex-from-real-imag 3 1)
                    (make-scheme-number 2)))))
(test-assert (test-apply-generic-raise))

(define (project x)
  ((get 'project (type-tag x)) x))

(define (test-project)
  (assert-equal (make-rational 1 1)
                (project (make-complex-from-real-imag 1 0)))
  (assert-equal (make-rational 3 1)
                (project (make-complex-from-real-imag 3 5)))
  (assert-equal (make-scheme-number 3)
                (project (make-rational 3 1)))
  (assert-equal (make-scheme-number 1)
                (project (make-rational 3 2)))
  (assert-equal (make-rational 4 3)
                (project (make-complex-from-real-imag 4/3 1/2))))
(test-assert (test-project))

(define (test-drop)
  (assert-equal 1 (drop (make-complex-from-real-imag 1 0)))
  (assert-equal (make-complex-from-real-imag 1 1)
                (drop (make-complex-from-real-imag 1 1)))
  (assert-equal (make-rational 3 2)
                (drop (make-complex-from-real-imag 3/2 0)))
  (assert-equal 0 (drop (make-rational 0 1))))
(test-assert (test-drop))

(define (test-apply-generic-with-drop)
  (assert-equal 0 (add (make-rational -1 2) (make-rational 1 2)))
  (assert-equal 2 (add (make-complex-from-real-imag 4 3)
                       (make-complex-from-real-imag -2 -3))))
(test-assert (test-apply-generic-with-drop))

(define =zero? is-zero)

(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))

  (define (variable? x) (symbol? x))

  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

  ;; representation of terms and term lists
  (define (add-terms l1 l2)
    (cond ((empty-termlist? l1) l2)
          ((empty-termlist? l2) l1)
          (else
           (let ((t1 (first-term l1)) (t2 (first-term l2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 (add-terms (rest-terms l1) l2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     t2 (add-terms l1 (rest-terms l2))))
                   (else
                    (adjoin-term
                     (make-term (order t1)
                                (add (coeff t1) (coeff t2)))
                     (add-terms (rest-terms l1)
                                (rest-terms l2)))))))))

  (define (mul-terms l1 l2)
    (if (empty-termlist? l1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term l1) l2)
                   (mul-terms (rest-terms l1) l2))))

  (define (mul-term-by-all-terms t1 l)
    (if (empty-termlist? l)
        (the-empty-termlist)
        (let ((t2 (first-term l)))
          (adjoin-term
           (make-term (+ (order t1) (order t2))
                      (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms l))))))

  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))

  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))

  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))

  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (term-list p2)))
        (error "polys not in same var -- add-poly"
               (list p1 p2))))

  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                              (term-list p2)))
        (error "polys not in same var -- mul-poly"
               (list p1 p2))))

  (define (neg-terms terms)
    (if (empty-termlist? terms)
        '()
        (let ((term (first-term terms)))
          (adjoin-term (make-term (order term)
                                  (neg (coeff term)))
                       (neg-terms (rest-terms terms))))))

  (define (neg-poly p)
    (make-poly (variable p)
               (neg-terms (term-list p))))

  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 (neg-poly p2)))))

  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))

  (define (terms-zero? terms)
    (or (empty-termlist? terms)
        (and (is-zero (coeff (first-term terms)))
             (terms-zero? (rest-terms terms)))))

  (put 'is-zero '(polynomial)
       (lambda (p) (terms-zero? (term-list p))))
  'done)
(install-polynomial-package)

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

(define (test-polynomial-is-zero)
  (assert (is-zero (make-polynomial 'x '())))
  (assert (not (is-zero (make-polynomial 'x '((1 4) (0 3))))))
  (assert (is-zero (make-polynomial 'x '((1 0) (0 0))))))
(test-assert (test-polynomial-is-zero))

(define (neg x) (apply-generic 'neg x))

(define (test-neg)
  (assert (equ -1 (neg 1)))
  (assert (equ (make-rational -2 1)
               (neg (make-rational 2 1))))
  (assert-equal -1
                (imag-part (neg (make-complex-from-real-imag 1 1)))))
(test-assert (test-neg))

(define (test-polynomial-sub)
  (assert-equal
   (make-polynomial 'x '())
   (sub (make-polynomial 'x '((2 1))) (make-polynomial 'x '((2 1)))))
  (assert-equal
   (make-polynomial 'x '((1 2) (0 1)))
   (sub (make-polynomial 'x '((1 3) (0 2)))
        (make-polynomial 'x '((1 1) (0 1))))))
(test-assert (test-polynomial-sub))
