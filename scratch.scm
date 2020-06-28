(require-extension test)

(define true #t)
(define false #f)

(define debug #f)
(define (log-line . args)
  (if debug
      (begin (display args)
             (newline))))

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

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(define debug #f)

(define (make-accumulator n)
  (lambda (m)
    (set! n (+ n m))
    n))

(test-group
 "accumulator"
 (test
  '(1 2 3)
  (let ((acc (make-accumulator 0)))
    (list (acc 1) (acc 1) (acc 1))))
 (test
  '(3 4 5)
  (let ((acc (make-accumulator 2)))
    (list (acc 1) (acc 1) (acc 1)))))
