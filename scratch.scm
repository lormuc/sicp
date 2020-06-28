(import (chicken random))
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

(define square (lambda (x) (* x x)))

(define (make-monitored f)
  (let ((count 0))
    (lambda (x)
      (cond ((eq? x 'reset-count)
             (set! count 0)
             count)
            ((eq? x 'how-many-calls?)
             count)
            (else
             (set! count (+ 1 count))
             (f x))))))

(test-group
 "make-monitored"
 (test
  '(0 0)
  (let ((s (make-monitored (lambda (x) (* 2 x)))))
    (list (s 'how-many-calls?) (s 0))))
 (test
  '(4 9 2 1 0 0)
  (let ((s (make-monitored square)))
    (list (s 2) (s 3) (s 'how-many-calls?)
          (s 1)
          (s 'reset-count)
          (s 'how-many-calls?)))))

(define cops-called #f)
(define (call-the-cops)
  (set! cops-called #t))

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "unknown request -- make-account"
                       m))))
  (let ((attempt-count 0))
    (lambda (given-password message)
      (if (not (eq? given-password password))
          (begin
            (set! attempt-count (+ 1 attempt-count))
            (if (> attempt-count 7)
                (call-the-cops))
            (lambda (x) "incorrect password"))
          (begin
            (set! attempt-count 0)
            (dispatch message))))))

(test-group
 "make-account password"
 (test
  '(0)
  (let ((acc (make-account 0 'password)))
    (list ((acc 'password 'deposit) 0))))
 (test
  '(60 "incorrect password")
  (let ((acc (make-account 100 'secret-password)))
    (list ((acc 'secret-password 'withdraw) 40)
          ((acc 'some-other-password 'deposit) 50)))))

(test-group
 "make-account password cops"
 (test
  '("incorrect password"
    "incorrect password"
    "incorrect password"
    "incorrect password"
    "incorrect password"
    "incorrect password"
    "incorrect password"
    #f
    "incorrect password"
    #t)
  (let ((acc (make-account 50 'password)))
    (list ((acc 'incorrect-password 'deposit) 40)
          ((acc 'incorrect-password 'deposit) 40)
          ((acc 'incorrect-password 'deposit) 40)
          ((acc 'incorrect-password 'deposit) 40)
          ((acc 'incorrect-password 'deposit) 40)
          ((acc 'incorrect-password 'deposit) 40)
          ((acc 'incorrect-password 'deposit) 40)
          cops-called
          ((acc 'incorrect-password 'deposit) 40)
          cops-called)))
 (set! cops-called #f))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (random-in-range low high)
  (+ low (* (pseudo-random-real) (- high low))))

(define (estimate-integral p x1 x2 y1 y2 trials)
  (let ((rectangle-area (* (- x2 x1) (- y2 y1))))
    (* rectangle-area
       (monte-carlo
        trials
        (lambda ()
          (p (random-in-range x1 x2)
             (random-in-range y1 y2)))))))

(display
 (exact->inexact
  (estimate-integral
   (lambda (x y)
     (<= (+ (* x x) (* y y)) 1))
   -1 1 -1 1
   3000000)))
(newline)
