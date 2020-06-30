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

(define random-init 0)

(define (rand-update x)
  (+ 1 x))

(define rand
  (let ((x random-init))
    (lambda (m)
      (cond ((eq? m 'generate)
             (set! x (rand-update x))
             x)
            ((eq? m 'reset)
             (lambda (new-x)
               (set! x new-x)
               x))
            (else (error "unknown request -- rand" m))))))

(test-group
 "rand reset"
 (test
  '(1 2 5 6)
  (list (rand 'generate)
        (rand 'generate)
        ((rand 'reset) 5)
        (rand 'generate))))

(define (make-joint acc acc-pass new-pass)
  (lambda (pass action)
    (if (eq? pass new-pass)
        (acc acc-pass action)
        (lambda (x) "incorrect password"))))

(test-group
 "make-joint"
 (test
  "incorrect password"
  (let ((peter-acc (make-account 100 'open-sesame)))
    (let ((paul-acc
           (make-joint peter-acc 'open-sesame 'rosebud)))
      ((paul-acc 'open-sesame 'withdraw) 1))))
 (test
  (list 50 50)
  (let ((peter-acc (make-account 100 'open-sesame)))
    (let ((paul-acc
           (make-joint peter-acc 'open-sesame 'rosebud)))
      (list ((paul-acc 'rosebud 'withdraw) 50)
            ((peter-acc 'open-sesame 'withdraw) 0))))))

(define s -1)

(define (f x)
  (if (= s -1)
      (set! s x))
  (/ s 2))

(let ((x (f 0)))
  (let ((y (f 1)))
    (display (+ x y))
    (newline)))

(set! s -1)
(let ((x (f 1)))
  (let ((y (f 0)))
    (display (+ x y))
    (newline)))

(define (count-pairs y)
  (define visited-pairs '())
  (define (rec x)
    (if (or (not (pair? x))
            (memq x visited-pairs))
        0
        (begin
          (set! visited-pairs (cons x visited-pairs))
          (+ (rec (car x))
             (rec (cdr x))
             1))))
  (rec y))

(test-group
 "count-pairs"
 (test 3 (count-pairs '(a b c)))
 (test
  3
  (let ((yy '(b c)))
    (let ((y (cons (cdr yy) yy)))
      (count-pairs y))))
 (test
  3
  (let ((zzz '(a)))
    (let ((zz (cons zzz zzz)))
      (let ((z (cons zz zz)))
        (count-pairs z)))))
 (test
  3
  (let ((w '(c a b)))
    (set-car! w w)
    (count-pairs w)))
 )

(define (has-cycle y)
  (let ((visited-pairs '()))
    (define (helper x)
      (cond ((null? x)
             #f)
            ((memq x visited-pairs)
             #t)
            (else
             (set! visited-pairs (cons x visited-pairs))
             (helper (cdr x)))))
    (helper y)))

(test-group
 "has-cycle"
 (test
  #f
  (let ((x '(0 1 2)))
    (has-cycle x)))
 (test
  #t
  (let ((x (cons 0 0)))
    (set-cdr! x x)
    (has-cycle x))))

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))
(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "front called with an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "delete! called with an empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue)))

(define (print-queue queue)
  (display (front-ptr queue)))

(define queue (make-queue))
(insert-queue! queue 'a)
(insert-queue! queue 'b)
(insert-queue! queue 'c)
(insert-queue! queue 'd)
(print-queue queue)
(newline)
(delete-queue! queue)

(define debug #t)
