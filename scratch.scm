(import (chicken random))
(require-extension test)

(define true #t)
(define false #f)

(define debug #f)
(define (log-line . args)
  (if debug
      (begin (display args)
             (newline))))

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

(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (dispatch m)
      (cond ((eq? m 'empty?)
             (null? front-ptr))
            ((eq? m 'front)
             (if (null? front-ptr)
                 (error "front called with an empty queue")
                 (car front-ptr)))
            ((eq? m 'insert!)
             (lambda (item)
               (let ((new-pair (cons item '())))
                 (cond ((null? front-ptr)
                        (set! front-ptr new-pair)
                        (set! rear-ptr new-pair))
                       (else
                        (set-cdr! rear-ptr new-pair)
                        (set! rear-ptr new-pair))))))
            ((eq? m 'delete!)
             (if (null? front-ptr)
                 (error "delete on empty queue")
                 (set! front-ptr (cdr front-ptr))))
            (else
             (error "unknown operation -- queue" m))))
    dispatch))

(define (empty-queue? queue)
  (queue 'empty?))
(define (front-queue queue)
  (queue 'front))
(define (insert-queue! queue item)
  ((queue 'insert!) item))
(define (delete-queue! queue)
  (queue 'delete!))

(test-group
 "queue"
 (test
  'x
  (let ((queue (make-queue)))
    (insert-queue! queue 'x)
    (front-queue queue)))
 (test
  '(a b)
  (let ((queue (make-queue)))
    (insert-queue! queue 'a)
    (insert-queue! queue 'b)
    (insert-queue! queue 'c)
    (let ((front-0 (front-queue queue)))
      (delete-queue! queue)
      (list front-0 (front-queue queue))))))

(define (make-deque) (cons '() '()))
(define (deque-empty? deque) (null? (car deque)))
(define (deque-front-ptr deque) (car deque))
(define (deque-rear-ptr deque) (cdr deque))
(define (deque-set-front-ptr! deque item) (set-car! deque item))
(define (deque-set-rear-ptr! deque item) (set-cdr! deque item))

(define (deque-front deque)
  (if (deque-empty? deque)
      (error "deque-front called with an empty deque")
      (caar (deque-front-ptr deque))))

(define (deque-rear deque)
  (if (deque-empty? deque)
      (error "deque-rear called with an empty deque")
      (caar (deque-rear-ptr deque))))

(define (deque-rear-insert! deque item)
  (let ((new-rear
         (cons (cons item (deque-rear-ptr deque))
               '())))
    (cond ((deque-empty? deque)
           (deque-set-front-ptr! deque new-rear)
           (deque-set-rear-ptr! deque new-rear))
          (else
           (set-cdr! (car (deque-rear-ptr deque)) new-rear)
           (deque-set-rear-ptr! deque new-rear)))))

(define (deque-rear-delete! deque)
  (cond ((deque-empty? deque)
         (error "deque-rear-delete! called with an empty deque"))
        (else
         (let ((rear-ptr (deque-rear-ptr deque))
               (front-ptr (deque-front-ptr deque)))
           (cond ((eq? rear-ptr front-ptr)
                  (deque-set-front-ptr! deque '())
                  (deque-set-rear-ptr! deque '()))
                 (else
                  (deque-set-rear-ptr! deque (cdr (car rear-ptr)))
                  (set-cdr! (deque-rear-ptr deque) '())))))))

(define (deque-front-insert! deque item)
  (let ((new-front
         (cons (cons item '())
               (deque-front-ptr deque))))
    (cond ((deque-empty? deque)
           (deque-set-front-ptr! deque new-front)
           (deque-set-rear-ptr! deque new-front))
          (else
           (set-cdr! (car (deque-front-ptr deque)) new-front)
           (deque-set-front-ptr! deque new-front)))))

(define (deque-front-delete! deque)
  (cond ((deque-empty? deque)
         (error "deque-front-delete! called with an empty deque"))
        (else
         (let ((rear-ptr (deque-rear-ptr deque))
               (front-ptr (deque-front-ptr deque)))
           (cond ((eq? rear-ptr front-ptr)
                  (deque-set-front-ptr! deque '())
                  (deque-set-rear-ptr! deque '()))
                 (else
                  (deque-set-front-ptr! deque (cdr front-ptr))
                  (set-cdr! (car (deque-front-ptr deque)) '())))))))

(test-group
 "deque"
 (test
  'x
  (let ((deque (make-deque)))
    (deque-front-insert! deque 'x)
    (deque-front deque)))
 (test
  '(b d)
  (let ((deque (make-deque)))
    (deque-front-insert! deque 'a)
    (deque-front-delete! deque)
    (deque-front-insert! deque 'b)
    (deque-rear-insert! deque 'c)
    (deque-rear-insert! deque 'd)
    (deque-rear-insert! deque 'e)
    (deque-rear-delete! deque)
    (list (deque-front deque)
          (deque-rear deque)))))

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

(define (make-table)
  (list '*table*))

(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        false)))

(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons key value) (cdr table)))))
  'ok)

(define (make-binary-tree)
  (define (make-empty-tree)
    (list '() '() '()))
  (define (tree-empty? tree)
    (null? (car tree)))
  (define (tree-set-entry! tree entry)
    (set-car! tree entry))
  (define (tree-entry tree)
    (car tree))
  (define (tree-set-left-branch! tree branch)
    (set-car! (cdr tree) branch))
  (define (tree-left-branch tree)
    (cadr tree))
  (define (tree-set-right-branch! tree branch)
    (set-car! (cddr tree) branch))
  (define (tree-right-branch tree)
    (caddr tree))
  (let ((tree (make-empty-tree)))
    (define (insert! a-key a-value a-tree)
      (cond ((tree-empty? a-tree)
             (tree-set-entry! a-tree (cons a-key a-value))
             (tree-set-left-branch! a-tree (make-empty-tree))
             (tree-set-right-branch! a-tree (make-empty-tree)))
            ((= a-key (car (tree-entry a-tree)))
             (set-cdr! (tree-entry a-tree) a-value))
            ((< a-key (car (tree-entry a-tree)))
             (insert! a-key a-value (tree-left-branch a-tree)))
            ((> a-key (car (tree-entry a-tree)))
             (insert! a-key a-value (tree-right-branch a-tree)))))
    (define (lookup a-key a-tree)
      (cond ((tree-empty? a-tree) #f)
            ((= a-key (car (tree-entry a-tree)))
             (tree-entry a-tree))
            ((< a-key (car (tree-entry a-tree)))
             (lookup a-key (tree-left-branch a-tree)))
            ((> a-key (car (tree-entry a-tree)))
             (lookup a-key (tree-right-branch a-tree)))))
    (define (dispatch m)
      (cond ((eq? m 'lookup)
             (lambda (key) (lookup key tree)))
            ((eq? m 'insert!)
             (lambda (key value) (insert! key value tree)))
            (else (error "unknown request -- binary-tree" m))))
    dispatch))

(define (binary-tree-lookup tree key)
  ((tree 'lookup) key))

(define (binary-tree-insert! tree key value)
  ((tree 'insert!) key value))

(test-group
 "mutable binary tree"
 (test
  '(#f)
  (let ((binary-tree (make-binary-tree)))
    (list (binary-tree-lookup binary-tree 0))))
 (test
  '(a b c)
  (let ((binary-tree (make-binary-tree)))
    (binary-tree-insert! binary-tree 1 'a)
    (binary-tree-insert! binary-tree 3 'b)
    (binary-tree-insert! binary-tree 0 'z)
    (binary-tree-insert! binary-tree 0 'c)
    (list (cdr (binary-tree-lookup binary-tree 1))
          (cdr (binary-tree-lookup binary-tree 3))
          (cdr (binary-tree-lookup binary-tree 0))))))

(define (make-fast-table)
  (make-binary-tree))

(define (fast-table-lookup key table)
  (binary-tree-lookup table key))

(define (fast-table-insert! key value table)
  (binary-tree-insert! table key value)
  'ok)

(test-group
 "table with binary tree"
 (test
  '(1 3 0 #f)
  (let ((table (make-fast-table)))
    (fast-table-insert! 0 1 table)
    (fast-table-insert! 1 3 table)
    (fast-table-insert! 2 0 table)
    (list (cdr (fast-table-lookup 0 table))
          (cdr (fast-table-lookup 1 table))
          (cdr (fast-table-lookup 2 table))
          (fast-table-lookup 3 table)))))


(define (make-wire)
  (let ((signal-value 0) (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "unknown operation -- wire" m))))
    dispatch))

(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin
        ((car procedures))
        (call-each (cdr procedures)))))

(define (get-signal wire)
  (wire 'get-signal))

(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))

(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

(define (after-delay delay action)
  (if (= delay 0)
      (action)
      (add-to-agenda! (+ delay (current-time the-agenda))
                      action
                      the-agenda)))

(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (newline)
                 (display name)
                 (display " ")
                 (display (current-time the-agenda))
                 (display "  new-value = ")
                 (display (get-signal wire)))))

(define (make-time-segment time queue)
  (cons time queue))
(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))

(define (make-agenda) (list 0))

(define (current-time agenda) (car agenda))
(define (set-current-time! agenda time)
  (set-car! agenda time))

(define (segments agenda) (cdr agenda))
(define (set-segments! agenda segments)
  (set-cdr! agenda segments))
(define (first-segment agenda) (car (segments agenda)))
(define (rest-segments agenda) (cdr (segments agenda)))

(define (empty-agenda? agenda)
  (null? (segments agenda)))

(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (car segments)))))
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
        (insert-queue! (segment-queue (car segments))
                       action)
        (let ((rest (cdr segments)))
          (if (belongs-before? rest)
              (set-cdr!
               segments
               (cons (make-new-time-segment time action)
                     (cdr segments)))
              (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments!
         agenda
         (cons (make-new-time-segment time action)
               segments))
        (add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
        (set-segments! agenda (rest-segments agenda)))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "agenda is empty -- first-agenda-item")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))

(define the-agenda (make-agenda))
(define and-gate-delay 0)
(define inverter-delay 0)
(define or-gate-delay 0)

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "invalid signal" s))))

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (logical-and x y)
  (if (and (= x 1) (= y 1))
      1
      0))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (logical-or x y)
  (if (or (= x 1) (= y 1))
      1
      0))

(define (or-gate x y output)
  (let ((inv-x (make-wire))
        (inv-y (make-wire))
        (inv-and (make-wire)))
    (inverter x inv-x)
    (inverter y inv-y)
    (and-gate inv-x inv-y inv-and)
    (inverter inv-and output)
    'ok))

(test-group
 "or-gate"
 (let ((check
        (lambda (x y z)
          (test
           z
           (let ((a (make-wire))
                 (b (make-wire))
                 (c (make-wire)))
             (or-gate a b c)
             (set-signal! a x)
             (set-signal! b y)
             (get-signal c))))))
   (check 0 0 0)
   (check 0 1 1)
   (check 1 0 1)
   (check 1 1 1)))

(define debug #t)
