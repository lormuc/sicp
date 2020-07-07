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
                 (display name)
                 (display " ")
                 (display (current-time the-agenda))
                 (display "  new-value = ")
                 (display (get-signal wire))
                 (newline))))

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
(define inverter-delay 0)
(define and-gate-delay 0)
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

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
           (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

(define (half-adder a b s c)
  (let ((d (make-wire))
        (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
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

(define (ripple-carry-adder a b sum carry)
  (if (null? a)
      (set-signal! carry 0)
      (let ((carry-aux (make-wire)))
        (full-adder (car a) (car b) carry-aux
                    (car sum) carry)
        (ripple-carry-adder (cdr a) (cdr b)
                            (cdr sum) carry-aux))))

(define (set-signals! signals values)
  (if (null? values)
      '()
      (begin
        (set-signal! (car signals) (car values))
        (set-signals! (cdr signals) (cdr values)))))

(define (get-signals signals)
  (map get-signal signals))

(define (make-wires n)
  (define (helper i result)
    (if (= i 0)
        result
        (helper (- i 1) (cons (make-wire) result))))
  (helper n '()))

(test-group
 "ripple-carry-adder"
 (define (check a-bits b-bits s-bits c-bit)
   (test
    (list s-bits c-bit)
    (let ((a (make-wires (length a-bits)))
          (b (make-wires (length b-bits)))
          (s (make-wires (length s-bits)))
          (c (make-wire)))
      (ripple-carry-adder a b s c)
      (set-signals! a a-bits)
      (set-signals! b b-bits)
      (list (get-signals s) (get-signal c)))))
 (check '(1) '(1) '(0) 1)
 (check '(1 0 1) '(0 0 1) '(1 1 0) 0)
 (check '(1 1 1 1) '(0 0 0 1) '(0 0 0 0) 1)
 (check '(0 1 0 0) '(0 1 0 0) '(1 0 0 0) 0))

(define (celsius-fahrenheit-converter c f)
  (let ((u (make-connector))
        (v (make-connector))
        (w (make-connector))
        (x (make-connector))
        (y (make-connector)))
    (multiplier c w u)
    (multiplier v x u)
    (adder v y f)
    (constant 9 w)
    (constant 5 x)
    (constant 32 y)
    'ok))

(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond ((and (has-value? a1) (has-value? a2))
           (set-value! sum
                       (+ (get-value a1) (get-value a2))
                       me))
          ((and (has-value? a1) (has-value? sum))
           (set-value! a2
                       (- (get-value sum) (get-value a1))
                       me))
          ((and (has-value? a2) (has-value? sum))
           (set-value! a1
                       (- (get-value sum) (get-value a2))
                       me))))
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'i-have-a-value)
           (process-new-value))
          ((eq? request 'i-lost-my-value)
           (process-forget-value))
          (else
           (error "unknown request -- adder" request))))
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)

(define (inform-about-value constraint)
  (constraint 'i-have-a-value))

(define (inform-about-no-value constraint)
  (constraint 'i-lost-my-value))

(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond ((or (and (has-value? m1) (= (get-value m1) 0))
               (and (has-value? m2) (= (get-value m2) 0)))
           (set-value! product 0 me))
          ((and (has-value? m1) (has-value? m2))
           (set-value! product
                       (* (get-value m1) (get-value m2))
                       me))
          ((and (has-value? product) (has-value? m1))
           (set-value! m2
                       (/ (get-value product) (get-value m1))
                       me))
          ((and (has-value? product) (has-value? m2))
           (set-value! m1
                       (/ (get-value product) (get-value m2))
                       me))))
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'i-have-a-value)
           (process-new-value))
          ((eq? request 'i-lost-my-value)
           (process-forget-value))
          (else
           (error "unknown request -- multiplier" request))))
  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)

(define (constant value connector)
  (define (me request)
    (error "unknown request -- constant" request))
  (connect connector me)
  (set-value! connector value me)
  me)

(define (probe name connector)
  (define (print-probe value)
    (display "probe: ")
    (display name)
    (display " = ")
    (display value)
    (newline))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value)
    (print-probe "?"))
  (define (me request)
    (cond ((eq? request 'i-have-a-value)
           (process-new-value))
          ((eq? request 'i-lost-my-value)
           (process-forget-value))
          (else
           (error "unknown request -- probe" request))))
  (connect connector me)
  me)

(define (make-connector)
  (let ((value false) (informant false) (constraints '()))
    (define (set-my-value newval setter)
      (cond ((not (has-value? me))
             (set! value newval)
             (set! informant setter)
             (for-each-except setter
                              inform-about-value
                              constraints))
            ((not (= value newval))
             (error "contradiction" (list value newval)))
            (else 'ignored)))
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
          (begin (set! informant false)
                 (for-each-except retractor
                                  inform-about-no-value
                                  constraints))
          'ignored))
    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
          (set! constraints
                (cons new-constraint constraints)))
      (if (has-value? me)
          (inform-about-value new-constraint))
      'done)
    (define (me request)
      (cond ((eq? request 'has-value?)
             (if informant true false))
            ((eq? request 'value) value)
            ((eq? request 'set-value!) set-my-value)
            ((eq? request 'forget) forget-my-value)
            ((eq? request 'connect) connect)
            (else (error "unknown operation -- connector"
                         request))))
    me))

(define (for-each-except exception procedure list)
  (define (loop items)
    (cond ((null? items) 'done)
          ((eq? (car items) exception) (loop (cdr items)))
          (else (procedure (car items))
                (loop (cdr items)))))
  (loop list))

(define (has-value? connector)
  (connector 'has-value?))

(define (get-value connector)
  (connector 'value))

(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))

(define (forget-value! connector retractor)
  ((connector 'forget) retractor))

(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))

(define c (make-connector))
(define f (make-connector))
(celsius-fahrenheit-converter c f)
(probe "celsius temp" c)
(probe "fahrenheit temp" f)
(set-value! c 25 'user)

(define (averager x y z)
  (let ((w (make-connector))
        (two (make-connector)))
    (adder x y w)
    (multiplier two z w)
    (constant 2 two)
    'ok))

(test-group
 "averager"
 (test
  2
  (let ((x (make-connector))
        (y (make-connector))
        (z (make-connector)))
    (averager x y z)
    (set-value! x 1 'user)
    (set-value! y 3 'user)
    (get-value z)))
 (test
  8
  (let ((x (make-connector))
        (y (make-connector))
        (z (make-connector)))
    (averager x y z)
    (set-value! x 0 'user)
    (set-value! z 4 'user)
    (get-value y))))

(define (squarer a b)
  (define (process-new-value)
    (if (has-value? b)
        (if (< (get-value b) 0)
            (error "square less than 0 -- squarer" (get-value b))
            (set-value! a (sqrt (get-value b)) me))
        (if (has-value? a)
            (set-value! b (* (get-value a) (get-value a)) me))))
  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'i-have-a-value)
           (process-new-value))
          ((eq? request 'i-lost-my-value)
           (process-forget-value))
          (else
           (error "unknown request -- square" request))))
  (connect a me)
  (connect b me)
  me)

(test-group
 "squarer"
 (test
  16
  (let ((x (make-connector))
        (y (make-connector)))
    (squarer x y)
    (set-value! x 4 'user)
    (get-value y)))
 (test
  2
  (let ((x (make-connector))
        (y (make-connector)))
    (squarer x y)
    (set-value! y 4 'user)
    (get-value x))))

(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))

(define (c- x y)
  (let ((z (make-connector)))
    (adder z y x)
    z))

(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))

(define (c/ x y)
  (let ((z (make-connector)))
    (multiplier y z x)
    z))

(define (cv x)
  (let ((z (make-connector)))
    (constant x z)
    z))

(test-group
 "c+ c* c/ cv"
 (let ((celsius-fahrenheit-converter
        (lambda (x)
          (c+ (c* (c/ (cv 9) (cv 5))
                  x)
              (cv 32)))))
   (test
    77
    (let ((c (make-connector)))
      (let ((f (celsius-fahrenheit-converter c)))
        (set-value! c 25 'user)
        (get-value f))))
   (test
    100
    (let ((c (make-connector)))
      (let ((f (celsius-fahrenheit-converter c)))
        (set-value! f 212 'user)
        (get-value c))))))

(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? true)
                 result)
          result))))

(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream x y) (cons x (delay y)))))

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define stream-null? null?)
(define the-empty-stream '())

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))


(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (display x)
  (newline))

(define (stream-x-map proc . streams)
  (if (null? (car streams))
      the-empty-stream
      (cons-stream (apply proc (map stream-car streams))
                   (apply stream-x-map
                          (cons proc
                                (map stream-cdr streams))))))

(test-group
 "stream-x-map"
 (test
  '(2 4)
  (let ((stream (stream-x-map
                 (lambda (x) (+ 1 x))
                 (cons-stream 1
                              (cons-stream 3
                                           the-empty-stream)))))
    (list (stream-car stream)
          (stream-car (stream-cdr stream)))))
 (test
  '(3 1)
  (let ((stream
         (stream-x-map
          +
          (cons-stream 0 (cons-stream 0 the-empty-stream))
          (cons-stream 1 (cons-stream 0 the-empty-stream))
          (cons-stream 2 (cons-stream 1 the-empty-stream)))))
    (list (stream-car stream)
          (stream-car (stream-cdr stream)))))
 (test
  #t
  (stream-null?
   (stream-x-map - the-empty-stream the-empty-stream))))

(define stream-map stream-x-map)

(define (make-stream . items)
  (if (null? items)
      the-empty-stream
      (cons-stream (car items)
                   (apply make-stream (cdr items)))))

(define (mul-streams stream-0 stream-1)
  (stream-map * stream-0 stream-1))

(define (stream-get-items k stream)
  (if (= k 1)
      (list (stream-car stream))
      (cons (stream-car stream)
            (stream-get-items (- k 1)
                              (stream-cdr stream)))))

(test-group
 "mul-streams"
 (test
  the-empty-stream
  (mul-streams the-empty-stream the-empty-stream))
 (test
  '(2 6 12)
  (stream-get-items
   3
   (mul-streams (make-stream 1 2 3)
                (make-stream 2 3 4)))))

(define (ints-from n)
  (cons-stream n (ints-from (+ n 1))))

(test-group
 "stream-get-items"
 (test
  '(0)
  (stream-get-items 1 (ints-from 0)))
 (test
  '(1 2 3 4)
  (stream-get-items 4 (ints-from 1))))

(define ones (cons-stream 1 ones))

(define (partial-sums stream)
  (if (stream-null? stream)
      the-empty-stream
      (cons-stream
       (stream-car stream)
       (stream-map (lambda (x)
                     (+ (stream-car stream) x))
                   (partial-sums (stream-cdr stream))))))

(test-group
 "partial-sums"
 (test
  '(1 2 3)
  (stream-get-items 3 (partial-sums ones)))
 (test
  '(1 3 6)
  (stream-get-items 3 (partial-sums (ints-from 1))))
 (test
  '(2 5 9)
  (stream-get-items 3 (partial-sums (make-stream 2 3 4)))))

(define (merge a b)
  (cond ((stream-null? a) b)
        ((stream-null? b) a)
        (else
         (let ((a-car (stream-car a))
               (b-car (stream-car b)))
           (cond ((< a-car b-car)
                  (cons-stream a-car (merge (stream-cdr a) b)))
                 ((> a-car b-car)
                  (cons-stream b-car (merge a (stream-cdr b))))
                 (else
                  (cons-stream a-car
                               (merge (stream-cdr a)
                                      (stream-cdr b)))))))))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define (integrate-series series)
  (stream-map (lambda (x y) (/ x y))
              series
              (ints-from 1)))

(define cosine-series
  (cons-stream 1 (integrate-series (stream-map - sine-series))))
(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

(test-group
 "integrate-series"
 (test
  '(1 1 1/2 1/6 1/24)
  (begin
    (define exp-series
      (cons-stream 1 (integrate-series exp-series)))
    (stream-get-items 5 exp-series)))
 (test
  '((1 0 -1/2 0 1/24 0 -1/720)
    (0 1 0 -1/6 0 1/120 0))
  (list (stream-get-items 7 cosine-series)
        (stream-get-items 7 sine-series))))

(define (add-streams a b)
  (stream-map + a b))

(define (mul-series a b)
  (cons-stream
   (* (stream-car a) (stream-car b))
   (add-streams (scale-stream (stream-cdr b)
                              (stream-car a))
                (mul-series (stream-cdr a) b))))

(test-group
 "mul-series"
 (test
  '(1 0 0 0)
  (stream-get-items
   4
   (add-streams
    (mul-series cosine-series cosine-series)
    (mul-series sine-series sine-series)))))

(define (invert-unit-series series)
  (assert (= (stream-car series) 1))
  (cons-stream 1
               (mul-series (invert-unit-series series)
                           (stream-map - (stream-cdr series)))))

(test-group
 "invert-unit-series"
 (test
  '(1 -1 0 0)
  (stream-get-items 4 (invert-unit-series ones))))

(define (div-series a b)
  (assert (not (= (stream-car b) 0)))
  (let ((c (/ 1 (stream-car b))))
    (scale-stream
     (mul-series a (invert-unit-series (scale-stream b c)))
     c)))

(test-group
 "div-series"
 (test
  '(0 1 0 1/3 0 2/15)
  (stream-get-items 6 (div-series sine-series cosine-series))))

(define (stream-limit stream tolerance)
  (let ((elt (stream-car stream))
        (next (stream-car (stream-cdr stream))))
    (if (< (abs (- elt next)) tolerance)
        next
        (stream-limit (stream-cdr stream) tolerance))))

(test-group
 "stream-limit"
 (test
  1
  (stream-limit (make-stream 0 1) 2))
 (test
  2
  (stream-limit (make-stream 0 3 7 4 2) 3)))

(define (average x y)
  (/ (+ x y) 2))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)

(define (stream-sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

(define (interleave s t)
  (if (stream-null? s)
      t
      (cons-stream (stream-car s)
                   (interleave t (stream-cdr s)))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

(define nats (ints-from 0))

(define (merge-weighted a b weight)
  (cond ((stream-null? a) b)
        ((stream-null? b) a)
        (else
         (let ((a-w (weight (stream-car a)))
               (b-w (weight (stream-car b))))
           (cond ((< a-w b-w)
                  (cons-stream (stream-car a)
                               (merge-weighted (stream-cdr a) b weight)))
                 ((> a-w b-w)
                  (cons-stream (stream-car b)
                               (merge-weighted a (stream-cdr b) weight)))
                 (else
                  (cons-stream
                   (stream-car a)
                   (cons-stream
                    (stream-car b)
                    (merge-weighted (stream-cdr a)
                                    (stream-cdr b) weight)))))))))

(test-group
 "merge-weighted"
 (test
  '(0 1 2 3 4 5)
  (stream-get-items
   6
   (merge-weighted (make-stream 0 1 2)
                   (make-stream 3 4 5)
                   (lambda (x) x))))
 (test
  '(4 3 2 1)
  (stream-get-items
   4
   (merge-weighted (make-stream 2 1)
                   (make-stream 4 3)
                   (lambda (x) (- x)))))
 (test
  '(1 1 0 -1)
  (stream-get-items
   4
   (merge-weighted (make-stream 1 0) (make-stream 1 -1)
                   (lambda (x) (- x))))))

(define (weighted-pairs s t weight)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
    weight)))

(test-group
 "weighted-pairs"
 (test
  '((0 0) (0 1) (0 2) (1 1) (0 3) (1 2))
  (stream-get-items
   6
   (weighted-pairs nats nats
                   (lambda (p) (+ (car p) (cadr p)))))))

(define (cube x) (* x x x))

(define (ramanujan-weight p)
  (+ (cube (car p)) (cube (cadr p))))

(define (find-ramanujan-numbers stream)
  (let ((n (ramanujan-weight (stream-car stream)))
        (m (ramanujan-weight (stream-car (stream-cdr stream)))))
    (if (= n m)
        (cons-stream
         n
         (find-ramanujan-numbers
          (stream-filter (lambda (p)
                           (not (= (ramanujan-weight p) n)))
                         (stream-cdr (stream-cdr stream)))))
        (find-ramanujan-numbers (stream-cdr stream)))))

(define ramanujan-pairs
  (weighted-pairs nats nats ramanujan-weight))

(define ramanujan-numbers
  (find-ramanujan-numbers ramanujan-pairs))

(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)

(define (rc r c dt)
  (lambda (i v-0)
    (add-streams
     (scale-stream i r)
     (integral (scale-stream i (/ 1.0 c)) v-0 dt))))

(define (sign-change-detector new-value old-value)
  (if (and (>= new-value 0) (< old-value 0))
      1
      (if (and (< new-value 0) (>= old-value 0))
          -1
          0)))

(define (make-zero-crossings input-stream last-value last-input)
  (let ((avpt (/ (+ (stream-car input-stream) last-input) 2)))
    (cons-stream (sign-change-detector avpt last-value)
                 (make-zero-crossings (stream-cdr input-stream)
                                      avpt
                                      (stream-car input-stream)))))

(test-group
 "make-zero-crossings"
 (test
  '(0 0 1)
  (stream-get-items
   3
   (make-zero-crossings (make-stream -8 0 2) -8 -8))))

(define (smooth stream)
  (cons-stream
   (average (stream-car stream)
            (stream-car (stream-cdr stream)))
   (smooth (stream-cdr stream))))

(test-group
 "smooth"
 (test
  '(1 3 5)
  (stream-get-items 3 (smooth (make-stream 0 2 4 6))))
 (test
  '(2 4 6 8)
  (stream-get-items 4 (smooth (make-stream 1 3 5 7 9)))))

(define (make-zero-crossings-with-map input-stream)
  (stream-map sign-change-detector input-stream
              (cons-stream 0 input-stream)))

(define (delayed-integral integrand initial-value dt)
  (cons-stream initial-value
               (let ((integrand (force integrand)))
                 (if (stream-null? integrand)
                     the-empty-stream
                     (delayed-integral (delay (stream-cdr integrand))
                                       (+ (* dt (stream-car integrand))
                                          initial-value)
                                       dt)))))

(define (solve f y0 dt)
  (define y (delayed-integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

(define (close? a b e)
  (< (abs (- a b)) e))

(test-group
 "delayed-integral"
 (test-assert
  (close?
   2.7
   (stream-ref (solve (lambda (y) y) 1 0.001) 1000)
   0.1)))

(define integral delayed-integral)

(define (solve-2nd a b dt y0 dy0)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy
    (add-streams (scale-stream dy a)
                 (scale-stream y b)))
  y)

(define (general-solve-2nd f dt y0 dy0)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (stream-map f dy y))
  y)

(define (rlc r l c dt)
  (lambda (vc0 il0)
    (define vc (integral (delay dvc) vc0 dt))
    (define il (integral (delay dil) il0 dt))
    (define dvc (scale-stream il (/ -1 c)))
    (define dil
      (add-streams (scale-stream vc (/ 1 l))
                   (scale-stream il (/ (- r) l))))
    (cons vc il)))

(define (rand-stream-helper update state commands)
  (let ((command (stream-car commands)))
    (let ((value
           (cond ((eq? command 'generate) state)
                 ((and (pair? command)
                       (eq? (car command) 'reset))
                  (cadr command))
                 (else (error "unknown command -- rand-stream"
                              command)))))
      (cons-stream
       value
       (rand-stream-helper
        update (update value) (stream-cdr commands))))))

(define (rand-stream commands)
  (rand-stream-helper (lambda (x) (+ x 1)) 0 commands))

(test-group
 "rand-stream"
 (test
  '(0 1 2 7 8)
  (stream-get-items
   5
   (rand-stream
    (make-stream 'generate
                 'generate
                 'generate
                 '(reset 7)
                 'generate)))))

(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
     (/ passed (+ passed failed))
     (monte-carlo
      (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

(define (stream-random-in-range low high)
  (cons-stream
   (random-in-range low high)
   (stream-random-in-range low high)))

(define (stream-estimate-integral p x1 x2 y1 y2)
  (let ((random-x (stream-random-in-range x1 x2))
        (random-y (stream-random-in-range y1 y2))
        (rectangle-area (* (- x2 x1) (- y2 y1))))
    (let ((experiments (stream-map p random-x random-y)))
      (scale-stream (monte-carlo experiments 0 0)
                    rectangle-area))))

(test-group
 "stream-estimate-integral"
 (test-assert (close? 1
                      (stream-ref
                       (stream-estimate-integral
                        (lambda (x y) true)
                        0 1 0 1)
                       1000)
                      0.1))
 (test-assert (close? 3.14
                      (stream-ref
                       (stream-estimate-integral
                        (lambda (x y)
                          (<= (+ (* x x) (* y y)) 1))
                        -1 1 -1 1)
                       1000)
                      0.1)))

(define debug #t)
