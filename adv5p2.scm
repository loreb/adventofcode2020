;; Just because... I'll try with different languages until I come up
;; with something better.

;; "binary space partitioning"
;; BFFFBBFRRR: row 70, column 7, seat ID 567
;; 128 rows: F = 0, B = 1; [FB]*6
;; 8 columns: R=1, L=0; [LR]*3

(define (take L n)
  (if (zero? n)
    '()
    (cons (car L) (take (cdr L) (- n 1)))
    ))

(define (seat-split seat) ;; seat -> (cons row column)
  (define (from-binary chars zero one acc)
    (if (null? chars)
      acc
      (let ((ch (car chars)))
        (cond ((equal? ch zero) (from-binary (cdr chars) zero one (+ acc acc)))
              ((equal? ch one)  (from-binary (cdr chars) zero one (+ acc acc 1)))
              (#t (error chars))
              ))))
  (define chars (string->list seat))
  (define (s->row s)
    (from-binary s #\F #\B 0))
  (define (s->column s)
    (from-binary s #\L #\R 0))
  (if (not (= 10 (string-length seat)))
    (error seat))
  (let ((row (s->row (take chars 7)))
        (column (s->column (list-tail chars 7))))
    (cons row column)
  ))
(display (seat-split "BFFFBBFRRR")) (newline)
(if (not (equal? (cons 70 7) (seat-split "BFFFBBFRRR")))
  (error (seat-split "BFFFBBFRRR")))
(define (seat-id seat)
  (let ((split (seat-split seat)))
    (+ (* (car split) 8)
       (cdr split))))
(if (not (equal? 820 (seat-id "BBFFBBFRLL")))
  (error (seat-id "BBFFBBFRLL")))

;; Funny thing: chrome asked me if I wanted to TRANSLATE
;; the input data...
;; anyway, let's look for the biggest id
(if #f ; part1
  (let ((maximum-id '()))
    (let loop()
      (let ((line (read-line (current-input-port))))
        (if (not (eof-object? line))
          (let ((id (seat-id line)))
            (if (or (null? maximum-id)
                    (< maximum-id id))
              (set! maximum-id id)
              )
            (loop)
            ))))
    (display maximum-id)
    (newline))
  )

;;Ding! The "fasten seat belt" signs have turned on. Time to find your seat.
;;
;;It's a completely full flight, so your seat should be the only missing boarding pass in your list. However, there's a catch: some of the seats at the very front and back of the plane don't exist on this aircraft, so they'll be missing from your list as well.
;;
;;Your seat wasn't at the very front or back, though; the seats with IDs +1 and -1 from yours will be in your list.
;;
;;What is the ID of your seat?

;; it's past midnight, and I don't want to think of how to write this
;; semi decently/idiomatically/... in scheme, at least not right now.
(let ((taken (make-vector 1024 #f)))
    (let loop()
      (let ((line (read-line (current-input-port))))
        (if (not (eof-object? line))
          (let ((id (seat-id line)))
            (if (vector-ref taken id)
              (error (list id " already taken")))
            (vector-set! taken id #t)
            (loop)))))
    ;; I'm 99% sure that vector-for-each is an optional srfi
    (let loop((i 1))
      (when (< i (- (vector-length taken) 1))
        (if (not (vector-ref taken i))
          (if (and (vector-ref taken (- i 1))
                   (vector-ref taken (+ i 1)))
            (begin (display i) (newline))
            ))
        (loop (+ i 1)) ; keep going regardless
        )))
