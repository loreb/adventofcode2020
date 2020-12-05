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
