
;;; Advent of code 2020/14
;;; lines are either:
;;;     mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
;;;     mem[8] = 42
;;; words are 36 bits, memory starts at zero.
;;; mask meaning (vaguely similar to umask in unix):
;;;     X => leave the bit untouched
;;;     0 => force the bit to zero
;;;     1 => force the bit to one

;;;A version 2 decoder chip doesn't modify the values being written at all.
;;;Instead, it acts as a memory address decoder.
;;;Immediately before a value is written to memory, each bit in the bitmask modifies the corresponding bit of the destination memory address in the following way:
;;;
;;;If the bitmask bit is 0, the corresponding memory address bit is unchanged.
;;;If the bitmask bit is 1, the corresponding memory address bit is overwritten with 1.
;;;If the bitmask bit is X, the corresponding memory address bit is floating.
;; => I should have had a hash table of (char, modification) :D

(defconstant *bits* 36)
(defconstant *wordmax* (- (ash 1 *bits*) 1))
(defconstant BITX #\X)
(defconstant BIT0 #\0)
(defconstant BIT1 #\1)

(defstruct bitmask
  (mask (make-array *bits* :initial-element BITX)))

(defstruct memorywrite
  ;;(addr 0 :type (unsigned-byte *bits*)) => SBCL complains it's a bad size...
  (addr 0 :type fixnum)
  (value 0 :type fixnum)
  )
(defstruct setmask
  (mask (make-bitmask) :type bitmask))

;; Floating means try all combinations!!!
;; ==> we MUST return a LIST of addresses!
(defun apply-floating-bits(value bits)
  (format t "afb ~A ~A~%" value bits)
  (if (not bits)
    (list value)
    (let ((b (car bits)))
      (append
        ;; force bit to 1
        (apply-floating-bits (logior value (ash 1 b)) (cdr bits))
        ;; clear bit
        (apply-floating-bits
          (logand value (lognot (logand *wordmax* (ash 1 b))))
          (cdr bits))
        )
      )))

(let ((bits '(0 2)) (value 7))
  (format t "floatbits ~A ~A = ~A~%" value bits (apply-floating-bits value bits))
  )

(defun apply-mask(mask value)
  ;; I'm 100% sure there's a very lisp-y way to explode the value to bits,
  ;; map each character to a simple functioin, and rejoin the results;
  ;; I just don't know what it is.
  (let ((bits '())
        (ret 0)
        (floating '()) ; list of bits that are floating
        (nf 0) ; which bit is floating?
        )
    (LOOP for ch in (reverse (coerce (bitmask-mask mask) 'list)) DO
          ;; In part 2:
          ;;If the bitmask bit is 0, the corresponding memory address bit is unchanged.
          ;;If the bitmask bit is 1, the corresponding memory address bit is overwritten with 1.
          ;;If the bitmask bit is X, the corresponding memory address bit is floating.
          (cond ((equal BIT0 ch) (setf bits (cons (logand 1 value) bits)))
                ((equal BIT1 ch) (setf bits (cons 1 bits)))
                ((equal BITX ch) (progn
                                   (setf bits (cons (logand 1 value) bits))
                                   (format t "floating bit ~A~%" nf)
                                   (setf floating (cons nf floating))))
                ('else (error "nope")))
          (setf value (ash value -1))
          (incf nf)
          )
    (LOOP for b in bits DO
          (setf ret (+ ret ret b))
          )
    ;; Handle floating bits...
    (if (not floating)
      (list ret)
      ;(apply-floating-bits value floating)
      ; ARGH! it took me forever to notice the bug above!
      (apply-floating-bits ret floating)
      )
    )
  )

;; return the memory as a hash table index=>value
;; apparently "run-program" is reserved in sbcl?
(defun run-instructions(instructions)
  (let ((mem (make-hash-table :test #'equalp))
        (bitmask (make-bitmask)))
    (dolist (ins instructions)
      (cond ((setmask-p ins) (setf bitmask (setmask-mask ins)))
            ((memorywrite-p ins) (format t "~A~%" ins)
                                 (LOOP for addr in (apply-mask bitmask (memorywrite-addr ins))
                                       DO
                                       (format t "MASK ~A~%ADDR ~A (~B) = ~A~%~%"
                                               bitmask
                                               addr addr
                                               (memorywrite-value ins))
                                       (setf (gethash addr mem)
                                             (memorywrite-value ins)
                                             )))
            ('else (error ins))
            )
      )
    mem
    )
  )

;; https://www.cliki.net/SPLIT-SEQUENCE {{{
;; "things that everybody writes since they're not part of..."
;; http://www.common-lisp.net/project/cl-utilities/cl-utilities-latest.tar.gz
;;;; SPLIT-SEQUENCE
;;;
;;; This code was based on Arthur Lemmens' in
;;; <URL:http://groups.google.com/groups?as_umsgid=39F36F1A.B8F19D20%40simplex.nl>;
;;;
;;; changes include:
;;;
;;; * altering the behaviour of the :from-end keyword argument to
;;; return the subsequences in original order, for consistency with
;;; CL:REMOVE, CL:SUBSTITUTE et al. (:from-end being non-NIL only
;;; affects the answer if :count is less than the number of
;;; subsequences, by analogy with the above-referenced functions).
;;;   
;;; * changing the :maximum keyword argument to :count, by analogy
;;; with CL:REMOVE, CL:SUBSTITUTE, and so on.
;;;
;;; * naming the function SPLIT-SEQUENCE rather than PARTITION rather
;;; than SPLIT.
;;;
;;; * adding SPLIT-SEQUENCE-IF and SPLIT-SEQUENCE-IF-NOT.
;;;
;;; * The second return value is now an index rather than a copy of a
;;; portion of the sequence; this index is the `right' one to feed to
;;; CL:SUBSEQ for continued processing.

;;; There's a certain amount of code duplication here, which is kept
;;; to illustrate the relationship between the SPLIT-SEQUENCE
;;; functions and the CL:POSITION functions.

;;; Examples:
;;;
;;; * (split-sequence #\; "a;;b;c")
;;; -> ("a" "" "b" "c"), 6
;;;
;;; * (split-sequence #\; "a;;b;c" :from-end t)
;;; -> ("a" "" "b" "c"), 0
;;;
;;; * (split-sequence #\; "a;;b;c" :from-end t :count 1)
;;; -> ("c"), 4
;;;
;;; * (split-sequence #\; "a;;b;c" :remove-empty-subseqs t)
;;; -> ("a" "b" "c"), 6
;;;
;;; * (split-sequence-if (lambda (x) (member x '(#\a #\b))) "abracadabra")
;;; -> ("" "" "r" "c" "d" "" "r" ""), 11
;;;
;;; * (split-sequence-if-not (lambda (x) (member x '(#\a #\b))) "abracadabra")
;;; -> ("ab" "a" "a" "ab" "a"), 11 
;;;
;;; * (split-sequence #\; ";oo;bar;ba;" :start 1 :end 9)
;;; -> ("oo" "bar" "b"), 9

;; cl-utilities note: the license of this file is unclear, and I don't
;; even know whom to contact to clarify it. If anybody objects to my
;; assumption that it is public domain, please contact me so I can do
;; something about it. Previously I required the split-sequence
 ; package as a dependency, but that was so unwieldy that it was *the*
;; sore spot sticking out in the design of cl-utilities. -Peter Scott

;;(in-package :cl-utilities)

(defun split-sequence (delimiter seq &key (count nil) (remove-empty-subseqs nil) (from-end nil) (start 0) (end nil) (test nil test-supplied) (test-not nil test-not-supplied) (key nil key-supplied))
  "Return a list of subsequences in seq delimited by delimiter.

If :remove-empty-subseqs is NIL, empty subsequences will be included
in the result; otherwise they will be discarded.  All other keywords
work analogously to those for CL:SUBSTITUTE.  In particular, the
behaviour of :from-end is possibly different from other versions of
this function; :from-end values of NIL and T are equivalent unless
:count is supplied. The second return value is an index suitable as an
argument to CL:SUBSEQ into the sequence indicating where processing
stopped."
  (let ((len (length seq))
        (other-keys (nconc (when test-supplied 
                             (list :test test))
                           (when test-not-supplied 
                             (list :test-not test-not))
                           (when key-supplied 
                             (list :key key)))))
    (unless end (setq end len))
    (if from-end
        (loop for right = end then left
              for left = (max (or (apply #'position delimiter seq 
					 :end right
					 :from-end t
					 other-keys)
				  -1)
			      (1- start))
              unless (and (= right (1+ left))
                          remove-empty-subseqs) ; empty subseq we don't want
              if (and count (>= nr-elts count))
              ;; We can't take any more. Return now.
              return (values (nreverse subseqs) right)
              else 
              collect (subseq seq (1+ left) right) into subseqs
              and sum 1 into nr-elts
              until (< left start)
              finally (return (values (nreverse subseqs) (1+ left))))
      (loop for left = start then (+ right 1)
            for right = (min (or (apply #'position delimiter seq 
					:start left
					other-keys)
				 len)
			     end)
            unless (and (= right left) 
                        remove-empty-subseqs) ; empty subseq we don't want
            if (and count (>= nr-elts count))
            ;; We can't take any more. Return now.
            return (values subseqs left)
            else
            collect (subseq seq left right) into subseqs
            and sum 1 into nr-elts
            until (>= right end)
            finally (return (values subseqs right))))))

(defun split-sequence-if (predicate seq &key (count nil) (remove-empty-subseqs nil) (from-end nil) (start 0) (end nil) (key nil key-supplied))
  "Return a list of subsequences in seq delimited by items satisfying
predicate.

If :remove-empty-subseqs is NIL, empty subsequences will be included
in the result; otherwise they will be discarded.  All other keywords
work analogously to those for CL:SUBSTITUTE-IF.  In particular, the
behaviour of :from-end is possibly different from other versions of
this function; :from-end values of NIL and T are equivalent unless
:count is supplied. The second return value is an index suitable as an
argument to CL:SUBSEQ into the sequence indicating where processing
stopped."
  (let ((len (length seq))
        (other-keys (when key-supplied 
		      (list :key key))))
    (unless end (setq end len))
    (if from-end
        (loop for right = end then left
              for left = (max (or (apply #'position-if predicate seq 
					 :end right
					 :from-end t
					 other-keys)
				  -1)
			      (1- start))
              unless (and (= right (1+ left))
                          remove-empty-subseqs) ; empty subseq we don't want
              if (and count (>= nr-elts count))
              ;; We can't take any more. Return now.
              return (values (nreverse subseqs) right)
              else 
              collect (subseq seq (1+ left) right) into subseqs
              and sum 1 into nr-elts
              until (< left start)
              finally (return (values (nreverse subseqs) (1+ left))))
      (loop for left = start then (+ right 1)
            for right = (min (or (apply #'position-if predicate seq 
					:start left
					other-keys)
				 len)
			     end)
            unless (and (= right left) 
                        remove-empty-subseqs) ; empty subseq we don't want
            if (and count (>= nr-elts count))
            ;; We can't take any more. Return now.
            return (values subseqs left)
            else
            collect (subseq seq left right) into subseqs
            and sum 1 into nr-elts
            until (>= right end)
            finally (return (values subseqs right))))))

(defun split-sequence-if-not (predicate seq &key (count nil) (remove-empty-subseqs nil) (from-end nil) (start 0) (end nil) (key nil key-supplied))
  "Return a list of subsequences in seq delimited by items satisfying
(CL:COMPLEMENT predicate).

If :remove-empty-subseqs is NIL, empty subsequences will be included
in the result; otherwise they will be discarded.  All other keywords
work analogously to those for CL:SUBSTITUTE-IF-NOT.  In particular,
the behaviour of :from-end is possibly different from other versions
of this function; :from-end values of NIL and T are equivalent unless
:count is supplied. The second return value is an index suitable as an
argument to CL:SUBSEQ into the sequence indicating where processing
stopped."				; Emacs syntax highlighting is broken, and this helps: "
  (let ((len (length seq))
	(other-keys (when key-supplied 
		      (list :key key))))
    (unless end (setq end len))
    (if from-end
        (loop for right = end then left
              for left = (max (or (apply #'position-if-not predicate seq 
					 :end right
					 :from-end t
					 other-keys)
				  -1)
			      (1- start))
              unless (and (= right (1+ left))
                          remove-empty-subseqs) ; empty subseq we don't want
              if (and count (>= nr-elts count))
              ;; We can't take any more. Return now.
              return (values (nreverse subseqs) right)
              else 
              collect (subseq seq (1+ left) right) into subseqs
              and sum 1 into nr-elts
              until (< left start)
              finally (return (values (nreverse subseqs) (1+ left))))
      (loop for left = start then (+ right 1)
            for right = (min (or (apply #'position-if-not predicate seq 
					:start left
					other-keys)
				 len)
			     end)
            unless (and (= right left) 
                        remove-empty-subseqs) ; empty subseq we don't want
            if (and count (>= nr-elts count))
            ;; We can't take any more. Return now.
            return (values subseqs left)
            else
            collect (subseq seq left right) into subseqs
            and sum 1 into nr-elts
            until (>= right end)
            finally (return (values subseqs right))))))

;;; clean deprecation

(defun partition (&rest args)
  (apply #'split-sequence args))

(defun partition-if (&rest args)
  (apply #'split-sequence-if args))

(defun partition-if-not (&rest args)
  (apply #'split-sequence-if-not args))

(define-compiler-macro partition (&whole form &rest args)
  (declare (ignore args))
  (warn "PARTITION is deprecated; use SPLIT-SEQUENCE instead.")
  form)

(define-compiler-macro partition-if (&whole form &rest args)
  (declare (ignore args))
  (warn "PARTITION-IF is deprecated; use SPLIT-SEQUENCE-IF instead.")
  form)

(define-compiler-macro partition-if-not (&whole form &rest args)
  (declare (ignore args))
  (warn "PARTITION-IF-NOT is deprecated; use SPLIT-SEQUENCE-IF-NOT instead")
  form)

(pushnew :split-sequence *features*)
;; }}}

(defun make-word(s)
  (let ((w (parse-integer s)))
    (if (< w 0)
      (error (format nil "~A < 0" s)))
    (if (> w (- (ash 1 *bits*) 1))
      (error (format nil "~A exceeds ~A bits" w *bits*)))
    w))

(defun decode-mask(s)
  (cond
    ((equal *bits* (length s))
     (let ((chars (coerce s 'list)))
       (dolist (ch chars)
         (if (not (position ch (list BITX BIT0 BIT1)))
           (error (format nil "bad char ~A" ch))))
       (make-bitmask :mask (coerce chars 'vector))
       ))))

(defun decode-line(line)
  (let ((words (split-sequence #\Space line)))
    (format t "(split ~A as ~A)~%" line words)
    (if (not (equal 3 (length words)))
      (error line))
    (cond
      ((equalp (car words) "mask")
       (if (not (equalp (cadr words) "="))
         (error (cdr words)))
       (make-setmask :mask (decode-mask (caddr words))))
      ((equalp (subseq (car words) 0 4) "mem[")
       (let ((addr (nth 0 words))
             (asgn (nth 1 words))
             (word (nth 2 words)))
         (if (not (equalp "=" asgn))
           (error (format nil "borked = in ~A" words)))
         (let ((idx (make-word (string-right-trim (list #\]) (subseq addr 4))))
               (val (make-word word)))
           (format t "DECODED: mem[~A] = ~A~%" idx val)
           (make-memorywrite :addr idx :value val))))
      ('else (error (format nil "~A => ~A" line words)))
      )
    )
  )
(let ((test (apply-mask (decode-mask "000000000000000000000000000000X1001X")
                           42)))
  (format t "bitmask 42 is ~A~%" test)
  (format t "expected is 26 27 58 59~%")
  (format t "only X is ~A~%"
          (apply-mask (decode-mask "000000000000000000000000000000X0000X") 42))
  )


(defun posix-argv()
  (or
    #+SBCL *posix-argv*
    #+CLISP (cons "clisp" *args*)
    #+LISPWORKS system:*line-arguments-list*
    #+CMU extensions:*command-line-words*
    ;; there's no #else in common lisp...
    ))

(defun kludge-ignore-arg(s)
  ;(position s (list "sbcl")) -- internet dead atm
  (cond ((equal s "sbcl") (format t "FIXME - sbcl fails, clisp works?") 'sbcl)
        ((equal s "clisp") s)
        ('else nil)))

(dolist (arg (posix-argv))
  ;; args include "sbcl" etc!
  (when (not (kludge-ignore-arg arg))
    (format t "~%~%file: ~A~%" arg)
    (with-open-file (f arg :direction :input)
      (let ((instructions '()))
        (do ((line (read-line f) (read-line f nil 'eof)))
          ((eq line 'eof) "Reached end of file.")
          ;; line does NOT contain newline :D
          (format t "read line(~A)~%" line)
          (setf instructions (cons (decode-line line) instructions))
          )
        (nreverse instructions) ; in place
        (let ((mem (run-instructions instructions))
              (total 0))
          (maphash (lambda(addr val)
                     ;; XXX I suspect it's make-hash-table but OFFLINE!
                     (format t "+= mem[~A] that is ~A~%" addr val)
                     (incf total val)) mem)
          (format t "part 1: run program, sum all values left in memory => ~A~%" total)
          )
        )
      )
    )
  )
