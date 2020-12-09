;;; Just because I used vimscript last time...
;;; emacs --script file.el arg1...

;; http://ergoemacs.org/emacs/elisp_read_file_content.html
(defun read-lines (filePath)
  "Return a list of lines of a file at filePath."
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) "\n" t)))
;;(mapcar #'print (read-lines "/etc/fstab"))

(defconst acc "acc")
(defconst jmp "jmp")
(defconst nop "nop")

(defun decode-instruction(line)
  (let ((maybe (split-string line)))
    (if (= 2 (length maybe))
      (cons (car maybe) (string-to-number (cadr maybe)))
      (error line)
      )))

(defun part-1(filename)
  ;; vconcat is list->vector in elisp
  (let* ((instructions (vconcat (read-lines filename)))
         (accumulator 0)
         (ip 0)
         (already-executed (make-hash-table :test 'equal)))
    (while  (< ip (length instructions))
            (if (gethash ip already-executed)
              ;; I decided against using cl - it's elisp!
              (error (list "elisp needs 'cl to get a RETURN..." accumulator)))
            (puthash ip 'yes already-executed)
            ;; destructuring bind in elisp => seq-let
            ;; not found? part of cl or something? ok...
            (let* ((junk (decode-instruction (aref instructions ip)))
                   (ins (car junk))
                   (arg (cdr junk)))
              (cond ((equal ins acc) (progn
                                       (cl-incf accumulator arg)
                                       (cl-incf ip)))
                    ((equal ins jmp) (progn ; jump is a *relative* offset
                                       (cl-incf ip arg)
                                       (if (>= ip (length instructions))
                                         (error "jmp out of program")
                                         )))
                    ((equal ins nop) (progn 'do-nothing
                                            (cl-incf ip)))
                    ('else (error ins))
                    )
              )
            )
    )
  (error "never repeats")
  )

;; part2: FIX a broken program by changing ONE jmp <--> nop and see if it terminates!
(defun program-terminates(instructions wrongidx)
  ;; XXX horrible name
  (let* ((accumulator 0)
	 (ip 0)
	 (already-executed (make-hash-table :test 'equal))
	 (infinite-loop nil))
    (let ((handle-nop (lambda(arg) (progn
				     'do-nothing
				     (cl-incf ip))))
	  (handle-jmp (lambda(arg) (progn ; jump is a *relative* offset
				     (cl-incf ip arg)
				     (when (>= ip (length instructions))
				       ;; DAFUK! I assumed you could NOT jump+1 to the end of the program!
				       (when (equal ip (length instructions))
					   (print (list "EXACTLY EQUAL -- accumulator is " accumulator))
					   (print (list "EXACTLY EQUAL -- jump offset is " arg))
					   )
				       ;;(error "jmp out of program")
				       ;; assume this is due to a changed instruction...
				       (setq infinite-loop 'yep)
				       )))))
      (while  (and (< ip (length instructions))
		   (not infinite-loop))
	      (if (gethash ip already-executed)
		;; I decided against using cl - it's elisp!
		(setq infinite-loop 'yep)
		(progn
		  (puthash ip 'yes already-executed)
		  ;; destructuring bind in elisp => seq-let
		  ;; not found? part of cl or something? ok...
		  (let* ((junk (decode-instruction (aref instructions ip)))
			 (ins (car junk))
			 (arg (cdr junk)))
		    (cond ((equal ins acc) (progn
					     (cl-incf accumulator arg)
					     (cl-incf ip)))
			  ((equal ins jmp) (if (equal ip wrongidx)
					     (funcall handle-nop arg)
					     (funcall handle-jmp arg)))
			  ((equal ins nop) (if (equal ip wrongidx)
					     (funcall handle-jmp arg)
					     (funcall handle-nop arg)))
			  ('else (error ins))
			  )
		    )
		  )
		)
	      )
      (if infinite-loop
	nil
	accumulator
	)
      )
    )
  )
(defun part-2(filename)
  ;; vconcat is list->vector in elisp
  (let* ((instructions (vconcat (read-lines filename)))
         (i 0))
    (while  (< i (length instructions))
	    (let ((retval (program-terminates instructions i)))
	      (print (list i retval))
	      (if retval
		(print (list "terminate ok - accumulator is " retval " -- #ins is " i))
		)
	      )
	    (cl-incf i)
	    )))

;; It's called "argv" but it does NOT include $0
(dolist (arg argv)
  (print arg)
  (part-2 arg)
  )
