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

;; It's called "argv" but it does NOT include $0
(dolist (arg argv)
  (print arg)
  (part-1 arg)
  )
