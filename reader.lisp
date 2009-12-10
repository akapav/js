(defpackage :net.svrg.reader-macro
    (:use :cl)
  (:export :define-reader :find-reader :remove-reader
	   :read-line-stream))

(in-package :net.svrg.reader-macro)

(defun scan-for-reader (s)
  (with-input-from-string
      (ss
       (loop for ch = (read-char s #\})
	     until (char= ch #\})
	     for str = (string ch) then (concatenate 'string str (string ch))
	     finally (return (or str ""))))
    (read ss nil)))

(let ((reader-table (make-hash-table)))
  (defun define-reader (name proc)
    (setf (gethash name reader-table) proc))
  (defun find-reader (name stream)
    (let ((reader (gethash name reader-table)))
      (if reader (funcall reader stream)
	  (error (format nil "reader ~S not found" name)))))
  (defun remove-reader (name)
    (remhash name reader-table)))

(set-dispatch-macro-character #\# #\{
			      (lambda (s c arg)
				(declare (ignore c arg))
				(find-reader (scan-for-reader s) s)))

;;
(defun read-line-stream (stream)
  (apply #'concatenate 'string
	 (loop for line = (read-line stream nil)
	       until (or (not line) (string-equal line "."))
	       collecting (concatenate 'string line (string #\newline)))))

;; example
(defun rassoc-infix (list)
  (cond ((atom list) list)
	((not (cdr list)) (car list))
	(t (list (second list) (first list)
		 (rassoc-infix (cddr list))))))

(defun infix-reader (stream)
  (rassoc-infix (read stream)))

(define-reader 'infix #'infix-reader)

#{infix} (1 - 2 + 3)
