(in-package :js)

(defun ->sym (str)
  (intern (string-upcase str)))

(defun js!intern (sym)
  (intern (concatenate 'string "JS!" (symbol-name sym))))

;; actually, we don't need an queue but set. important thing is that reference must be kept
(defstruct queue
  list
  last)

(defun queue-make (&optional (head :head))
  (let ((head (list head)))
    (make-queue :list head :last head)))

(defun queue-enqueue (q el)
  (unless (member el (queue-list q))
    (let ((last (queue-last q)))
      (setf (queue-last q)
	    (setf (cdr last) (list el))))))

(defun queue-top (q)
  (second (queue-list q)))

(defun queue-dequeue (q)
  (let ((front (second (queue-list q))))
    (pop (cdr (queue-list q)))
    (unless (cdr (queue-list q))
      (setf (queue-last q) (queue-list q)))
    (values front q)))

(defun queue-empty? (q)
  (eq (queue-list q) (queue-last q)))

(defun queue-copy (q)
  (let ((list (copy-list (queue-list q))))
  (make-queue :list list :last (last list))))
