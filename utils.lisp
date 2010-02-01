(in-package :js)

(defun ->sym (str)
  (intern (string-upcase str) :js-user))

(defun js-intern (sym)
  (intern (concatenate 'string "!" (symbol-name sym)) :js))

;;
(defmacro with-ignored-style-warnings (&body body)
  `(locally #+sbcl (declare (sb-ext:muffle-conditions style-warning))
	    #-sbcl ()
	    (progn ,@body)))

;;queue
(defstruct queue
  list
  last)

(defun queue-make (&optional (head :head))
  (let ((head (list head)))
    (make-queue :list head :last head)))

(defun queue-enqueue (q el)
  (let ((last (queue-last q)))
	(setf (queue-last q)
		  (setf (cdr last) (list el)))))

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


;;set
(defun set-make (&rest args)
  (apply #'make-hash-table :test 'equal args))

(defun set-add (set elem)
  (when elem
	(setf (gethash elem set) t))
  set)

(defun set-remove (set elem)
  (remhash elem set)
  set)

(defun set-remove-all (set)
  (mapc (lambda (el)
	  (set-remove set el)) (set-elems set))
  set)

(defun set-elems (set)
  (let (elems)
	(maphash (lambda (k v)
			   (declare (ignore v))
			   (push k elems)) set)
	elems))

(defun set-copy (set)
  (let ((new-set (set-make :test (hash-table-test set))))
	(mapc (lambda (el) (set-add new-set el)) (set-elems set))
	new-set))

