(in-package :js)

(defun ->usersym (obj)
  (typecase obj
    (symbol obj)
    (string (intern obj :js-user))
    (t (error "invalid variable identifier"))))

(defun js-intern (sym)
  (intern (concatenate 'string "!" (symbol-name sym)) :js))

;;
(defmacro with-ignored-style-warnings (&body body)
  `(locally #+sbcl (declare (sb-ext:muffle-conditions style-warning))
	    #-sbcl ()
	    (progn ,@body)))
