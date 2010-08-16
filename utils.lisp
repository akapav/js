(in-package :js)

(defun ->usersym (obj)
  (typecase obj
    (symbol obj)
    (string (intern obj :js-user))
    (t (error "invalid variable identifier"))))

(defun js-intern (sym)
  (intern (concatenate 'string (symbol-name :js) (symbol-name sym)) :js))

;;
(defmacro with-ignored-style-warnings (&body body)
  `(locally #+sbcl (declare (sb-ext:muffle-conditions style-warning))
	    #-sbcl ()
	    (progn ,@body)))

(defmacro trunc32 (int)
  (let ((r1 (gensym)))
    `(let ((,r1 (ldb (byte 32 0) ,int)))
       (if (>= ,r1 #.(expt 2 31)) (- ,r1 #.(expt 2 32)) ,r1))))
