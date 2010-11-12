(in-package :cl-js)

(defun js-intern (sym)
  (intern (concatenate 'string (symbol-name :js) (symbol-name sym)) :cl-js))

(defmacro with-ignored-style-warnings (&body body)
  `(locally #+sbcl (declare (sb-ext:muffle-conditions style-warning)) ,@body))

(defmacro trunc32 (int)
  (let ((r1 (gensym)))
    `(let ((,r1 (ldb (byte 32 0) ,int)))
       (if (>= ,r1 #.(expt 2 31)) (- ,r1 #.(expt 2 32)) ,r1))))
