(in-package :js)

(defmacro !this () 'js-user::|this|)

;; Float special values
#+sbcl-no-float-traps
(sb-int:set-floating-point-modes :traps ())
#+sbcl-no-float-traps
(defparameter *nan*
  (- sb-ext:double-float-positive-infinity sb-ext:double-float-positive-infinity))

(defparameter *float-traps*
  #+(or allegro sbcl-no-float-traps) nil
  #-(or allegro sbcl-no-float-traps) t)

(defmacro positive-infinity ()
  #+allegro #.excl:*infinity-double*
  #+sbcl-no-float-traps sb-ext:double-float-positive-infinity
  #-(or allegro sbcl-no-float-traps) :Inf)
(defmacro negative-infinity ()
  #+allegro #.excl:*negative-infinity-double*
  #+sbcl-no-float-traps sb-ext:double-float-negative-infinity
  #-(or allegro sbcl-no-float-traps) :-Inf)
(defmacro nan ()
  #+allegro #.excl:*nan-double*
  #+sbcl-no-float-traps '*nan*
  #-(or allegro sbcl-no-float-traps) :NaN)
(defmacro is-nan (val)
  #+allegro `(excl::nan-p ,val)
  #+sbcl-no-float-traps (let ((name (gensym)))
                          `(let ((,name ,val))
                             (and (floatp ,name) (sb-ext:float-nan-p ,name))))
  #-(or allegro sbcl-no-float-traps) `(eq ,val :NaN))
  

;;
(defun js-funcall (func &rest args)
  (apply (the function (proc func)) nil args))

(defmacro js-function (args &body body)
  (let ((other nil))
    (labels ((add-default (args)
               (cond ((not args) (setf other t) '(&rest other-args))
                     ((eq (car args) '&rest) args)
                     ((symbolp (car args))
                      (cons (list (car args) :undefined) (add-default (cdr args))))
                     (t (cons (car args) (add-default (cdr args)))))))
      (setf args (cons '&optional (add-default args))))
    `(make-instance
      'native-function :prototype function.prototype
      :proc (lambda (js-user::|this| ,@args)
              (declare (ignorable js-user::|this| ,@(and other '(other-args))))
              ,@body))))

(defun js-new (func args)
  (let* ((proto (prop func "prototype"))
	 (ret (make-instance (placeholder-class func)
			     :prototype proto
			     :sealed (sealed proto))))
    (apply (the function (proc func)) ret args)
    (setf (prop ret "constructor") func)
    ;;todo: put set-default here
    ret))

(defmacro undefined? (exp)
  `(eq ,exp :undefined))

(defun js->boolean (exp)
  (when exp
    (typecase exp
      (fixnum (not (zerop exp)))
      (number (not (zerop exp)))
      (string (not (zerop (length exp))))
      (symbol
       (not
	(or
	 (undefined? exp)
	 (eq exp :null)
	 (eq exp nil)
         (is-nan exp))))
      (t t))))

;;
(defmacro !eval (str)
  (translate-ast (parse-js-string str)))

(defun compile-eval (code)
  (funcall (compile nil `(lambda () ,code))))

(defun js-load-file (fname)
  (with-open-file (str fname)
    (compile-eval (translate-ast (parse-js str)))))

(defun js-reader (stream)
  `(!eval ,(read-line-stream stream)))

(define-reader 'javascript #'js-reader)
