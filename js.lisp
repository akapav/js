(in-package :js)

(defmacro !this () 'js-user::|this|)

;; Float special values

#+sbcl
(progn
  (defmacro without-traps (&body body)
    `(unwind-protect (progn (sb-int:set-floating-point-modes :traps ()) ,@body)
       (sb-int:set-floating-point-modes :traps '(:overflow :invalid :divide-by-zero))))
  (defun make-nan-helper (x) ;; It's not so easy to get a NaN value on SBCL
    (without-traps (- x sb-ext:double-float-positive-infinity)))
  (defparameter *nan* (make-nan-helper sb-ext:double-float-positive-infinity)))

(defparameter *float-traps*
  #+(or allegro sbcl) nil
  #-(or allegro sbcl) t)

(defmacro wrap-js (&body body)
  #+sbcl `(without-traps ,@body)
  #-sbcl `(progn ,@body))

(defmacro positive-infinity ()
  #+allegro #.excl:*infinity-double*
  #+sbcl sb-ext:double-float-positive-infinity
  #-(or allegro sbcl) :Inf)
(defmacro negative-infinity ()
  #+allegro #.excl:*negative-infinity-double*
  #+sbcl sb-ext:double-float-negative-infinity
  #-(or allegro sbcl) :-Inf)
(defmacro nan ()
  #+allegro #.excl:*nan-double*
  #+sbcl '*nan*
  #-(or allegro sbcl) :NaN)
(defmacro is-nan (val)
  #+allegro `(excl::nan-p ,val)
  #+sbcl (let ((name (gensym)))
           `(let ((,name ,val))
              (and (floatp ,name) (sb-ext:float-nan-p ,name))))
  #-(or allegro sbcl) `(eq ,val :NaN))
  

;;
(defun js-funcall (func &rest args)
  (wrap-js
    (apply (the function (proc func)) nil args)))

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
  `(wrap-js ,(translate-ast (parse-js-string str))))

;; Compile-time translation and inclusion of JS code.
(defmacro !include (file)
  `(wrap-js ,(translate-ast (with-open-file (in (eval file)) (parse-js in)))))

(defun compile-eval (code)
  (funcall (compile nil `(lambda () ,code))))

(defun js-load-file (fname)
  (with-open-file (str fname)
    (compile-eval `(wrap-js ,(translate-ast (parse-js str))))))
