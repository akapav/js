(in-package :js)

(defmacro !this () 'js-user::|this|)

;; Float special values

#+sbclx
(progn
  (defmacro without-traps (&body body)
    `(unwind-protect (progn (sb-int:set-floating-point-modes :traps ()) ,@body)
       (sb-int:set-floating-point-modes :traps '(:overflow :invalid :divide-by-zero))))
  (defun make-nan-helper (x) ;; It's not so easy to get a NaN value on SBCL
    (without-traps (- x sb-ext:double-float-positive-infinity)))
  (defparameter *nan* (make-nan-helper sb-ext:double-float-positive-infinity)))

(defparameter *float-traps*
  #+(or allegro sbclx) nil
  #-(or allegro sbclx) t)

(defmacro wrap-js (&body body)
  #+sbclx `(without-traps ,@body)
  #-sbclx `(progn ,@body))

(defmacro positive-infinity ()
  #+allegro #.excl:*infinity-double*
  #+sbcxl sb-ext:double-float-positive-infinity
  #-(or allegro sbclx) :Inf)
(defmacro negative-infinity ()
  #+allegro #.excl:*negative-infinity-double*
  #+sbclx sb-ext:double-float-negative-infinity
  #-(or allegro sbclx) :-Inf)
(defmacro nan ()
  #+allegro #.excl:*nan-double*
  #+sbclx '*nan*
  #-(or allegro sbclx) :NaN)
(defmacro is-nan (val)
  #+allegro `(excl::nan-p ,val)
  #+sbclx (let ((name (gensym)))
           `(let ((,name ,val))
              (and (floatp ,name) (sb-ext:float-nan-p ,name))))
  #-(or allegro sbclx) `(eq ,val :NaN))


;;
(mapc #'ensure-accessors
      '("prototype" "constructor"))

(defun %finalize-new-protocol (obj func args)
    (apply (the function (proc func)) obj args)
    (set-attribute obj "constructor" func)
    ;;todo: move set-default here
    obj)

(defun js-new-ignore-prototype (func args &optional (class-name 'native-hash))
  (let ((new-object (make-instance class-name)))
    (%finalize-new-protocol new-object func args)))

(defun js-new (func args)
  (let* ((proto (prop** (get-attribute func "prototype") nil))
	 (new-object (js-clone proto)))
    (%finalize-new-protocol new-object func args)))

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
