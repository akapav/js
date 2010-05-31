(in-package :js)

(defmacro !this () 'js-user::|this|)

;;
(mapc #'ensure-accessors
      '("prototype" "constructor"))

(defun %finalize-new-protocol (obj func args)
    (apply (the function (proc func)) obj args)
    (setf (prop obj "constructor") func)
    ;;todo: move set-default here
    obj)

(defun js-new-ignore-prototype (func args &optional (class-name 'native-hash))
  (let ((new-object (make-instance class-name)))
    (%finalize-new-protocol new-object func args)))

(defun js-new (func args)
  (let* ((proto (prop* func "prototype" nil))
	 (new-object (js-clone proto)))
    (%finalize-new-protocol new-object func args)))

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
	 (eq exp :false)
	 (eq exp :NaN))))
      (t t))))

;;
(defmacro !eval (str)
  (translate (parse-js-string str)))

(defun compile-eval (code)
  (funcall (compile nil `(lambda () ,code))))

(defun js-load-file (fname)
  (with-open-file (str fname)
    (compile-eval (translate (parse-js str)))))

(defun js-reader (stream)
  `(!eval ,(read-line-stream stream)))

(define-reader 'javascript #'js-reader)
