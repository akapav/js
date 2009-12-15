(in-package :js)

(defclass native-hash ()
  ((dict :accessor dict :initform (make-hash-table :test 'eq))
   (prototype :accessor prototype :initform nil :initarg :prototype)))

(defgeneric prop (hash key)
  (:method (hash key) (declare (ignore hash key)) (error "no properties")))

(defmethod prop ((hash native-hash) key)
  (multiple-value-bind (val exists)
      (gethash key (dict hash))
    (if exists val
	(and (prototype hash)
	     (prop (prototype hash) key)))))

(defgeneric (setf prop) (val hash key)
  (:method (val hash key) (declare (ignore hash key)) (error "no properties")))

(defmethod (setf prop) (val (hash native-hash) key)
  (setf (gethash key (dict hash)) val))

;;; sub: similar to prop, but key is not necessary a symbol
(defgeneric sub (hash key)
  (:method (hash key) (declare (ignore hash key)) (error "no properties")))

(defmethod sub ((hash native-hash) key)
  (let ((key (or (and (stringp key) (->sym key)) key)))
    (prop hash key)))

(defgeneric (setf sub) (val hash key)
  (:method (val hash key) (declare (ignore hash key)) (error "no properties")))

(defmethod (setf sub) (val (hash native-hash) key)
  (let ((key (or (and (stringp key) (->sym key)) key)))
    (setf (prop hash key) val)))

(defclass global-object (native-hash)
  ())

(defmethod (setf prop) (val (hash global-object) key)
  (set key val)
  (call-next-method val hash key))

(defmethod prop ((hash global-object) key)
  #+js-debug (format t "looking for ~A~%" key)
  (call-next-method hash key))

(defclass native-function (native-hash)
  ((name :accessor name :initarg :name)
   (proc :accessor proc :initarg :proc)
   (env :accessor env :initarg :env)))

(defparameter *global* (make-instance 'global-object))

(defparameter this *global*)

;;;
(defmacro js!toplevel (form)
  `(progn ,@form))

(defmacro js!name (attr)
  (if (eq attr 'this)
      'this
      `(prop this ',attr)))

(defmacro js!dot (obj attr)
  `(prop ,obj ',attr))

(defmacro js!sub (obj attr)
  `(sub ,obj ,attr))

(defmacro js!assign (op place val)
  `(setf ,place ,val))

(defmacro js!num (num) num)
(defmacro js!string (str) str)

(defmacro js!object (props)
  (let ((obj (gensym)))
    `(let ((,obj (make-instance 'native-hash)))
       ,@(mapcar (lambda (*prop) `(setf (sub ,obj ,(car *prop)) ,(cdr *prop))) props)
       ,obj)))

(defmacro js!stat (form)
  `(progn ,form))

(defmacro js!block (form)
  `(progn ,@form))

(defmacro js!var (vars)
  `(progn ,@(mapcar (lambda (var)
					  (when (cdr var)
						`(js!assign t (js!name ,(car var)) ,(cdr var))))
					vars)))

;;;
(defmacro js!call (func args) ;;;todo: check if a caller isn't  cons
  (let ((*proc (gensym)))
  `(let ((,*proc (proc ,func)))
     (declare (function ,*proc))
     (funcall ,*proc
	      ,(case (car func)
		 ((js!dot) (second func))
		 (t this)) ,@args))))

(defmacro js!new (func args)
  (let ((ret (gensym)))
    `(let ((,ret (make-instance 'native-hash :prototype (prop ,func 'prototype))))
       (funcall (proc ,func) ,ret ,@args)
       (setf (prop ,ret 'constructor) ,func)
       ,ret)))

(defmacro js!return (ret)
  (declare (ignore ret))
  (error "return not in function"))

#+nil (defun find-name (tree)
  (labels ((f (tree)
	     (cond
	       ((atom tree) nil)
	       ((eq (first tree) 'js!name) (return-from find-name (second tree)))
	       (t (mapcar #'f tree)))))
    (f tree)))

(defmacro js!named-lambda (name env args locals body)
  `(let (,name)
	 (setf ,name (js!function ,env nil ,args ,locals ,body))
	 (proc ,name)))

(defmacro js!lambda (env args locals body)
  (let* ((additional-args (gensym))
		 (local-variable-p
		  (lambda (var)
			(or (eq var 'this)
				(member var env)
				(member var args)
				(member var locals))))
		 (blockname (gensym)))
    `(macrolet ((js!name (name)
				  (cond ((eq name 'arguments)
						 (format t "!!!!!!!~%")
						 `(or arguments (setf arguments
											  (make-args ,',args ,',additional-args))))
						((funcall ,local-variable-p name) name)
						(t `,`(prop *global* #+nil this ',name))))
				(js!return (ret) `,`(return-from ,',blockname ,ret)))
       (lambda (this
				&optional ,@(mapcar (lambda (arg) `(,arg :undefined)) args)
				&rest ,additional-args)
	 #+js-debug (format t "this: ~A~%" this)
	 (let (arguments)
	   (let (,@(mapcar (lambda (var)
						 (list var :undefined)) locals))
	     (block ,blockname ,@body)))))))

(defmacro js!function (env name args locals body)
  `(make-instance 'native-function
		  :name ',name
		  :proc ,(if name
					 `(js!named-lambda ,name ,env ,args ,locals ,body)
					 `(js!lambda ,env ,args ,locals ,body))
		  :env ',env))

(defmacro js!defun (env name args locals body)
  (let ((args2 (gensym))
	(func (gensym)))
    `(let ((,func (js!function ,env ,name ,args ,locals ,body)))
       (setf (prop this ',name) ,func)
       (defun ,name (&rest ,args2)
		 (apply (proc ,func) this ,args2)))))

(defmacro js!binary-operators (&rest ops)
  `(progn
     ,@(mapcar (lambda (op)
		 (if (symbolp op)
		     `(setf (symbol-function ',(js!intern op)) (function ,op))
		     `(setf (symbol-function ',(js!intern (first op))) (function ,(second op)))))
	     ops)))

;;;;;;;;
(defun plus (ls rs)
  (declare (fixnum ls rs))
  (the fixnum (+ ls rs)))

(defun minus (ls rs)
  (declare (fixnum ls rs))
  (the fixnum (- ls rs)))

(defun less (ls rs)
  (declare (fixnum ls rs))
  (the boolean (< ls rs)))
;;;;;;;;
(js!binary-operators
  (+ plus) (- minus) * /
 (== equalp) (< less) > <= >= (!= /=))

(defmacro js!binary (op-sym ls rs)
  (let ((op (symbol-function op-sym)))
    `(funcall ,op ,ls ,rs)))

(defmacro js->boolean (exp)
  (let ((rexp (gensym)))
    `(let ((,rexp ,exp))
       (not
		(or (not ,exp)
			(eq ,exp :undefined)
			(and (numberp ,rexp) (zerop ,rexp)))))))

(defmacro js!if (exp then else)
  `(if (js->boolean ,exp) ,then ,else))

(defmacro js!while (exp body)
  `(loop while (js->boolean ,exp) do ,body))

(defmacro js!eval (str)
  (process-ast (parse-js-string str)))

;;;

(defun js-reader (stream)
  `(js!eval ,(read-line-stream stream)))

(define-reader 'javascript #'js-reader)

;;;

(defclass arguments (native-hash)
  ((len :initarg :len :reader len)
   (get-arr :initarg :get-arr :reader get-arr)
   (set-arr :initarg :set-arr :reader set-arr)))

(defmethod sub ((args arguments) key)
  (format t "sub ~A~%" key)
  (if (and (integerp key) (>= key 0))
      (if (< key (len args)) (funcall (aref (get-arr args) key) key)
	  (funcall (aref (get-arr args) (len args)) key))
      (progn (format t "sub c-n-m~A~%" key) (call-next-method args key))))

(defmethod (setf sub) (val (args arguments) key)
  (format t "setf syb~%")
  (if (and (integerp key) (>= key 0))
      (if (< key (len args)) (funcall (aref (set-arr args) key) key val)
	  (funcall (aref (set-arr args) (len args)) key val))
      (call-next-method val args key)))

(defmacro make-args (vars oth)
  (let ((get-arr (gensym))
	(set-arr (gensym))
	(len (length vars)))
    `(let ((,get-arr (make-array ,(1+ len)
				 :element-type 'function
				 :initial-contents (list
						    ,@(mapcar (lambda (var)
								`(lambda (n) (declare (ignore n)) ,var))
							      vars)
						    (lambda (n) (nth (- n ,len) ,oth)))))
	   (,set-arr (make-array ,(1+ len)
				 :element-type 'function
				 :initial-contents (list
						    ,@(mapcar (lambda (var)
								`(lambda (n val) (declare (ignore n)) (setf ,var val)))
							      vars)
						    (lambda (n val) (setf (nth (- n ,len) ,oth) val))))))
       (format t "creating args~%")
       (make-instance 'arguments :len ,len :get-arr ,get-arr :set-arr ,set-arr))))

;;;

(setf (prop this 'Object) (js!function () () () () ()))
