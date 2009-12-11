(asdf:oos 'asdf:load-op :parse-js)
(asdf:oos 'asdf:load-op :ptester)

(load "reader.lisp")

(defpackage :js
  (:use :ptester :parse-js :cl
	:net.svrg.reader-macro))

(in-package :js)

(defun ->sym (str)
  (intern (string-upcase str)))

(defun intern-keywords (tree)
  (cond ((null tree) nil)
	((atom tree)
	 (if (keywordp tree)
	     (js!intern tree) tree))
	(t
	 (case (car tree)
	   ((:var) (cons 'js!var
			 (list (mapcar
				(lambda (var-desc) (cons (->sym (car var-desc))
							 (intern-keywords (cdr var-desc))))
				(second tree)))))
	   ((:name) (list 'js!name (->sym (second tree))))
	   ((:dot) (list 'js!dot (intern-keywords (second tree)) (->sym (third tree))))
	   ((:function :defun)
	      (list (js!intern (first tree)) (->sym (second tree))
		    (mapcar #'->sym (third tree))
		    (intern-keywords (fourth tree))))
	   (t (mapcar #'intern-keywords tree))))))

(defun js!intern (sym)
  (intern (concatenate 'string "JS!" (symbol-name sym)))) ;;todo: use in intern-keywords
;;;

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
   (proc :accessor proc :initarg :proc)))

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

(defmacro js!assign (op place val)
  `(setf ,place ,val))

(defmacro js!num (num) num)

(defmacro js!stat (form)
  `(progn ,form))

(defmacro js!block (form)
  `(progn ,form))

(defmacro js!var (vars)
  (declare (ignore vars)) nil)

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

(defun find-vars (tree) ;;todo: nested functions are included. should be ignored
  (cond
    ((atom tree) nil)
;;; todo: ignore nested functions
    ((eq (first tree) 'js!var) (cadr tree))
    (t (apply #'append (mapcar #'find-vars tree)))))

(defun find-name (tree)
  (labels ((f (tree)
	     (cond
	       ((atom tree) nil)
	       ((eq (first tree) 'js!name) (return-from find-name (second tree)))
	       (t (mapcar #'f tree)))))
    (f tree)))

(defmacro js!lambda (args body)
  (let* ((additional-args (gensym))
	 (local-var-list (find-vars body))
	 (local-variable-p
	  (lambda (var)
	    (or (member var (list 'this 'arguments))
		(member var args)
		(member var local-var-list :key #'car))))
	 (blockname (gensym)))
    `(macrolet ((js!name (name)
		  (if (funcall ,local-variable-p name)
		      name
		      `,`(prop *global* #+nil this ',name)))
		(js!assign (op exp val)
		  (let ((name (find-name exp)))
		    (if (funcall ,local-variable-p name)
			`(setf ,exp ,val)
			`,`(setf (prop *global* #+nil this ,exp) ,val))))
		(js!return (ret) `,`(return-from ,',blockname ,ret)))
       (lambda (this &optional ,@args &rest ,additional-args)
	 (declare (dynamic-extent ,additional-args))
	 #+js-debug (format t "this: ~A~%" this)
	 (let (#+nil(arguments (coerce ,argument-list 'simple-vector))
	       ,@(mapcar (lambda (var-desc)
			   (list (car var-desc)
				 (cdr var-desc))) local-var-list))
	   (block ,blockname ,@body))))))

(defmacro js!function (name args body)
  `(make-instance 'native-function
		  :name ',name
		  :proc (js!lambda ,args ,body)))

(defmacro js!defun (name args body)
  (let ((args2 (gensym))
	(func (gensym)))
    `(let ((,func (js!function ,name ,args ,body)))
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

(js!binary-operators
  + - * /
 (== equalp) < > <= >= (!= /=))

(defmacro js!binary (op-sym ls rs)
  (let ((op (symbol-function op-sym)))
    `(funcall ,op ,ls ,rs)))

(defmacro js!if (exp then else)
  (let ((rexp (gensym)))
    `(let ((,rexp ,exp))
       (cond ((not ,rexp) ,else)
	     ((and (numberp ,rexp) (zerop ,rexp))
	      ,else)
	     (t ,then)))))

(defmacro js!eval (str)
  (intern-keywords (parse-js-string str)))

;;;

(defun js-reader (stream)
  `(js!eval ,(read-line-stream stream)))

(define-reader 'javascript #'js-reader)

;;;

(setf (prop this 'Object) (js!function () () ()))

(defun test1 ()
  #+nil(setf (prop this "a") (make-instance 'native-hash))
#{javascript}

a = new Object;
f = function (o) {
  return o.a;
}

a.a = 1;

r1 = f(this);
r2 = f(a);

f2 = function f2() {
  return this.a;
}

r3 = f2();

a.f3 = f2;
r4 = a.f3();

f4 = function f4() {
 return f(this);
};

r5 = f4();
r6 = this.f4();
a.f5 = f4;
r7 = a.f5();

fifi = 3;
a.fifi = 4;
a.f = function () {return fifi;}
r8 = a.f()

.

  (test r1 a)
  (test r2 1)
  (test r3 a)
  (test r4 1)
  (test r5 a)
  (test r6 a)
  (test r7 1)
  (test r8 3))

(defun test2 ()

#{javascript}
a = new Object
a.x = 3;

function f33()
{
  var v = new Object;
  v.v1 = new Object;
  v.v1.v2 = 4;
  this.y = v.v1.v2;
}

f33.prototype = a;

b = new f33();
r1 = b.x;
r2 = b.y;
.

  (test r1 3)
  (test r2 4))

(defun test3 ()
  (labels ((fib2 (n)
	     (if (< n 2) 1
		 (+ (fib2 (1- n)) (fib2 (- n 2))))))
#{javascript}
function fib(n)
{
  if(n < 2) return 1;
  return fib(n - 1) + fib(n - 2);
}
.
    (loop for i from 1 to 10 do
      (test (fib i) (fib2 i))
	  finally (return t))))
