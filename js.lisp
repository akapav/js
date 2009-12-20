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

(defun normalize-key (key)
  (or (and (numberp key) (->sym (format nil "~A" key)))
      (and (stringp key) (->sym key))
      key))

(defmethod sub ((hash native-hash) key)
  (let ((key (normalize-key key)))
    (prop hash key)))

(defgeneric (setf sub) (val hash key)
  (:method (val hash key) (declare (ignore hash key)) (error "no properties")))

(defmethod (setf sub) (val (hash native-hash) key)
  (let ((key (normalize-key key)))
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

(defmethod initialize-instance :after ((f native-function) &rest args)
  (declare (ignore args))
  (setf (prop f 'prototype) (make-instance 'native-hash)))

(defparameter *global* (make-instance 'global-object))
(defparameter this *global*)

;;;
(defmacro !toplevel (form)
  `(progn ,@form))

(defmacro !name (attr)
  (if (eq attr 'this)
      'this
      `(prop this ',attr)))

(defmacro !dot (obj attr)
  `(prop ,obj ',attr))

(defmacro !sub (obj attr)
  `(sub ,obj ,attr))

(defmacro !assign (op place val)
  (if (eq op t)
      `(setf ,place ,val)
      `(setf ,place (!binary ,op ,place ,val))))

(defmacro !unary-prefix (op place)
  `(setf ,place (,op ,place)))

(defmacro !unary-postfix (op place)
  (let ((ret (gensym)))
  `(let ((,ret ,place))
     (setf ,place (,op ,place))
     ,ret)))

(defmacro !num (num) num)
(defmacro !string (str) str)

(defmacro !object (props)
  (let ((obj (gensym)))
    `(let ((,obj (make-instance 'native-hash)))
       ,@(mapcar (lambda (*prop) `(setf (sub ,obj ,(car *prop)) ,(cdr *prop))) props)
       ,obj)))

(defmacro !stat (form)
  `(progn ,form))

(defmacro !block (form)
  `(progn ,@form))

(defmacro !var (vars)
  `(progn ,@(mapcar (lambda (var)
		      (when (cdr var)
			`(!assign t (!name ,(car var)) ,(cdr var))))
		    vars)))

;;;
(defmacro !call (func args) ;;;todo: check if a caller isn't  cons
  (let ((*proc (gensym)))
    `(let ((,*proc (proc ,func)))
       (declare (function ,*proc))
       (funcall ,*proc
		,(case (car func)
		   ((!dot) (second func))
		   (t this)) ,@args))))

(defmacro !new (func args)
  (let ((ret (gensym)))
    `(let ((,ret (make-instance 'native-hash :prototype (prop ,func 'prototype))))
       (funcall (proc ,func) ,ret ,@args)
       (setf (prop ,ret 'constructor) ,func)
       ,ret)))

(defmacro !return (ret)
  (declare (ignore ret))
  (error "return not in function"))

#+nil (defun find-name (tree)
	(labels ((f (tree)
		   (cond
		     ((atom tree) nil)
		     ((eq (first tree) '!name) (return-from find-name (second tree)))
		     (t (mapcar #'f tree)))))
	  (f tree)))

(defmacro !named-lambda (name env args locals body)
  `(let (,name)
     (setf ,name (!function ,env nil ,args ,locals ,body))
     (proc ,name)))

(defmacro !lambda (env args locals body)
  (let* ((additional-args (gensym))
	 (local-variable-p
	  (lambda (var)
	    (or (eq var 'this)
		(member var env)
		(member var args)
		(member var locals))))
	 (blockname (gensym)))
    `(macrolet ((!name (name)
		  (cond ((eq name 'arguments)
			 (format t "!!!!!!!~%")
			 `(or arguments (setf arguments
					      (make-args ,',args ,',additional-args))))
			((funcall ,local-variable-p name) name)
			(t `,`(prop *global* #+nil this ',name))))
		(!defun (env name args locals body)
		  `(setf ,name (!function ,env ,name ,args ,locals ,body)))
		(!return (ret) `,`(return-from ,',blockname ,(or ret :undefined))))
       (locally #+sbcl (declare (sb-ext:muffle-conditions style-warning))
		#-sbcl ()
		(lambda (this
			 &optional ,@(mapcar (lambda (arg) `(,arg :undefined)) args)
			 &rest ,additional-args)
		  #+js-debug (format t "this: ~A~%" this)
		  (let (arguments)
		    (let (,@(mapcar (lambda (var)
				      (list var :undefined)) locals))
		      (block ,blockname ,@body :undefined))))))))

(defmacro !function (env name args locals body)
  `(make-instance 'native-function
		  :name ',name
		  :proc ,(if name
			     `(!named-lambda ,name ,env ,args ,locals ,body)
			     `(!lambda ,env ,args ,locals ,body))
		  :env ',env))

(defmacro !defun (env name args locals body)
  (let ((args2 (gensym))
	(func (gensym)))
    `(let ((,func (!function ,env ,name ,args ,locals ,body)))
       (setf (prop this ',name) ,func)
       (defun ,name (&rest ,args2)
	 (apply (proc ,func) this ,args2)))))

(defmacro js-operators (&rest ops)
  `(progn
     ,@(mapcar (lambda (op)
		 (if (symbolp op)
		     `(setf (symbol-function ',(js-intern op)) (function ,op))
		     `(setf (symbol-function ',(js-intern (first op))) (function ,(second op)))))
	       ops)))

;;;;;;;;
(defun plus (ls rs)
  (declare (fixnum ls rs))
  (+ ls rs))

(defun minus (ls rs)
  (declare (fixnum ls rs))
  (the fixnum (- ls rs)))

(defun less (ls rs)
  (declare (fixnum ls rs))
  (the boolean (< ls rs)))
;;;;;;;;
(js-operators
;;binary
 (+ plus) (- minus) * / (% mod)
 (== equalp) (< less) > <= >= (!= /=)
;;unary
 (++ 1+) (-- 1-))

(defmacro !binary (op-sym ls rs)
  (let ((op (symbol-function op-sym)))
    `(funcall ,op ,ls ,rs)))

(defmacro js->boolean (exp)
  (let ((rexp (gensym)))
    `(let ((,rexp ,exp))
       (not
	(or (not ,exp)
	    (eq ,exp :undefined)
	    (and (numberp ,rexp) (zerop ,rexp)))))))

#+nil (defmacro !label (name body)
     (tagbody ,(->sym name) ,body))

(defmacro !if (exp then else)
  `(if (js->boolean ,exp) ,then ,else))

(defmacro !do (cond body label)
  (let ((lbl (or label (gensym))))
    `(macrolet ((!break (named-lbl)
		  `,`(return-from ,(or (->sym named-lbl) ',lbl)))
		(!continue (named-lbl)
		  `,`(go ,(or (->sym named-lbl) ',lbl))))
       (block ,lbl
	 (tagbody ,lbl
	    (progn ,body
		   (when (js->boolean ,cond)
		     (go ,lbl))))))))

(defmacro !for (init cond step body label)
  (let ((lbl (or label (gensym)))
	(execute-step (gensym))
	(cond (or cond t))) ;;when for(init;;step) ...
    `(macrolet ((!break (named-lbl)
		  `,`(return-from ,(or (->sym named-lbl) ',lbl)))
		(!continue (named-lbl)
		  `,`(go ,(or (->sym named-lbl) ',lbl))))
       (block ,lbl
	 (let (,execute-step)
	   ,init
	   (tagbody ,lbl
	      (when ,execute-step ,step)
	      (setf ,execute-step t)
	      (when (js->boolean ,cond)
		,body
		(go ,lbl))))))))

#+nil (defmacro !while (cond body)
  `(!for nil ,cond nil ,body))

(defmacro !eval (str)
  (process-ast (parse-js-string str)))

;;;

(defun js-reader (stream)
  `(!eval ,(read-line-stream stream)))

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

(defmacro define-js-function (name args &body body)
  `(setf (prop this ',name)
	 (!function nil nil ,args nil
		    ((!return (or (progn ,@body) :undefined))))))

(define-js-function Object ())

(define-js-function print (arg)
  (format t "~A~%" arg))
