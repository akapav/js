(in-package :js)

(defclass native-hash ()
  ((dict :accessor dict :initform (make-hash-table :test 'eq))
   (prototype :accessor prototype :initform nil :initarg :prototype)))

(defgeneric prop (hash key)
  (:method (hash key) (declare (ignore hash key)) (error "no properties")))

(defun find-property (hash key &optional default)
  (multiple-value-bind (val exists)
      (gethash key (dict hash))
    (if exists val
	(or (and (prototype hash)
		 (find-property (prototype hash) key default))
	    default))))

(defmethod prop ((hash native-hash) key)
  (find-property hash key :undefined))

(defgeneric (setf prop) (val hash key)
  (:method (val hash key) (declare (ignore hash key)) (error "no properties")))

(defun set-property (hash key val)
  (setf (gethash key (dict hash)) val))

(defmethod (setf prop) (val (hash native-hash) key)
  (set-property hash key val))

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

#+nil (defmethod prop ((hash global-object) key)
  (call-next-method hash key))

(defclass native-function (native-hash)
  ((name :accessor name :initarg :name)
   (proc :accessor proc :initarg :proc)
   (env :accessor env :initarg :env)))

(defmethod initialize-instance :after ((f native-function) &rest args)
  (declare (ignore args))
  (setf (prop f 'js-user::prototype) (make-instance 'native-hash)))

(defparameter *global* (make-instance 'global-object))
(defparameter js-user::this *global*)
(defparameter *object-env-stack* nil)

;;;

(defmacro lookup-in-lexchain (name lexchain)
  (let ((obj (gensym))
	(prop (gensym)))
    (labels ((count-lookups (chain cnt) ;;todo: move count-lookups to 
	       (cond ((not chain) (values cnt nil)) ;;common labels around both macros
		     ((eq (car chain) :obj)
		      (count-lookups (cdr chain) (1+ cnt)))
		     ((member name (car chain)) (values cnt t))
		     (t (count-lookups (cdr chain) cnt))))
	     (build-tree (n found)
	       (if (zerop n)
		   (if found `,name `(error (format nil "no prop ~A" ',name)))
		   `(let* ((,obj (car -object-env-stack-))
			   (,prop (find-property ,obj ',name)))
		      (or ,prop (let ((-object-env-stack-
				       (cdr -object-env-stack-)))
				  ,(build-tree (1- n) found)))))))
      (case name
	((js-user::this) 'js-user::this) ;this is always bound
	((js-user::arguments) '(!arguments))
	(t (multiple-value-bind (n found) (count-lookups lexchain 0)
	     `(,@(build-tree n found))))))))

(defmacro set-in-lexchain (name val-exp lexchain)
  (let ((obj (gensym))
	(prop (gensym))
	(val (gensym)))
    (labels ((count-lookups (chain cnt) ;;todo: ... look above ...
	       (cond ((not chain) (values cnt nil))
		     ((eq (car chain) :obj)
		      (count-lookups (cdr chain) (1+ cnt)))
		     ((member name (car chain)) (values cnt t))
		     (t (count-lookups (cdr chain) cnt))))
	     (build-tree (n found)
	       (if (zerop n)
		   (if found
		       `(setf ,name ,val)
		       `(setf (prop *global* ',name) ,val))
		   `(let* ((,obj (car -object-env-stack-))
			   (,prop (find-property ,obj ',name)))
		      (if ,prop (setf (prop ,obj ',name) ,val)
			  (let ((-object-env-stack- (cdr -object-env-stack-)))
			    ,(build-tree (1- n) found)))))))
      (case name
	((this) '(error "invalid assignment left hand side")) ;this is always bound
	#+nil ((arguments) ...) ;todo: arguments support should be put in macrolet
	(t (multiple-value-bind (n found) (count-lookups lexchain 0)
	     `(let ((,val ,val-exp)) ,(build-tree n found))))))))

;;;

(defmacro !toplevel (toplevel-vars lex-chain form)
  `(macrolet ((!arguments () 'arguments)
	      (!name (name) (macroexpand `(lookup-in-lexchain ,name ,',lex-chain)))
	      (!setf-name (name val) (macroexpand `(set-in-lexchain ,name ,val ,',lex-chain))))
     (with-ignored-style-warnings
	 (let* ((*object-env-stack* (cons *global* *object-env-stack*))
		(-object-env-stack- *object-env-stack*))
	   (progn ,@(mapcar (lambda (var)
			      `(setf (prop *global* ',var)
				     (prop *global* ',var))) (cons 'arguments toplevel-vars)))
	   (progn ,@form)))))

(defmacro !dot (obj attr)
  `(prop ,obj ',attr))

(defmacro !sub (obj attr)
  `(sub ,obj ,attr))

(defmacro !setf (place val)
  (if (eq (car place) '!name)
      `(!setf-name ,(second place) ,val)
      `(setf ,place ,val)))

(defmacro !assign (op place val)
  (if (eq op t)
      `(!setf ,place ,val)
      `(!setf ,place (!binary ,op ,place ,val))))

(defmacro !unary-prefix (op place)
  `(!setf ,place (,op ,place)))

(defmacro !unary-postfix (op place)
  (let ((ret (gensym)))
    `(let ((,ret ,place))
       (!setf ,place (,op ,place))
       ,ret)))

(defmacro !num (num) num)
(defmacro !string (str) str)

(defmacro !object (props)
  (let ((obj (gensym)))
    `(let ((,obj (make-instance 'native-hash)))
       ,@(mapcar (lambda (*prop) `(setf (sub ,obj ,(first *prop)) ,(second *prop))) props)
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
		       (t js-user::this)) ,@args))))

(defmacro !new (func args)
  (let ((ret (gensym)))
    `(let ((,ret (make-instance 'native-hash :prototype (prop ,func 'js-user::prototype))))
       (funcall (proc ,func) ,ret ,@args)
       (setf (prop ,ret 'js-user::constructor) ,func)
       ,ret)))

(defmacro !return (ret)
  (declare (ignore ret))
  (error "return not in function"))

(defmacro !named-lambda (name lex-chain args locals body)
  `(let (,name)
     (setf ,name (!function ,lex-chain nil ,args ,locals ,body))
     (proc ,name)))

(defmacro !lambda (lex-chain args locals body)
  (let* ((additional-args (gensym))
	 (blockname (gensym)))
    `(macrolet ((!arguments () `(or js-user::arguments (setf js-user::arguments (make-args ,',args ,',additional-args))))
		(!name (name) (macroexpand `(lookup-in-lexchain ,name ,',lex-chain)))
		(!setf-name (name val) (macroexpand `(set-in-lexchain ,name ,val ,',lex-chain)))
		(!defun (lex-chain name args locals body)
		  `(setf ,name (!function ,lex-chain ,name ,args ,locals ,body)))
		(!return (ret) `,`(return-from ,',blockname ,(or ret :undefined))))
       (let ((-object-env-stack- *object-env-stack*))
	 (lambda (js-user::this
		  &optional ,@(mapcar (lambda (arg) `(,arg :undefined)) args)
		  &rest ,additional-args)
	   (let (js-user::arguments)
	     (let (,@(mapcar (lambda (var)
			       (list var :undefined)) locals))
	       (block ,blockname ,@body :undefined))))))))

(defmacro !function (lex-chain name args locals body)
  `(make-instance 'native-function
		  :name ',name
		  :proc ,(if name
			     `(!named-lambda ,name ,lex-chain ,args ,locals ,body)
			     `(!lambda ,lex-chain ,args ,locals ,body))
		  :env ',(car lex-chain)))

(defmacro !defun (lex-chain name args locals body)
  (let ((args2 (gensym))
	(func (gensym)))
    `(let ((,func (!function ,lex-chain ,name ,args ,locals ,body)))
       (setf (prop js-user::this ',name) ,func)
       (defun ,name (&rest ,args2)
	 (apply (proc ,func) js-user::this ,args2)))))

(defmacro !with (lex-chain obj body)
  `(let* ((*object-env-stack* (cons ,obj *object-env-stack*))
	  (-object-env-stack- *object-env-stack*))
     (macrolet ((!name (name) (macroexpand `(lookup-in-lexchain ,name ,',lex-chain)))
		(!setf-name (name val) (macroexpand `(set-in-lexchain ,name ,val ,',lex-chain))))
       ,body)))

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
  (if (and (integerp key) (>= key 0))
      (if (< key (len args)) (funcall (aref (get-arr args) key) key)
	  (funcall (aref (get-arr args) (len args)) key))
      (call-next-method args key)))

(defmethod (setf sub) (val (args arguments) key)
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
       (make-instance 'arguments :len ,len :get-arr ,get-arr :set-arr ,set-arr))))

;;;

(defmacro define-js-function (name args &body body)
  `(with-ignored-style-warnings
       (setf (prop js-user::this ',name)
	     (!function nil nil ,args nil
			((!return (or (progn ,@body) :undefined)))))))

(in-package :js-user)

(define-js-function Object ())

(define-js-function print (arg)
  (format t "~A~%" arg))
