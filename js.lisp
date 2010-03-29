(in-package :js)

(defclass native-hash ()
  ((default-value :accessor value :initform nil :initarg :value)
   (dict :accessor dict :initform (make-hash-table :test 'equal))
   (sealed :accessor sealed :initform nil :initarg :sealed)
   (prototype :accessor prototype :initform nil :initarg :prototype)))

(defgeneric set-default (hash val) ;;todo: put it as a slot writer
  (:method (hash val) (declare (ignore hash)) val))

(defmethod set-default ((hash native-hash) val)
  (setf (value hash) val))

(defun add-sealed-property (hash key proc)
  (flet ((ensure-sealed-table (hash)
	   (or (sealed hash)
	       (setf (sealed hash) (make-hash-table :test 'equal)))))
    (let ((sealed-table (ensure-sealed-table hash)))
      (setf (gethash key sealed-table) proc))))

(defgeneric prop (hash key &optional default)
  (:method (hash key &optional default)
    (declare (ignore hash key default)) :undefined))

(defmethod prop ((hash native-hash) key &optional (default :undefined))
  (let* ((sealed (sealed hash))
	 (action (and sealed (gethash key sealed))))
    (if action (funcall action hash)
	(multiple-value-bind (val exists)
	    (gethash key (dict hash))
	  (if exists val
	      (or (and (prototype hash)
		       (prop (prototype hash) key default))
		  default))))))

(defgeneric (setf prop) (val hash key)
  (:method (val hash key) (declare (ignore hash key)) val))

(defmethod (setf prop) (val (hash native-hash) key)
  (setf (gethash key (dict hash)) val))

;;; sub: by default same as prop
(defgeneric sub (hash key)
  (:method (hash key) (prop hash key)))

(defgeneric (setf sub) (val hash key)
  (:method (val hash key) (setf (prop hash key) val)))

(defclass global-object (native-hash)
  ())

(defmethod (setf prop) (val (hash global-object) key)
  (set (if (stringp key) (intern (string-upcase key) :js-user) key) val);;todo:
  (call-next-method val hash key))

(defmethod set-default ((hash global-object) val)
  val)

#+nil (defmethod prop ((hash global-object) key)
	(call-next-method hash key))

(defclass native-function (native-hash)
  ((name :accessor name :initarg :name)
   (proc :accessor proc :initarg :proc)
   (env :accessor env :initarg :env)))

(defmethod initialize-instance :after ((f native-function) &rest args)
  (declare (ignore args))
  (setf (prop f "prototype") (make-instance 'native-hash)))

(defparameter *global* (make-instance 'global-object))
(defparameter js-user::this *global*)

;;;

(defmacro lookup-in-lexchain (name lexchain)
  (let ((obj (gensym))
	(prop (gensym))
	(name-sym (->usersym name)))
    (labels ((count-lookups (chain cnt) ;;todo: move count-lookups to 
	       (cond ((not chain) (values cnt nil)) ;;common labels around both macros
		     ((eq (car chain) :obj)
		      (count-lookups (cdr chain) (1+ cnt)))
		     ((member name (car chain) :test #'string-equal) (values cnt t))
		     (t (count-lookups (cdr chain) cnt))))
	     (build-tree (n found)
	       (if (zerop n)
		   (if found name-sym  `(error (format nil "no prop ~A" ',name)))
		   `(let* ((,obj (car -object-env-stack-))
			   (,prop (prop ,obj ,name nil)))
		      (or ,prop (let ((-object-env-stack-
				       (cdr -object-env-stack-)))
				  ,(build-tree (1- n) found)))))))
      (cond
	((string-equal name "this") 'js-user::this) ;this is always bound
	((string-equal name "arguments") '(!arguments))
	((string-equal name "eval") '(!eval-in-lexenv))
	(t (multiple-value-bind (n found) (count-lookups lexchain 0)
	     `(,@(build-tree n found))))))))

(defmacro set-in-lexchain (name val-exp lexchain)
  (let ((obj (gensym))
	(prop (gensym))
	(name-sym (->usersym name))
	(val (gensym)))
    (labels ((count-lookups (chain cnt) ;;todo: ... look above ...
	       (cond ((not chain) (values cnt nil))
		     ((eq (car chain) :obj)
		      (count-lookups (cdr chain) (1+ cnt)))
		     ((member name (car chain)  :test #'string-equal) (values cnt t))
		     (t (count-lookups (cdr chain) cnt))))
	     (build-tree (n found)
	       (if (zerop n)
		   (if found
		       `(setf ,name-sym ,val)
		       `(setf (prop *global* ,name) ,val))
		   `(let* ((,obj (car -object-env-stack-))
			   (,prop (prop ,obj ,name nil)))
		      (if ,prop (setf (prop ,obj ,name) ,val)
			  (let ((-object-env-stack- (cdr -object-env-stack-)))
			    ,(build-tree (1- n) found)))))))
      (cond
	((string-equal name "this") '(error "invalid assignment left hand side")) ;this is always bound
	#+nil ((arguments) ...) ;todo: arguments support should be put in macrolet
	(t (multiple-value-bind (n found) (count-lookups lexchain 0)
	     `(let ((,val ,val-exp)) ,(build-tree n found))))))))

(defmacro eval-in-lexenv (lex-chain obj-env-stack form)
  (let* ((vars `(,@(apply #'append
			  (mapcar (lambda (el)
				    (when (listp el)
				      (mapcar #'->usersym el))) lex-chain))))
	 (binds `((list '-object-env-stack- (cons 'list ,obj-env-stack))
		  ,@(mapcar (lambda (var) `(list ',var ,var)) vars)))
	 (eval-form 
	  `(cons 'with-ignored-style-warnings
		 (list 
		  (cons 'let (list (list ,@binds) ,form))))))
    `(eval ,eval-form)))

(defmacro eval-function (lex-chain)
  `(!function nil nil (str) nil
	      ((let ((form (process-ast (parse-js-string str) ',lex-chain)))
		 (!return
		  (eval-in-lexenv ,lex-chain -object-env-stack- form))))))

;;;

(defmacro !toplevel (toplevel-vars lex-chain from-eval-p form)
  `(macrolet ((!arguments () 'arguments)
	      (!eval-in-lexenv ()
		(macroexpand `(eval-function ,',lex-chain)))
	      (!name (name)
		(macroexpand `(lookup-in-lexchain ,name ,',lex-chain)))
	      (!setf-name (name val)
		(macroexpand `(set-in-lexchain ,name ,val ,',lex-chain))))
     (with-ignored-style-warnings
       (let* ((-object-env-stack- ,(if from-eval-p
				       '-object-env-stack-
				       '(list *global*))))
	 (progn
	   ,@(mapcar (lambda (var)
		       `(setf (prop *global* ,var)
			      (prop *global* ,var)))
		     (cons "arguments" toplevel-vars)))
	 (progn ,@form)))))

(defmacro !dot (obj attr)
  `(prop ,obj ,attr))

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
       ,@(mapcar (lambda (*prop)
		   `(setf (sub ,obj ,(first *prop)) ,(second *prop)))
		 props)
       ,obj)))

(defmacro !array (elems)
  `(js-new array.ctor (list ,@elems) 'array-object))

(defmacro !stat (form)
  `(progn ,form))

(defmacro !block (&optional form)
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
		       ((!sub !dot) (second func))
		       (t js-user::this)) ,@args))))

(defun js-new (func args class)
  (let* ((proto (prop func "prototype"))
	 (ret (make-instance class
			     :prototype proto
			     :sealed (sealed proto))))
    (apply (proc func) ret args)
    (setf (prop ret "constructor") func)
    ret))

(defmacro !new (func args)
  `(js-new ,func (list ,@args) 'native-hash))

(defmacro !return (ret)
  (declare (ignore ret))
  (error "return not in function"))

(defmacro !named-lambda (name lex-chain args locals body)
  `(let (,(->usersym name))
     (setf ,(->usersym name) (!function ,lex-chain nil ,args ,locals ,body))
     (proc ,(->usersym name))))

(defmacro !lambda (lex-chain args locals body)
  (let* ((additional-args (gensym))
	 (blockname (gensym)))
    `(macrolet ((!arguments ()
		  `(or js-user::arguments
		       (setf js-user::arguments
			     (make-args ,',args ,',additional-args))))
		(!eval-in-lexenv ()
		  (macroexpand `(eval-function ,',lex-chain)))
		(!name (name)
		  (macroexpand `(lookup-in-lexchain ,name ,',lex-chain)))
		(!setf-name (name val)
		  (macroexpand `(set-in-lexchain ,name ,val ,',lex-chain)))
		(!defun (lex-chain name args locals body)
		  `(setf ,(->usersym name)
			 (!function ,lex-chain ,name ,args ,locals ,body)))
		(!return (ret) `,`(return-from ,',blockname ,(or ret :undefined))))
       (lambda (js-user::this
		&optional ,@(mapcar (lambda (arg)
				      `(,(->usersym arg) :undefined)) args)
		&rest ,additional-args)
	 (let (js-user::arguments)
	   (let (,@(mapcar (lambda (var)
			     (list (->usersym var) :undefined)) locals))
	     (block ,blockname ,@body :undefined)))))))

;;todo: adhoc solution -- Function object should be added
(defparameter function.prototype (make-instance 'native-hash))

(defmacro !function (lex-chain name args locals body)
  `(make-instance 'native-function
		  :prototype function.prototype
		  :name ,name
		  :proc ,(if name
			     `(!named-lambda ,name ,lex-chain ,args ,locals ,body)
			     `(!lambda ,lex-chain ,args ,locals ,body))
		  :env ',(car lex-chain)))

(defmacro !defun (lex-chain name args locals body)
  (let ((args2 (gensym))
	(func (gensym)))
    `(let ((,func (!function ,lex-chain ,name ,args ,locals ,body)))
       (setf (prop js-user::this ,name) ,func)
       ;;; intentional violating -- all global functions are case
       ;;; insensitive for easier usage from lisp
       (defun ,(intern (string-upcase name) :js-user) (&rest ,args2)
	 (apply (proc ,func) js-user::this ,args2)))))

(defmacro !with (lex-chain obj body)
  `(let* ((-object-env-stack- (cons ,obj -object-env-stack-)))
     (macrolet ((!eval-in-lexenv ()
		  (macroexpand `(eval-function ,',lex-chain)))
		(!name (name)
		  (macroexpand `(lookup-in-lexchain ,name ,',lex-chain)))
		(!setf-name (name val)
		  (macroexpand `(set-in-lexchain ,name ,val ,',lex-chain))))
       ,body)))

(defmacro js-operators (&rest ops)
  `(progn
     ,@(mapcar (lambda (op)
		 (if (symbolp op)
		     `(setf (symbol-function ',(js-intern op)) (function ,op))
		     `(setf (symbol-function ',(js-intern (first op)))
			    (function ,(second op)))))
	       ops)))

;;;;;;;;
#+nil (defun plus (ls rs)
	(declare (fixnum ls rs))
	(+ ls rs))

#+nil (defun minus (ls rs)
  (declare (fixnum ls rs))
  (the fixnum (- ls rs)))

(defun less (ls rs)
  (declare (fixnum ls rs))
  (the boolean (< ls rs)))
;;;;;;;;
(js-operators
 ;;binary
 #+nil (+ plus) #+nil (- minus) * / (% mod)
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
		  `,`(return-from ,(or (->usersym named-lbl) ',lbl)))
		(!continue (named-lbl)
		  `,`(go ,(or (->usersym named-lbl) ',lbl))))
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
		  `,`(return-from ,(or (->usersym named-lbl) ',lbl)))
		(!continue (named-lbl)
		  `,`(go ,(or (->usersym named-lbl) ',lbl))))
       (block ,lbl
	 (let (,execute-step)
	   ,init
	   (tagbody ,lbl
	      (when ,execute-step ,step)
	      (setf ,execute-step t)
	      (when (js->boolean ,cond)
		,body
		(go ,lbl))))))))

(defmacro !try (lex-chain body var catch finally)
  `(unwind-protect
	(handler-case ,body
	  (t (,(->usersym var))
	    (macrolet ((!name (name)
			 (macroexpand
			  `(lookup-in-lexchain ,name ,',lex-chain)))
		       (!setf-name (name val)
			 (macroexpand
			  `(set-in-lexchain ,name ,val ,',lex-chain)))
		       #+nil (!defun (lex-chain name args locals body)
			       (error "can't build defun in catch block")))
	      ,catch)))
     ,finally))

(defmacro !eval (str)
  (process-ast (parse-js-string str)))

;;;
(defun js-load-file (fname)
  (with-open-file (str fname)
    (eval (process-ast (parse-js str)))))

(defun js-reader (stream)
  `(!eval ,(read-line-stream stream)))

(define-reader 'javascript #'js-reader)

;;;

(defclass arguments (native-hash)
  ((vlen :initarg :vlen :reader vlen)
   (length :initarg :length :reader arg-length)
   (get-arr :initarg :get-arr :reader get-arr)
   (set-arr :initarg :set-arr :reader set-arr)))

(defmethod sub ((args arguments) key)
  (if (and (integerp key) (>= key 0))
      (if (< key (vlen args)) (funcall (aref (get-arr args) key) key)
	  (funcall (aref (get-arr args) (vlen args)) key))
      (call-next-method args key)))

(defmethod (setf sub) (val (args arguments) key)
  (if (and (integerp key) (>= key 0))
      (if (< key (vlen args)) (funcall (aref (set-arr args) key) key val)
	  (funcall (aref (set-arr args) (vlen args)) key val))
      (call-next-method val args key)))

(defmacro make-args (var-names oth)
  (let* ((vars (mapcar (lambda (var) (->usersym var)) var-names))
	 (get-arr (gensym))
	 (set-arr (gensym))
	 (len (length vars)))
    `(let ((,get-arr
	    (make-array ,(1+ len)
			:element-type 'function
			:initial-contents
			(list
			 ,@(mapcar (lambda (var)
				     `(lambda (n) (declare (ignore n)) ,var))
				   vars)
			 (lambda (n) (nth (- n ,len) ,oth)))))
	   (,set-arr
	    (make-array ,(1+ len)
			:element-type 'function
			:initial-contents
			(list
			 ,@(mapcar (lambda (var)
				     `(lambda (n val)
					(declare (ignore n))
					(setf ,var val)))
				   vars)
			 (lambda (n val) (setf (nth (- n ,len) ,oth) val))))))
       (make-instance 'arguments
		      :vlen ,len
		      :length (+ ,len (length ,oth))
		      :get-arr ,get-arr :set-arr ,set-arr))))
