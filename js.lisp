(in-package :js)

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
  (let* ((result (gensym))
	 (lex-values (gensym))
	 (vars `(,@(apply #'append
			  (mapcar (lambda (el)
				    (when (listp el)
				      (mapcar #'->usersym el))) lex-chain))))
	 (binds `((list '-object-env-stack- (cons 'list ,obj-env-stack))
		  ,@(mapcar (lambda (var) `(list ',var ,var)) vars)
		  (list ',result ,form)))
	 (eval-form 
	  `(cons 'with-ignored-style-warnings
		 (list 
		  (cons 'let* (list (list ,@binds)
				    (list 'values ',result
					  (list 'list
						,@(mapcar (lambda (var) `',var) vars)))))))))
    `(multiple-value-bind (,result ,lex-values) (eval ,eval-form)
       (flet ((assign-locals (,lex-values)
		,@(mapcar (lambda (var)
			    `(progn
			       (setf ,var (car ,lex-values))
			       (setf ,lex-values (cdr ,lex-values)))) vars)))
	 (assign-locals ,lex-values)
	 ,result))))

(defmacro eval-function (lex-chain)
  `(!function nil nil (str) nil
	      ((let ((form (process-ast (parse-js-string str) ',lex-chain)))
		 (!return
		  (eval-in-lexenv ,lex-chain -object-env-stack- form))))))

;;
(defmacro !toplevel (toplevel-vars lex-chain from-eval-p form)
  `(macrolet ((!arguments () :undefined)
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

(defmacro !atom (atom) atom)

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
  (case op
    ((!++ !--) `(!setf ,place (,op ,place)))
    ((!- !+) `(,op 0 ,place))
    ((!void) :undefined)
    (t `(,op ,place))))

(defmacro !unary-postfix (op place)
  (let ((ret (gensym)))
    `(let ((,ret ,place))
       (!setf ,place (,op ,place))
       ,ret)))

(defmacro !num (num) (if (integerp num) num
			 (coerce num 'double-float)))

(defmacro !string (str) str)

(defmacro !object (props)
  (let ((obj (gensym)))
    `(let ((,obj (make-instance 'native-hash)))
       ,@(mapcar (lambda (*prop)
		   `(setf (sub ,obj ,(first *prop)) ,(second *prop)))
		 props)
       ,obj)))

(defmacro !array (elems)
  `(js-new array.ctor (list ,@elems)))

(defmacro !regexp (regexp flags)
  `(make-regexp ,regexp ,flags))

(defmacro !stat (form)
  `(progn ,form))

(defmacro !block (&optional form)
  `(progn ,@form))

(defmacro !seq (form1 result)
  `(prog2 ,form1 ,result))

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
		       (t 'js-user::this)) ,@args))))

(defmacro !new (func args)
  `(js-new ,func (list ,@args)))

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
				      `(,(->usersym arg) :undefined-unset)) args)
		&rest ,additional-args)
	 (let (js-user::arguments)
	   (let (,@(mapcar (lambda (var)
			     (list (->usersym var) :undefined)) locals))
	     (block ,blockname ,@body :undefined)))))))

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

(defmacro !binary (op ls rs)
  `(,op ,ls ,rs))

(defmacro !conditional (exp then else)
  `(!if ,exp ,then ,else))

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
	    (macrolet ((!eval-in-lexenv ()
			 (macroexpand `(eval-function ,',lex-chain)))
		       (!name (name)
			 (macroexpand
			  `(lookup-in-lexchain ,name ,',lex-chain)))
		       (!setf-name (name val)
			 (macroexpand
			  `(set-in-lexchain ,name ,val ,',lex-chain)))
		       #+nil (!defun (lex-chain name args locals body)
			       (error "can't build defun in catch block")))
	      ,catch)))
     ,finally))

(defmacro make-args (var-names oth)
  (let* ((vars (mapcar (lambda (var) (->usersym var)) var-names))
	 (get-arr (gensym))
	 (set-arr (gensym))
	 (inst (gensym))
	 (argument-cnt (gensym))
	 (var (gensym))
	 (len (length vars)))
    `(let* ((,argument-cnt(loop for ,var in (list ,@vars)
			     until (eq ,var :undefined-unset)
			     count t))
	   (,get-arr
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
			 (lambda (n val) (setf (nth (- n ,len) ,oth) val)))))
	    (,inst (make-instance 'arguments
				   :vlen ,len
				   :length (+ ,argument-cnt (length ,oth))
				   :get-arr ,get-arr :set-arr ,set-arr)))
       (add-sealed-property ,inst
			    "length"
			    (lambda (-)
			      (declare (ignore -))
			      (+ ,argument-cnt (length ,oth))))
       ,inst)))

;;
(defun js-funcall (func &rest args)
  (apply (the function (proc func)) nil args))

(defmacro js-function (args &body body)
  `(with-ignored-style-warnings
     (!function nil nil ,args nil
		((!return (or (progn ,@body) :undefined))))))

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
  `(or (eq ,exp :undefined)
       (eq ,exp :undefined-unset)))

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
  (process-ast (parse-js-string str)))

(defun js-load-file (fname)
  (with-open-file (str fname)
    (eval (process-ast (parse-js str)))))

(defun js-reader (stream)
  `(!eval ,(read-line-stream stream)))

(define-reader 'javascript #'js-reader)
