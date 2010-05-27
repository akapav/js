(in-package :js)

;;;;;

(eval-when (:compile-toplevel :load-toplevel :execute)

(defun stringify (name)
  (if (stringp name) name
      (format nil "~a" name)))

(defun generic-getter-name (name)
  (->usersym (concatenate 'string (symbol-name 'generic-getter-) (stringify name))))

(defun generic-setter-name (name)
  (->usersym (concatenate 'string (symbol-name 'generic-setter-) (stringify name))))

(defun slot-name->initarg (slot-name)
  (list :name (->usersym (stringify slot-name))
	:initform :%unset
	:initfunction (lambda () :%unset)
	:readers `(,(generic-getter-name slot-name))
	:writers `(,(generic-setter-name slot-name))))

(defun ensure-getter (key)
  (let ((name (generic-getter-name key)))
    (if (fboundp name)
	(symbol-function name)
	(let ((gf (ensure-generic-function name)))
	  (add-method
	   gf
	   (make-instance
	    'standard-method
	    :specializers (list (find-class 'js-object))
	    :lambda-list '(obj)
	    :function (lambda (args nm)
			(declare (ignore nm))
			(let ((obj (car args)))
			  (%ensure-getter obj key)
			  (funcall gf obj)))))))))

(defun ensure-setter (key)
  (let ((name (generic-setter-name key)))
    (if (fboundp name)
	(symbol-function name)
	(let ((gf (ensure-generic-function name)))
	  (add-method
	   gf
	   (make-instance
	    'standard-method
	    :specializers (list (find-class t) (find-class 'js-object))
	    :lambda-list '(val obj)
	    :function (lambda (args nm)
			(declare (ignore nm))
			(setf (prop (second args) key) (first args)))))))))


(define-compiler-macro prop (&whole form obj key)
  (if (stringp key)
      (progn
	(ensure-getter key)
	`(get-using-getter (function ,(generic-getter-name key)) ,obj))
      `,form))

(define-compiler-macro (setf prop) (&whole form val obj key)
  (if (stringp key)
      (progn
	(ensure-setter key)
	`(,(generic-setter-name key) ,val ,obj))
      `,form))

;;
(defclass js-class (standard-class)
  ((slot-class-mappings
    :accessor slot-class-mappings
    :initform (make-hash-table :test 'equal))))

(defmethod validate-superclass
    ((clss js-class) (base standard-class)) t)

(defmethod validate-superclass
    ((clss js-class) (base js-class)) t)

(defmethod validate-superclass
    ((clss standard-class) (base js-class)) t)

;;
(defclass js-object ()
  ((prototype :accessor prototype :initarg :prototype :initform nil)
   (sealed :accessor sealed :initform nil))
  (:metaclass js-class))

;;
(defgeneric js-add-property (obj property-name))

(defun %inherit-with-property (cls property-name)
  (let ((new-class-name (gensym "jsos")))
    (ensure-class new-class-name
		  :direct-superclasses (list cls)
		  :direct-slots (list (slot-name->initarg property-name))
		  :metaclass 'js-class)
    (setf (gethash property-name (slot-class-mappings cls)) new-class-name)
    new-class-name))

(defmethod js-add-property ((obj js-object) property-name)
  (let* ((cls (class-of obj))
	 (to-class (gethash property-name (slot-class-mappings cls))))
    (ensure-getter property-name)
    (ensure-setter property-name)
    (change-class obj
		  (or to-class
		      (%inherit-with-property cls property-name)))))

(defgeneric js-add-sealed-property (obj property-name proc))

(defmethod js-add-sealed-property ((obj js-object) property-name proc)
  (let* ((get-generic (ensure-getter property-name))
	 (set-generic (ensure-setter property-name))
	 (prop-getter
	  (make-instance
	   'standard-method
	   :specializers (list (class-of obj))
	   :lambda-list '(obj)
	   :function (lambda (args nm)
		       (declare (ignore nm))
		       (funcall proc (car args)))))
	 (prop-setter
	  (make-instance
	   'standard-method
	   :specializers (list (find-class t) (class-of obj))
	   :lambda-list '(val obj)
	   :function (lambda (args nm)
		       (declare (ignore nm))
		       (car args)))))
    (pushnew property-name (sealed obj) :test #'string=)
    (add-method set-generic prop-setter)
    (add-method get-generic prop-getter)))

(defun sealed-property? (obj property-name)
  (or (member property-name (sealed obj) :test #'string=)
      (and (prototype obj) (sealed-property? (prototype obj) property-name))))

;;
(defun get-using-getter (getter obj)
  (declare (function getter))
  (let ((ret (funcall getter obj)))
    (if (eq ret :%unset)
	(let ((prototype (prototype obj)))
	  (if prototype
	      (get-using-getter getter prototype)
	      (values :undefined nil)))
	(values ret t))))

(defun %ensure-getter (obj key)
  (let* ((key (stringify key))
	 (key-sym (->usersym key))
	 #+nil (getter (ensure-getter key)))
    (unless (or (slot-exists-p obj key-sym) (sealed-property? obj key))
      (js-add-property obj key))
    #+nil (funcall getter obj)))

(defgeneric prop (obj key)
  (:method (obj key)
;    (%ensure-getter obj key)
    (get-using-getter (ensure-getter key) obj)))

(defgeneric (setf prop) (val obj key)
  (:method (val obj key)
    (let* ((key (stringify key))
	   (key-sym (->usersym key))
	   (setter (ensure-setter key)))
      (unless (or (slot-exists-p obj key-sym) (sealed-property? obj key))
	(js-add-property obj key))
      (funcall setter val obj))))

;;
(defun js-clone (&optional prototype)
  (if prototype
      (let* ((proto-class (class-of prototype))
	     (obj (make-instance (class-name proto-class) :prototype prototype)))
	obj)
      (make-instance 'js-object))))

;;

(defmacro prop* (obj key default)
  (let ((val (gensym))
	(found (gensym)))
    `(multiple-value-bind (,val ,found) (prop ,obj ,key)
       (if ,found ,val ,default))))

;;;;
;;
#+nil (defgeneric prop (hash key &optional default)
  (:method (hash key &optional default)
    (declare (ignore hash key default)) :undefined))

#+nil (defgeneric (setf prop) (val hash key)
  (:method (val hash key) (declare (ignore hash key)) val))

(defgeneric sub (hash key)
  (:method (hash key) (prop hash key)))

(defgeneric (setf sub) (val hash key)
  (:method (val hash key) (setf (prop hash key) val)))

(defgeneric list-props (hash)
  (:method (hash) (declare (ignore hash)) ()))

(defgeneric placeholder-class (func)
  (:method (func) (declare (ignore func)) 'native-hash))

(defgeneric set-default (hash val) ;;todo: put it as a slot writer
  (:method (hash val) (declare (ignore hash)) val))

(defgeneric value (obj)
  (:method (obj) obj))

(defgeneric to-string (obj)
  (:method (obj) (format nil "~A" obj)))

(defgeneric prototype (obj)
  (:method (obj) (declare (ignore obj)) nil))

(defun ensure-accessors (key)
  (ensure-getter key)
  (ensure-setter key))

(defmacro set-ensured (obj key val)
  `(progn
     (ensure-accessors ,key)
     (setf (prop ,obj ,key) ,val)))

(defun finish-class-construction (name ctor proto &key explicit-ctor)
  (declare (special *global*))
  (set-ensured ctor "prototype" proto)
  (set-ensured *global* name ctor)
  (when explicit-ctor
    (set-ensured proto "constructor" ctor)))

;;
(defparameter value-of nil) ;;function.prototype is not defined yet
(defparameter to-string nil)

(defun add-standard-properties (obj)
  (set-ensured obj "valueOf" value-of)
  (set-ensured obj "toString" to-string)
  obj)

(defparameter *primitive-prototypes* nil)

(defmacro define-primitive-prototype (name exp)
  `(progn
     (defparameter ,name ,exp)
     (push ,name *primitive-prototypes*)))

;;
(defclass native-hash (js-object)
  ((default-value :accessor value :initform nil :initarg :value))
  (:metaclass js-class))

(defmethod initialize-instance :after ((obj native-hash) &rest args)
  (declare (ignore args))
  (setf (value obj) obj))

(defmethod set-default ((hash native-hash) val)
  (setf (value hash) val))

(defmethod list-props ((hash native-hash))
  #+nil(loop :for prop :being :the :hash-keys :in (dict hash) :collect prop) nil)

(mapc #'ensure-accessors
      '("prototype" "constructor"))

(defun js-new (func args)
  (let* ((proto (prop* func "prototype" nil))
	 (ret (js-clone proto)))
    (apply (the function (proc func)) ret args)
    (setf (prop ret "constructor") func)
    ;;todo: put set-default here
    ret))

;;
(defclass global-object (native-hash)
  ()
  (:metaclass js-class))

(defmethod set-default ((hash global-object) val)
  val)

(defparameter *global* (make-instance 'global-object))

(set-ensured *global* "this" *global*)
(set-ensured *global* "undefined" :undefined)

;;
(defclass native-function (native-hash)
  ((name :accessor name :initarg :name)
   (proc :accessor proc :initarg :proc))
  (:metaclass js-class))

(defmethod proc (arg) ;; TODO make proper Error objects
  (error "~a is not a function." arg))

(defmethod initialize-instance :after ((f native-function) &rest args)
  (declare (ignore args))
  (setf (prop f "prototype") (add-standard-properties (make-instance 'native-hash))))

(define-primitive-prototype function.prototype
    (make-instance 'native-function
		   :proc (lambda (&rest args) (declare (ignore args)) :undefined)))

(defun new-function (&rest args)
  (eval
   (translate
    (parse-js-string
     (if args
	 (format nil "(function (~{~a~^, ~}) {~A});"
		 (butlast args) (car (last args)))
	 "(function () {});")))))

(defmethod set-default ((func native-function) val)
  (setf (prototype func) function.prototype)
  (setf (proc func) (proc val))
  (setf (name func) nil)
  (call-next-method func val))

(defparameter function.ctor
  (js-function (&rest args)
    (let ((func (apply #'new-function args)))
      (set-default (!this) func)
      func)))

(defmethod placeholder-class ((func (eql function.ctor))) 'native-function)

(finish-class-construction "Function"
			   function.ctor function.prototype :explicit-ctor t)

(defparameter object.ctor
  (js-function (val)
    (set-default (!this) val)
    val))

(define-primitive-prototype object.prototype (js-new object.ctor (list (make-instance 'native-hash))))

(finish-class-construction "Object" object.ctor object.prototype)

;;
(defclass arguments (native-hash)
  ((argument-vector :initarg :vector :reader argument-vector))
  (:metaclass js-class))

(defun make-args (arguments)
  (let* ((vec (coerce arguments 'vector))
         (obj (make-instance 'arguments :vector vec)))
    (js-add-sealed-property obj "length"
                         (lambda (ignore) (declare (ignore ignore)) (length vec)))
    obj))

(defmethod sub ((args arguments) key)
  (let ((vec (argument-vector args)))
    (if (and (integerp key) (>= key 0) (< key (length vec)))
        (svref vec key)
        (call-next-method args key))))

(defmethod (setf sub) (val (args arguments) key)
  (let ((vec (argument-vector args)))
    (if (and (integerp key) (>= key 0) (< key (length vec)))
        (setf (svref vec key) val)
        (call-next-method val args key))))

(defun arguments-as-list (args) ;; TODO drop, access vector
  (coerce (argument-vector args) 'list))

;;; todo: array differs from string (according to spidermonkey) in
;;; respect of calling constructor without operator new. string
;;; returns a new basic string instead of an object, but arrays behave
;;; like there is no difference whether the ctor is invoked with or
;;; without new. our current implementation implements array to behave
;;; like string. recheck the spec
;;
(defclass array-object (native-hash)
  ()
  (:metaclass js-class))

(defmethod prop ((arr array-object) key)
  (if (and (integerp key)
	   (< key (length (value arr)))) ;;todo: safe conversion to integer
      (aref (value arr) key)
      (call-next-method arr key)))

(defmethod (setf prop) (val (arr array-object) key)
  (if (integerp key) ;;todo: ... as above ...
      (let* ((arr (value arr))
	     (len (length arr)))
	(when (>= key len)
	  (loop for ndx from len to key do
	       (vector-push-extend :undefined arr)))
	(setf (aref (value arr) key) val))
      (call-next-method val arr key)))

(defparameter array.ctor
  (js-function (&rest args)
    (let* ((len (length args))
	   (arr (make-array len :adjustable t
			    :fill-pointer len
			    :initial-contents args)))
      (set-default (!this) arr)
      arr)))

(defmethod placeholder-class ((func (eql array.ctor))) 'array-object)

(define-primitive-prototype array.prototype (js-new js::array.ctor ()))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (shadow 'Array 'js-user)) ;;todo: ...

(finish-class-construction "Array" array.ctor array.prototype)

;;
(defun js-string? (o)
  (declare (special string.prototype))
  (typecase o
    (string t)
    (t (eq (prototype o) string.prototype))))

(defparameter string.ctor
  (js-function (obj)
    (let ((str (if (eq obj :undefined) "" (to-string (value obj)))))
      (set-default (!this) str)
      (the string str))))

(defparameter string.ensure string.ctor)

(define-primitive-prototype string.prototype (js-new js::string.ctor '("")))

(defmethod prop ((str string) key)
  (let* ((sealed (sealed string.prototype))
	 (action (gethash key sealed)))
    (if action
	(funcall action str)
	(prop string.prototype key))))

(finish-class-construction "String" string.ctor string.prototype)

;;
(defun js-number? (o)
  (declare (special number.prototype))
  (typecase o
    (real t)
    (symbol
     (or (realp o)
	 (eq o :NaN)
	 (eq o :Inf)
	 (eq o :-Inf)))
    (t (eq (prototype o) number.prototype))))

(deftype js.number ()
  `(satisfies js-number?))

(defparameter number.ctor
  (js-function (n)
    (let ((val (cond ((eq n :undefined) 0)
		     ((js-number? n) (value n))
		     (t (with-input-from-string (s (to-string (value n)))
			  (let ((n2 (read s)))
			    (if (js-number? n2) n2 :NaN)))))))
      (set-default (!this) val)
      (the js.number val))))

(defparameter number.ensure
  (js-function (arg)
    (if (js-number? arg) arg
	(js-funcall number.ctor arg))))

(define-primitive-prototype number.prototype (js-new number.ctor '(0)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (shadow 'Number 'js-user)) ;;todo: ...

(defmethod prop ((num number) key)
  (let* ((sealed (sealed number.prototype))
	 (action (and sealed (gethash key sealed)))) ;todo: not sure yet wether nums have sealed props
    (if action
	(funcall action num)
	(prop number.prototype key))))

(finish-class-construction "Number" number.ctor number.prototype)

;;
(defclass math (native-hash)
  ()
  (:metaclass js-class))

(defparameter math.obj (make-instance 'math))

(set-ensured *global* "Math" math.obj)

;;
(defclass regexp (native-function)
  ((expr :accessor expr :initarg :expr)
   (scanner :accessor scanner :initarg :scanner)
   (globalp :accessor globalp :initarg :global)
   (case-sensitive-p :accessor case-sensitive-p :initarg :case-insensitive))
  (:metaclass js-class))

(defun check-flag (flag)
  (or (car (member flag '("" "i" "g" "ig" "gi") :test #'string=))
      (error (format nil "SyntaxError: invalid regular expression flag ~A" flag))))

(defparameter regexp.ctor
  (js-function (expr flags)
    (let ((expr (if (eq expr :undefined) "(?:)"
		    (js-funcall string.ensure  expr)))
	  (flags (if (eq flags :undefined) ""
		     (check-flag flags))))
      ;(set-default (!this) (format nil "/~A/~A" expr flags))
      (let ((re (make-regexp expr flags)))
	(set-default (!this) re)
	re))))

(defun make-regexp (expr flags)
  (let* ((case-sens (search "i" flags))
	 (scanner (ppcre:create-scanner expr
					:case-insensitive-mode case-sens)))
    (make-instance 'regexp
		   :expr expr
		   :scanner scanner
		   :global (search "g" flags)
		   :case-insensitive case-sens
		   :proc (lambda (-- &optional str)
			   (declare (ignore --))
			   (let ((str (js-funcall string.ensure str)))
			     (multiple-value-bind (from to) (ppcre:scan scanner str)
			       (if from (subseq str from to) :null)))))))

(define-primitive-prototype regexp.prototype
    (make-regexp "(?:)" ""))

(defmethod placeholder-class ((func (eql regexp.ctor))) 'regexp)

(finish-class-construction "RegExp"
			   regexp.ctor regexp.prototype :explicit-ctor t)

;;todo: regexp object constructed with // haven't valid prototype
