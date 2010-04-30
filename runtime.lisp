(in-package :js)

;;
(defgeneric prop (hash key &optional default)
  (:method (hash key &optional default)
    (declare (ignore hash key default)) :undefined))

(defgeneric (setf prop) (val hash key)
  (:method (val hash key) (declare (ignore hash key)) val))

(defgeneric sub (hash key)
  (:method (hash key) (prop hash key)))

(defgeneric (setf sub) (val hash key)
  (:method (val hash key) (setf (prop hash key) val)))

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

(defun finish-class-construction (name ctor proto &key explicit-ctor)
  (declare (special *global*))
  (setf (prop ctor "prototype") proto)
  (setf (prop *global* name) ctor)
  (when explicit-ctor
    (setf (prop proto "constructor") ctor)))

;;
(defparameter value-of nil) ;;function.prototype is not defined yet
(defparameter to-string nil)

(defun add-standard-properties (obj)
  (setf (prop obj "valueOf") value-of)
  (setf (prop obj "toString") to-string)
  obj)

(defparameter *primitive-prototypes* nil)

(defmacro define-primitive-prototype (name exp)
  `(progn
     (defparameter ,name ,exp)
     (push ,name *primitive-prototypes*)))

;;
(defclass native-hash ()
  ((default-value :accessor value :initform nil :initarg :value)
   (dict :accessor dict :initform (make-hash-table :test 'equal))
   (sealed :accessor sealed :initform nil :initarg :sealed)
   (prototype :accessor prototype :initform nil :initarg :prototype)))

(defmethod initialize-instance :after ((obj native-hash) &rest args)
  (declare (ignore args))
  (setf (value obj) obj))

(defmethod set-default ((hash native-hash) val)
  (setf (value hash) val))

(defun add-sealed-property (hash key proc)
  (flet ((ensure-sealed-table (hash)
	   (or (sealed hash)
	       (setf (sealed hash) (make-hash-table :test 'equal)))))
    (let ((sealed-table (ensure-sealed-table hash)))
      (setf (gethash key sealed-table) proc))))

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

(defmethod (setf prop) (val (hash native-hash) key)
  (setf (gethash key (dict hash)) val))

;;
(defclass global-object (native-hash)
  ())

(defmethod (setf prop) (val (hash global-object) key)
  (set (if (stringp key) (intern (string-upcase key) :js-user) key) val);;todo:
  (call-next-method val hash key))

(defmethod set-default ((hash global-object) val)
  val)

(defparameter *global* (make-instance 'global-object))
(defparameter js-user::this *global*)

(setf (prop *global* "undefined") :undefined)

;;
(defclass native-function (native-hash)
  ((name :accessor name :initarg :name)
   (proc :accessor proc :initarg :proc)
   #+(or)(env :accessor env :initarg :env)))

(defmethod initialize-instance :after ((f native-function) &rest args)
  (declare (ignore args))
  (setf (prop f "prototype") (add-standard-properties (make-instance 'native-hash))))

(define-primitive-prototype function.prototype
    (make-instance 'native-function
		   :proc (lambda (&rest args) (declare (ignore args)) :undefined)))

(defun new-function (&rest args) ;;due to parser error it is
				 ;;impossible to use anonymous
				 ;;function as a atandalone expression
				 ;;so we propagate it via identity
				 ;;lambda
  (eval
   (translate
    (parse-js-string
     (if args
	 (format nil "(function(val) {return val;})(function (~{~a~^, ~}) {~A});"
		 (butlast args) (car (last args)))
	 "(function(val) {return val;})(function () {});")))))

(defmethod set-default ((func native-function) val)
  (setf (prototype func) function.prototype)
  (setf (proc func) (proc val))
  (setf (name func) nil)
  (call-next-method func val))

(defparameter function.ctor
  (js-function ()
    (let ((func (apply #'new-function (arguments-as-list (!arguments)))))
      (set-default js-user::this func)
      func)))

(defmethod placeholder-class ((func (eql function.ctor))) 'native-function)

(finish-class-construction "Function"
			   function.ctor function.prototype :explicit-ctor t)

;;
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

(defun arguments-as-list (args)
  (loop for i from 0 below (arg-length args)
     collecting (sub args i)))


;;; todo: array differs from string (according to spidermonkey) in
;;; respect of calling constructor without operator new. string
;;; returns a new basic string instead of an object, but arrays behave
;;; like there is no difference whether the ctor is invoked with or
;;; without new. our current implementation implements array to behave
;;; like string. recheck the spec
;;
(defclass array-object (native-hash)
  ())

(defmethod prop ((arr array-object) key &optional (default :undefined))
  (if (and (integerp key)
	   (< key (length (value arr)))) ;;todo: safe conversion to integer
      (aref (value arr) key)
      (call-next-method arr key default)))

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
  (js-function ()
    (let* ((len (js::arg-length (!arguments)))
	   (arr (make-array len
			    :fill-pointer len
			    :initial-contents
			    (arguments-as-list (!arguments)))))
      (set-default js-user::this arr)
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
    (let ((str (if (eq obj :undefined-unset) "" (to-string (value obj)))))
      (set-default js-user::this str)
      (the string str))))

(defparameter string.ensure string.ctor)

(define-primitive-prototype string.prototype (js-new js::string.ctor '("")))

(defmethod prop ((str string) key &optional (default :undefined))
  (let* ((sealed (sealed string.prototype))
	 (action (gethash key sealed)))
    (if action
	(funcall action str)
	(prop string.prototype key default))))

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
    (let ((val (cond ((eq n :undefined-unset) 0)
		     ((js-number? n) (value n))
		     (t (with-input-from-string (s (to-string (value n)))
			  (let ((n2 (read s)))
			    (if (js-number? n2) n2 :NaN)))))))
      (set-default js-user::this val)
      (the js.number val))))

(defparameter number.ensure
  (js-function (arg)
    (if (js-number? arg) arg
	(js-funcall number.ctor arg))))

(define-primitive-prototype number.prototype (js-new number.ctor '(0)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (shadow 'Number 'js-user)) ;;todo: ...

(defmethod prop ((num number) key &optional (default :undefined))
  (let* ((sealed (sealed number.prototype))
	 (action (and sealed (gethash key sealed)))) ;todo: not sure yet wether nums have sealed props
    (if action
	(funcall action num)
	(prop number.prototype key default))))

(finish-class-construction "Number" number.ctor number.prototype)

;;
(defclass math (native-hash)
  ())

(defparameter math.obj (make-instance 'math))

(setf (prop *global* "Math") math.obj)

;;
(defclass regexp (native-function)
  ((expr :accessor expr :initarg :expr)
   (scanner :accessor scanner :initarg :scanner)
   (globalp :accessor globalp :initarg :global)
   (case-sensitive-p :accessor case-sensitive-p :initarg :case-insensitive)))

(defun check-flag (flag)
  (or (car (member flag '("" "i" "g" "ig" "gi") :test #'string=))
      (error (format nil "SyntaxError: invalid regular expression flag ~A" flag))))

(defparameter regexp.ctor
  (js-function (expr flags)
    (let ((expr (if (eq expr :undefined-unset) "(?:)"
		    (js-funcall string.ensure  expr)))
	  (flags (if (eq flags :undefined-unset) ""
		     (check-flag flags))))
      ;(set-default js-user::this (format nil "/~A/~A" expr flags))
      (let ((re (make-regexp expr flags)))
	(set-default js-user::this re)
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
