(in-package :js)

#+nil (defgeneric prop (hash key &optional default))

#+nil (defgeneric (setf prop) (val hash key))

(defgeneric sub (hash key)
  (:method (hash key) (prop hash key)))

(defgeneric (setf sub) (val hash key)
  (:method (val hash key) (setf (prop hash key) val)))

(defgeneric list-props (hash)
  (:method (hash) (declare (ignore hash)) ()))

(defgeneric (setf value) (obj val)
  (:method (obj val) (declare (ignore obj)) val))

(defgeneric value (obj)
  (:method (obj) obj))

(defgeneric proc (arg)
  (:method (arg)
    (error "~a is not a function." arg)))

(defgeneric to-string (obj)
  (:method (obj) (format nil "~A" obj)))

(defgeneric prototype (obj)
  (:method (obj) (declare (ignore obj)) nil))

(defun finalize-class-construction (name ctor proto &key explicit-ctor)
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

(defmethod list-props ((hash native-hash))
  #+nil(loop :for prop :being :the :hash-keys :in (dict hash) :collect prop) nil)

;;
(defclass global-object (native-hash)
  ()
  (:metaclass js-class))

(defmethod (setf value) (val (hash global-object))
  (declare (ignore hash))
  val)

(defparameter *global* (make-instance 'global-object))

(set-ensured *global* "this" *global*)
(set-ensured *global* "undefined" :undefined)

;;
(defclass native-function (native-hash)
  ((name :accessor name :initarg :name)
   (proc :accessor proc :initarg :proc))
  (:metaclass js-class))

(defmethod initialize-instance :after ((f native-function) &rest args)
  (declare (ignore args))
  (set-attribute f "prototype"
		 (add-standard-properties (make-instance 'native-hash))))

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

(defmethod (setf value) (val (func native-function))
  (setf (prototype func) function.prototype)
  (setf (proc func) (proc val))
  (setf (name func) nil)
  (call-next-method func val))

(defparameter function.ctor
  (js-function (&rest args)
    (let ((func (apply #'new-function args)))
      (setf (value (!this)) func)
      func)))

(finalize-class-construction
 "Function" function.ctor function.prototype :explicit-ctor t)

;;
(defparameter object.ctor
  (js-function (val)
    (setf (value (!this)) val)
    val))

(define-primitive-prototype
    object.prototype
    (js-new-ignore-prototype
     object.ctor (list (make-instance 'native-hash))))

(finalize-class-construction
 "Object" object.ctor object.prototype)

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
      (setf (value (!this)) arr)
      arr)))

(define-primitive-prototype
    array.prototype
    (js-new-ignore-prototype js::array.ctor () 'array-object))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (shadow 'Array 'js-user)) ;;todo: ...

(finalize-class-construction
 "Array" array.ctor array.prototype)

;;
(defun js-string? (o)
  (declare (special string.prototype))
  (typecase o
    (string t)
    (t (eq (prototype o) string.prototype))))

(defparameter string.ctor
  (js-function (obj)
    (let ((str (if (eq obj :undefined) "" (to-string (value obj)))))
      (setf (value (!this)) str)
      (the string str))))

(defparameter string.ensure string.ctor)

(define-primitive-prototype
    string.prototype
    (js-new-ignore-prototype js::string.ctor '("")))

(defmethod prop ((str string) key)
  (let ((getter (ensure-getter key)))
    (if (sealed-property? string.prototype key)
	(funcall getter str)
	(funcall getter string.prototype))))

(finalize-class-construction
 "String" string.ctor string.prototype)

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
      (setf (value (!this)) val)
      (the js.number val))))

(defparameter number.ensure
  (js-function (arg)
    (if (js-number? arg) arg
	(js-funcall number.ctor arg))))

(define-primitive-prototype
    number.prototype
    (js-new-ignore-prototype number.ctor '(0)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (shadow 'Number 'js-user)) ;;todo: ...

(defmethod prop ((num number) key)
  (prop number.prototype key))

(finalize-class-construction
 "Number" number.ctor number.prototype)

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
      (let ((re (make-regexp expr flags)))
	(setf (value (!this)) re)
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

(finalize-class-construction
"RegExp" regexp.ctor regexp.prototype :explicit-ctor t)

;;todo: regexp object constructed with // haven't valid prototype
