(in-package :js)

(eval-when (:compile-toplevel :load-toplevel)
  (defun ctor (sym) (symconc sym "." 'ctor))
  (defun ensure (sym) (symconc sym "." 'ensure)))

(defmacro define-js-method (type name args &body body)
  (flet ((arg-names (args)
	   (mapcar (lambda (arg) (if (consp arg) (first arg) arg)) args))
	 (arg-defaults (args)
	   (mapcar (lambda (arg) (if (consp arg) (second arg) :undefined)) args)))
    (let ((canonical-name (symconc type "." name))
	  (ctor-name (ctor type))
	  (ensure-name (ensure type))
	  (prototype-name (symconc type "." 'prototype))
	  (arg-names (arg-names args))
	  (arg-defaults (arg-defaults args)))
      `(progn
	 (defparameter ,canonical-name
	   (js-function ,arg-names
	     (let ((,(car arg-names)
		    (js-funcall ,ensure-name ,(car arg-names)))
		   ,@(mapcar
		      (lambda (name val)
			`(,name (if (undefined? ,name) ,val ,name)))
		      (cdr arg-names) (cdr arg-defaults)))
	       ,@body)))
	 (set-ensured ,prototype-name ,name
	       (js-function ,(cdr arg-names)
		 (js-funcall
		  ,canonical-name (value (!this)) ,@(cdr arg-names))))
	 (set-ensured ,ctor-name ,name ,canonical-name)))))

(defmacro with-asserted-types ((&rest type-pairs) &body body)
  `(let (,@(mapcar (lambda (pair)
		     `(,(first pair) (js-funcall
				      ,(ensure (second pair)) ,(first pair))))
		   type-pairs))
     ;;todo: type declarations
     ,@body))

;;
(setf value-of (js-function () (value (!this))))

(defmethod to-string ((obj (eql *global*)))
  "[object global]")

(defmethod to-string ((obj native-hash))
  "[object Object]")

(defmethod to-string ((obj native-function))
  (if (name obj)
      (format nil "function ~A() {[code]}" (name obj))
      "function () {[code]}"))

(defmethod to-string ((obj string))
  obj)

(defmethod to-string ((obj vector))
  (declare (special #.(symconc 'array ".join")))
  (js-funcall #.(symconc 'array ".join") obj ","))

(setf to-string (js-function () (to-string (value (!this)))))

(mapc 'add-standard-properties (cons *global* *primitive-prototypes*))

;;
(js-add-sealed-property string.prototype
		     "length"
		     (lambda (obj) (length (value obj))))

(js-add-sealed-property string.ctor "length" (constantly 1))
;;not sure what is the meaning of that property. recheck the spec

(defun clip-index (n)
  (if (eq n :NaN) 0
      (let ((n (floor n)))
	(if (< n 0) 0 n))))

(define-js-method string "charAt" (str (ndx 0))
  (with-asserted-types ((ndx number))
    (string (aref str (clip-index ndx)))))

(define-js-method string "indexOf" (str substr (beg 0))
  (if (undefined? substr) -1
      (with-asserted-types ((substr string)
			    (beg number))
	(or (search substr str :start2 (clip-index beg)) -1))))

(define-js-method string "lastIndexOf" (str substr)
  (if (undefined? substr) -1
      (with-asserted-types ((substr string))
	(or (search substr str :from-end t) -1))))

(define-js-method string "substring" (str (from 0) to)
  (if (undefined? to)
      (with-asserted-types ((from number))
	(subseq str (clip-index from)))
      (with-asserted-types ((from number)
			    (to number))
	(let* ((from (clip-index from))
	       (to (max from (clip-index to))))
	  (subseq str from (min to (length str)))))))

;todo: maybe rewrite it to javascript (see Array.shift)
(define-js-method string "substr" (str (from 0) to)
  (if (undefined? to)
      (js-funcall #.(symconc 'string ".substring") str from)
      (with-asserted-types ((from number)
			    (to number))
	(let ((from (clip-index from)))
	  (js-funcall #.(symconc 'string ".substring") str from (+ from (clip-index to)))))))

(define-js-method string "toUpperCase" (str)
  (string-upcase str))

(define-js-method string "toLowerCase" (str)
  (string-downcase str))

(defun string-split (str delim)
  (let ((step (length delim)))
    (if (zerop step) (list str)
	(loop for beg = 0 then (+ pos step)
	   for pos = (search delim str) then (search delim str :start2 (+ pos step))
	   collecting (subseq str beg pos)
	   while pos))))

(define-js-method string "split" (str delimiter)
  (with-asserted-types ((str string)
			(delimiter string))
    (declare (special array.ctor))
    (js-new array.ctor (string-split str delimiter))))

;;
(set-ensured
 function.prototype "call"
 (js-function (context &rest args)
   (apply (proc (!this)) context args)))

(set-ensured
 function.prototype "apply"
 (js-function (context argarr)
   (apply (proc (!this)) context
	  (coerce (typecase argarr
		    (vector argarr)
		    (array-object (value argarr))
		    (arguments (arguments-as-list argarr))
		    (t (error "second argument to apply must be an array")))
		  'list))))
;;
(defparameter array.ensure
  (js-function (arg)
    (let ((val (value arg)))
      (if (typep val 'vector) val
	  (js-funcall array.ctor arg)))))

(js-add-sealed-property array.prototype
		     "length"
		     (lambda (obj) (length (value obj))))

(js-add-sealed-property array.ctor "length" (constantly 1))
;;not sure what is the meaning of that property. recheck the spec

;; concat can't use standard define-js-method macro because it takes
;; variable number of arguments
(defparameter #.(symconc 'array ".concat")
  (js-function (&rest args)
    (let ((arr (apply #'concatenate 'list
                      (loop :for arg :in args :collect
                         (js-funcall array.ensure arg)))))
      (js-new array.ctor arr))))

(set-ensured
 array.prototype "concat"
 (js-function (&rest args)
   (apply #'js-funcall #.(symconc 'array ".concat")
	  (value (!this)) args)))

(set-ensured array.ctor "concat" #.(symconc 'array ".concat"))

(define-js-method array "join" (arr str)
  (let ((str (if (undefined? str) "," str)))
    (with-asserted-types ((str string))
      (format nil
	      (format nil "~~{~~A~~^~A~~}" str)
	      (mapcar (lambda (val) (to-string (value val)))
		      (coerce arr 'list))))))

(defmacro with-asserted-array ((var) &body body)
  `(let ((,var (value (if (typep (value ,var) 'vector) ,var
			  (js-funcall array.ctor ,var)))))
     ,@body))

(defun nthcdr+ (n list)
  (loop for i from 0 to n
     for l = list then (cdr l)
     for coll = (list (car l)) then (cons (car l) coll)
     while l
     finally (return (values l (reverse (cdr coll))))))

(defun vector-splice (v ndx elements-to-remove &rest insert)
  (let* ((elems (coerce v 'list))
	 (len (length v))
	 (insert (copy-list insert))
	 (removed
	  (cond ((>= ndx len) (nconc elems insert) nil)
		((zerop ndx)
		 (multiple-value-bind (elems- removed)
		     (nthcdr+ elements-to-remove elems)
		   (setf elems (append insert elems-))
		   removed))
		(t
		 (let ((c1 (nthcdr (1- ndx) elems)))
		   (multiple-value-bind (c2 removed)
		       (nthcdr+ (1+ elements-to-remove) c1)
		     (setf (cdr c1) insert)
		     (if insert
			 (let ((c3 (last insert)))
			   (setf (cdr c3) c2))
			 (setf (cdr c1) c2))
		     (cdr removed)))))))
    (let ((len (length elems)))
      (adjust-array v len
		    :fill-pointer len
		    :initial-contents elems))
    removed))

(defun apply-splicing (arr ndx howmany new-elts)
  (js-new array.ctor
          (apply #'vector-splice arr ndx howmany new-elts)))

(defparameter #.(symconc 'array ".splice")
  (js-function (arr ndx howmany &rest args)
    (with-asserted-array (arr)
      (cond ((and (undefined? ndx) (undefined? howmany))
	     (apply-splicing arr 0 0 args))
	    ((undefined? ndx)
	     (with-asserted-types ((howmany number))
	       (apply-splicing arr 0 howmany args)))
	    ((undefined? howmany)
	     (with-asserted-types ((ndx number))
	       (apply-splicing arr ndx (length arr) args)))
	    (t (with-asserted-types ((ndx number)
				     (howmany number))
		 (apply-splicing arr ndx howmany args)))))))

(set-ensured
 array.prototype "splice"
 (js-function (&rest args)
   (apply #'js-funcall #.(symconc 'array ".splice") (!this)
	  args)))

(set-ensured array.ctor "splice" #.(symconc 'array ".splice"))

(define-js-method array "pop" (arr)
  (unless (zerop (length arr))
    (vector-pop arr)))

(defparameter #.(symconc 'array ".push")
  (js-function (arr &rest args)
    (with-asserted-array (arr)
      (mapc (lambda (el) (vector-push-extend el arr)) args)
      (length arr))))

(set-ensured array.ctor "push" #.(symconc 'array ".push"))

(set-ensured
 array.prototype "push"
 (js-function (&rest args)
   (apply #'js-funcall #.(symconc 'array ".push") (!this) args)))

(define-js-method array "reverse" (arr)
  (nreverse arr))

(defparameter js.lexsort
  (js-function (ls rs)
    (string< (js-funcall string.ensure ls)
             (js-funcall string.ensure rs))))

(define-js-method array "sort" (arr (func js.lexsort))
    (print func)
    (sort arr (lambda (ls rs) (js-funcall func ls rs))))
  
;;
(set-ensured number.ctor "MAX_VALUE" most-positive-double-float)
(set-ensured number.ctor "MIN_VALUE" most-negative-double-float)
(set-ensured number.ctor "POSITIVE_INFINITY" :NaN)
(set-ensured number.ctor "NEGATIVE_INFINITY" :-NaN)

;;
(defmacro math-function ((arg &key (inf :NaN) (minf :-NaN) (nan :NaN)) &body body)
  `(js-function (,arg)
     (let ((,arg (js-funcall number.ensure ,arg)))
       (case ,arg
	 ((:NaN) ,nan)
	 ((:Inf) ,inf)
	 ((:-Inf) ,minf)
	 (t (progn ,@body))))))	     

(set-ensured math.obj "E" (exp 1))
(set-ensured math.obj "LN2" (log 2))
(set-ensured math.obj "LN10" (log 10))
(set-ensured math.obj "LOG2E" (log (exp 1) 2))
(set-ensured math.obj "LOG10E" (log (exp 1) 10))
(set-ensured math.obj "SQRT1_2" (sqrt .5))
(set-ensured math.obj "SQRT1_2" (sqrt 2))
(set-ensured math.obj "PI" pi)

(set-ensured
 math.obj "abs"
 (math-function (arg :minf :Inf :inf :Inf)
   (abs arg)))

(set-ensured
 math.obj "acos"
 (math-function (arg)
   (let ((res (acos arg)))
     (if (realp res) res :NaN))))

(set-ensured
 math.obj "asin"
 (math-function (arg)
   (let ((res (asin arg)))
     (if (realp res) res :NaN))))

(set-ensured
 math.obj "atan"
 (math-function (arg :minf (- (/ pi 2)) :inf (/ pi 2))
   (atan arg)))

(set-ensured
 math.obj "atan2"
 (js-function (y x)
   (js-funcall (prop math.obj "atan") (!/ y x))))

(set-ensured
 math.obj "ceil"
 (math-function (arg :minf :-Inf :inf :Inf)
   (ceiling arg)))

(set-ensured
 math.obj "cos"
 (math-function (arg)
   (cos arg)))

(set-ensured
 math.obj "exp"
 (math-function (arg :minf 0 :inf :Inf)
   (exp arg)))

(set-ensured
 math.obj "floor"
 (math-function (arg :minf :-Inf :inf :Inf)
   (floor arg)))

(set-ensured
 math.obj "log"
 (math-function (arg :inf :Inf)
   (cond ((zerop arg) :-Inf)
	 ((minusp arg) :NaN)
	 (t (log arg)))))

(set-ensured
 math.obj "round"
 (math-function (arg :minf :-Inf :inf :Inf)
   (round arg)))

(set-ensured
 math.obj "sin"
 (math-function (arg)
   (sin arg)))

(set-ensured
 math.obj "sqrt"
 (math-function (arg)
   (let ((res (sqrt arg)))
     (if (realp res) res :NaN))))

(set-ensured
 math.obj "tan"
 (math-function (arg)
   (sin arg)))

(set-ensured
 math.obj "pow"
 (js-function (base exp)
   (with-asserted-types ((base number)
			 (exp number))
     (cond ((or (eq base :NaN) (eq exp :NaN)) :NaN)
	   ((eq exp :-Inf) 0)
	   ((and (realp exp) (zerop exp)) 1)
	   ((or (eq base :Inf) (eq exp :Inf)) :Inf)
	   ((eq base :-Inf) :-Inf)
	   (t (coerce (expt base exp) 'double-float))))))

(defmacro num-comparator (name (gt lt cmp))
  (let ((ls (gensym))
	(rs (gensym)))
    `(defun ,name (,ls ,rs)
       (let ((,ls (js-funcall number.ensure ,ls))
	     (,rs (js-funcall number.ensure ,rs)))
	 (cond ((or (eq ,ls :NaN) (eq ,rs :NaN)) :NaN)
	       ((or (eq ,ls ,gt) (eq ,rs ,gt)) ,gt)
	       ((eq ,ls ,lt) ,rs)
	       ((eq ,rs ,lt) ,ls)
	       (t (,cmp ,ls ,rs)))))))

(num-comparator num.max (:Inf :-Inf max))
(num-comparator num.min (:-Inf :Inf min))

;;NaN breaks <number.ext (and >number.ext) invariant so num-comparator
;;is implemented. should be fixed soon

(set-ensured
 math.obj "max"
 (js-function (&rest args)
   (reduce #'num.max args :initial-value :-Inf)))

(set-ensured
 math.obj "min"
 (js-function (&rest args)
   (reduce #'num.min args :initial-value :Inf)))

(set-ensured
 math.obj "random"
 (js-function ()
   (random 1.0)))
;;
(set-ensured
 *global* "print"
 (js-function (arg)
   (if (eq arg :undefined) (format t "~%")
       (format t "~A~%" (js-funcall string.ctor arg)))))

(set-ensured
 *global* "parseInt"
 (js-function (arg)
   (let ((arg (value arg)))
     (cond ((integerp  arg) arg)
	   ((or (eq arg :NaN) (eq arg :-Inf) (eq arg :Inf)) arg)
	   (t (with-asserted-types ((arg string))
		(or (parse-integer arg :junk-allowed t) :NaN)))))))

(set-ensured
 *global* "parseFloat"     ;todo: e.g.'123xx' returns :NaN ...
 (js-function (arg)        ;instead of 123. replace with regexp
   (if (js-number? arg) (value arg)
       (js-funcall number.ensure arg)))) 

(set-ensured
 *global* "isNaN"
 (js-function (arg)
   (or (eq arg :NaN) (undefined? arg))))

;;
(set-ensured
 *global* "not_implemented"
 (js-function ()
   (error "Function not implemented")))

(set-ensured
 *global* "eval"
 (js-function (str)
   (with-asserted-types ((str string))
     (compile-eval (translate (parse-js:parse-js-string str))))))

(defun lexical-eval (str scope)
  (with-asserted-types ((str string))
    (let ((*scope* (list scope)))
      (compile-eval (translate (parse-js:parse-js-string str))))))
