(in-package :js)

;;general
(defun js-funcall (func &rest args)
  (apply (proc func) nil args))

(defmacro js-function (args &body body)
  `(with-ignored-style-warnings
     (!function nil nil ,args nil
		((!return (or (progn ,@body) :undefined))))))

(eval-when (:compile-toplevel :load-toplevel)
  (defun ctor (sym)
    (intern (concatenate 'string
			 (symbol-name sym) "." (symbol-name 'ctor)))))

(defmacro define-js-method (type name args &body body)
  (flet ((arg-names (args)
	   (mapcar (lambda (arg) (if (consp arg) (first arg) arg)) args))
	 (arg-defaults (args)
	   (mapcar (lambda (arg) (if (consp arg) (second arg) :undefined)) args)))
    (let ((canonical-name
	   (intern (concatenate 'string
				(symbol-name type) "." name)))
	  (ctor-name (ctor type))
	  (prototype-name
	   (intern (concatenate 'string (symbol-name type) ".PROTOTYPE")))
	  (arg-names (arg-names args))
	  (arg-defaults (arg-defaults args)))
      `(progn
	 (defparameter ,canonical-name
	   (js-function ,arg-names
	     (let ((,(car arg-names)
		    (js-funcall ,ctor-name ,(car arg-names)))
		   ,@(mapcar
		      (lambda (name val)
			`(,name (if (eq ,name :undefined) ,val ,name)))
		      (cdr arg-names) (cdr arg-defaults)))
	       ,@body)))
	 (setf (prop ,prototype-name ,name)
	       (js-function ,(cdr arg-names)
		 (js-funcall
		  ,canonical-name (value js-user::this) ,@(cdr arg-names))))
	 (setf (prop ,ctor-name ,name) ,canonical-name)))))

(defmacro with-asserted-types ((&rest type-pairs) &body body)
  `(let (,@(mapcar (lambda (pair)
		     `(,(first pair) (js-funcall
				      ,(ctor (second pair)) ,(first pair))))
		   type-pairs))
     ;;todo: type declarations
     ,@body))

;;
(defparameter number.ctor ;;todo: set-default (same as string)
  (js-function (n)
    (cond ((numberp n) n)
	  ((stringp n)
	   (with-input-from-string (s n)
	     (js-funcall number.ctor (read s))))
	  (t :NaN))))

;;
(defparameter string.ctor
  (js-function (obj)
    (let ((str (if (stringp obj) obj (format nil "~A" obj))))
      (set-default js-user::this str)
      (the string str))))

(defparameter string.prototype
  (js-new js::string.ctor '("") 'native-hash))

(setf (prop string.ctor "prototype") string.prototype)
(setf (prop *global* "String") string.ctor)

;;
(defmethod prop ((str string) key &optional (default :undefined))
  (let* ((sealed (sealed string.prototype))
	 (action (gethash key sealed)))
    (if action
	(funcall action str)
	(prop string.prototype key default))))

;(defmethod value ((str string)) str)
(defmethod value ((v t)) v)

;;

(add-sealed-property string.prototype
		     "length"
		     (lambda (obj) (length (value obj))))


(add-sealed-property string.ctor "length" (constantly 1))
;;not sure what is the meaning of that property. recheck the spec

;;

(defun clip-index (n)
  (if (eq n :NaN) 0
      (let ((n (floor n)))
	(if (< n 0) 0 n))))

(define-js-method string "charAt" (str (ndx 0))
  (with-asserted-types ((ndx number))
    (string (aref str (clip-index ndx)))))

(define-js-method string "indexOf" (str substr (beg 0))
  (if (eq substr :undefined) -1
      (with-asserted-types ((substr string)
			    (beg number))
	(or (search substr str :start2 (clip-index beg)) -1))))

(define-js-method string "lastIndexOf" (str substr)
  (if (eq substr :undefined) -1
      (with-asserted-types ((substr string))
	(or (search substr str :from-end t) -1))))

(define-js-method string "substring" (str (from 0) to)
  (if (eq to :undefined)
      (with-asserted-types ((from number))
	(subseq str (clip-index from)))
      (with-asserted-types ((from number)
			    (to number))
	(let* ((from (clip-index from))
	       (to (max from (clip-index to))))
	  (subseq str from (min to (length str)))))))

;todo: maybe rewrite it to javascript (see Array.shift)
(define-js-method string "substr" (str (from 0) to)
  (if (eq to :undefined)
      (js-funcall |STRING.substring| str from)
      (with-asserted-types ((from number)
			    (to number))
	(let ((from (clip-index from)))
	  (js-funcall |STRING.substring| str from (+ from (clip-index to)))))))

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
    (js-new array.ctor (string-split str delimiter) 'array-object)))
  

;;; todo: array differs from string (according to spidermonkey) in
;;; respect of calling constructor without operator new. string
;;; returns a new basic string instead of an object, but arrays behave
;;; like there is no difference whether the ctor is invoked with or
;;; without new. our current implementation implements array to behave
;;; like string. recheck the spec
;;;

(defclass array-object (native-hash)
  ())

(defmethod prop ((arr array-object) key &optional (default :undefined))
  (if (integerp key) ;;todo: safe conversion to integer and boundary check
      (aref (value arr) key)
      (call-next-method arr key default)))

(defmethod (setf prop) (val (arr array-object) key)
  (if (integerp key) ;;todo: ... as above ...
      (setf (aref (value arr) key) val)
      (call-next-method val arr key)))

(defparameter array.ctor
  (js-function ()
    (let* ((len (js::arg-length (!arguments)))
	   (arr (make-array len
			    :fill-pointer len
			    :initial-contents
			    (loop for i from 0 below len
			       collect (sub (!arguments) i)))))
      (set-default js-user::this arr)
      arr)))

(defparameter array.prototype (js-new js::array.ctor () 'array-object))

(setf (prop array.ctor "prototype") array.prototype)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (shadow 'Array 'js-user)) ;;todo: ...
(setf (prop *global* "Array") array.ctor)

(add-sealed-property array.prototype
		     "length"
		     (lambda (obj) (length (value obj))))

(add-sealed-property array.ctor "length" (constantly 1))
;;not sure what is the meaning of that property. recheck the spec


;; concat can't use standard define-js-method macro because it takes
;; variable number of arguments
(defparameter |ARRAY.concat|
  (js-function ()
    (let* ((len (arg-length (!arguments)))
	   (arr (apply #'concatenate 'list
		       (loop for i from 0 below len
			  collect
			    (let* ((arg (sub (!arguments) i))
				   (val (value arg)))
			      (if (typep val 'vector)
				  val
				  (js-funcall array.ctor arg)))))))
      (js-new array.ctor arr 'array-object))))

(setf (prop array.prototype "concat")
	(js-function ()
	  (js-funcall |ARRAY.concat| (value net.svrg.js-user::this))))

(setf (prop array.ctor "concat") |ARRAY.concat|)

#+nil (define-js-method array "join" (str)
  (with-asserted-types ((str string))))

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

(defun apply-splicing (arr ndx howmany arguments)
  (let ((arguments (loop for i from 3 below (arg-length arguments)
		      collecting (sub arguments i))))
    (js-new array.ctor
	    (apply #'vector-splice arr ndx howmany arguments) 'array-object)))

(defparameter |ARRAY.splice|
  (js-function (arr ndx howmany)
    (with-asserted-array (arr)
      (cond ((and (eq ndx :undefined) (eq howmany :undefined))
	     (apply-splicing arr 0 0 (!arguments)))
	    ((eq ndx :undefined)
	     (with-asserted-types ((howmany number))
	       (apply-splicing arr 0 howmany (!arguments))))
	    ((eq howmany :undefined)
	     (with-asserted-types ((ndx number))
	       (apply-splicing arr ndx (length arr) (!arguments))))
	    (t (with-asserted-types ((ndx number)
				     (howmany number))
		 (apply-splicing arr ndx howmany (!arguments))))))))

(setf (prop array.prototype "splice")
      (js-function ()
	(let ((arguments (loop for i from 0 below (arg-length (!arguments))
			    collecting (sub (!arguments) i))))
	  (apply #'js-funcall |ARRAY.splice| net.svrg.js-user::this arguments))))

(setf (prop array.ctor "splice") |ARRAY.splice|)

#| todo: fix eval-whens
#{javascript}
Array.shift = function(arr) {if(arr.length > 0) return  arr.splice(0,1)[0];}
Array.prototype.shift = function() {return Array.shift(this);}
.
|#

(defparameter |ARRAY.pop|
  (js-function (arr)
    (with-asserted-array (arr)
      (unless (zerop (length arr))
	(vector-pop arr)))))

(setf (prop array.prototype "pop")
      (js-function ()
	(js-funcall |ARRAY.pop| net.svrg.js-user::this)))

(setf (prop array.ctor "pop") |ARRAY.pop|)

;
(defun test-splice (ndx elems &rest insert)
  (let ((v (make-array 3 :fill-pointer 3 :initial-contents '(1 2 3))))
    (values (apply #'vector-splice v ndx elems insert) v)))


(test-splice 0 0)
(test-splice 0 0 100 200)
(test-splice 1 1000)
(test-splice 0 1)
(test-splice 0 2 10 11 12)
(test-splice 0 3 10 11 12)
(test-splice 2 2)
(test-splice 2 2 10 12)
(test-splice 1 1 10 12)
(test-splice 100 100 10 12)
(test-splice 1 1 7 8 9)

;;
(setf (prop *global* "Object")
      (js-function (arg) arg))

(setf (prop *global* "print")
      (js-function (arg)
	(format t "~A~%" (js-funcall string.ctor arg))))

#|
;;test ...
#{javascript}
s1 = String(123)
s2 = new String(456)
String.prototype.y=12
print(s2.y)
print(s2.charAt(1))
print(String.prototype.length)
print(s2.length)
print(s2.length = 555)
print(s2.length)
print(s2.substr("1",8))
print(s2.substring (0, 100))
.
|#
