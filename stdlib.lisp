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
  (js::!new js::string.ctor ("")))

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
	  (subseq str from (min to (length str)))))) )

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


;;; todo: array differs from string (according to spidermonkey) in
;;; respect of calling constructor without operator new. string
;;; returns a new basic string instead of an object, but arrays behave
;;; like there is no difference whether the ctor is invoked with or
;;; without new. our current implementation implements array to behave
;;; like string. recheck the spec
;;;
(defparameter array.ctor
  (js-function ()
    (let* ((len (js::arg-length (!arguments)))
	   (arr (make-array len
			    :initial-contents
			    (loop for i from 0 below len
			       collect (sub (!arguments) i)))))
      (set-default js-user::this arr)
      arr)))

(defparameter array.prototype (js::!new js::array.ctor ()))

(setf (prop array.ctor "prototype") array.prototype)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (shadow 'Array 'js-user)) ;;todo: ...
(setf (prop *global* "Array") array.ctor)

(add-sealed-property array.prototype
		     "length"
		     (lambda (obj) (length (value obj))))

(add-sealed-property array.ctor "length" (constantly 1))
;;not sure what is the meaning of that property. recheck the spec


(defparameter |ARRAY.concat|
  (js-function ()
    (let* ((len (arg-length (!arguments)))
	   (arr (apply #'concatenate 'list
		       (loop for i from 0 below len
			  collect
			    (let* ((arg (sub (!arguments) i))
				   (val (value arg)))
			      (if (typep val 'simple-array)
				  val
				  (js-funcall array.ctor arg)))))))
      (apply #'js-new array.ctor arr))))

(setf (prop array.prototype "concat")
	(js-function ()
	  (js-funcall |ARRAY.concat| (value net.svrg.js-user::this))))

(setf (prop array.ctor "concat") |ARRAY.concat|)
  
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
