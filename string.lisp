(in-package :js)

;;general
(defun js-funcall (func &rest args)
  (apply (proc func) nil args))

(defmacro js-function (args &body body)
  `(with-ignored-style-warnings
       (!function nil nil ,args nil
		  ((!return (or (progn ,@body) :undefined))))))

(defun ctor (sym)
  (intern (concatenate 'string
		       (symbol-name sym) "." (symbol-name 'ctor))))

(defmacro define-js-method (type name args &body body)
  (flet ((arg-names (args) (mapcar (lambda (arg) (if (consp arg) (first arg) arg)) args))
	 (arg-defaults (args) (mapcar (lambda (arg) (if (consp arg) (second arg) :undefined)) args)))
    (let ((canonical-name (intern (concatenate 'string (symbol-name type) "." (symbol-name name))))
	  (ctor-name (ctor type))
	  (prototype-name (intern (concatenate 'string (symbol-name type) ".PROTOTYPE")))
	  (arg-names (arg-names args))
	  (arg-defaults (arg-defaults args)))
      `(progn
	 (defparameter ,canonical-name
	   (js-function ,arg-names
			(let ((,(car arg-names) (js-funcall ,ctor-name ,(car arg-names)))
			      ,@(mapcar (lambda (name val)
					  `(,name (if (eq ,name :undefined) ,val ,name)))
					(cdr arg-names) (cdr arg-defaults)))
			  ,@body)))
	 (defparameter ,name
	   (js-function ,(cdr arg-names)
			(js-funcall ,canonical-name (value js-user::this) ,@(cdr arg-names))))
	 (setf (prop ,prototype-name ',(intern (symbol-name name) :js-user)) ,name)
	 (setf (prop ,ctor-name ',(intern (symbol-name name) :js-user)) ,canonical-name)))))

(defmacro with-asserted-types ((&rest type-pairs) &body body)
  `(let (,@(mapcar (lambda (pair)
		     `(,(first pair) (js-funcall ,(ctor (second pair)) ,(first pair))))
		   type-pairs))
     ;;todo: type declarations
     ,@body))
;;

(defparameter string.ctor
  (js-function (obj)
	       (let ((str (if (stringp obj) obj (format nil "~A" obj))))
		 (set-default js-user::this str)
		 str)))

(defparameter string.prototype
  (make-instance 'native-hash :value ""))

(setf (prop string.ctor 'js-user::prototype) string.prototype)
(setf (prop *global* 'js-user::String) string.ctor)

;;
(defmethod prop ((str string) key &optional (default :undefined))
  (let* ((sealed (sealed string.prototype))
	 (action (gethash key sealed)))
    (if action
	(funcall action str)
	(prop string.prototype key))))

(defmethod value ((str string)) str)

;;

(add-sealed-property string.prototype
		     'js-user::length
		     (lambda (obj) (length (value obj))))


(add-sealed-property string.ctor 'js-user::length (constantly 1))
;;not sure what is the meaning of that property. recheck the spec

;;

(defun clip-index (n)
  (if (eq n :NaN) 0
      (let ((n (floor n)))
	(if (< n 0) 0 n))))

(define-js-method string charAt (str (ndx 0))
  (with-asserted-types ((ndx number))
    (string (aref str (clip-index ndx)))))

(define-js-method string indexOf (str substr (beg 0))
  (if (eq substr :undefined) -1
      (with-asserted-types ((substr string)
			    (beg number))
	(or (search substr str :start2 (clip-index beg)) -1))))

(define-js-method string lastIndexOf (str substr)
  (if (eq substr :undefined) -1
      (with-asserted-types ((substr string))
	(or (search substr str :from-end t) -1))))

(define-js-method string substring (str (from 0) to)
  (if (eq to :undefined)
      (with-asserted-types ((from number))
	(subseq str (clip-index from)))
      (with-asserted-types ((from number)
			    (to number))
	(let* ((from (clip-index from))
	       (to (max from (clip-index to))))
	  (subseq str from (min to (length str)))))))

(define-js-method string substr (str (from 0) to)
  (if (eq to :undefined)
      (js-funcall string.substring str from)
      (with-asserted-types ((from number)
			    (to number))
	(let ((from (clip-index from)))
	  (js-funcall string.substring str from (+ from (clip-index to)))))))

(define-js-method string toUpperCase (str)
  (string-upcase str))

(define-js-method string toLowerCase (str)
  (string-downcase str))

;;

(defparameter number.ctor ;;todo: set-default (same as string)
  (js-function (n)
    (cond ((numberp n) n)
	  ((stringp n)
	   (with-input-from-string (s n)
	     (js-funcall number.ctor (read s))))
	  (t :NaN))))

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
