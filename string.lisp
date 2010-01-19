(in-package :js)

(defun js-funcall (func &rest args)
  (apply (proc func) nil args))

(defmacro js-function (args &body body)
  `(with-ignored-style-warnings
     (!function nil nil ,args nil
		((!return (or (progn ,@body) :undefined))))))

(defparameter string.ctor
  (js-function (obj)
	       (let ((str (if (stringp obj) obj (format nil "~A" obj))))
		 (set-default js-user::this str)
		 str)))

;;
(defun .char-at (str ndx)
  (declare (string str) (fixnum ndx))
  (string (aref str ndx)))

(defparameter string.char-at
  (js-function (obj ndx)
	       (.char-at (js-funcall string.ctor obj) ndx)))

(defparameter char-at
  (js-function (ndx)
	       (handler-case (.char-at (value js-user::this) ndx) ;todo: not safe 
		 (t (err)
		   (declare (ignore err))
		   (string.char-at (value js-user::this) ndx)))))

(defparameter string.prototype
  (make-instance 'native-hash :value ""))

(setf (prop string.prototype 'js-user::charAt) char-at)

(setf (prop string.ctor 'js-user::prototype) string.prototype)

(setf (prop *global* 'js-user::String) string.ctor)

(add-sealed-property string.prototype
		     'js-user::length
		     (lambda (obj) (length (value obj))))


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
.
