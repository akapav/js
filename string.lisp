(in-package :js)

;;todo: move the block somewhere else
(defparameter *class-cache* (make-hash-table :test 'equal))

(defun make-class (&rest supers)
  (let ((cls (gethash supers *class-cache*)))
    (or cls
	(let ((new-cls (make-instance 'standard-class :direct-superclasses supers)))
	  (setf (gethash supers *class-cache*) new-cls)))))

(defclass obj-with-sealed-props ()
  ((sealed-properties :accessor sealed-properties)))

(defmethod prop :around ((hash obj-with-sealed-props) key)
  (let ((sealed-prop (gethash key (sealed-properties hash))))
    (if sealed-prop (js-funcall sealed-prop hash)
        (call-next-method hash key))))

(defmethod (setf prop) :around (val (hash obj-with-sealed-props) key)
  (let ((sealed-prop (gethash key (sealed-properties hash))))
    (if sealed-prop val
        (call-next-method val hash key))))

(defun seal-properties (obj &rest props)
  (let ((cls (make-class (class-of obj) (find-class 'obj-with-sealed-props)))
	(hash (make-hash-table)))
    (change-class obj cls)
    (mapc (lambda (prop) (setf (gethash (first prop) hash) (second prop))) props)
    (setf (sealed-properties obj) hash)))
;;

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

(seal-properties string.prototype (list 'js-user::length
					(js-function (obj) (length (value obj)))))

;;at the moment bug with prop and (setf prop) exists -- instead of
;;using find-property they should be implemented directly

;;test ...
#{javascript}
s1 = String(123)
s2 = new String(456)
String.prototype.y=12
print(s2.y)
print(s2.charAt(1))
print(String.prototype.length)
print(s2.length)
.
