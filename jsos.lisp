(in-package :js)

(eval-when (:compile-toplevel :load-toplevel :execute)

#+sbcl
(defmacro wrap-method-lambda ((&rest arglist) &body body)
  (let ((args (gensym))
	(nm (gensym)))
    `(lambda (,args ,nm)
       (declare (ignore ,nm))
       (let (,@(loop :for var :in arglist
		  :for i = 0 :then (1+ i)
		  :collect (list var `(nth ,i ,args))))
	 ,@body))))

#-sbcl
(defmacro wrap-method-lambda ((&rest arglist) &body body)
  `(lambda ,arglist ,@body))

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
	    :function
	    (wrap-method-lambda (obj)
	      (%ensure-getter obj key)
	      (funcall gf obj))))
	  (add-method
	   gf
	   (make-instance
	    'standard-method
	    :specializers (list (find-class t))
	    :lambda-list '(obj)
	    :function
	    (wrap-method-lambda (obj)
	      ;todo: this with *global* is just a temporary hack. we
	      ;need to find a robust solution for accessing atributes
	      ;for native types (e.g. string or number). probably we
	      ;will need to implement something like generic
	      ;'property-proxy'
	      ;--aka 10/06/10
	      (declare (special *global*))
	      (prop (or obj *global*) key))))))))

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
	    :function
	    (wrap-method-lambda (val obj)
	      (setf (prop obj key) val))))
	  (add-method
	   gf
	   (make-instance
	    'standard-method
	    :specializers (list (find-class t) (find-class t))
	    :lambda-list '(val obj)
	    :function
	    (wrap-method-lambda (val obj)
	      (declare (ignore obj))
	      val)))))))

(defun ensure-accessors (key)
  (ensure-getter key)
  (ensure-setter key))

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
   (sealed :accessor sealed :initform nil)
   (attributes :accessor attributes :initform nil))
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
    (push property-name (attributes obj))
    (change-class obj
		  (or to-class
		      (%inherit-with-property cls property-name)))))

(defgeneric js-add-sealed-property (obj property-name proc))

(defmethod js-add-sealed-property ((obj t) property-name proc)
  (let* ((get-generic (ensure-generic-function
		       (generic-getter-name property-name)))
	 (set-generic (ensure-generic-function
		       (generic-setter-name property-name)))
	 (prop-getter
	  (make-instance
	   'standard-method
	   :specializers (list (class-of obj))
	   :lambda-list '(obj)
	   :function
	   (wrap-method-lambda (obj)
	     (funcall proc obj))))
	 (prop-setter
	  (make-instance
	   'standard-method
	   :specializers (list (find-class t) (class-of obj))
	   :lambda-list '(val obj)
	   :function
	   (wrap-method-lambda (val obj)
	     (declare (ignore obj))
	     val))))
    (add-method set-generic prop-setter)
    (add-method get-generic prop-getter)))

(defmethod js-add-sealed-property ((obj js-object) property-name proc)
  (declare (ignore proc))
  (call-next-method)
  (pushnew property-name (sealed obj) :test #'string=))

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
	 (key-sym (->usersym key)))
    (unless (or (slot-exists-p obj key-sym) (sealed-property? obj key))
      (js-add-property obj key))))

(defgeneric prop (obj key)
  (:method (obj key)
    (get-using-getter (ensure-getter key) obj)))

(defgeneric (setf prop) (val obj key)
  (:method (val obj key)
    (let* ((key (stringify key))
	   (key-sym (->usersym key))
	   (setter (ensure-setter key)))
      (unless (or (slot-exists-p obj key-sym) (sealed-property? obj key))
	(js-add-property obj key))
      (funcall setter val obj))))

(defun js-clone (&optional prototype)
  (if prototype
      (let* ((proto-class (class-of prototype))
	     (obj (make-instance (class-name proto-class) :prototype prototype)))
	obj)
      (make-instance 'js-object))))

(defmacro prop** (form default)
  (let ((val (gensym))
	(found (gensym)))
    `(multiple-value-bind (,val ,found) ,form
       (if ,found ,val ,default))))

(defun %fast-get (obj attr)
  `(get-using-getter
    (function ,(generic-function-name (ensure-getter attr)))
    ,obj))

(defun %fast-set (obj attr val)
  `(funcall
    (function ,(generic-function-name (ensure-setter attr)))
    ,val ,obj))

(defmacro get-attribute (obj attr)
  (if (stringp attr)
      `(,@(%fast-get obj attr))
      `(prop ,obj ,attr)))

(defmacro set-attribute (obj attr val)
  (if (stringp attr)
      `(,@(%fast-set obj attr val))
      `(setf (prop ,obj ,attr) ,val)))

#+nil (defmacro set-ensured (obj attr val)
  `(funcall ,(ensure-setter attr) ,val ,obj))

(defmacro set-ensured (obj key val)
  `(progn
     (ensure-accessors ,key)
     (setf (prop ,obj ,key) ,val)))
