(in-package :js)

(defun make-args (var-names all-arguments)
  (let* ((vars (mapcar (lambda (var) (->usersym var)) var-names))
         (extra-args (gensym)) (arg-count (gensym))
	 (get-arr (gensym)) (set-arr (gensym))
	 (inst (gensym))
	 (len (length vars)))
    `(let* ((,arg-count (length ,all-arguments))
            (,extra-args (concatenate 'vector (nthcdr ,len ,all-arguments)))
            (,get-arr
             (make-array ,(1+ len)
                         :element-type 'function
                         :initial-contents
                         (list
                          ,@(loop :for var :in vars :collect `(lambda () ,var))
                          (lambda (n) (aref ,extra-args (- n ,len))))))
            (,set-arr
             (make-array ,(1+ len)
                         :element-type 'function
                         :initial-contents
                         (list
                          ,@(loop :for var :in vars :collect
                               `(lambda (val) (setf ,var val)))
                          (lambda (n val) (setf (aref ,extra-args (- n ,len)) val)))))
	    (,inst (make-instance 'arguments
                                  :vlen ,len
                                  :length ,arg-count
                                  :get-arr ,get-arr :set-arr ,set-arr)))
       (add-sealed-property ,inst
			    "length"
			    (lambda (-) (declare (ignore -)) ,arg-count))
       ,inst)))

;;
(defun js-funcall (func &rest args)
  (apply (the function (proc func)) nil args))

(defmacro js-function (args &body body)
  `(make-instance
    'native-function :prototype function.prototype
    :proc ,(wrap-function args body)))

(defmacro js-function/arguments (args &body body)
  `(make-instance
    'native-function :prototype function.prototype
    :proc ,(wrap-function/arguments args body)))

(defun js-new (func args)
  (let* ((proto (prop func "prototype"))
	 (ret (make-instance (placeholder-class func)
			     :prototype proto
			     :sealed (sealed proto))))
    (apply (the function (proc func)) ret args)
    (setf (prop ret "constructor") func)
    ;;todo: put set-default here
    ret))

(defmacro undefined? (exp)
  `(eq ,exp :undefined))

(defun js->boolean (exp)
  (when exp
    (typecase exp
      (fixnum (not (zerop exp)))
      (number (not (zerop exp)))
      (string (not (zerop (length exp))))
      (symbol
       (not
	(or
	 (undefined? exp)
	 (eq exp :null)
	 (eq exp :false)
	 (eq exp :NaN))))
      (t t))))

;;
(defmacro !eval (str)
  (translate (parse-js-string str)))

(defun js-load-file (fname)
  (with-open-file (str fname)
    (eval (translate (parse-js str)))))

(defun js-reader (stream)
  `(!eval ,(read-line-stream stream)))

(define-reader 'javascript #'js-reader)
