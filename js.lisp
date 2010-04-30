(in-package :js)

(defmacro eval-function (lex-chain)
  `(!function nil nil (str) nil
	      ((let ((form (translate (parse-js-string str) ',lex-chain)))
		 (return-from function
		  (eval-in-lexenv ,lex-chain -object-env-stack- form))))))

(defun make-args (var-names)
  (let* ((vars (mapcar (lambda (var) (->usersym var)) var-names))
	 (get-arr (gensym))
	 (set-arr (gensym))
	 (inst (gensym))
	 (argument-cnt (gensym))
	 (var (gensym))
	 (len (length vars)))
    `(let* ((,argument-cnt(loop for ,var in (list ,@vars)
			     until (eq ,var :undefined-unset)
			     count t))
	   (,get-arr
	    (make-array ,(1+ len)
			:element-type 'function
			:initial-contents
			(list
			 ,@(mapcar (lambda (var)
				     `(lambda (n) (declare (ignore n)) ,var))
				   vars)
			 (lambda (n) (nth (- n ,len) extra-args)))))
	   (,set-arr
	    (make-array ,(1+ len)
			:element-type 'function
			:initial-contents
			(list
			 ,@(mapcar (lambda (var)
				     `(lambda (n val)
					(declare (ignore n))
					(setf ,var val)))
				   vars)
			 (lambda (n val) (setf (nth (- n ,len) extra-args) val)))))
	    (,inst (make-instance 'arguments
				   :vlen ,len
				   :length (+ ,argument-cnt (length extra-args))
				   :get-arr ,get-arr :set-arr ,set-arr)))
       (add-sealed-property ,inst
			    "length"
			    (lambda (-)
			      (declare (ignore -))
			      (+ ,argument-cnt (length extra-args))))
       ,inst)))

;;
(defun js-funcall (func &rest args)
  (apply (the function (proc func)) nil args))

(defmacro js-function (args &body body)
  `(make-instance
    'native-function :prototype function.prototype
    :proc ,(with-arguments args (wrap-function args body))))

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
  `(or (eq ,exp :undefined)
       (eq ,exp :undefined-unset)))

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
