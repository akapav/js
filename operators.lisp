(in-package :js)

(defmacro trivial-op (type op)
  (let ((name (intern (concatenate
		       'string (symbol-name op) (symbol-name type))))
	(ls (gensym))
	(rs (gensym)))
    `(defun ,name (,ls ,rs)
       (declare (,type ,ls ,rs))
       (,op ,ls ,rs))))

(defmacro extended-number-op ((op &key var)
			      inf-inf inf-minf minf-inf minf-minf
			      num-inf num-minf inf-num minf-num)
  (let* ((namestr (concatenate 'string (symbol-name op) "NUMBER"))
	 (name (intern namestr))
	 (nameext (intern (concatenate 'string namestr ".EXT")))
	 (ls (or var (gensym)))
	 (rs (gensym)))
    `(defun ,nameext (,ls ,rs)
       (declare (js.number ,ls ,rs))
       (cond
	 ((or (eq ,ls :NaN) (eq ,rs :NaN)) :NaN)
	 ((and (eq ,ls :Inf) (eq ,rs :Inf)) ,inf-inf)
	 ((and (eq ,ls :Inf) (eq ,rs :-Inf)) ,inf-minf)
	 ((and (eq ,ls :-Inf) (eq ,rs :Inf)) ,minf-inf)
	 ((and (eq ,ls :-Inf) (eq ,rs :-Inf)) ,minf-minf)
	 ((eq ,rs :Inf) ,num-inf)
	 ((eq ,rs :-Inf) ,num-minf)
	 ((eq ,ls :Inf) ,inf-num)
	 ((eq ,ls :-Inf) ,minf-num)
	 (t (,name ,ls ,rs))))))

;;

(defun js-number? (o)
  (or (numberp o)
      (eq o :NaN)
      (eq o :Inf)
      (eq o :-Inf)))

(deftype js.number ()
  `(satisfies js-number?))

;;
(trivial-op fixnum +)
(trivial-op number +)

(extended-number-op (+)
		    :Inf :NaN :NaN :-Inf
		    :Inf :-Inf :Inf :-Inf)

(defun +string (ls rs)
  (let ((ls (js-funcall string.ctor ls))
	(rs (js-funcall string.ctor rs)))
    (concatenate 'string ls rs)))

(defun !+ (ls rs)
  (funcall
   (cond
     ((and (typep ls 'fixnum) (typep rs 'fixnum)) #'+fixnum)
     ((and (numberp ls) (numberp rs)) #'+number)
     ;;;todo: fast string concat when both are strings
     ((and (js-number? ls) (js-number? rs)) #'+number.ext)
     ((and (js-number? ls) (undefined? rs)) (constantly :NaN))
     ((and (undefined? ls) (js-number? rs)) (constantly :NaN))
     ((and (undefined? ls) (undefined? rs)) (constantly :NaN))
     (t #'+string)) ls rs))

;;
(trivial-op fixnum -)
(trivial-op number -)

(extended-number-op (-)
		    :NaN :Inf :-Inf :-NaN
		    :-Inf :Inf :Inf :-Inf)

(defun !- (ls rs)
  (funcall 
   (cond
     ((and (typep ls 'fixnum) (typep rs 'fixnum)) #'-fixnum)
     ((and (numberp ls) (numberp rs)) #'-number)
     ((and (js-number? ls) (js-number? rs)) #'-number.ext)
     (t (constantly :NaN))) ls rs))

;;
(trivial-op fixnum *)
(trivial-op number *)

(extended-number-op (*)
		    :Inf :-Inf :-Inf :Inf
		    :Inf :-Inf :Inf :-Inf)

(defun !* (ls rs)
  (funcall
   (cond
     ((and (typep ls 'fixnum) (typep rs 'fixnum)) #'*fixnum)
     ((and (numberp ls) (numberp rs)) #'*number)
     ((and (js-number? ls) (js-number? rs)) #'*number.ext)
     (t (constantly :NaN))) ls rs))     

;;
(defun /number (ls rs)
  (declare (number ls rs))
  (if (zerop rs)
      (if (minusp ls) :-Inf :Inf)
      (coerce (/ ls rs) 'double-float)))

(extended-number-op (/)
		    :Inf :-Inf :-Inf :Inf
		    0 0 :Inf :-Inf)

(defun !/ (ls rs)
  (funcall
   (cond
     ((and (numberp ls) (numberp rs)) #'/number)
     ((and (js-number? ls) (js-number? rs)) #'/number.ext)
     (t (constantly :NaN))) ls rs))

;;
(defun %number (ls rs) (mod ls rs))

(extended-number-op (% :var num)
		    :NaN :NaN :NaN :NaN
		    num num :NaN :NaN)

(defun !% (ls rs)
  (funcall
   (cond
     ((and (numberp ls) (numberp rs)) #'mod)
     ((and (js-number? ls) (js-number? rs)) #'%number.ext)
     (t (constantly :NaN))) ls rs))
