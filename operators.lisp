(in-package :js)

(defmacro trivial-op (type op)
  (let ((name (intern (concatenate
		       'string (symbol-name op) (symbol-name type))))
	(ls (gensym))
	(rs (gensym)))
    `(defun ,name (,ls ,rs)
       (declare (,type ,ls ,rs))
       (,op ,ls ,rs))))

(defmacro extended-number-op ((op &key var (nan :NaN))
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
	 ((and (numberp (value ,ls)) (numberp (value ,rs)))
	  (,name (value ,ls) (value ,rs)))
	 ((or (eq ,ls :NaN) (eq ,rs :NaN)) ,nan)
	 ((and (eq ,ls :Inf) (eq ,rs :Inf)) ,inf-inf)
	 ((and (eq ,ls :Inf) (eq ,rs :-Inf)) ,inf-minf)
	 ((and (eq ,ls :-Inf) (eq ,rs :Inf)) ,minf-inf)
	 ((and (eq ,ls :-Inf) (eq ,rs :-Inf)) ,minf-minf)
	 ((eq ,rs :Inf) ,num-inf)
	 ((eq ,rs :-Inf) ,num-minf)
	 ((eq ,ls :Inf) ,inf-num)
	 ((eq ,ls :-Inf) ,minf-num)
	 (t (error (format nil "internal error in ~A~%" ',op)))))))

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
      (cond ((zerop ls) :NaN)
	    ((minusp ls) :-Inf)
	    (t :Inf))
      (coerce (/ ls rs) 'double-float)))

(extended-number-op (/)
		    :NaN :NaN :NaN :NaN
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

;;
(defun !=== (ls rs)
  (unless (or (eq ls :NaN) (eq rs :NaN))
    (equal ls rs)))

(defun !!== (ls rs)
  (not (!=== ls rs)))

;;
(defun !== (ls rs)
  (or (!=== ls rs)
      (cond
	((eq (type-of (value ls)) (type-of (value rs)))
	 (!=== (value ls) (value rs)))
	((and (js-number? ls) (js-string? rs))
	 (!=== ls (js-funcall number.ctor rs)))
	((and (js-string? ls) (js-number? rs))
	 (!=== (js-funcall number.ctor ls) rs))
	((or
	  (and (undefined? ls) (eq rs :null))
	  (and (eq ls :null) (undefined? rs))) t)
	((eq ls :true) (!== 1 rs))
	((eq ls :false) (!== 0 rs))
	((eq rs :true) (!== ls 1))
	((eq rs :false) (!== ls 0))
	(t (!=== (to-string (value ls))
		 (to-string (value rs)))))))

(defun !!= (ls rs)
  (not (!== ls rs)))

;;
(trivial-op fixnum <)
(trivial-op number <)

(extended-number-op (< :nan nil)
		    nil nil t nil
		    t nil nil t)

(defun !< (ls rs)
  (cond
    ((and (typep ls 'fixnum) (typep rs 'fixnum)) (<fixnum ls rs))
    ((and (numberp ls) (numberp rs)) (<number ls rs))
    ((and (js-number? ls) (js-number? rs)) (<number.ext ls rs))
    ((and (js-number? ls) (js-string? rs))
     (<number.ext ls (js-funcall number.ctor rs)))
    ((and (js-string? ls) (js-number? rs))
     (<number.ext (js-funcall number.ctor ls) rs))
    (t (when (string< (to-string (value ls))
		      (to-string (value rs))) t))))

;;
(trivial-op fixnum >)
(trivial-op number >)

(extended-number-op (> :nan nil)
		    nil nil t nil
		    t nil nil t)

(defun !> (ls rs)
  (cond
    ((and (typep ls 'fixnum) (typep rs 'fixnum)) (>fixnum ls rs))
    ((and (numberp ls) (numberp rs)) (>number ls rs))
    ((and (js-number? ls) (js-number? rs)) (>number.ext ls rs))
    ((and (js-number? ls) (js-string? rs))
     (>number.ext ls (js-funcall number.ctor rs)))
    ((and (js-string? ls) (js-number? rs))
     (>number.ext (js-funcall number.ctor ls) rs))
    (t (when (string> (to-string (value ls))
		      (to-string (value rs))) t))))

;;
(defun !<= (ls rs)
  (unless (or (eq ls :NaN) (eq rs :NaN))
    (not (!> ls rs))))

(defun !>= (ls rs)
  (unless (or (eq ls :NaN) (eq rs :NaN))
    (not (!< ls rs))))

;;
(defun !++ (arg)
  (!+ (if (js-number? arg) arg
	  (js-funcall number.ctor arg))
      1))

(defun !-- (arg)
  (!- (if (js-number? arg) arg
	  (js-funcall number.ctor arg))
      1))

;;
(defmacro !&& (ls rs)
  (let ((resls (gensym)))
    `(let ((,resls ,ls))
       (if (js->boolean ,resls) ,rs ,resls))))

(defmacro |!\|\|| (ls rs)
  (let ((resls (gensym)))
    `(let ((,resls ,ls))
       (if (js->boolean ,resls) ,resls ,rs))))

(defmacro !! (exp)
  `(not (js->boolean ,exp)))

;;
(defun !instanceof (ls rs)
  (and (typep ls 'native-hash)
       (eq (prop ls "constructor") rs)))

(defgeneric js.typeof (exp)
  (:method (exp) "object"))

(defmethod js.typeof ((exp native-function))
  "function")

(defmethod js.typeof ((exp string))
  "string")

(defmethod js.typeof ((exp number))
  "number")

(defmethod js.typeof ((exp (eql :undefined)))
  "undefined")

(defmethod js.typeof ((exp (eql :NaN)))
  "number")

(defmethod js.typeof ((exp (eql :Inf)))
  "number")

(defmethod js.typeof ((exp (eql :-Inf)))
  "number")

(defun !typeof (exp)
  (js.typeof exp))
