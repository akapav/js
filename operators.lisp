(in-package :js)

(defmacro trivial-op (type op)
  (let ((name (intern (concatenate
		       'string (symbol-name op) (symbol-name type))))
	(ls (gensym))
	(rs (gensym)))
    `(defun ,name (,ls ,rs)
       (declare (,type ,ls ,rs))
       (,op ,ls ,rs))))

(defmacro extended-number-op ((op &key var (nan (nan)))
			      inf-inf inf-minf minf-inf minf-minf
			      num-inf num-minf inf-num minf-num)
  (let* ((namestr (concatenate 'string (symbol-name op) (symbol-name 'number)))
	 (name (intern namestr))
	 (nameext (intern (concatenate 'string namestr "." (symbol-name 'ext))))
	 (ls (or var (gensym)))
	 (rs (gensym)))
    (if *float-traps*
        `(defun ,nameext (,ls ,rs)
           (declare (js.number ,ls ,rs))
           (cond
             ((and (numberp (value ,ls)) (numberp (value ,rs)))
              (,name (value ,ls) (value ,rs)))
             ((or (is-nan ,ls) (is-nan ,rs)) ,nan)
             ((and (eq ,ls ,(positive-infinity)) (eq ,rs ,(positive-infinity))) ,inf-inf)
             ((and (eq ,ls ,(positive-infinity)) (eq ,rs ,(negative-infinity))) ,inf-minf)
             ((and (eq ,ls ,(negative-infinity)) (eq ,rs ,(positive-infinity))) ,minf-inf)
             ((and (eq ,ls ,(negative-infinity)) (eq ,rs ,(negative-infinity))) ,minf-minf)
             ((eq ,rs ,(positive-infinity)) ,num-inf)
             ((eq ,rs ,(negative-infinity)) ,num-minf)
             ((eq ,ls ,(positive-infinity)) ,inf-num)
             ((eq ,ls ,(negative-infinity)) ,minf-num)
             (t (error (format nil "internal error in ~A~%" ',op)))))
        `(defun ,nameext (,ls ,rs)
           (declare (number ,ls ,rs))
           (,name (value ,ls) (value ,rs))))))

;;

(trivial-op fixnum +)
(trivial-op number +)

(extended-number-op (+)
		    (positive-infinity) (nan) (nan) (negative-infinity)
		    (positive-infinity) (negative-infinity) (positive-infinity)
                    (negative-infinity))

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
     ((and (js-number? ls) (undefined? rs)) (constantly (nan)))
     ((and (undefined? ls) (js-number? rs)) (constantly (nan)))
     ((and (undefined? ls) (undefined? rs)) (constantly (nan)))
     (t #'+string)) ls rs))

;;
(trivial-op fixnum -)
(trivial-op number -)

(extended-number-op (-)
		    (nan) (positive-infinity) (negative-infinity) (nan)
		    (negative-infinity) (positive-infinity) (positive-infinity)
                    (negative-infinity))

(defun !- (ls rs)
  (funcall 
   (cond
     ((and (typep ls 'fixnum) (typep rs 'fixnum)) #'-fixnum)
     ((and (numberp ls) (numberp rs)) #'-number)
     ((and (js-number? ls) (js-number? rs)) #'-number.ext)
     (t (constantly (nan)))) ls rs))

;;
(trivial-op fixnum *)
(trivial-op number *)

(extended-number-op (*)
		    (positive-infinity) (negative-infinity) (negative-infinity)
                    (positive-infinity) (positive-infinity) (negative-infinity)
                    (positive-infinity) (negative-infinity))

(defun !* (ls rs)
  (funcall
   (cond
     ((and (typep ls 'fixnum) (typep rs 'fixnum)) #'*fixnum)
     ((and (numberp ls) (numberp rs)) #'*number)
     ((and (js-number? ls) (js-number? rs)) #'*number.ext)
     (t (constantly (nan)))) ls rs))     

;;
(defun /number (ls rs)
  (declare (number ls rs))
  (if (zerop rs)
      (cond ((zerop ls) (nan))
	    ((minusp ls) (negative-infinity))
	    (t (positive-infinity)))
      (coerce (/ ls rs) 'double-float)))

(extended-number-op (/)
		    (nan) (nan) (nan) (nan)
		    0 0 (positive-infinity) (negative-infinity))

(defun !/ (ls rs)
  (funcall
   (cond
     ((and (numberp ls) (numberp rs)) #'/number)
     ((and (js-number? ls) (js-number? rs)) #'/number.ext)
     (t (constantly (nan)))) ls rs))

;;
(defun %number (ls rs) (mod ls rs))

(extended-number-op (% :var num)
		    (nan) (nan) (nan) (nan)
		    num num (nan) (nan))

(defun !% (ls rs)
  (funcall
   (cond
     ((and (numberp ls) (numberp rs)) #'mod)
     ((and (js-number? ls) (js-number? rs)) #'%number.ext)
     (t (constantly (nan)))) ls rs))

;;
(defun !=== (ls rs)
  (unless (or (eq ls (nan)) (eq rs (nan)))
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
	((eq ls t) (!== 1 rs))
	((eq ls nil) (!== 0 rs))
	((eq rs t) (!== ls 1))
	((eq rs nil) (!== ls 0))
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
  (unless (or (is-nan ls) (is-nan rs))
    (not (!> ls rs))))

(defun !>= (ls rs)
  (unless (or (is-nan ls) (is-nan rs))
    (not (!< ls rs))))

;;
(defun !++ (arg)
  (!+ (js-funcall number.ensure arg) 1))

(defun !-- (arg)
  (!- (js-funcall number.ensure arg) 1))

;;
(defun !instanceof (ls rs)
  (and (typep ls 'native-hash)
       (eq (prop ls "constructor") rs)))

(defun !typeof (exp)
  (cond
    ((js-number? exp) "number")
    ((undefined? exp) "undefined")
    ((stringp exp) "string")
    ((typep exp 'native-function) "function")
    ((or (eq exp t) (eq exp nil)) "boolean")
    (t "object")))
