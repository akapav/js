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
           (cond
             ((and (numberp ,ls) (numberp ,rs)) (,name ,ls ,rs))
             ((or (is-nan ,ls) (is-nan ,rs)) ,nan)
             ((and (eq ,ls ,(infinity)) (eq ,rs ,(infinity))) ,inf-inf)
             ((and (eq ,ls ,(infinity)) (eq ,rs ,(-infinity))) ,inf-minf)
             ((and (eq ,ls ,(-infinity)) (eq ,rs ,(infinity))) ,minf-inf)
             ((and (eq ,ls ,(-infinity)) (eq ,rs ,(-infinity))) ,minf-minf)
             ((eq ,rs ,(infinity)) ,num-inf)
             ((eq ,rs ,(-infinity)) ,num-minf)
             ((eq ,ls ,(infinity)) ,inf-num)
             ((eq ,ls ,(-infinity)) ,minf-num)
             (t (error (format nil "internal error in ~A~%" ',op)))))
        `(defun ,nameext (,ls ,rs)
           (declare (number ,ls ,rs))
           (,name ,ls ,rs)))))

;;

(trivial-op fixnum +)
(trivial-op number +)

(extended-number-op (+)
		    (infinity) (nan) (nan) (-infinity)
		    (infinity) (-infinity) (infinity)
                    (-infinity))

(defun +string (ls rs)
  (concatenate 'string (to-string ls) (to-string rs)))

;; TODO optimize
(defun !+ (ls rs)
  (funcall
   (cond
     ((and (typep ls 'fixnum) (typep rs 'fixnum)) #'+fixnum)
     ((and (numberp ls) (numberp rs)) #'+number)
     ;;;todo: fast string concat when both are strings
     ((and (numberp ls) (numberp rs)) #'+number.ext)
     ((and (numberp ls) (eq rs :undefined)) (constantly (nan)))
     ((and (eq ls :undefined) (eq rs :undefined)) (constantly (nan)))
     ((and (eq ls :undefined) (eq rs :undefined)) (constantly (nan)))
     (t #'+string)) ls rs))

;;
(trivial-op fixnum -)
(trivial-op number -)

(extended-number-op (-)
		    (nan) (infinity) (-infinity) (nan)
		    (-infinity) (infinity) (infinity)
                    (-infinity))

(defun !- (ls rs)
  (funcall 
   (cond
     ((and (typep ls 'fixnum) (typep rs 'fixnum)) #'-fixnum)
     ((and (numberp ls) (numberp rs)) #'-number)
     ((and (numberp ls) (numberp rs)) #'-number.ext)
     (t (constantly (nan)))) ls rs))

;;
(trivial-op fixnum *)
(trivial-op number *)

(extended-number-op (*)
		    (infinity) (-infinity) (-infinity)
                    (infinity) (infinity) (-infinity)
                    (infinity) (-infinity))

(defun !* (ls rs)
  (funcall
   (cond
     ((and (typep ls 'fixnum) (typep rs 'fixnum)) #'*fixnum)
     ((and (numberp ls) (numberp rs)) #'*number)
     ((and (numberp ls) (numberp rs)) #'*number.ext)
     (t (constantly (nan)))) ls rs))     

;;
(defun /number (ls rs)
  (declare (number ls rs))
  (if (zerop rs)
      (cond ((zerop ls) (nan))
	    ((minusp ls) (-infinity))
	    (t (infinity)))
      (coerce (/ ls rs) 'double-float)))

(extended-number-op (/)
		    (nan) (nan) (nan) (nan)
		    0 0 (infinity) (-infinity))

(defun !/ (ls rs)
  (funcall
   (cond
     ((and (numberp ls) (numberp rs)) #'/number)
     ((and (numberp ls) (numberp rs)) #'/number.ext)
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
     ((and (numberp ls) (numberp rs)) #'%number.ext)
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
	((eq (type-of ls) (type-of rs))
	 (!=== ls rs))
	((and (numberp ls) (stringp rs))
	 (!=== ls (to-number rs)))
	((and (stringp ls) (numberp rs))
	 (!=== (to-number ls) rs))
	((or
	  (and (eq ls :undefined) (eq rs :null))
	  (and (eq ls :null) (eq rs :undefined))) t)
	((eq ls t) (!== 1 rs))
	((eq ls nil) (!== 0 rs))
	((eq rs t) (!== ls 1))
	((eq rs nil) (!== ls 0))
	(t (!=== (to-string ls)
		 (to-string rs))))))

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
    ((and (numberp ls) (numberp rs)) (<number.ext ls rs))
    ((and (numberp ls) (stringp rs)) (<number.ext ls (to-number rs)))
    ((and (stringp ls) (numberp rs)) (<number.ext (to-number ls) rs))
    (t (when (string< (to-string ls) (to-string rs)) t))))

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
    ((and (numberp ls) (numberp rs)) (>number.ext ls rs))
    ((and (numberp ls) (stringp rs)) (>number.ext ls (to-number rs)))
    ((and (stringp ls) (numberp rs)) (>number.ext (to-number ls) rs))
    (t (when (string> (to-string ls) (to-string rs)) t))))

;;
(defun !<= (ls rs)
  (unless (or (is-nan ls) (is-nan rs))
    (not (!> ls rs))))

(defun !>= (ls rs)
  (unless (or (is-nan ls) (is-nan rs))
    (not (!< ls rs))))

;;
(defun !++ (arg)
  (!+ (to-number arg) 1))

(defun !-- (arg)
  (!- (to-number arg) 1))

;;
(defun !instanceof (ls rs)
  (and (obj-p ls) (fobj-p rs)
       (let ((proto (lookup rs "prototype")))
         (loop :for cur := ls :then (cls-prototype (obj-cls cur)) :while cur :do
            (when (eq cur proto) (return t))))))

(defun !typeof (exp)
  (cond
    ((numberp exp) "number")
    ((eq exp :undefined) "undefined")
    ((stringp exp) "string")
    ((typep exp 'fobj) "function")
    ((or (eq exp t) (eq exp nil)) "boolean")
    (t "object")))
