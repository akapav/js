(in-package :js)

;; TODO try operators as clos methods

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
(defun js+ (ls rs)
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

(defun js- (ls rs)
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

(defun js* (ls rs)
  (funcall
   (cond
     ((and (typep ls 'fixnum) (typep rs 'fixnum)) #'*fixnum)
     ((and (numberp ls) (numberp rs)) #'*number)
     ((and (numberp ls) (numberp rs)) #'*number.ext)
     (t (constantly (nan)))) ls rs))

(trivial-op fixnum logxor)


(defun js^ (ls rs)
  (cond
    ((and (typep ls 'fixnum) (typep rs 'fixnum)) (funcall #'logxorfixnum ls rs))
    (t (funcall #'logxorfixnum (the fixnum (to-integer32 ls)) (the fixnum (to-integer32 rs))))))

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

(defun js/ (ls rs)
  (if (and (numberp ls) (numberp rs))
      (/number ls rs)
      (/number.ext (to-number ls) (to-number rs))))

;;
(defun %number (ls rs) (mod ls rs))

(extended-number-op (% :var num)
		    (nan) (nan) (nan) (nan)
		    num num (nan) (nan))

(defun js% (ls rs)
  (funcall
   (cond
     ((and (numberp ls) (numberp rs)) #'mod)
     ((and (numberp ls) (numberp rs)) #'%number.ext)
     (t (constantly (nan)))) ls rs))

;;
(defun js=== (ls rs)
  (cond ((is-nan ls) nil)
        ((eq ls rs) t)
        ((stringp ls) (and (stringp rs) (string= ls rs)))
        ((numberp ls) (and (numberp rs) (= ls rs)))))

(defun js!== (ls rs)
  (not (js=== ls rs)))

;;
(defun js== (ls rs)
  (cond ((is-nan ls) nil)
        ((eq ls rs) t)
        ((eq ls :null) (eq rs :undefined))
        ((eq ls :undefined) (eq rs :null))
        ((numberp ls) (let ((rsn (to-number rs)))
                        (and (not (is-nan rsn)) (= ls rsn))))
        ((stringp ls) (string= ls (to-string rs)))
        ((or (eq ls t) (eq ls nil)) (js== (if ls 1 0) rs))
        ((or (eq rs t) (eq rs nil)) (js== ls (if rs 1 0)))
        ((obj-p ls) (cond ((stringp rs) (js== (default-value ls) rs))
                          ((typep rs 'js-number) (js== (default-value ls :number) rs))))))

(defun js!= (ls rs)
  (not (js== ls rs)))

;;
(trivial-op fixnum <)
(trivial-op number <)

(extended-number-op (< :nan nil)
		    nil nil t nil
		    t nil nil t)

(defun js< (ls rs)
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

(defun js> (ls rs)
  (cond
    ((and (typep ls 'fixnum) (typep rs 'fixnum)) (>fixnum ls rs))
    ((and (numberp ls) (numberp rs)) (>number ls rs))
    ((and (numberp ls) (numberp rs)) (>number.ext ls rs))
    ((and (numberp ls) (stringp rs)) (>number.ext ls (to-number rs)))
    ((and (stringp ls) (numberp rs)) (>number.ext (to-number ls) rs))
    (t (when (string> (to-string ls) (to-string rs)) t))))

;;
(defun js<= (ls rs)
  (unless (or (is-nan ls) (is-nan rs))
    (not (js> ls rs))))

(defun js>= (ls rs)
  (unless (or (is-nan ls) (is-nan rs))
    (not (js< ls rs))))

;;
(defun js++ (arg)
  (js+ (to-number arg) 1))

(defun js-- (arg)
  (js- (to-number arg) 1))

;; TODO to-int-32
(defun js>> (a b)
  (ash (to-integer a) (- (to-integer b))))
(defun js<< (a b)
  (ash (to-integer a) (to-integer b)))
(defun js>>> (a b)
  (ash (to-integer a) (- (to-integer b))))

;;
(defun jsinstanceof (ls rs)
  (and (obj-p ls) (fobj-p rs)
       (let ((proto (lookup rs "prototype")))
         (loop :for cur := ls :then (cls-prototype (obj-cls cur)) :while cur :do
            (when (eq cur proto) (return t))))))

(defun jsin (prop obj)
  (if-not-found (nil (lookup obj prop)) nil t))

(defun js-type-of (exp)
  (etypecase exp
    (string "string")
    (js-number "number")
    (fobj "function")
    (obj "object")
    (symbol (ecase exp ((t nil) "boolean") (:undefined "undefined") (:null "object")))))
