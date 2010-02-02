(in-package :js)

(defmacro trivial-op (type op)
  (let ((name (intern (concatenate
		       'string (symbol-name op) (symbol-name type))))
	(ls (gensym))
	(rs (gensym)))
    `(defun ,name (,ls ,rs)
       (declare (,type ,ls ,rs))
       (,op ,ls ,rs))))
				   
;;
(trivial-op fixnum +)
(trivial-op number +)

(defun +string (ls rs)
  (let ((ls (js-funcall string.ctor ls))
	(rs (js-funcall string.ctor rs)))
    (concatenate 'string ls rs)))

(defun !+ (ls rs)
  (funcall 
   (cond
     ((and (typep ls 'fixnum) (typep rs 'fixnum)) #'+fixnum)
     ((and (numberp ls) (numberp rs)) #'+number)
     (t #'+string)) ls rs))

;;
(trivial-op fixnum -)
(trivial-op number -)

(defun !- (ls rs)
  (funcall 
   (cond
     ((and (typep ls 'fixnum) (typep rs 'fixnum)) #'-fixnum)
     ((and (numberp ls) (numberp rs)) #'-number)
     (t (constantly :NaN))) ls rs))

;;
