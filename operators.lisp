(in-package :js)

(defun +fixnum (ls rs)
  (declare (fixnum ls rs))
  (+ ls rs))

(defun +number (ls rs)
  (declare (number ls rs))
  (+ ls rs))

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
