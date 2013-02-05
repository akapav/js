(in-package :cl-js)

(defmacro complicated-numeric-op (ls rs op nan
                                  inf-inf inf-minf minf-inf minf-minf
                                  num-inf num-minf inf-num minf-num)
  `(let ((ls ,ls) (rs ,rs))
     ,(if *float-traps*
          `(cond
             ((and (numberp ls) (numberp rs)) (,op ls rs))
             ((or (is-nan ls) (is-nan rs)) ,nan)
             ((and (eq ls ,(infinity)) (eq rs ,(infinity))) ,inf-inf)
             ((and (eq ls ,(infinity)) (eq rs ,(-infinity))) ,inf-minf)
             ((and (eq ls ,(-infinity)) (eq rs ,(infinity))) ,minf-inf)
             ((and (eq ls ,(-infinity)) (eq rs ,(-infinity))) ,minf-minf)
             ((eq rs ,(infinity)) ,num-inf)
             ((eq rs ,(-infinity)) ,num-minf)
             ((eq ls ,(infinity)) ,inf-num)
             ((eq ls ,(-infinity)) ,minf-num))
          `(,op ls rs))))

(defun js+ (ls rs)
  (cond ((and (numberp ls) (numberp rs)) (+ ls rs))
        ((and (stringp ls) (stringp rs)) (concatenate 'string ls rs))
        (t (let ((ls (default-value ls)) (rs (default-value rs)))
             (cond ((stringp ls) (concatenate 'string ls (to-string rs)))
                   ((stringp rs) (concatenate 'string (to-string ls) rs))
                   ((and (numberp ls) (numberp rs)) (+ ls rs))
                   (t (complicated-numeric-op
                       (to-number ls) (to-number rs) + (nan)
                       (infinity) (nan) (nan) (-infinity) (infinity)
                       (-infinity) (infinity) (-infinity))))))))

(defun js++ (arg)
  (js+ (to-number arg) 1))

(defmacro maybe-complicated-numeric-op (ls rs op &rest specs)
  `(let ((ls ,ls) (rs ,rs))
     (if (and (numberp ls) (numberp rs))
         (,op ls rs)
         (complicated-numeric-op (to-number ls) (to-number rs) ,op ,@specs))))

(defun js- (ls rs)
  (maybe-complicated-numeric-op
   ls rs - (nan)
   (nan) (infinity) (-infinity) (nan) (-infinity)
   (infinity) (infinity) (-infinity)))

(defun js-- (arg)
  (js- (to-number arg) 1))

(defun js* (ls rs)
  (maybe-complicated-numeric-op
   ls rs * (nan)
   (infinity) (-infinity) (-infinity) (infinity) (infinity)
   (-infinity) (infinity) (-infinity)))

(defun sign-of (val)
  (cond ((eq val (-infinity)) nil)
        ((eq val (infinity)) t)
        ((is-nan val) t)
        ((integerp val) (>= val 0))
        (t (float-sign val)))) ;; Doesn't work for -0 on all implementations (SBCL works, ACL doesn't)

(defun js/ (ls rs)
  (let ((ls (to-number ls)) (rs (to-number rs)))
    (if (zerop rs)
        (if (eq (sign-of ls) (sign-of rs)) (infinity) (-infinity))
        (complicated-numeric-op
         ls rs / (nan)
         (nan) (nan) (nan) (nan) 0 0 (infinity) (-infinity)))))

(defun js% (ls rs)
  (let ((ls (to-number ls)) (rs (to-number rs)))
    (if (zerop rs)
        (nan)
        (complicated-numeric-op
         ls rs rem (nan)
         (nan) (nan) (nan) (nan) ls ls (nan) (nan)))))

(defun js^ (ls rs)
  (logxor (to-int32 ls) (to-int32 rs)))
(defun js\| (ls rs)
  (logior (to-int32 ls) (to-int32 rs)))
(defun js& (ls rs)
  (logand (to-int32 ls) (to-int32 rs)))
(defun js~ (rs)
  (lognot (to-int32 rs)))

(defun js>> (a b)
  (ash (to-int32 a) (- (to-int32 b))))
(defun js<< (a b)
  (ash (to-int32 a) (to-int32 b)))
(defun js>>> (a b)
  (bitshift32 (to-int32 a) (to-int32 b)))

(defun bitshift32 (a b)
  (if (< a 0)
      (ash (ldb (byte 32 0) a) (- b))
      (ash a (- b))))

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
        ((eq rs :null) (eq ls :undefined))
        ((eq rs :undefined) (eq ls :null))
        ((numberp ls) (let ((rsn (to-number rs)))
                        (and (not (is-nan rsn)) (= ls rsn))))
        ((stringp ls) (string= ls (to-string rs)))
        ((or (eq ls t) (eq ls nil)) (js== (if ls 1 0) rs))
        ((or (eq rs t) (eq rs nil)) (js== ls (if rs 1 0)))
        ((obj-p ls) (cond ((stringp rs) (js== (default-value ls) rs))
                          ((typep rs 'js-number) (js== (default-value ls :number) rs))))))

(defun js!= (ls rs)
  (not (js== ls rs)))


(defmacro complicated-comparision-op (ls rs op &rest specs)
  (let ((str-op (intern (format nil "~a~a" :string op))))
    `(let ((ls (default-value ,ls)) (rs (default-value ,rs)))
       (if (and (stringp ls) (stringp rs))
           (,str-op ls rs)
           (let ((ls (to-number ls)) (rs (to-number rs)))
             ,(let ((compl `(complicated-numeric-op (to-number ls) (to-number rs) ,op nil ,@specs)))
                (if *float-traps*
                    compl
                    `(unless (or (is-nan ls) (is-nan rs)) ,compl))))))))
                    
(defun js< (ls rs)
  (complicated-comparision-op ls rs < nil nil t nil t nil nil t))
(defun js> (ls rs)
  (complicated-comparision-op ls rs > nil nil t nil t nil nil t))
(defun js<= (ls rs)
  (complicated-comparision-op ls rs <= t t nil t nil t t nil))
(defun js>= (ls rs)
  (complicated-comparision-op ls rs >= t t nil t nil t t nil))

(defun jsinstanceof (ls rs)
  (and (obj-p ls) (fobj-p rs)
       (let ((proto (js-prop rs "prototype")))
         (loop :for cur := ls :then (cls-prototype (obj-cls cur)) :while cur :do
            (when (eq cur proto) (return t))))))

(defun jsin (prop obj)
  (if-not-found (nil (js-prop obj prop)) nil t))

(defgeneric js-type-of (expr)
  (:method ((expr string)) "string")
  (:method ((expr number)) "number")
  (:method ((expr symbol))
    (ecase expr ((t nil) "boolean") (:undefined "undefined") (:null "object")
                ((:NaN :Inf :-Inf) "number")))
  (:method ((expr fobj)) "function")
  (:method ((expr obj)) "object")
  (:method (expr) (error "No type-of defined for value ~a" expr)))
