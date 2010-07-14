(in-package :js)

(defun js-type-error () (error "type error"))

(defun default-value (val &optional (hint :string))
  (block nil
    (unless (obj-p val) (return val))
    (when (vobj-p val) (return (vobj-value val)))
    (let ((first "toString") (second "valueOf"))
      (when (eq hint :number) (rotatef first second))
      (let ((method (lookup val first)))
        (when (obj-p method)
          (let ((res (jscall* method val)))
            (unless (obj-p res) (return res)))))
      (let ((method (lookup val second)))
        (when (obj-p method)
          (let ((res (jscall* method val)))
            (unless (obj-p res) (return res)))))
      (js-type-error))))

(defun obj-class (obj) (declare (ignore obj)) "Object") ;; TODO
  
(deftype js-number ()
  (if *float-traps*
      '(or number (member :Inf :-Inf :NaN))
      'number))

;; TODO these might be much faster as methods (profile)
(defun to-string (val)
  (etypecase val
    (string val)
    (js-number (cond ((is-nan val) "NaN")
                     ((eq val (infinity)) "Infinity")
                     ((eq val (-infinity)) "-Infinity")
                     ((integerp val) (princ-to-string val))
                     (t (format nil "~,,,,,,'eE" val))))
    (boolean (if val "true" "false"))
    (symbol (ecase val (:undefined "undefined") (:null "null")))
    (obj (to-string (default-value val)))))

(defun to-number (val)
  (etypecase val
    (js-number val)
    (string (cond ((string= val "Infinity") (infinity))
                  ((string= val "-Infinity") (-infinity))
                  (t (or (read-js-number val) (nan)))))
    (boolean (if val 1 0))
    (symbol (ecase val (:undefined (nan)) (:null 0)))
    (obj (to-number (default-value val :number)))))

(defun to-integer (val)
  (etypecase val
    (integer val)
    (js-number (cond ((is-nan val) 0)
                     ((eq val (infinity)) most-positive-fixnum)
                     ((eq val (-infinity)) most-negative-fixnum)
                     (t (floor val))))
    (string (let ((read (read-js-number val)))
              (etypecase read (null 0) (integer read) (number (floor read)))))
    (boolean (if val 1 0))
    (symbol 0)
    (obj (to-integer (default-value val :number)))))

(defun to-boolean (val)
  (etypecase val
    (boolean val)
    (number (not (or (is-nan val) (zerop val))))
    (string (not (string= val "")))
    (symbol (case val (:Inf t) (:-Inf t) (t nil)))
    (obj t))) ;; TODO check standard

(defun fvector (&rest elements)
  (let ((len (length elements)))
    (make-array len :fill-pointer len :initial-contents elements :adjustable t)))
(defun build-array (vector)
  (make-aobj (find-cls :array) vector))

(defun build-func (lambda)
  (make-fobj (find-cls :function) lambda nil))

(defun lexical-eval (str scope)
  (let* ((str (to-string str))
         (parsed (parse-js:parse-js-string str))
         (*scope* (list scope))
         (env-obj (car (captured-scope-objs scope)))
         (captured-locals (captured-scope-local-vars scope))
         (new-locals (and (not (eq captured-locals :null))
                          (set-difference (mapcar '->usersym (find-locals (second parsed)))
                                          captured-locals))))
    (declare (special *scope*))
    (dolist (local new-locals) (setf (lookup env-obj (symbol-name local)) :undefined))
    (compile-eval (translate-ast parsed))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Ensures safe and predictable redefinitions
  (defun update-set (set name val)
    (let ((prev nil))
      (loop :for cons :on set :do
         (when (equal (caar cons) name)
           (setf (cdar cons) val)
           (return set))
         (setf prev cons)
         :finally (let ((cell (list (cons name val))))
                    (if prev
                        (progn (setf (cdr prev) cell) (return set))
                        (return cell))))))
  (defun slot-flags (props)
    (let ((base (if (member :enum props) 0 +slot-noenum+)))
      (dolist (prop props)
        (case prop (:active (setf base (logior base +slot-active+)))
                   (:ro (setf base (logior base +slot-ro+)))
                   (:nodel (setf base (logior base +slot-nodel+)))))
      base)))

;; List of name->func pairs, where func initializes the value.
(defvar *stdenv-props* ())
(defun addstdprop (name value)
  (setf *stdenv-props* (update-set *stdenv-props* name value)))

(defmacro defobj (proto &body props)
  `(obj-from-props ,proto (list ,@props)))
;; Mostly there to help emacs indent definition bodies
(defmacro mth (name args &body body)
  (multiple-value-bind (name flags)
      (if (consp name) (values (car name) (slot-flags (cdr name))) (values name +slot-noenum+))
    `(list* ,name (build-func ,(wrap-js-lambda args body)) ,flags)))
(defmacro pr (name value &rest flags) `(list* ,name ,value ,(slot-flags flags)))

(defparameter *std-prototypes* ())
(defmacro stdproto (id &body props)
  `(setf *std-prototypes* (update-set *std-prototypes* ,id (lambda () (list ,@props)))))

(defun init-env ()
  (let* ((bootstrap (loop :for (name) :in *std-prototypes* :collect (cons name (make-obj nil nil))))
         (objproto (cdr (assoc :object bootstrap)))
         (clss (loop :for id :in '(:object :arguments :function :array :regexp) :collect
                  (cons id (make-scls () (or (cdr (assoc id bootstrap)) objproto)))))
         (*global* (make-gobj (make-hcls objproto) (make-hash-table :test 'eq) bootstrap clss)))
    (loop :for (nil . shell) :in bootstrap :for (name . create) :in *std-prototypes* :do
       (let ((real-obj (obj-from-props (if (eq name :object) nil objproto) (funcall create))))
         (setf (obj-vals shell) (obj-vals real-obj)
               (obj-cls shell) (obj-cls real-obj))))
    (loop :for (name . func) :in *stdenv-props* :do
       (setf (lookup *global* name) (funcall func)))
    *global*))

(defun find-proto (id)
  (cdr (assoc id (gobj-protos *global*))))
(defun find-cls (id)
  (cdr (assoc id (gobj-common-cls *global*))))

(defmacro stdprop (name value)
  `(addstdprop ,name (lambda () ,value)))
(defmacro stdfunc (name args &body body)
  `(addstdprop ,name (lambda () (build-func ,(wrap-js-lambda args body)))))

(addstdprop "this" (lambda () *global*))
(stdprop "undefined" :undefined)
(stdprop "Infinity" (infinity))
(stdprop "NaN" (nan))

(stdfunc "print" (val)
  (format t "~a~%" (to-string val)))
(stdfunc "parseInt" (val (radix 10))
  (or (parse-integer (to-string val) :junk-allowed t :radix (to-integer radix))
      (nan)))
(stdfunc "parseFloat" (val)
  (let ((val (to-string val)))
    (cond ((string= val "Infinity") (infinity))
          ((string= val "-Infinity") (-infinity))
          (t (or (read-js-number val) (nan))))))
(stdfunc "isNaN" (val)
  (is-nan (to-number val)))
(stdfunc "eval" (str)
  (compile-eval (translate (parse-js:parse-js-string (to-string str)))))

(defun ensure-proto (spec)
  (if (keywordp spec)
      (find-proto spec)
      (obj-from-props (find-proto :object) (cons (pr "constructor" nil) spec))))

(defun build-constructor (self proto props constr)
  (obj-from-props (find-proto :function)
                  (cons (pr "prototype" proto) props)
                  (lambda (cls vals)
                    (setf (fobj-cls self) cls (fobj-vals self) vals (fobj-proc self) constr)))
  (setf (lookup proto "constructor") self)
  self)

;; TODO prevent bogus this objects from being created for many of these
(defmacro stdconstructor (name args &body body/rest)
  (destructuring-bind (body proto &rest props) body/rest
    `(addstdprop ,name (lambda ()
                         (let ((-self- (make-fobj nil nil nil nil)))
                           (build-constructor
                            -self-
                            (ensure-proto ,(if (keywordp proto) proto `(list ,@proto)))
                            (list ,@props)
                            ,(wrap-js-lambda args (list body))))))))

(defmacro stdobject (name &body props)
  `(addstdprop ,name (lambda ()
                       (obj-from-props (find-proto :object) (list ,@props)))))

(stdconstructor "Object" (&rest args)
  (if args
      (make-vobj (ensure-fobj-cls -self-) (car args))
      this)
  :object)

(stdproto :object
  (mth "toString" () (format nil "[object ~a]" (obj-class this)))
  (mth "toLocaleString" () (jsmethod this "toString"))
  (mth "valueOf" () this))

(stdconstructor "Function" (&rest args)
  (let ((body (format nil "(function (~{~a~^, ~}) {~A});"
                      (butlast args) (car (last args)))))
    (compile-eval (translate (parse-js-string body))))
  :function)

(stdproto :function
  ;; TODO hidden property (api)
  (pr "prototype" (cons (js-lambda () (setf (lookup this "prototype") (simple-obj)))
                        (js-lambda (val) (ensure-slot this "prototype" val))) :active)

  (mth "apply" (self args)
    (apply (proc this) self
           (coerce (typecase args
                     (vector args)
                     (aobj (aobj-arr args))
                     (argobj (argobj-vector args))
                     (t (error "second argument to apply must be an array")))
                   'list)))
  (mth "call" (self &rest args)
    (apply (proc this) self args)))

(stdconstructor "Array" (&rest args)
  (let* ((len (length args))
         (arr (if (and (= len 1) (integerp (car args)))
                  (make-array (car args) :initial-element :undefined :fill-pointer (car args) :adjustable t)
                  (make-array len :initial-contents args :fill-pointer len :adjustable t))))
    (make-aobj (ensure-fobj-cls -self-) arr))
  :array)

(defmacro unless-array (default &body body)
  `(if (aobj-p this) (progn ,@body) ,default))

(stdproto :array
  (pr "length" (cons (js-lambda () (if (aobj-p this) (length (aobj-arr this)) 0)) nil) :active)

  (mth "toString" ()
    (jsmethod this "join"))

  (mth "concat" (&rest others)
    (let* ((elements (loop :for elt :in (cons this others) :collect
                        (if (aobj-p elt) (aobj-arr elt) (vector elt))))
           (size (reduce #'+ elements :key #'length))
           (arr (make-array size :fill-pointer size :adjustable t))
           (pos 0))
      (dolist (elt elements)
        (loop :for val :across elt :do
           (setf (aref arr pos) val)
           (incf pos)))
      (build-array arr)))
  (mth "join" ((sep ","))
    (unless-array ""
      (let ((sep (to-string sep)))
        (with-output-to-string (out)
          (loop :for val :across (aobj-arr this) :for first := t :then nil :do
             (unless first (write-string sep out))
             (write-string (to-string val) out))))))

  (mth "splice" (index howmany &rest elems)
    (unless-array (build-array (fvector))
      (let* ((vec (aobj-arr this))
             (index (clip-index (to-integer index) (length vec)))
             (removed (clip-index (to-integer howmany) (- (length vec) index)))
             (added (length elems))
             (diff (- added removed))
             (new-len (- (+ (length vec) added) removed))
             (result (make-array removed :fill-pointer removed :adjustable t)))
        (replace result vec :start2 index :end2 (+ index removed))
        (cond ((< diff 0) ;; shrink
               (replace vec vec :start1 (+ index added) :start2 (+ index removed))
               (setf (fill-pointer vec) new-len))
              ((> diff 0) ;; grow
               (adjust-array vec new-len :fill-pointer new-len)
               (replace vec vec :start1 (+ index added) :start2 (+ index removed))))
        (replace vec elems :start1 index)
        (build-array result))))

  (mth "pop" ()
    (unless-array :undefined
      (let ((vec (aobj-arr this)))
        (if (= (length vec) 0)
            :undefined
            (vector-pop vec)))))
  (mth "push" (val)
    (unless-array 0
      (let ((vec (aobj-arr this)))
        (vector-push-extend val vec)
        (length vec))))

  (mth "shift" ()
    (unless-array :undefined
      (let* ((vec (aobj-arr this)) (len (length vec)))
        (if (> len 0)
            (let ((result (aref vec 0)))
              (replace vec vec :start2 1)
              (setf (fill-pointer vec) (1- len))
              result)
            :undefined))))
  (mth "unshift" (val)
    (unless-array 0
      (let ((vec (aobj-arr this)))
        (setf (fill-pointer vec) (1+ (length vec)))
        (replace vec vec :start1 1)
        (setf (aref vec 0) val)
        (length vec))))

  (mth "reverse" ()
    (unless-array (build-array (fvector this))
      (setf (aobj-arr this) (nreverse (aobj-arr this)))
      this))
  (mth "sort" (compare)
    (unless-array (build-array (fvector this))
      (let ((func (if (eq compare :undefined)
                      (lambda (a b) (string< (to-string a) (to-string b))) ;; TODO less wasteful
                      (let ((proc (proc compare)))
                        (lambda (a b) (funcall proc *global* a b))))))
        (sort (aobj-arr this) func)
        this))))

(stdproto :arguments
  (pr "length" (cons (js-lambda () (length (argobj-vector this))) nil) :active)
  (pr "callee" (cons (js-lambda () (argobj-callee this)) nil) :active))

(stdconstructor "String" (value)
  (if (eq this *global*)
      (to-string value)
      (make-vobj (ensure-fobj-cls -self-) (to-string value)))
  :string
  (mth "fromCharCode" (code)
    (string (code-char (to-integer code)))))

(defun clip-index (index len)
  (setf index (to-integer index))
  (cond ((< index 0) 0)
        ((> index len) len)
        (t index)))

(defun careful-substr (str from to)
  (let* ((len (length str))
         (from (clip-index from len)))
    (if (eq to :undefined)
        (subseq str from)
        (subseq str from (max from (clip-index to len))))))

(defun really-string (val)
  (if (stringp val) val (and (vobj-p val) (stringp (vobj-value val)) (vobj-value val))))

(stdproto :string
  (pr "length" (cons (js-lambda () (let ((str (really-string this))) (if str (length str) 0))) nil) :active)

  (mth "toString" () (or (really-string this) (js-type-error)))
  (mth "valueOf" () (or (really-string this) (js-type-error)))

  (mth "charAt" (index)
    (let ((str (to-string this))
          (idx (to-integer index)))
      (if (< -1 idx (length str)) (string (char str idx)) "")))
  (mth "indexOf" (substr (start 0))
    (or (search (to-string substr) (to-string this) :start2 (to-integer start)) -1))
  (mth "lastIndexOf" (substr start)
    (let* ((str (to-string this))
           (start (if (eq start :undefined) (length str) (to-integer start))))
      (or (search (to-string substr) str :from-end t :end2 start))))

  (mth "substring" ((from 0) to)
    (careful-substr (to-string this) from to))
  (mth "substr" ((from 0) len)
    (careful-substr (to-string this) from
                    (if (eq len :undefined) len (+ (to-integer from) (to-integer len)))))

  (mth "toUpperCase" ()
    (string-upcase (to-string this)))
  (mth "toLowerCase" ()
    (string-downcase (to-string this)))

  (mth "split" (delim)
    (let ((str (to-string this))
          (delim (to-string delim)))
      (build-array
       (if (equal delim "")
           (fvector str)
           (coerce (loop :with step := (length delim) :for beg := 0 :then (+ pos step)
                         :for pos := (search delim str :start2 beg) :while pos :collect
                      (subseq str beg pos)) 'simple-vector))))))

(stdconstructor "Number" (value)
  (if (eq this *global*)
      (to-number value)
      (make-vobj (ensure-fobj-cls -self-) (to-number value)))
  :number
  (pr "MAX_VALUE" most-positive-double-float)
  (pr "MIN_VALUE" most-negative-double-float)
  (pr "POSITIVE_INFINITY" (infinity))
  (pr "NEGATIVE_INFINITY" (-infinity)))

(defun typed-value-of (obj type)
  (if (and (vobj-p obj) (typep (vobj-value obj) type)) (vobj-value obj) (js-type-error)))

(stdproto :number
  (mth "toString" ((radix 10))
    (let ((num (typed-value-of this 'js-number)))
      (if (= radix 10)
          (to-string num)
          (let ((*print-radix* (to-integer radix))) (princ-to-string (floor num))))))
  (mth "valueOf" () (typed-value-of this 'js-number)))

(stdconstructor "Boolean" (value)
  (if (eq this *global*)
      (to-boolean value)
      (make-vobj (ensure-fobj-cls -self-) (to-boolean value)))
  ())

(stdproto :boolean
  (mth "toString" () (if (typed-value-of this 'boolean) "true" "false"))
  (mth "valueOf" () (typed-value-of this 'boolean)))

(defun init-reobj (obj pattern flags)
  (let* ((flags (if (eq flags :undefined) "" (to-string flags)))
         (pattern (to-string pattern))
         (scanner (ppcre:create-scanner
                   pattern :case-insensitive-mode (position #\i flags))))
    (unless (every (lambda (ch) (position ch "igm")) flags)
      (error "Invalid regular expression flags: ~a" flags))
    (setf (reobj-proc obj) (js-lambda (str)
                             (let ((str (to-string str)))
                               (multiple-value-bind (from to) (ppcre:scan scanner str)
                                 (if from (subseq str from to) :null))))
          (reobj-scanner obj) scanner
          (reobj-args obj) (cons pattern flags))
    obj))

(stdconstructor "RegExp" (pattern flags)
  (init-reobj (make-reobj (ensure-fobj-cls -self-) nil nil nil) pattern flags)
  :regexp)

(stdproto :regexp
  (mth "toString" ()
    (if (reobj-p this)
        (format nil "/~a/~a" (car (reobj-args this)) (cdr (reobj-args this)))
        (to-string this)))

  (mth "exec" (str)
    (if (reobj-p this)
        (funcall (reobj-proc this) this (to-string str))
        nil))
  (mth "compile" (expr flags)
    (when (reobj-p this) (init-reobj this expr flags))
    this)
  (mth "test" (str)
    (if (reobj-p this)
        (and (ppcre:scan (reobj-scanner this) (to-string str)) t)
        nil)))

(defmacro math-case (var &body cases)
  (flet ((find-case (id)
           (or (cdr (assoc id cases)) '((nan)))))
    `(let ((,var (to-number ,var)))
       (cond ((is-nan ,var) ,@(find-case :NaN))
             ((eq ,var (infinity)) ,@(find-case :Inf))
             ((eq ,var (-infinity)) ,@(find-case :-Inf))
             (t ,@(find-case t))))))

(defun my-atan (arg)
  (math-case arg (:-Inf (- (/ pi 2))) (:Inf (/ pi 2)) (t (atan arg))))

(defmacro compare-num (a b gt lt cmp)
  `(let ((ls ,a) (rs ,b))
     (cond ((or (is-nan ls) (is-nan rs)) (nan))
           ((or (eq ls ,gt) (eq rs ,gt)) ,gt)
           ((eq ls ,lt) rs)
           ((eq rs ,lt) ls)
           (t (,cmp ls rs)))))

(stdobject "Math"
  (pr "E" (exp 1))
  (pr "LN2" (log 2))
  (pr "LN10" (log 10))
  (pr "LOG2E" (log (exp 1) 2))
  (pr "LOG10E" (log (exp 1) 10))
  (pr "SQRT1_2" (sqrt .5))
  (pr "SQRT1_2" (sqrt 2))
  (pr "PI" pi)

  (mth "abs" (arg)
    (math-case arg (:-Inf (infinity)) (:Inf (infinity)) (t (abs arg))))

  (mth "cos" (arg)
    (math-case arg (t (cos arg))))
  (mth "sin" (arg)
    (math-case arg (t (sin arg))))
  (mth "tan" (arg)
    (math-case arg (t (tan arg))))

  (mth "acos" (arg)
    (math-case arg (t (let ((res (acos arg))) (if (realp res) res (nan))))))
  (mth "asin" (arg)
    (math-case arg (t (let ((res (asin arg))) (if (realp res) res (nan))))))
  (mth "atan" (arg)
    (my-atan arg))
  (mth "atan2" (x y)
    (my-atan (!/ x y)))

  (mth "ceil" (arg)
    (math-case arg (:-Inf (-infinity)) (:Inf (infinity)) (t (ceiling arg))))
  (mth "floor" (arg)
    (math-case arg (:-Inf (-infinity)) (:Inf (infinity)) (t (floor arg))))
  (mth "round" (arg)
    (math-case arg (:-Inf (-infinity)) (:Inf (infinity)) (t (round arg))))

  (mth "exp" (arg)
    (math-case arg (:-Inf 0) (:Inf (infinity)) (t (exp arg))))
  (mth "log" (arg)
    (math-case arg
      (:Inf (infinity))
      (t (cond ((zerop arg) (-infinity))
               ((minusp arg) (nan))
               (t (log arg))))))
  (mth "sqrt" (arg)
    (math-case arg (let ((res (sqrt arg))) (if (realp res) res (nan)))))
  (mth "pow" (base exp)
    (let ((base (to-number base)) (exp (to-number exp)))
      (cond ((or (is-nan base) (is-nan exp)) (nan))
            ((eq exp (-infinity)) (nan))
            ((and (realp exp) (zerop exp)) 1)
            ((or (eq base (infinity)) (eq exp (infinity))) (infinity))
            ((eq base (-infinity)) (-infinity))
            (t (coerce (expt base exp) 'double-float)))))

  (mth "max" (&rest args)
    (let ((cur (-infinity)))
      (dolist (arg args)
        (setf cur (compare-num cur (to-number arg) (infinity) (-infinity) max)))
      cur))
  (mth "min" (&rest args)
    (let ((cur (infinity)))
      (dolist (arg args)
        (setf cur (compare-num cur (to-number arg) (-infinity) (infinity) min)))
      cur))

  (mth "random" ()
    (random 1.0)))

(setf *global* (init-env))
