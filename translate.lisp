(in-package :js)

(defvar *scope* ())
(defparameter *label-name* nil)

#+nil (defun generic-function-name (gf)
  (slot-value gf 'sb-pcl::name))

(defun fast-get (obj attr)
  `(get-using-getter
    (function ,(generic-function-name (ensure-getter attr)))
    ,obj))

(defun fast-set (obj attr val)
  `(funcall
    (function ,(generic-function-name (ensure-setter attr)))
    ,val ,obj))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun get-attribute (obj attr)
  (if (stringp attr)
      (fast-get obj attr)
      `(prop obj ,attr)))

(defun set-attribute (obj attr val)
  (if (stringp attr)
      (fast-set obj attr val)
      `(setf (prop obj ,attr) ,val))))

(defmacro lookup-global (name)
  `(or (prop** ,(get-attribute *global* name) nil)
      (error "Undefined variable: ~a" ,name)))

(defgeneric lookup-variable (name scope rest))
(defgeneric set-variable (name valname scope rest))

(defmethod lookup-variable (name (scope null) rest)
  (declare (ignore rest))
  `(lookup-global ,name))
(defmethod set-variable (name valname (scope null) rest)
  (declare (ignore rest))
  (set-attribute *global* name valname))

(defstruct with-scope var)
(defmethod lookup-variable (name (scope with-scope) rest)
  `(or (prop** ,(get-attribute (with-scope-var scope) name) nil)
       ,(lookup-variable name (car rest) (cdr rest))))
(defmethod set-variable (name valname (scope with-scope) rest)
  `(if (prop** ,(get-attribute (with-scope-var scope) name) nil)
       ,(set-attribute (with-scope-var scope) name valname)
       ,(set-variable name valname (car rest) (cdr rest))))

(defstruct simple-scope vars)
(defmethod lookup-variable (name (scope simple-scope) rest)
  (let ((sym (->usersym name)))
    (if (member sym (simple-scope-vars scope))
        sym
        (lookup-variable name (car rest) (cdr rest)))))
(defmethod set-variable (name valname (scope simple-scope) rest)
  (let ((sym (->usersym name)))
    (if (member sym (simple-scope-vars scope))
        `(setf ,sym ,valname)
        (set-variable name valname (car rest) (cdr rest)))))

(defstruct (arguments-scope (:include simple-scope)) args)
(defmethod lookup-variable (name (scope arguments-scope) rest)
  (declare (ignore rest))
  (let ((arg-pos (position (->usersym name) (arguments-scope-args scope))))
    (if arg-pos
        `(svref (argument-vector js-user::|arguments|) ,arg-pos)
        (call-next-method))))
(defmethod set-variable (name valname (scope arguments-scope) rest)
  (declare (ignore rest))
  (let ((arg-pos (position (->usersym name) (arguments-scope-args scope))))
    (if arg-pos
        `(setf (svref (argument-vector js-user::|arguments|) ,arg-pos) ,valname)
        (call-next-method))))

(defstruct captured-scope vars local-vars objs next)
(defun capture-scope ()
  (let ((varnames ())
        (val-arg (gensym))
        (locals :null)
        (objs ())
        (next nil))
    (dolist (level *scope*)
      (typecase level
        (simple-scope
         (setf varnames (union varnames (simple-scope-vars level)))
         (when (eq locals :null) (setf locals (simple-scope-vars level))))
        (with-scope (push (with-scope-var level) objs))
        (captured-scope (setf next level))))
    `(make-captured-scope
      :vars (list ,@(loop :for var :in varnames :collect
                       `(list ',var (lambda () ,(lookup var))
                              (lambda (,val-arg) ,(set-in-scope var val-arg)))))
      :local-vars ',locals :objs (list ,@(nreverse objs)) :next ,next)))
(defun lookup-in-captured-scope (name scope)
  (let ((var (assoc (->usersym name) (captured-scope-vars scope))))
    (if var
        (funcall (second var))
        (loop :for obj :in (captured-scope-objs scope) :do
           (let ((val (prop** (get-attribute obj name) nil)))
             (when val (return val)))
           :finally (return (if (captured-scope-next scope)
                                (lookup-in-captured-scope name (captured-scope-next scope))
                                (lookup-global name)))))))
(defmethod lookup-variable (name (scope captured-scope) rest)
  (declare (ignore rest))
  `(lookup-in-captured-scope ,name ,scope))
(defun set-in-captured-scope (name value scope)
  (let ((var (assoc (->usersym name) (captured-scope-vars scope))))
    (if var
        (funcall (third var) value)
        (loop :for obj :in (captured-scope-objs scope) :do
           (when (prop** (get-attribute obj name) nil)
             (return (set-attribute obj name value)))
           :finally (if (captured-scope-next scope)
                        (set-in-captured-scope name value (captured-scope-next scope))
                        (set-attribute *global* name value))))))
(defmethod set-variable (name valname (scope captured-scope) rest)
  (declare (ignore rest))
  `(set-in-captured-scope ,name ,valname ,scope))

(defun lookup (name)
  (lookup-variable name (car *scope*) (cdr *scope*)))
(defun set-in-scope (name value &optional is-defun)
  (let ((valname (gensym))
        (scopes (if is-defun (remove-if (lambda (s) (typep s 'with-scope)) *scope*) *scope*)))
    `(let ((,valname ,value))
       ,(set-variable name valname (car scopes) (cdr scopes)))))

(defmacro with-scope (local &body body)
  `(let ((*scope* (cons ,local *scope*))) ,@body))

(defun in-function-scope-p ()
  (some (lambda (s) (typep s 'simple-scope)) *scope*))

(defun translate (form)
  (apply-translate-rule (car form) (cdr form)))

(defmacro deftranslate ((type &rest arguments) &body body)
  (let ((form-arg (gensym)))
    `(defmethod apply-translate-rule ((,(gensym) (eql ,type)) ,form-arg)
       (destructuring-bind ,arguments ,form-arg ,@body))))

(defgeneric apply-translate-rule (keyword form)
  (:method (keyword form)
    (declare (ignore keyword))
    (mapcar #'translate form)))

(deftranslate (nil) nil)

(deftranslate (:atom atom)
  atom)

(deftranslate (:dot obj attr)
  (get-attribute (translate obj) attr))

(deftranslate (:sub obj attr)
  `(sub ,(translate obj) ,(translate attr)))

(deftranslate (:var bindings)
  `(progn ,@(loop :for (name . val) :in bindings
                  :when val :collect (set-in-scope name (translate val))
                  :else :if (not *scope*) :collect `(set-attribute *global* ,name :undefined))))

(deftranslate (:object properties)
  (let ((obj (gensym)))
    `(let ((,obj (js-new object.ctor ())))
       ,@(loop :for (name . val) :in properties :collect
            `(setf (sub ,obj ,name) ,(translate val)))
       ,obj)))

(deftranslate (:regexp expr)
  `(make-regexp ,(car expr) ,(cdr expr)))

;flags

(deftranslate (:label name form)
  (let ((*label-name* (->usersym name)))
    (translate form)))

;; Used in ,@
(defun translate@ (form)
  (and form (list (translate form))))

(defun translate-for (init cond step body)
  (let ((label (and *label-name* (->usersym *label-name*)))
        (*label-name* nil))
    `(block ,label
       (tagbody
          ,@(translate@ init)
        loop-start
        ,@(and label (list label))
          ,@(translate@ step)
          (unless (js->boolean ,(or (translate cond) t))
            (go loop-end))
          ,@(translate@ body)
          (go loop-start)
        loop-end))))

(deftranslate (:for init cond step body)
  (translate-for init cond step body))

(deftranslate (:while cond body)
  (translate-for nil cond nil body))

(deftranslate (:do cond body)
  (let ((label (and *label-name* (->usersym *label-name*)))
        (*label-name* nil))
    `(block ,label
       (tagbody
        loop-start
        ,@(and label (list label))
          ,@(translate@ body)
          (when (js->boolean ,(translate cond))
            (go loop-start))
        loop-end))))

(deftranslate (:break label)
  (if label
      `(return-from ,(->usersym label))
      `(go loop-end)))

(deftranslate (:continue label)
  `(go ,(if label (->usersym label) 'loop-start)))

(deftranslate (:for-in var name obj body)
  (declare (ignore var))
  (let ((label (and *label-name* (->usersym *label-name*)))
        (*label-name* nil)
        (props (gensym)))
    `(block ,label
       (let ((,props (list-props ,(translate obj))))
         (tagbody
          loop-start
          ,@(and label (list label))
            ,(set-in-scope name `(or (pop ,props) (go loop-end)))
            ,(translate body)
            (go loop-start)
          loop-end)))))

(deftranslate (:if test then else)
  `(if (js->boolean ,(translate test)) ,(translate then) ,(translate else)))

(define-condition js-condition (error)
  ((value :initarg :value :reader js-condition-value))
  (:report (lambda (e stream)
             (format stream "[js] ~s" (js-condition-value e)))))

(deftranslate (:try body catch finally)
  `(,(if finally 'unwind-protect 'prog1)
     ,(if catch
          (with-scope (make-simple-scope :vars (list (->usersym (car catch))))
            `(handler-case ,(translate body)
               (error (,(->usersym (car catch)))
                 (when (typep ,(->usersym (car catch)) 'js-condition)
                   (setf ,(->usersym (car catch)) (js-condition-value ,(->usersym (car catch)))))
                 ,(translate (cdr catch)))))
          (translate body))
     ,@(and finally (list (translate finally)))))

(deftranslate (:throw expr)
  `(error 'js-condition :value ,(translate expr)))

(deftranslate (:name name)
  (lookup name))

(deftranslate (:with obj body)
  (let ((obj-var (gensym "with")))
    `(let ((,obj-var ,(translate obj)))
       (declare (ignorable ,obj-var))
       ,(with-scope (make-with-scope :var obj-var) (translate body)))))

(defun find-locals (body &optional others)
  (let ((found (make-hash-table)))
    (labels ((add (name)
               (setf (gethash (->usersym name) found) t))
             (scan (ast)
               (case (car ast)
                 (:block (mapc #'scan (second ast)))
                 ((:do :while :switch :with :label) (scan (third ast)))
                 (:for-in (when (second ast) (add (third ast)))
                          (scan (fifth ast)))
                 (:for (scan (second ast)) (scan (fifth ast)))
                 (:defun (add (second ast)))
                 (:var (dolist (def (second ast)) (add (car def))))
                 (:if (scan (third ast)) (scan (fourth ast)))
                 (:try (scan (second ast)) (scan (cdr (third ast))) (scan (fourth ast))))))
      (mapc #'add others)
      (mapc #'scan body)
      (let ((others (mapcar '->usersym others)))
        (loop :for name :being :the :hash-key :of found
              :collect name :into all
              :unless (member name others) :collect name :into internal
              :finally (return (values all internal)))))))

(defun references-arguments (body)
  (labels ((scan (expr)
             (when (and (consp expr) (not (member (car expr) '(:function :defun)))) ;; Don't enter inner functions
               (when (and (eq (car expr) :name) (string= (second expr) "arguments"))
                 (return-from references-arguments t))
               (mapc #'scan expr))))
    (scan body)
    nil))
(defun uses-lexical-eval (body)
  (labels ((scan (expr)
             (when (and (consp expr) (not (member (car expr) '(:function :defun)))) ;; Don't enter inner functions
               (when (and (eq (car expr) :call) (equal (second expr) '(:name "eval")))
                 (return-from uses-lexical-eval t))
               (mapc #'scan expr))))
    (scan body)
    nil))

(defun lift-defuns (forms)
  (loop :for form :in forms
        :when (eq (car form) :defun) :collect form :into defuns
        :else :collect form :into other
        :finally (return (append defuns other))))

(defun translate-function (name args body)
  (let ((uses-args (references-arguments body))
        (uses-eval (uses-lexical-eval body))
        (eval-scope (gensym "eval-scope"))
        (base-locals (cons "this" args)))
    (when name (push name base-locals))
    (when uses-args (push "arguments" base-locals))
    (multiple-value-bind (locals internal) (find-locals body base-locals)
      (with-scope (if uses-args
                      (make-arguments-scope :vars locals :args (mapcar '->usersym args))
                      (make-simple-scope :vars locals))
        (when uses-eval
          (push (make-with-scope :var eval-scope) *scope*))
        (let ((funcval
               `(make-instance
                 'native-function :prototype function.prototype :name ,name
                 :proc ,(let ((body1 `((let* (,@(loop :for var :in internal :collect `(,var :undefined))
                                              ,@(and uses-eval `((,eval-scope (js-new object.ctor ()))
                                                                 (eval-env ,(capture-scope)))))
                                         (declare (ignorable ,@internal ,@(and uses-eval (list eval-scope))))
                                         ,@(mapcar 'translate (lift-defuns body))
                                         :undefined))))
                             (if uses-args
                                 (wrap-function/arguments body1)
                                 (wrap-function args body1))))))
          (if name
              `(let (,(->usersym name))
                 (declare (ignorable ,(->usersym name)))
                 (setf ,(->usersym name) ,funcval))
              funcval))))))

(defun wrap-function (args body)
  `(lambda (js-user::|this|
            &optional ,@(loop :for arg :in args :collect
                           `(,(->usersym arg) :undefined))
            &rest extra-args)
     (declare (ignore extra-args)
              (ignorable js-user::|this| ,@(mapcar '->usersym args)))
     (block function ,@body)))

(defun wrap-function/arguments (body)
  (let ((argument-list (gensym "arguments")))
    `(lambda (js-user::|this| &rest ,argument-list)
       (declare (ignorable js-user::|this|))
       (let* ((js-user::|arguments| (make-args ,argument-list)))
         (declare (ignorable js-user::|arguments|))
         (block function ,@body)))))

(deftranslate (:return value)
  (unless (in-function-scope-p)
    (error "return outside of function"))
  `(return-from function ,(if value (translate value) :undefined)))

(deftranslate (:defun name args body)
  (set-in-scope name (translate-function name args body) t))

(deftranslate (:function name args body)
  (translate-function name args body))

(deftranslate (:toplevel body)
  `(progn ,@(mapcar 'translate (lift-defuns body))))

(deftranslate (:new func args)
  `(js-new ,(translate func) (list ,@(mapcar 'translate args))))

(deftranslate (:call func args)
  (cond ((equal func '(:name "eval"))
         `(lexical-eval ,(translate (or (car args) :undefined))
                        ,(if (in-function-scope-p) 'eval-env (capture-scope))))
        ((member (car func) '(:sub :dot))
         (let ((obj (gensym)))
           `(let ((,obj ,(translate (second func))))
              (funcall (the function (proc ,(case (car func)
                                                  (:dot `(prop ,obj ,(third func)))
                                                  (:sub `(sub ,obj ,(translate (third func)))))))
                       ,obj
                       ,@(mapcar 'translate args)))))
        (t `(funcall (the function (proc ,(translate func))) *global* ,@(mapcar 'translate args)))))

(defun translate-assign (place val)
  (case (car place)
    ((:name) (set-in-scope (second place) val))
    ((:dot) (set-attribute (translate (second place)) (third place) val))
    (t `(setf ,(translate place) ,val))))

;; TODO cache path-to-place
(deftranslate (:assign op place val)
  (translate-assign place (translate (if (eq op t) val (list :binary op place val)))))

(deftranslate (:unary-prefix op place)
  (case op
    ((:++ :--) (translate-assign place `(,(js-intern op) ,(translate place))))
    ((:- :+) `(,(js-intern op) 0 ,(translate place)))
    (t `(,(js-intern op) ,(translate place)))))

(deftranslate (:unary-postfix op place)
  (let ((ret (gensym)))
    `(let ((,ret ,(translate place)))
       ,(translate-assign place `(,(js-intern op) ,ret))
       ,ret)))

(deftranslate (:num num)
  (if (integerp num) num (coerce num 'double-float)))

(deftranslate (:string str) str)

(deftranslate (:array elems)
  `(js-new array.ctor (list ,@(mapcar 'translate elems))))

(deftranslate (:stat form)
  (translate form))

(deftranslate (:block forms)
  `(progn ,@(mapcar 'translate forms)))

(deftranslate (:seq form1 result)
  `(prog2 ,(translate form1) ,(translate result)))

(deftranslate (:binary op lhs rhs)
  `(,(js-intern op) ,(translate lhs) ,(translate rhs)))

(defun see (js) (translate (parse-js:parse-js-string js)))
