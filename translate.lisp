(in-package :js)

;; TODO :delete, :switch

(defvar *scope* ())
(defparameter *label-name* nil)
(defvar *break/cont* ())

(defgeneric lookup-variable (name scope rest))
(defgeneric set-variable (name valname scope rest))

(defmethod lookup-variable (name (scope null) rest)
  (declare (ignore rest))
  (expand-global-lookup name))
(defmethod set-variable (name valname (scope null) rest)
  (declare (ignore rest))
  (expand-global-set name valname))

(defstruct with-scope var)
(defmethod lookup-variable (name (scope with-scope) rest)
  (let ((found (gensym)))
    `(if-not-found (,found ,(expand-cached-lookup (with-scope-var scope) name))
       ,(lookup-variable name (car rest) (cdr rest))
       ,found)))
(defmethod set-variable (name valname (scope with-scope) rest) ;; TODO hasOwnProperty?
  `(if-not-found (nil ,(expand-cached-lookup (with-scope-var scope) name))
     ,(set-variable name valname (car rest) (cdr rest))
     ,(expand-cached-set (with-scope-var scope) name valname)))

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
        `(svref (argobj-vector js-user::|arguments|) ,arg-pos)
        (call-next-method))))
(defmethod set-variable (name valname (scope arguments-scope) rest)
  (declare (ignore rest))
  (let ((arg-pos (position (->usersym name) (arguments-scope-args scope))))
    (if arg-pos
        `(setf (svref (argobj-vector js-user::|arguments|) ,arg-pos) ,valname)
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
                       `(list ',var (lambda () ,(lookup-var (symbol-name var)))
                              (lambda (,val-arg) ,(set-in-scope (symbol-name var) val-arg)))))
      :local-vars ',locals :objs (list ,@(nreverse objs)) :next ,next)))
(defun lookup-in-captured-scope (name scope)
  (let ((var (assoc (->usersym name) (captured-scope-vars scope))))
    (if var
        (funcall (second var))
        (loop :for obj :in (captured-scope-objs scope) :do
           (if-not-found (val (lookup obj name))
             nil
             (return val))
           :finally (return (if (captured-scope-next scope)
                                (lookup-in-captured-scope name (captured-scope-next scope))
                                (global-lookup name)))))))
(defmethod lookup-variable (name (scope captured-scope) rest)
  (declare (ignore rest))
  `(lookup-in-captured-scope ,name ,scope))
(defun set-in-captured-scope (name value scope)
  (let ((var (assoc (->usersym name) (captured-scope-vars scope))))
    (if var
        (funcall (third var) value)
        (loop :for obj :in (captured-scope-objs scope) :do
           (if-not-found (nil (lookup obj name))
             nil
             (return (setf (lookup obj name) value)))
           :finally (if (captured-scope-next scope)
                        (set-in-captured-scope name value (captured-scope-next scope))
                        (setf (lookup *global* name) value))))))
(defmethod set-variable (name valname (scope captured-scope) rest)
  (declare (ignore rest))
  `(set-in-captured-scope ,name ,valname ,scope))

(defun lookup-var (name)
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

(let ((integers-are-fixnums
       (and (>= most-positive-fixnum (1- (expt 2 53)))
            (<= most-negative-fixnum (- (expt 2 53))))))
  (defun inferred-type-to-lisp-type (type)
    (case type
      (:integer (if integers-are-fixnums 'fixnum 'integer))
      (:number 'number))))

(defun translate (form)
  (let ((result (apply-translate-rule (car form) (cdr form)))
        (typing (ast-type form)))
    (if (and typing (setf typing (inferred-type-to-lisp-type typing)))
        `(the ,typing ,result)
        result)))
(defun translate-ast (ast)
  (translate (infer-types ast)))

(defmacro deftranslate ((type &rest arguments) &body body)
  (let ((form-arg (gensym)))
    `(defmethod apply-translate-rule ((,(gensym) (eql ,type)) ,form-arg)
       (destructuring-bind (,@arguments &rest rest) ,form-arg
         (declare (ignore rest)) ,@body))))

(defgeneric apply-translate-rule (keyword form)
  (:method (keyword form)
    (declare (ignore keyword))
    (mapcar #'translate form)))

(deftranslate (nil) nil)

(deftranslate (:atom atom)
  (case atom
    (:true t)
    (:false nil)
    (t atom)))

(deftranslate (:dot obj attr)
  (expand-cached-lookup (translate obj) attr))

(deftranslate (:sub obj attr)
  `(lookup ,(translate obj) ,(translate attr)))

(deftranslate (:var bindings)
  `(progn ,@(loop :for (name . val) :in bindings
                  :when val :collect (set-in-scope name (translate val))
                  :else :if (not *scope*) :collect (expand-global-set name :undefined))))

(deftranslate (:object properties)
  (expand-static-obj '(find-proto :object) (loop :for (name . val) :in properties :collect
                                              (cons name (translate val)))))

;; TODO reuse class
(deftranslate (:regexp expr flags)
  `(load-time-value (init-reobj (make-reobj (find-cls :regexp) nil nil nil) ,expr ,flags)))

;flags

(deftranslate (:label name form)
  (let ((*label-name* (->usersym name)))
    (translate form)))

;; Used in ,@
(defun translate@ (form)
  (and form (list (translate form))))

(defun translate/break-continue (label form)
  (let* ((*break/cont* (cons nil *break/cont*))
         (translated (translate form)))
    (let (br cn lb-br lb-cn)
      (dolist (evt (car *break/cont*))
        (cond ((eq evt :break) (setf br t))
              ((eq evt :continue) (setf cn t))
              ((string= (cdr evt) (string label))
               (ecase (car evt) (:break (setf lb-br t)) (:continue (setf lb-cn t))))
              ((cdr *break/cont*) (push evt (second *break/cont*)))))
      (values translated br cn lb-br lb-cn))))

(defmacro with-label (var &body body)
  `(let ((,var (and *label-name* (->usersym *label-name*)))
         (*label-name* nil))
     ,@body))

(defun translate-for (init cond step body)
  (with-label label
    (multiple-value-bind (body br cn lb-br lb-cn) (translate/break-continue label body)
      (declare (ignore br lb-br))
      `(block ,label
         (tagbody
            ,@(translate@ init)
          loop-start
            (unless ,(if cond
                         (to-boolean-typed (translate cond) (ast-type cond))
                         t)
              (go loop-end))
            ,@(and body (list body))
          ,@(and lb-cn (list label))
          ,@(and cn '(loop-continue))
            ,@(translate@ step)
            (go loop-start)
          loop-end)))))

(deftranslate (:for init cond step body)
  (translate-for init cond step body))

(deftranslate (:while cond body)
  (translate-for nil cond nil body))

(deftranslate (:do cond body)
  (with-label label
    (multiple-value-bind (body br cn lb-br lb-cn) (translate/break-continue label body)
      (declare (ignore lb-br))
      `(block ,label
         (tagbody
          loop-start
          ,@(and label (list label))
            ,@(and body (list body))
          ,@(and lb-cn (list label))
          ,@(and cn '(loop-continue))
            (when ,(to-boolean-typed (translate cond) (ast-type cond))
              (go loop-start))
          ,@(and br '(loop-end)))))))

(deftranslate (:break label)
  (push (if label (cons :break label) :break) (car *break/cont*))
  (if label
      `(return-from ,(->usersym label))
      `(go loop-end)))

(deftranslate (:continue label)
  (push (if label (cons :continue label) :continue) (car *break/cont*))
  `(go ,(if label (->usersym label) 'loop-continue)))

(deftranslate (:for-in var name obj body)
  (declare (ignore var))
  (with-label label
    (multiple-value-bind (body br cn lb-br lb-cn) (translate/break-continue label body)
      (declare (ignore br lb-br cn))
      (let ((props (gensym)))
        `(block ,label
           (let ((,props (list-props ,(translate obj))))
             (tagbody
              loop-continue
              ,@(and lb-cn (list label))
                ,(set-in-scope name `(or (pop ,props) (go loop-end)))
                ,body
                (go loop-continue)
              loop-end)))))))

(flet ((expand-if (test then else)
         `(if ,(to-boolean-typed (translate test) (ast-type test))
              ,(translate then) ,(translate else))))
  (deftranslate (:if test then else)
    (expand-if test then else))
  (deftranslate (:conditional test then else)
    (expand-if test then else)))

(deftranslate (:try body catch finally)
  `(,(if finally 'unwind-protect 'prog1)
     ,(if catch
          (with-scope (make-simple-scope :vars (list (->usersym (car catch))))
            (let ((var (->usersym (car catch))))
              `(handler-case ,(translate body)
                 (error (,var)
                   (setf ,var (if (typep ,var 'js-condition) (js-condition-value ,var) (princ-to-string ,var)))
                   ,(translate (cdr catch))))))
          (translate body))
     ,@(and finally (list (translate finally)))))

(deftranslate (:throw expr)
  `(error 'js-condition :value ,(translate expr)))

(deftranslate (:name name)
  (lookup-var name))

(deftranslate (:with obj body)
  (let ((obj-var (gensym "with")))
    `(let ((,obj-var ,(translate obj)))
       (declare (ignorable ,obj-var))
       ,(with-scope (make-with-scope :var obj-var) (translate body)))))

(defun find-locals (body &optional others)
  (let ((found (make-hash-table :test 'equal)))
    (labels ((add (name)
               (setf (gethash name found) t))
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
      (loop :for name :being :the :hash-key :of found
            :collect name :into all
            :unless (member name others :test #'string=) :collect name :into internal
            :finally (return (values all internal))))))

(defun references-arguments (body)
  (labels ((scan (expr)
             ;; Don't enter inner functions
             (when (and (consp expr) (not (member (car expr) '(:function :defun))))
               (when (and (eq (car expr) :name) (string= (second expr) "arguments"))
                 (return-from references-arguments t))
               (mapc #'scan expr))))
    (scan body)
    nil))
(defun ast-is-eval-var (ast)
  (and (eq (car ast) :name) (equal (second ast) "eval")))
(defun uses-lexical-eval (body)
  (labels ((scan (expr)
             (when (and (consp expr) (not (member (car expr) '(:function :defun)))) ;; Don't enter inner functions
               (when (and (eq (car expr) :call) (ast-is-eval-var (second expr)))
                 (return-from uses-lexical-eval t))
               (mapc #'scan expr))))
    (scan body)
    nil))

(defun split-out-defuns (forms)
  (loop :for form :in forms
        :when (eq (car form) :defun) :collect form :into defuns
        :else :collect form :into other
        :finally (return (values defuns other))))
(defun lift-defuns (forms)
  (multiple-value-call #'append (split-out-defuns forms)))

(defun translate-function (name args body)
  (let* ((uses-eval (uses-lexical-eval body))
         (uses-args (or uses-eval (references-arguments body)))
         (eval-scope (gensym "eval-scope"))
         (base-locals (cons "this" args))
         (fname (and uses-args (or name (gensym)))))
    (when name (push name base-locals))
    (when uses-args (push "arguments" base-locals))
    (multiple-value-bind (locals internal) (find-locals body base-locals)
      (setf locals (mapcar '->usersym locals) internal (mapcar '->usersym internal))
      (with-scope (if uses-args
                      (make-arguments-scope :vars locals :args (mapcar '->usersym args))
                      (make-simple-scope :vars locals))
        (when uses-eval
          (push (make-with-scope :var eval-scope) *scope*))
        (let ((funcval
               `(make-fobj
                 (find-cls :function)
                 ,(let ((body1 `((let* (,@(loop :for var :in internal :collect `(,var :undefined))
                                        ;; TODO sane object init
                                        ,@(and uses-eval `((,eval-scope (make-obj (find-cls :object)))
                                                           (eval-env ,(capture-scope)))))
                                   (declare (ignorable ,@internal ,@(and uses-eval (list eval-scope))))
                                   ,@(mapcar 'translate (lift-defuns body))
                                   :undefined))))
                       (if uses-args
                           (wrap-function/arguments body1 fname)
                           (wrap-function args body1)))
                 nil)))
          (if (or name fname)
              (let ((n (->usersym (or name fname))))
                `(let (,n)
                   (declare (ignorable ,n))
                   (setf ,n ,funcval)))
              funcval))))))

(defun wrap-function (args body)
  `(lambda (js-user::|this|
            &optional ,@(loop :for arg :in args :collect
                           `(,(->usersym arg) :undefined))
            &rest extra-args)
     (declare (ignore extra-args)
              (ignorable js-user::|this| ,@(mapcar '->usersym args)))
     (block function ,@body)))

(defun wrap-function/arguments (body fname)
  (let ((argument-list (gensym "arguments")))
    `(lambda (js-user::|this| &rest ,argument-list)
       (declare (ignorable js-user::|this|))
       (let* ((js-user::|arguments|
                ;; TODO reuse class
                (make-argobj (find-cls :arguments) (coerce ,argument-list 'vector) ,fname)))
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
  `(js-new ,(translate func) ,@(mapcar 'translate args)))

(deftranslate (:call func args)
  (cond ((ast-is-eval-var func)
         `(lexical-eval ,(translate (or (car args) :undefined))
                        ,(if (in-function-scope-p) 'eval-env (capture-scope))))
        ((member (car func) '(:sub :dot))
         (let ((obj (gensym)))
           `(let ((,obj ,(translate (second func))))
              (funcall (the function (proc ,(case (car func)
                                                  (:dot (expand-cached-lookup obj (third func)))
                                                  (:sub `(lookup ,obj ,(translate (third func)))))))
                       ,obj
                       ,@(mapcar 'translate args)))))
        (t `(funcall (the function (proc ,(translate func))) *global* ,@(mapcar 'translate args)))))

(defun translate-assign (place val)
  (case (car place)
    ((:name) (set-in-scope (second place) val))
    ((:dot) (expand-cached-set (translate (second place)) (third place) val))
    (t `(setf ,(translate place) ,val))))

;; TODO cache path-to-place
(deftranslate (:assign op place val)
  (translate-assign place (translate (if (eq op t) val (list :binary op place val)))))

(deftranslate (:num num)
  (if (integerp num) num (coerce num 'double-float)))

(deftranslate (:string str) str)

(deftranslate (:array elems)
  (let ((arr (gensym)))
    `(let ((,arr (make-array ,(length elems) :fill-pointer ,(length elems) :adjustable t)))
       ,@(loop :for elt :in elems :for pos :from 0 :collect
            `(setf (aref ,arr ,pos) ,(translate elt)))
       (build-array ,arr))))

(deftranslate (:stat form)
  (translate form))

(deftranslate (:block forms)
  `(progn ,@(mapcar 'translate forms)))

(deftranslate (:seq form1 result)
  `(prog2 ,(translate form1) ,(translate result)))

(deftranslate (:binary op lhs rhs)
  (let ((lhs1 (translate lhs)) (rhs1 (translate rhs))
        (lht (ast-type lhs)) (rht (ast-type rhs)))
    ;; Hack to join 'a' + 'b' + 'c' into a single concatenate call (if string type is known)
    (flet ((unwrap-conc (expr)
             (if (and (consp expr) (eq (car expr) 'concatenate)) (cddr expr) (list expr))))
      (if (and (eq op :+) (eq lht :string) (eq rht :string))
          `(concatenate 'string ,@(unwrap-conc lhs1) ,@(unwrap-conc rhs1))
          (or (expand op lht rht lhs1 rhs1)
              `(,(js-intern op) ,lhs1 ,rhs1))))))

(deftranslate (:unary-prefix op place)
  (let ((rhs (translate place))
        (type (ast-type place)))
    (case op
      ((:++ :--) (translate-assign
                  place `(,(cond ((not (num-type type)) (js-intern op))
                                 ((eq op :--) '1-) ((eq op :++) '1+)) ,rhs)))
      ((:+ :-) (or (expand op nil type nil rhs)
                   `(,(js-intern op) 0 ,rhs)))
      (t (or (expand op nil type nil rhs)
             `(,(js-intern op) ,rhs))))))

(deftranslate (:unary-postfix op place)
  (let ((ret (gensym)) (type (ast-type place)))
    `(let ((,ret ,(translate place)))
       ,(translate-assign place `(,(cond ((not (num-type type)) (js-intern op))
                                         ((eq op :--) '1-) ((eq op :++) '1+)) ,ret))
       ,ret)))

(defun see (js) (translate-ast (parse-js:parse-js-string js)))
