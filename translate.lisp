(in-package :cl-js)

(defvar *scope* ())
(defparameter *label-name* nil)
(defvar *break* ())
(defvar *continue* ())
(defvar *symbol-table*)

(defun as-sym (name)
  (or (gethash name *symbol-table*)
      (setf (gethash name *symbol-table*) (make-symbol name))))

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
       (values ,found ,(with-scope-var scope)))))
(defmethod set-variable (name valname (scope with-scope) rest) ;; TODO hasOwnProperty?
  `(if-not-found (nil ,(expand-cached-lookup (with-scope-var scope) name))
     ,(set-variable name valname (car rest) (cdr rest))
     ,(expand-cached-set (with-scope-var scope) name valname)))

(defstruct simple-scope vars)
(defmethod lookup-variable (name (scope simple-scope) rest)
  (let ((sym (as-sym name)))
    (if (member sym (simple-scope-vars scope))
        sym
        (lookup-variable name (car rest) (cdr rest)))))
(defmethod set-variable (name valname (scope simple-scope) rest)
  (let ((sym (as-sym name)))
    (if (member sym (simple-scope-vars scope))
        `(setf ,sym ,valname)
        (set-variable name valname (car rest) (cdr rest)))))

(defstruct (arguments-scope (:include simple-scope)) args)
(defmethod lookup-variable (name (scope arguments-scope) rest)
  (declare (ignore rest))
  (if (member (as-sym name) (arguments-scope-args scope))
      `(car ,(as-sym name))
      (call-next-method)))
(defmethod set-variable (name valname (scope arguments-scope) rest)
  (declare (ignore rest))
  (if (member (as-sym name) (arguments-scope-args scope))
      `(setf (car ,(as-sym name)) ,valname)
      (call-next-method)))

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
         (when (eq locals :null) (setf locals (mapcar 'symbol-name (simple-scope-vars level)))))
        (with-scope (push (with-scope-var level) objs))
        (captured-scope (setf next level))))
    `(make-captured-scope
      :vars (list ,@(loop :for var :in varnames :for name := (symbol-name var) :collect
                       `(list ',name (lambda () ,(lookup-var name))
                              (lambda (,val-arg) ,(set-in-scope name val-arg)))))
      :local-vars ',locals :objs (list ,@(nreverse objs)) :next ,next)))
(defun lookup-in-captured-scope (name scope)
  (let ((var (assoc name (captured-scope-vars scope) :test #'string=)))
    (if var
        (funcall (second var))
        (loop :for obj :in (captured-scope-objs scope) :do
           (if-not-found (val (js-prop obj name))
             nil
             (return val))
           :finally (return (if (captured-scope-next scope)
                                (lookup-in-captured-scope name (captured-scope-next scope))
                                (global-lookup name)))))))
(defmethod lookup-variable (name (scope captured-scope) rest)
  (declare (ignore rest))
  `(lookup-in-captured-scope ,name ,scope))
(defun set-in-captured-scope (name value scope)
  (let ((var (assoc name (captured-scope-vars scope) :test #'string=)))
    (if var
        (funcall (third var) value)
        (loop :for obj :in (captured-scope-objs scope) :do
           (if-not-found (nil (js-prop obj name))
             nil
             (return (setf (js-prop obj name) value)))
           :finally (if (captured-scope-next scope)
                        (set-in-captured-scope name value (captured-scope-next scope))
                        (setf (js-prop *env* name) value))))))
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

(defun in-function-scope-p () ;; TODO do not count catch scopes
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
  (let ((*symbol-table* (make-hash-table :test 'equal)))
    (translate (infer-types ast))))

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
  `(js-prop ,(translate obj) ,(translate attr)))

(deftranslate (:var bindings)
  `(progn ,@(loop :for (name . val) :in bindings
                  :when val :collect (set-in-scope name (translate val))
                  :else :if (not *scope*) :collect `(if-not-found (nil (js-prop *env* ,name))
                                                      (setf (js-prop *env* ,name) :undefined)))))

(deftranslate (:object properties)
  (expand-static-obj '(find-proto :object) (loop :for (name . val) :in properties :collect
                                              (cons (to-string name) (translate val)))))

(deftranslate (:regexp expr flags)
  `(load-time-value (new-regexp ,expr ,flags)))

(defmacro extend-label (var (name &rest expr) &body body)
  `(let ((,var (cons (cons ,name (lambda () ,@expr)) ,var))) ,@body))

(deftranslate (:label name form)
  (if (member (car form) '(:for :for-in :switch :do :while))
      ;; These handle their own label
      (let ((*label-name* name)) (translate form))
      (let ((block (gensym)))
        (extend-label *break* (name `(return-from ,block :undefined))
          `(block ,block ,(translate form))))))

;; Used in ,@
(defun translate@ (form)
  (and form (list (translate form))))

(defmacro with-label (var &body body)
  `(let ((,var *label-name*)
         (*label-name* nil))
     ,@body))

(defun translate-for (init cond step body)
  (with-label label
    (let ((continued nil)
          (break-block (gensym))
          translated-body)
      (extend-label *break* (label `(return-from ,break-block :undefined))
        (extend-label *continue* (label `(go ,(setf continued (or continued (gensym)))))
          (setf translated-body (translate body))))
      `(block ,break-block
         (tagbody
            (progn ,@(translate@ init))
          loop-start
            (unless ,(if cond (to-boolean-typed (translate cond) (ast-type cond)) t)
              (go loop-end))
            (progn ,@(and translated-body (list translated-body)))
          ,@(and continued (list continued))
            (progn ,@(translate@ step))
            (go loop-start)
          loop-end)))))

(deftranslate (:for init cond step body)
  (translate-for init cond step body))

(deftranslate (:while cond body)
  (translate-for nil cond nil body))

(deftranslate (:do cond body)
  (with-label label
    (let ((continued nil)
          (break-block (gensym))
          translated-body)
      (extend-label *break* (label `(return-from ,break-block :undefined))
        (extend-label *continue* (label `(go ,(setf continued (or continued (gensym)))))
          (setf translated-body (translate body))))
      `(block ,break-block
         (tagbody
          loop-start
            (progn ,@(and translated-body (list translated-body)))
          ,@(and continued (list continued))
            (when ,(to-boolean-typed (translate cond) (ast-type cond))
              (go loop-start)))))))

(deftranslate (:break label)
  (loop :for (lbl . thunk) :in *break* :do
     (when (or (not label) (equal label lbl))
       (return (funcall thunk)))
     ;; These should be caught by parser. This is just a sanity check.
     :finally (error "Break without matching context.")))

(deftranslate (:continue label)
  (loop :for (lbl . thunk) :in *continue* :do
     (when (or (not label) (equal label lbl))
       (return (funcall thunk)))
     :finally (error "Continue without matching context.")))

(deftranslate (:for-in var name obj body)
  (declare (ignore var))
  (with-label label
    (let ((continued nil)
          (break-block (gensym))
          translated-body
          (prop (gensym)))
      (extend-label *break* (label `(return-from ,break-block :undefined))
        (extend-label *continue* (label `(go ,(setf continued (or continued (gensym)))))
          (setf translated-body (translate body))))
      `(block ,break-block
         (js-for-in ,(translate obj)
                    (lambda (,prop)
                      ,(set-in-scope name prop)
                      ,(if continued
                           `(tagbody (progn ,translated-body) ,continued)
                           translated-body)))))))

(deftranslate (:switch val cases)
  (with-label label
    (let ((break-block (gensym))
          (val-sym (gensym))
          (default-case nil)
          blocks)
      (extend-label *break* (label `(return-from ,break-block :undefined))
        (setf blocks
              (loop :for ((val . body) . rest) :on cases
                    :for data := (list val (gensym) (mapcar 'translate body)) :collect data
                    :do (unless val (setf default-case data))
                    :when (and (not rest) (not default-case))
                    :collect (setf default-case (list nil (gensym) nil)))))
      `(let ((,val-sym ,(translate val)))
         (block ,break-block
           (tagbody
              (cond ,@(loop :for (case label) :in blocks :when case :collect
                         `((js=== ,val-sym ,(translate case)) (go ,label)))
                    (t (go ,(second default-case))))
              ,@(loop :for (nil label statements) :in blocks :append
                   (cons label statements))))))))
          
(deftranslate (:case)
  (js-error :syntax-error "Misplaced case label."))
(deftranslate (:default)
  (js-error :syntax-error "Misplaced default label."))

(flet ((expand-if (test then else)
         `(if ,(to-boolean-typed (translate test) (ast-type test))
              ,(translate then) ,(translate else))))
  (deftranslate (:if test then else)
    (expand-if test then else))
  (deftranslate (:conditional test then else)
    (expand-if test then else)))

(deftranslate (:try body catch finally)
  (let ((body (translate body)))
    `(,(if finally 'unwind-protect 'prog1)
       ,(if catch
            (with-scope (make-simple-scope :vars (list (as-sym (car catch))))
              (let ((var (as-sym (car catch))))
                `(handler-case ,body
                   (js-condition (,var)
                     (setf ,var (js-condition-value ,var))
                     ,(translate (cdr catch))))))
            body)
       ,@(and finally (list (translate finally))))))

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
         (fname (and uses-args (or name (symbol-name (gensym))))))
    (when name (push name base-locals))
    (when uses-args (push "arguments" base-locals))
    (multiple-value-bind (locals internal) (find-locals body base-locals)
      (setf locals (mapcar 'as-sym locals) internal (mapcar 'as-sym internal))
      (with-scope (if uses-args
                      (make-arguments-scope :vars locals :args (mapcar 'as-sym args))
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
                           (wrap-function/arguments args body1 (as-sym fname))
                           (wrap-function args body1)))
                 nil)))
          (if (or name fname)
              (let ((n (as-sym (or name fname))))
                `(let (,n)
                   (declare (ignorable ,n))
                   (setf ,n ,funcval)))
              funcval))))))

(defun wrap-function (args body)
  `(lambda (,(as-sym "this")
            &optional ,@(loop :for arg :in args :collect
                           `(,(as-sym arg) :undefined))
            &rest extra-args)
     (declare (ignore extra-args)
              (ignorable ,(as-sym "this") ,@(mapcar 'as-sym args)))
     (block function ,@body)))

(defun wrap-function/arguments (args body fname)
  (let ((argument-list (gensym "arguments"))
        (arg-names (mapcar #'as-sym args)))
    `(lambda (,(as-sym "this") &rest ,argument-list)
       (declare (ignorable ,(as-sym "this")))
       ;; Make sure the argument list covers at least the named args
       (let ((,(as-sym "arguments")
               (make-argobj (find-cls :arguments) ,argument-list (length ,argument-list) ,fname)))
         ,@(when args
             `((if ,argument-list
                   (loop :for cons :on ,argument-list :repeat ,(length args) :do
                      (unless (cdr cons) (setf (cdr cons) (list :undefined))))
                   (setf ,argument-list (make-list ,(length args) :initial-element :undefined)))))
         (let ,(loop :for arg :in arg-names :collect `(,arg (prog1 ,argument-list (pop ,argument-list))))
           (declare (ignorable ,(as-sym "arguments") ,@arg-names))
           (block function ,@body))))))

(deftranslate (:return value)
  (unless (in-function-scope-p)
    (js-error :syntax-error "Return outside of function."))
  `(return-from function (values ,(if value (translate value) :undefined))))

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
         (let ((obj (gensym)) (mth (gensym)))
           `(let* ((,obj ,(translate (second func)))
                   (,mth ,(case (car func)
                            (:dot (expand-cached-lookup obj (third func)))
                            (:sub `(js-prop ,obj ,(translate (third func)))))))
              (if (fobj-p ,mth)
                  (funcall (the function (fobj-proc ,mth)) ,obj ,@(mapcar 'translate args))
                  ,(case (car func)
                     (:dot `(js-error :type-error "Can not call method ~a in ~a." ,(third func) (to-string ,obj)))
                     (:sub `(js-error :type-error "Invalid method call on ~a." (to-string ,obj))))))))
        ((and (eq (car func) :name) (some (lambda (e) (typep e 'with-scope)) *scope*))
         (let ((fval (gensym)) (objval (gensym)))
           `(multiple-value-bind (,fval ,objval) ,(translate func)
              (funcall (the function (proc ,fval)) (or ,objval *env*) ,@(mapcar 'translate args)))))
        (t `(funcall (the function (proc ,(translate func))) *env* ,@(mapcar 'translate args)))))

(defun translate-assign (place val)
  (case (car place)
    ((:name) (set-in-scope (second place) val))
    ((:dot) (expand-cached-set (translate (second place)) (third place) val))
    (t `(setf ,(translate place) ,val))))

;; TODO cache path-to-place
(deftranslate (:assign op place val)
  (translate-assign place (translate (if (eq op t) val (list :binary op place val)))))

(deftranslate (:num num)
  (etypecase num
    (keyword (ecase num (:infinity (infinity)) (:-infinity (-infinity))))
    (number num)))

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

(deftranslate (:unary-prefix op rhs)
  (let ((type (ast-type rhs)))
    (case op
      ((:++ :--) (translate-assign
                  rhs `(,(cond ((not (num-type type)) (js-intern op))
                               ((eq op :--) '1-) ((eq op :++) '1+)) ,(translate rhs))))
      ((:+ :-) (or (expand op nil type nil (translate rhs))
                   `(,(js-intern op) 0 ,(translate rhs))))
      (:delete (if (member (car rhs) '(:sub :dot))
                   `(delete-prop ,(translate (second rhs))
                                 ,(ecase (car rhs) (:dot (third rhs)) (:sub (translate (third rhs)))))
                   `(progn ,(translate rhs) t)))
      (:typeof (if (eq (car rhs) :name)
                   `(handler-case (js-type-of ,(translate rhs))
                      (undefined-variable () "undefined"))
                   `(js-type-of ,(translate rhs))))
      (t (or (expand op nil type nil (translate rhs))
             `(,(js-intern op) ,(translate rhs)))))))

(deftranslate (:unary-postfix op place)
  (let ((ret (gensym)) (type (ast-type place)))
    `(let ((,ret ,(translate place)))
       ,(translate-assign place `(,(cond ((not (num-type type)) (js-intern op))
                                         ((eq op :--) '1-) ((eq op :++) '1+)) ,ret))
       ,ret)))

(defun see (js) (translate-ast (parse-js-string js)))
