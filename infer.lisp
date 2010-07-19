;; JavaScript type-inferrer. Takes an AST as parse-js produces them
;; and tags the elements in that AST with types. Code isn't very
;; pretty, and is too messy to prove correct in even the slightest
;; way, so testing will have to show whether it works for all cases.
;;
;; Interface:
;;  infer-types (ast)
;;    Tags the given AST.
;;  ast-type (ast)
;;    Returns the type (if any) of the given AST node.

(in-package :js)

;; A type-cell, an updatable structure that is used to track the
;; widest type of something.
(defstruct tc
  tp ;; Current type of the cell. Any of T, nil, :null, :undefined,
     ;; :object, :number, :integer, :boolean, :string. T means this
     ;; could be any type, nil means no information is available yet.
  rels ;; Relations this tc has to other tcs. Resolved by resolve-tc
  deps) ;; Used internally by resolve-tc when solving cyclic relations
(defun tc (tp &rest rels)
  (make-tc :tp tp :rels rels))

;; Function type, used to deduce return types and argument types in a
;; few simple situations (direct call to function literal, local
;; functions that don't escape). args is a list of tcs, returns a
;; single tc.
(defstruct ft args returns)

;; Takes two types (as in the tp slot of a type cell), and combines
;; them into a single type.
(defun combine-types (tp1 tp2)
  (cond ((not tp1) tp2) ((not tp2) tp1)
        ((eq tp1 tp2) tp1)
        ((and (member tp1 '(:number :integer)) (member tp2 '(:number :integer))) :number)
        (t t)))
;; Add a possible type to a type-cell.
(defun add-type (tc tp)
  (setf (tc-tp tc) (combine-types tp (tc-tp tc))))
;; Make a type-cell depend on another cell. We don't just do (add-type
;; tc1 (tc-tp tc2)), since tc2 might be updated down the line, so the
;; adding happens at resolution time.
(defun link-tc (tc1 tc2)
  (push (list :and tc2) (tc-rels tc1)))

;; Environments are a list of scopes. Scopes are either :with (for a
;; with(x){} scope), or lists of variables. Each variable is a (init
;; delayed-functions "name" tc) list. Any change requires a new
;; environment to be consed up, since they are shared.

;; They are used mostly to track variable initialization. In JS, a
;; variable always starts out undefined, but is usually used only
;; after assignment. Environments are threaded through the infer
;; methods in such a way that each method receives an environment that
;; passed through all code paths that could possibly be executed
;; before it.

;; Delayed functions are function definitions that should be inferred
;; once the variable is actually being used. This is done to prevent
;; the function from being inferred in an environment where all
;; variables are uninitialized. This may cause function bodies to be
;; inferred multiple times, but this is harmless.

;; Called when exiting a scope.
(defun pop-scope (env) (cdr env))

;; Update the mutable fields of a var in an environment, consing as
;; little as possible.
(defun update-env (env var init delayed-function)
  (labels ((iter-scope (scope)
             (let ((found (and (not (eq (car scope) :with)) (find var (car scope)))))
               (if found
                   (cons (iter-var (car scope)) (cdr scope))
                   (cons (car scope) (iter-scope (cdr scope))))))
           (iter-var (list)
             (if (eq (car list) var)
                 (cons (list* init delayed-function (cddr var)) (cdr list))
                 (cons (car list) (iter-var (cdr list))))))
    (iter-scope env)))
;; Mark all visible locals as potentially containing any type.
(defun ruin-env-for-eval (env)
  (let ((vars-seen ()))
    (dolist (scope env)
      (unless (eq scope :with)
        (dolist (var scope)
          (unless (member (third var) vars-seen :test #'string=)
            (push (third var) vars-seen)
            (add-type (var-tc var) t)))))))

;; Lookup a variable definition in the current env. Returns a second
;; value of T if a with scope was passed before finding the variable.
(defun find-in-env (env name)
  (let ((passed-with nil))
    (dolist (scope env)
      (if (eq scope :with)
          (setf passed-with t)
          (dolist (var scope)
            (when (string= (third var) name)
              (return-from find-in-env (values var passed-with))))))))

;; Accessors for environment var lists.
(defun var-tc (var) (fourth var))
(defun var-init (var) (first var))
(defun var-delayed-function (var) (second var))

;; Merge two environments (for example, after an if/else branch). The
;; arguments should have the same shape, with only different values
;; for init and delayed-functions slots
(defun merge-env (env1 env2)
  (when (eq env1 env2) (return-from merge-env env1))
  (loop :for scope1 :in env1 :for scope2 :in env2
        :if (eq scope1 :with) :collect ':with :else :collect
     (loop :for var1 :in scope1 :for var2 :in scope2 :collect
        (if (and (eq (var-init var1) (var-init var2))
                 (equal (var-delayed-function var1) (var-delayed-function var2)))
            var1
            (list* (and (var-init var1) (var-init var2))
                   (or (var-delayed-function var1) (var-delayed-function var2))
                   (cddr var1))))))

;; Assign a type to a local variable. type can be a type cell or a
;; type symbol. Returns a new environment in which the variable will
;; be marked as initialized.
(defun assign (name type env)
  (multiple-value-bind (found passed-with) (find-in-env env name)
    (when found
      (etypecase type
        (tc (link-tc (var-tc found) type))
        (symbol (add-type (var-tc found) type)))
      (let ((initialized (or (and (var-init found) t) (not passed-with))))
        (when (or (not (eq (var-init found) initialized))
                  (var-delayed-function found))
          (setf env (update-env env found initialized ())))))
    env))

;; Compute the various type-cell relations.
(defun compute-rel (type a b)
  (ecase type
    (:and a) ;; used by link-tc
    (:+ ;; the interesting effects of the + operator
     (cond ((or (eq a :string) (eq b :string)) :string)
           ((or (member a '(t :object)) (member b '(t :object))) t)
           ((and (eq a :integer) (eq b :integer)) :integer)
           ((or (not a) (not b)) nil) ;; Will be recomputed later
           (t :number)))
    (:either (combine-types a b))
    (:maybe-int ;; result of other numeric operations
     (cond ((eq b :none) (case a (:integer :integer) ((nil) nil) (t :number)))
           ((and (eq a :integer) (eq b :integer)) :integer)
           ((or (not a) (not b)) nil)
           (t :number)))))

;; 'Resolve' a type-cell, computing its final type from its current
;; type and its relations. Done in a second pass after inference has
;; completed.
(defun resolve-tc (tc)
  ;; Solves a system of dependencies by iterating until no more
  ;; changes are produced. Since types always become looser, never
  ;; stricter, this terminates.
  (labels ((apply-rel (tc rel)
             (let* ((result (compute-rel (car rel) (resolve-tc (second rel))
                                         (if (third rel) (resolve-tc (third rel)) :none)))
                    (combined (combine-types result (tc-tp tc))))
               (unless (eq combined (tc-tp tc))
                 (setf (tc-tp tc) combined)
                 (loop :for (tc . rel) :in (tc-deps tc) :do
                    (apply-rel tc rel))))))
    (when (eq (tc-tp tc) t) (return-from resolve-tc t))
    (let ((rels (or (tc-rels tc) (return-from resolve-tc (tc-tp tc)))))
      (setf (tc-rels tc) nil)
      (loop :for rel :in rels :while (not (eq (tc-tp tc) t)) :do
         (push (cons tc rel) (tc-deps (second rel)))
         (unless (or (not (third rel)) (eq (third rel) (second rel)))
           (push (cons tc rel) (tc-deps (third rel))))
         (apply-rel tc rel))
      (tc-tp tc))))

(defstruct typing val) ;; Used in returned ast
(defun resolve-tcs (ast)
  (labels ((resolve (val)
             (typecase val
               (cons (map-into val #'resolve val))
               (tc (make-typing :val (or (resolve-tc val) (error "Unresolved type"))))
               (t val))))
    (resolve ast)))

;; API to the inferrer. Tags the given ast.
(defun infer-types (ast)
  (infer ast ())
  (resolve-tcs ast))

;; Access the type of a tagged AST element.
(defun ast-type (ast)
  (let ((maybe (car (last ast))))
    (and (typing-p maybe) (typing-val maybe))))
(defun num-type (type)
  (or (eq type :number) (eq type :integer)))

;; This is later defined for all possible elements in the AST. Returns
;; (values env [tc] [ft]), where only expressions should return a tc,
;; and only things that return a function that we are trying to
;; statically analyse should return an ft.
(defgeneric apply-infer-rule (ast-tag ast-args env))
(defmacro definfer ((type &rest args) &body body)
  (let ((form-arg (gensym)))
    `(defmethod apply-infer-rule ((,(gensym) (eql ,type)) ,form-arg env)
       (destructuring-bind (,@args &rest ,form-arg) ,form-arg
         (declare (ignore ,form-arg))
         ,@body))))

;; Call the apply-infer-rule method for the ast element, and then tags
;; the ast with the tc for that expression, if a tc was returned.
(defun infer (form env &optional context)
  (multiple-value-bind (env tc ftype) (apply-infer-rule (car form) (cdr form) env)
    ;; If an ftype is used in a non-call context, we can no longer
    ;; guarantee how it will be called, so the argtypes have to be
    ;; cleared. This is a bit of a hack---the whole function analysis
    ;; is, in fact, a hack.
    (when (and ftype (not (eq context :call)))
      (dolist (arg (ft-args ftype)) (add-type arg t))
      (setf ftype nil))
    ;; Tag the AST list by adding an extra element (if it doesn't
    ;; exist yet, since sometimes inner functions are inferred
    ;; multiple times).
    (when tc
      (let ((last (last form)))
        (unless (tc-p (car last))
          (setf (cdr last) (list tc)))))
    (values env tc ftype)))

;; Used to hold the (arg-tcs . return-tc) for the current function,
;; since some of the methods need direct access to those.
(defparameter *function-tcs* nil)

;; See if a function may fall off its end without returning, since
;; that is relevant for the return type we assign to it. This isn't
;; always correct (there are many complicated ways in which a function
;; can guarantee returning), but works for basic cases.
(defun may-fall-off (fbody)
  (labels ((see-body (stats)
             (see (car (last stats))))
           (see (stat)
             (case (car stat)
               ((nil :for :for-in :do :while :stat :break
                     :continue :defun :var :switch) t)
               ((:return :throw) nil)
               (:if (or (not (fourth stat)) (see (third stat)) (see (fourth stat))))
               (:with (see (third stat)))
               (:block (see-body (second stat)))
               (:try (and (or (not (fourth stat)) (see (fourth stat)))
                          (or (not (third stat)) (see (second stat)) (see (third stat))))))))
    (see-body fbody)))

;; Sets up the scope for a function, and infers it in this scope.
(defun infer-func (fname args body env)
  (let ((locals (find-locals body `("this" "arguments" ,@args
                                    ,@(and fname (list fname)))))
        (ret-tc (tc ()))
        (arg-tcs (loop :repeat (length args) :collect (tc ()))))
    (multiple-value-bind (defuns body) (split-out-defuns body)
      (when (may-fall-off body) (add-type ret-tc :undefined))
      (let* ((*function-tcs* (cons arg-tcs ret-tc))
             tmp
             (ft (make-ft :args arg-tcs :returns ret-tc))
             ;; The new scope list
             (sc (loop :for name :in locals :collect
                    (cond ((string= name "this") (list t nil name (tc :object)))
                          ;; defuns get some magic to store them as delayed functions,
                          ;; and to be able to keep their ftype
                          ((setf tmp (find name defuns :key #'second :test #'string=))
                           (list (list nil) (cons :function (cdr tmp)) name (tc :object)))
                          ((equal name fname)
                           (list (list ft) nil name (tc :object)))
                          ;; arguments
                          ((setf tmp (position name args :test #'string=))
                           (list t nil name (nth tmp arg-tcs)))
                          ((string= name "arguments") (list t nil name (tc :object)))
                          (t (list nil nil name (tc ()))))))
             (env (cons sc env)))
        (dolist (stat body) (setf env (infer stat env)))
        (values (pop-scope env) ft)))))

;; Note that definfer automatically adds an env parameter to each of
;; these methods.
(definfer (:function name args body)
  (multiple-value-bind (env ftype) (infer-func name args body env)
    (values env (tc :object) ftype)))
;; This is only invoked for defuns that are not at the top-level of a
;; function. Those are hoisted to the top of the function, and handled
;; more cleverly.
(definfer (:defun name args body)
  (setf env (assign name :object env))
  (multiple-value-bind (env ftype) (infer-func nil args body env)
    (dolist (tc (ft-args ftype)) (add-type tc t))
    env))
(definfer (:atom atom)
  (values env (case atom
                ((:true :false) (tc :boolean))
                (:null (tc :null))
                (t (tc t)))))
(definfer (:object props)
  (loop :for (nil . val) :in props :do (setf env (infer val env)))
  (values env (tc :object)))
(definfer (:regexp)
  (values env (tc :object)))
(definfer (:label name form)
  (declare (ignore name))
  (values (infer form env)))
(definfer (:var bindings)
  (loop :for (name . val) :in bindings :do
     (when val
       (multiple-value-bind (env1 tc) (infer val env)
         (setf env (assign name tc env1)))))
  env)
(definfer (:name name)
  ;; Stuff like arguments[1] = null can mess with argument types, so
  ;; we clear those at the first sign of trouble.
  (when (string= name "arguments")
    (dolist (tc (car *function-tcs*)) (add-type tc t)))
  (multiple-value-bind (var passed-with) (find-in-env env name)
    (if var
        ;; For local variables, stuff gets complicated...
        (let* ((update nil)
               (init (var-init var))
               ;; For locals that hold a function, the init value is a
               ;; cons, and its car is the ft structure for this
               ;; function. Ergh.
               (ftype (and (consp init) (car init))))
          ;; If this is not initialized yet, it is used before
          ;; initialization, and :undefined is added to the type.
          (unless init
            (add-type (var-tc var) :undefined)
            (setf update t))
          ;; When a delayed function is present, infer it in this
          ;; environment. Also, if we don't have an ftype yet, use the
          ;; one returned by this infer call.
          (when (var-delayed-function var)
            (multiple-value-bind (env1 tc ftype1)
                (infer (var-delayed-function var) env :call)
              (declare (ignore tc))
              (setf env env1)
              (unless ftype (setf ftype (setf (car init) ftype1))))
            (setf update t))
          ;; Update the env if anything was changed.
          (values (if update (update-env env var init nil) env)
                  ;; If the lookup passed a with environment, we can't
                  ;; say anything about the type.
                  (if passed-with (tc t) (var-tc var))
                  ftype))
        ;; Global variables are unknowable.
        (values env (tc t)))))
(definfer (:num num)
  (values env (tc (if (typep num 'fixnum) :integer :number))))
(definfer (:toplevel body)
  (dolist (stat body) (setf env (infer stat env)))
  env)
(definfer (:assign op place val)
  (unless (eq (car place) :name) (setf env (infer place env)))
  (multiple-value-bind (env tc)
      (infer (if (eq op t) val `(:binary ,op ,place ,val)) env)
    (when (eq (car place) :name)
      (setf env (assign (second place) tc env)))
    (values env tc)))
(definfer (:stat form)
  (values (infer form env)))
(definfer (:string)
  (values env (tc :string)))
(definfer (:return value)
  (multiple-value-bind (env tc) (if value (infer value env) env)
    (when *function-tcs*
      (if tc
          (link-tc (cdr *function-tcs*) tc)
          (add-type (cdr *function-tcs*) :undefined)))
    env))
(definfer (:for init cond step body)
  (when init (setf env (infer init env)))
  (let ((env2 env))
    (when cond (setf env (infer cond env)))
    (when step (setf env (infer step env)))
    (merge-env env2 (infer body env))))
(definfer (:while cond body)
  (merge-env env (infer body (infer cond env))))
(definfer (:do cond body)
  (merge-env env (infer cond (infer body env))))
(definfer (:for-in var name obj body)
  (declare (ignore var))
  (setf env (infer obj env))
  (merge-env env (infer body (assign name :string env))))
(definfer (:switch expr body)
  (setf env (infer expr env))
  (dolist (stat body) (setf env (infer stat env)))
  env)
(definfer (:default)
  env)
(definfer (:case expr)
  (infer expr env))
(definfer (:if test then else)
  (setf env (infer test env))
  (merge-env (infer then env) (if else (infer else env) env)))
(definfer (:conditional test then else)
  (setf env (infer test env))
  (multiple-value-bind (env1 tc1) (infer then env)
    (multiple-value-bind (env2 tc2) (infer else env)
      (let ((tc (tc ())))
        (link-tc tc tc1) (link-tc tc tc2)
        (values (merge-env env1 env2) tc)))))
(definfer (:try body catch finally)
  (let ((catch-env (if catch
                       (let ((env (cons `((t nil ,(car catch) ,(tc t))) env)))
                         (pop-scope (infer (cdr catch) env)))
                       env)))
    (setf env (merge-env (infer body env) catch-env))
    (if finally (infer finally env) env)))
(definfer (:throw expr)
  (values (infer expr env)))
(definfer (:with obj body)
  (pop-scope (infer body (infer obj (cons :with env)))))
(definfer (:new func args)
  (setf env (infer func env))
  (dolist (arg args)
    (setf env (infer arg env)))
  (values env (tc :object)))
(definfer (:call func args)
  (if (ast-is-eval-var func)
      ;; Eval might screw up every visible local variable.
      (progn (ruin-env-for-eval env) (values env (tc t)))
      (multiple-value-bind (env ftc ftype) (infer func env :call)
        (declare (ignore ftc))
        ;; Infer the arguments, linking the arg type-cells for this
        ;; function (if any) to the results.
        (loop :for arg-tc := (and ftype (ft-args ftype)) :then (cdr arg-tc)
              :for arg :in args :do
           (multiple-value-bind (env1 tc) (infer arg env)
             (setf env env1)
             (when arg-tc (link-tc (car arg-tc) tc)))
           :finally (dolist (tc arg-tc) (add-type tc :undefined)))
        (values env (if ftype (ft-returns ftype) (tc t))))))
(definfer (:binary op lhs rhs)
  (multiple-value-bind (env lhst) (infer lhs env)
    (multiple-value-bind (env rhst) (infer rhs env)
      (values env
        (case op
          (:+ (tc nil `(:+ ,lhst ,rhst)))
          ((:== :=== :!= :!== :instanceof :in :< :> :<= :>=) (tc :boolean))
          ((:^ :& :|\|| :>> :<< :>>>) (tc :integer))
          ((:&& :|\|\||) (tc nil `(:either ,lhst ,rhst)))
          ((:- :* :%) (tc nil `(:maybe-int ,lhst ,rhst)))
          (:/ (tc :number)))))))
(definfer (:unary-prefix op place)
  (multiple-value-bind (env argt) (infer place env)
    (ecase op
      (:typeof (values env (tc :string)))
      (:void (values env (tc :undefined)))
      ((:delete :!) (values env (tc :boolean)))
      ((:-- :++) (let ((tc (tc nil `(:maybe-int ,argt))))
                   (when (eq (car place) :name)
                     (setf env (assign (second place) tc env)))
                   (values env tc)))
      ((:- :+) (values env (tc nil `(:maybe-int ,argt))))
      (:~ (values env (tc :integer))))))
(definfer (:unary-postfix op place)
  (declare (ignore op)) ;; always :++ or :--
  (multiple-value-bind (env argt) (infer place env)
    (let ((tc (tc nil `(:maybe-int ,argt))))
      (when (eq (car place) :name)
        (setf env (assign (second place) tc env)))
      (values env tc))))
(definfer (:array elems)
  (dolist (elem elems) (setf env (infer elem env)))
  (values env (tc :object)))
(definfer (:block forms)
  (dolist (stat forms) (setf env (infer stat env)))
  env)
(definfer (:seq form1 result)
  (infer result (infer form1 env)))
(definfer (:dot obj)
  (values (infer obj env) (tc t)))
(definfer (:sub obj attr)
  (values (infer attr (infer obj env)) (tc t)))
(definfer (:break)
  env)
(definfer (:continue)
  env)
