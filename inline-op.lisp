(in-package :js)

(defgeneric expand-op (op lht rht lhs rhs)
  (:method (op lht rht lhs rhs)
    (declare (ignore op lht rht lhs rhs))
    nil))

(defmacro defexpand (op (lht rht) &body body)
  (flet ((spec (type)
           (if (eq type t) t `(eql ,type))))
    `(defmethod expand-op ((,(gensym) (eql ,op)) (lht ,(spec lht))
                           (rht ,(spec rht)) lhs rhs)
       (declare (ignorable lhs rhs))
       ,@body)))

(defun expand (op lht rht lhs rhs)
  (let ((result (expand-op op lht rht lhs rhs)))
    (when (and (not result) (eq lht :integer))
      (setf result (expand-op op :number rht lhs rhs)))
    (when (and (not result) (eq rht :integer))
      (setf result (expand-op op (if (eq lht :integer) :number lht) :number lhs rhs)))
    result))

(defun to-boolean-typed (expr type)
  (case type
    (:boolean expr)
    ((:undefined :null) `(progn ,expr nil))
    (:object `(progn ,expr t))
    (:integer `(not (= ,expr 0)))
    (:number (let ((tmp (gensym))) `(let ((,tmp ,expr)) (not (or (= ,tmp 0) (is-nan ,tmp))))))
    (t `(to-boolean ,expr))))

(defmacro defnumop (op expansion)
  `(progn (defexpand ,op (:integer :integer) ,expansion)
          (defexpand ,op (:number :number) (unless *float-traps* ,expansion))))

;; (string + string is handled specially in the :binary translate rule)
(defnumop :+ `(+ ,lhs ,rhs))
(defexpand :+ (nil :number) rhs)
(defexpand :+ (t :number)
  (unless *float-traps*
    (let ((lh (gensym)) (rh (gensym)))
      `(let ((,lh ,lhs) (,rh ,rhs))
         (typecase ,lh
           (fixnum (+ (the fixnum ,lh) ,rh))
           (double-float (+ (the double-float ,lh) ,rh))
           (t (!+ ,lh ,rh)))))))
(defexpand :+ (:number t)
  (unless *float-traps*
    (let ((lh (gensym)) (rh (gensym)))
      `(let ((,lh ,lhs) (,rh ,rhs))
         (typecase ,rh
           (fixnum (+ ,lh (the fixnum ,rh)))
           (double-float (+ ,lh (the double-float ,rh)))
           (t (!+ ,lh ,rh)))))))

(defnumop :- `(- ,lhs ,rhs))
(defexpand :- (nil :integer) `(- ,rhs))
(defexpand :- (nil :number) (unless *float-traps* `(- ,rhs)))

(defnumop :* `(* ,lhs ,rhs))
(defnumop :% `(mod ,lhs ,rhs))

(defnumop :< `(< ,lhs ,rhs))
(defnumop :> `(> ,lhs ,rhs))
(defnumop :<= `(<= ,lhs ,rhs))
(defnumop :>= `(>= ,lhs ,rhs))
(defnumop :== `(= ,lhs ,rhs))
(defnumop :!= `(/= ,lhs ,rhs))
(defnumop :=== `(< ,lhs ,rhs))
(defnumop :!== `(< ,lhs ,rhs))

(defexpand :& (:integer :integer) `(logand ,lhs ,rhs))
(defexpand :|\|| (:integer :integer) `(logior ,lhs ,rhs))
(defexpand :^ (:integer :integer) `(logxor ,lhs ,rhs))
(defexpand :~ (nil :integer) `(lognot ,rhs))
(defexpand :>> (:integer :integer) `(ash ,lhs (- ,rhs)))
(defexpand :<< (:integer :integer) `(ash ,lhs ,rhs))
(defexpand :>>> (:integer :integer) `(ash ,lhs (- ,rhs))) ;; TODO not correct!

(defexpand :&& (t t)
  (let ((temp (gensym)))
    `(let ((,temp ,lhs))
       (if ,(to-boolean-typed temp lht) ,rhs ,temp))))
(defexpand :|\|\|| (t t)
  (let ((temp (gensym)))
    `(let ((,temp ,lhs))
       (if ,(to-boolean-typed temp lht) ,temp ,rhs))))
(defexpand :! (t t) `(not ,(to-boolean-typed rhs rht)))

(defexpand :void (t t)
  `(progn ,rhs :undefined))
