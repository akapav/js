(in-package :cl-js)

(defstruct lib
  prototypes
  toplevel)

(defstruct objspec
  (prototype :object)
  props)

(defstruct (funcspec (:include objspec))
  call
  proto-spec
  make-new)

(defvar *lib*)
(defvar *objspec*)
(defparameter *default-slot-flags* +slot-dflt+)

(defun slot-flags (&rest flags)
  (let ((val *default-slot-flags*))
    (macrolet ((add (flag) `(setf val (logior val ,flag)))
               (rm (flag) `(setf val (logand val (lognot ,flag)))))
      (dolist (flag flags)
        (case flag
          (:enum (rm +slot-noenum+)) (:noenum (add +slot-noenum+))
          (:ro (add +slot-ro+)) (:rw (rm +slot-ro+))
          (:del (rm +slot-nodel+)) (:nodel (add +slot-nodel+))
          (active (add +slot-active+)))))
    val))

(defun check-spec (spec &rest allowed)
  (loop :for elt :in spec :do
     (if (and (consp elt) (keywordp (car elt)))
         (unless (member (car elt) allowed)
           (error "No ~a specs allowed in this form." (car elt)))
         (unless (member t allowed)
           (error "No body (non-keyword-list element) allowed in this form.")))))

(defmacro with-default-slot-flags ((&rest flags) &body body)
  `(let ((*default-slot-flags* (slot-flags ,@flags))) ,@body))

(defun spec-val (spec type &optional default)
  (let ((found (find type spec :test #'eq :key (lambda (sp) (and (consp sp) (car sp))))))
    (if found (second found) default)))
(defun spec-list (spec type &optional default)
  (let ((any nil))
    (loop :for part :in spec
          :when (and (consp part) (eq (car part) type))
          :do (setf any t) :and :append (cdr part)
       :finally (unless any (return default)))))
(defun spec-body (spec)
  (loop :for part :in spec
        :unless (and (consp part) (keywordp (car part))) :collect part))

(defun add-prop (name val &optional (flags +slot-dflt+))
  (let* ((props (objspec-props *objspec*))
         (found (assoc name props :test #'string=)))
    (cond (found (setf (cdr found) (cons val flags)))
          (props (setf (cdr (last props)) (list (list* name val flags))))
          (t (setf (objspec-props *objspec*) (list (list* name val flags)))))))
              
(defun add-prototype (tag spec)
  (let* ((protos (lib-prototypes *lib*))
         (found (assoc tag protos :test #'eq)))
    (cond (found (setf (cdr found) spec))
          (protos (setf (cdr (last protos)) (list (cons tag spec))))
          (t (setf (lib-prototypes *lib*) (list (cons tag spec)))))))

(defun empty-lib ()
  (make-lib :toplevel (make-objspec :prototype :object)))

(defmacro add-to-lib (lib &body body)
  `(let* ((*lib* ,lib)
          (*objspec* (lib-toplevel *lib*)))
     ,@body
     (values)))

(defun default-constructor-name (structname)
  (intern (format nil "%make-new-~a-~a" (symbol-name structname) (package-name (symbol-package structname))) :cl-js))

(defmacro define-js-obj (name &body slots)
  (multiple-value-bind (name opts)
      (if (consp name) (values (car name) (cdr name)) (values name ()))
    `(defstruct (,name (:include obj) (:constructor ,(default-constructor-name name) (cls)) ,@opts) ,@slots)))

(defparameter *stdlib* (empty-lib))

(defmacro .prototype (tag &body spec)
  (check-spec spec :parent :slot-default t)
  `(let ((*objspec* (make-objspec :prototype ,(spec-val spec :parent :object)))
         (*default-slot-flags* (slot-flags ,@(let ((list (spec-list spec :slot-default)))
                                               (if (member :enum list) list (cons :noenum list))))))
     ,@(spec-body spec)
     (add-prototype ,tag *objspec*)))

(defmacro .constructor (name (&rest args) &body spec)
  (check-spec spec :prototype :slot-default t :properties :slot :make-new :type)
  (let* ((proto (spec-list spec :prototype))
         (proto (if (keywordp (car proto))
                    (car proto)
                    (progn
                      (check-spec proto :slot-default t)
                      `(let ((*objspec* (make-objspec))
                             (*default-slot-flags* (slot-flags ,@(let ((list (spec-list proto :slot-default)))
                                                                   (if (member :enum list) list (cons :noenum list))))))
                         ,@(spec-body proto)
                         *objspec*)))))
    `(add-prop
      ,name
      (let ((*objspec* (make-funcspec :call ,(wrap-js-lambda args (spec-body spec))
                                      :prototype :function
                                      :proto-spec ,proto
                                      :make-new ,(let ((type (spec-val spec :type)))
                                                   (if type
                                                       `',(default-constructor-name type)
                                                       (spec-val spec :make-new)))))
            (*default-slot-flags* (slot-flags ,@(spec-list spec :slot-default '(:enum)))))
        ,@(spec-list spec :properties)
        *objspec*)
      (slot-flags ,@(spec-list spec :slot)))))

(defmacro .object (name &body spec)
  (check-spec spec :parent :slot-default t :slot)
  `(add-prop
    ,name
    (let ((*objspec* (make-objspec :prototype ,(spec-val spec :parent :object)))
          (*default-slot-flags* (slot-flags ,@(spec-list spec :slot-default '(:enum)))))
      ,@(spec-body spec)
      *objspec*)
    (slot-flags ,@(spec-list spec :slot))))

(defmacro .value (name &body spec)
  (check-spec spec :slot t)
  `(add-prop ,name (lambda () ,@(spec-body spec)) (slot-flags ,(spec-list spec :slot))))

(defmacro .func (name (&rest args) &body spec)
  (check-spec spec :slot :slot-default :properties t)
  `(add-prop
    ,name
    (let ((*objspec* (make-funcspec :call ,(wrap-js-lambda args (spec-body spec))
                                    :prototype :function))
          (*default-slot-flags* (slot-flags ,@(spec-list spec :slot-default '(:enum)))))
      ,@(spec-list spec :properties)
      *objspec*)
    (slot-flags ,@(spec-list spec :slot))))

(defmacro .active (name &body spec)
  (check-spec spec :read :write :slot)
  `(add-prop
    ,name
    (cons ,(let ((read (spec-list spec :read)))
             (and read (wrap-js-lambda (car read) (cdr read))))
          ,(let ((write (spec-list spec :write)))
             (and write (wrap-js-lambda (car write) (cdr write)))))
    (slot-flags 'active ,@(spec-list spec :slot))))

(defmacro .active-r (name &body spec)
  (check-spec spec :slot t)
  `(add-prop
    ,name
    (cons ,(wrap-js-lambda () (spec-body spec)) nil)
    (slot-flags 'active ,@(spec-list spec :slot))))

;; Building environments.

(defun init-val (value)
  (typecase value
    (function (funcall value))
    (objspec (init-obj value))
    (t value)))

(defun init-obj (spec &optional fill)
  (when (keywordp spec)
    (return-from init-obj (find-proto spec)))
  (let* ((props (objspec-props spec))
         (is-func (funcspec-p spec))
         (new-proto (when (and is-func (funcspec-proto-spec spec))
                      (let ((proto-obj (init-obj (funcspec-proto-spec spec))))
                        (push (list* "prototype" proto-obj +slot-noenum+) props)
                        proto-obj)))
         (vals (make-array (max 2 (length props))))
         (cls (make-scls (loop :for off :from 0 :for (name value . flags) :in props
                               :do (setf (svref vals off) (init-val value))
                               :collect (list* (intern-prop name) off flags))
                         (and (objspec-prototype spec) (init-obj (objspec-prototype spec))))))
    (cond (fill (setf (obj-vals fill) vals (obj-cls fill) cls))
          ((funcspec-p spec)
           (let ((built (if (funcspec-make-new spec)
                            (make-cfobj cls (funcspec-call spec) new-proto (funcspec-make-new spec) vals)
                            (make-fobj cls (funcspec-call spec) new-proto vals))))
             (when new-proto
               (ensure-slot new-proto "constructor" built +slot-noenum+))
             built))
          (t (make-obj cls vals)))))

(defun create-env (&rest libs)
  (let* ((bootstrap (make-array (length *std-types*) :initial-contents
                                (loop :repeat (length *std-types*) :collect (make-obj nil nil))))
         (objproto (svref bootstrap (type-offset :object)))
         (clss (make-array (length *std-types*) :initial-contents
                           (loop :for id :across *std-types* :for i :from 0 :collect
                              (make-scls () (svref bootstrap i)))))
         (*env* (make-gobj (make-hcls objproto) (make-hash-table :test 'eq) bootstrap clss)))
    (loop :for (id . obj) :in (lib-prototypes *stdlib*) :do
       (let ((pos (position id *std-types*)))
         (when pos
           (init-obj obj (aref bootstrap pos)))))
    (loop :for shell :across bootstrap :for i :from 0 :do
       (unless (obj-cls shell) (error "Missing definition for standard class ~a" (aref *std-types* i))))
    (apply 'add-to-env *env* *stdlib* libs)
    *env*))

(defun add-to-env (*env* &rest libs)
  (dolist (lib libs)
    (loop :for (id . obj) :in (lib-prototypes lib) :do
       (unless (find id *std-types*)
         (let ((proto (init-obj obj)))
           (push (list* id proto (make-scls () proto)) (gobj-proto-list *env*)))))
    (loop :for (name val . flags) :in (objspec-props (lib-toplevel lib)) :do
       (ensure-slot *env* name (init-val val) flags)))
  *env*)

(defmacro with-js-env ((&rest libs) &body body)
  `(let ((*env* (create-env ,@libs))) ,@body))

(defmacro integrate-type (specializer &body spec)
  (check-spec spec :string :boolean :number :typeof :proto-id)
  (flet ((arg/body (list)
           (if (and (consp (car list)) (cdr list))
               (values (caar list) (cdr list))
               (values (gensym) list))))
    `(progn
       ,@(let ((proto-id (spec-list spec :proto-id)))
           (when proto-id
             (multiple-value-bind (arg body) (arg/body proto-id)
               `((defmethod static-js-prop ((,arg ,specializer) cache)
                   (funcall (the function (cache-op cache)) ,arg (find-proto (progn ,@body)) cache))
                 (defmethod js-prop ((,arg ,specializer) prop)
                   (do-lookup ,arg (find-proto (progn ,@body)) prop))
                 (defmethod (setf static-js-prop) (val (obj ,specializer) wcache)
                   (declare (ignore wcache)) val)
                 (defmethod (setf js-prop) (val (obj ,specializer) prop)
                   (declare (ignore prop)) val)
                 (defmethod js-for-in ((,arg ,specializer) func &optional shallow)
                   (js-for-in (find-proto (progn ,@body)) func shallow))))))
       ,@(loop :for (tag method default) :in '((:string js-to-string "[object Object]") (:number js-to-number (nan))
                                              (:boolean js-to-boolean t) (:typeof js-type-of "foreign")) :collect
            (let ((found (spec-list spec tag)))
              (multiple-value-bind (arg body) (if found (arg/body found) (values (gensym) (list default)))
                `(defmethod ,method ((,arg ,specializer)) ,@body)))))))
