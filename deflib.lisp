(in-package :cl-js)

;; TODO check for spurious options

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
  (push (list* name val flags) (objspec-props *objspec*)))

(defmacro deflib (name &body spec)
  `(defparameter ,name
     (let* ((*objspec* (make-objspec :prototype :object))
            (*lib* (make-lib :toplevel *objspec*)))
       ,@spec
       *lib*)))

(defmacro .prototype (tag &body spec)
  `(let ((*objspec* (make-objspec :prototype ,(spec-val spec :parent :object)))
         (*default-slot-flags* (slot-flags ,@(spec-list spec :slot-default '(:noenum)))))
     ,@(spec-body spec)
     (push (cons ,tag *objspec*) (lib-prototypes *lib*))))

(defmacro .constructor (name (&rest args) &body spec)
  (let* ((proto (spec-list spec :prototype))
         (proto (if (keywordp (car proto))
                    (car proto)
                    `(let ((*objspec* (make-objspec))
                           (*default-slot-flags* (slot-flags ,@(spec-list proto :slot-default '(:enum)))))
                       ,@(spec-body proto)
                       *objspec*))))
    `(add-prop
      ,name
      (let ((*objspec* (make-funcspec :call ,(wrap-js-lambda args (spec-body spec))
                                      :prototype :function
                                      :proto-spec ,proto
                                      :make-new ,(spec-val spec :make-new))) ;; TODO type spec for client code
            (*default-slot-flags* (slot-flags ,@(spec-list spec :slot-default '(:enum)))))
        ,@(spec-list spec :properties)
        *objspec*)
      (slot-flags ,@(spec-list spec :slot)))))

(defmacro .object (name &body spec)
  `(add-prop
    ,name
    (let ((*objspec* (make-objspec :prototype ,(spec-val spec :parent :object)))
          (*default-slot-flags* (slot-flags ,@(spec-list spec :slot-default '(:enum)))))
      ,@(spec-body spec)
      *objspec*)
    (slot-flags ,@(spec-list spec :slot))))

(defmacro .value (name &body spec)
  `(add-prop ,name (lambda () ,@(spec-body spec)) (slot-flags ,(spec-list spec :slot))))

(defmacro .func (name (&rest args) &body spec)
  `(add-prop
    ,name
    (let ((*objspec* (make-funcspec :call ,(wrap-js-lambda args (spec-body spec))
                                    :prototype :function))
          (*default-slot-flags* (slot-flags ,@(spec-list spec :slot-default '(:enum)))))
      ,@(spec-list spec :properties)
      *objspec*)
    (slot-flags ,@(spec-list spec :slot))))

(defmacro .active (name &body spec)
  `(add-prop
    ,name
    (cons ,(let ((read (spec-list spec :read)))
             (and read (wrap-js-lambda (car read) (cdr read))))
          ,(let ((write (spec-list spec :write)))
             (and write (wrap-js-lambda (car write) (cdr write)))))
    (slot-flags 'active ,@(spec-list spec :slot))))

(defmacro .active-r (name &body spec)
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
             (when new-proto (setf (lookup new-proto "constructor") built))
             built))
          (t (make-obj cls vals)))))

(defun create-env (lib)
  (let* ((bootstrap (make-array (length *std-types*) :initial-contents
                                (loop :repeat (length *std-types*) :collect (make-obj nil nil))))
         (objproto (svref bootstrap (type-offset :object)))
         (clss (make-array (length *std-types*) :initial-contents
                           (loop :for id :across *std-types* :for i :from 0 :collect
                              (make-scls () (svref bootstrap i)))))
         (*env* (make-gobj (make-hcls objproto) (make-hash-table :test 'eq) bootstrap clss)))
    (loop :for (id . obj) :in (lib-prototypes lib) :do
       (let ((pos (position id *std-types*)))
         (when pos
           (init-obj obj (aref bootstrap pos)))))
    (loop :for shell :across bootstrap :for i :from 0 :do
       (unless (obj-cls shell) (error "Missing definition for standard class ~a" (aref *std-types* i))))
    (add-to-env *env* lib)
    *env*))

(defun add-to-env (*env* lib)
  (loop :for (id . obj) :in (reverse (lib-prototypes lib)) :do
     (unless (find id *std-types*)
       (let ((proto (init-obj obj)))
         (push (list* id proto (make-scls () proto)) (gobj-proto-list *env*)))))
  (loop :for (name val . flags) :in (objspec-props (lib-toplevel lib)) :do
     (ensure-slot *env* name (init-val val) flags)))
