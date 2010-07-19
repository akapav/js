(in-package :js)

(defvar *global*)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *common-classes*
    #(:object :arguments :function :array :regexp :type-error :parse-error :reference-error :syntax-error))
  (defparameter *proto-offsets*
    #(:object :function :array :arguments :string :number :boolean :regexp :error
      :syntax-error :reference-error :type-error :uri-error :eval-error :range-error))
  (defun proto-offset (id)
    (declare (optimize speed (safety 0)))
    (loop :for off :of-type fixnum :below (length (the simple-vector *proto-offsets*)) :do
       (when (eq id (svref *proto-offsets* off)) (return off))))
  (defun cls-offset (id)
    (declare (optimize speed (safety 0)))
    (loop :for off :of-type fixnum :below (length (the simple-vector *common-classes*)) :do
       (when (eq id (svref *common-classes* off)) (return off)))))

(defmacro find-proto (id)
  `(svref (gobj-protos *global*) ,(if (keywordp id) (proto-offset id) `(proto-offset ,id))))
(defmacro find-cls (id)
  `(svref (gobj-common-cls *global*) ,(if (keywordp id) (cls-offset id) `(cls-offset ,id))))

;; (Some of this code is *really* unorthogonal, repeating itself a
;; lot. This is mostly due to the fact that we are using different
;; code paths, some of which can assume previously-checked conditions,
;; to optimize.)

;; TODO thread-safety (maybe)

;; TODO give up caching when it fails too often

(locally (declare (optimize speed (safety 0)))

(defstruct cls prototype)
(defstruct (scls (:constructor make-scls (props prototype)) (:include cls))
  props children)
(defstruct (hcls (:constructor make-hcls (prototype)) (:include cls)))

(defstruct (obj (:constructor make-obj (cls &optional vals)))
  cls (vals (make-array 4)))
(defstruct (vobj (:constructor make-vobj (cls value)) (:include obj))
  value)
(defstruct (fobj (:constructor make-fobj (cls proc new-cls &optional vals)) (:include obj))
  proc new-cls)
(defstruct (gobj (:constructor make-gobj (cls vals protos common-cls)) (:include obj))
  protos common-cls)
(defstruct (aobj (:constructor make-aobj (cls arr)) (:include obj))
  arr)
(defstruct (reobj (:constructor make-reobj (cls proc scanner global)) (:include fobj))
  scanner global)
(defstruct (argobj (:constructor make-argobj (cls list length callee)) (:include obj))
  list length callee)

(defmethod print-object ((obj obj) stream) (format stream "#<js obj>"))

;; Slots are (offset . flags) conses for scls objects, (value . flags) conses for hcls
(defconstant +slot-ro+ 1)
(defconstant +slot-active+ 2)
(defconstant +slot-noenum+ 4)
(defconstant +slot-nodel+ 8)
(defconstant +slot-dflt+ 0)

(defun hash-obj (obj hcls)
  (let* ((scls (obj-cls obj))
         (hcls (or hcls (make-hcls (cls-prototype scls))))
         (vec (obj-vals obj))
         (table (make-hash-table :test 'eq :size (* (length vec) 2))))
    (loop :for (prop offset . flags) :in (scls-props scls) :do
       (setf (gethash prop table) (cons (svref vec offset) flags)))
    (setf (obj-cls obj) hcls (obj-vals obj) table))
  obj)

(defun proc (val)
  (if (fobj-p val)
      (fobj-proc val)
      (js-error :type-error "~a is not a function." (to-string val))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *prop-names*
    #+allegro (make-hash-table :test 'equal :weak-keys t :values :weak)
    #+sbcl (make-hash-table :test 'equal :weakness :key-or-value)
    #-(or allegro sbcl) (make-hash-table :test 'equal))) ;; Space leak when we don't have weak hashes
(defun intern-prop (prop)
  (or (gethash prop *prop-names*)
      (setf (gethash prop *prop-names*) prop)))

(defmacro lookup-slot (scls prop)
  `(cdr (assoc ,prop (scls-props ,scls) :test #'eq)))
(defmacro dcall (proc obj &rest args)
  `(funcall (the function ,proc) ,obj ,@args))

(defstruct (cache (:constructor make-cache (prop)))
  (op #'cache-miss) prop cls a1 a2)

(defmethod static-lookup ((obj obj) cache)
  (funcall (the function (cache-op cache)) obj obj cache))
(defmethod static-lookup (obj cache)
  (declare (ignore cache))
  (js-error :type-error "~a has no properties." (to-string obj)))

(defun do-lookup (obj start prop)
  (simple-lookup obj start (intern-prop (if (stringp prop) prop (to-string prop)))))
(defmethod lookup ((obj obj) prop)
  (do-lookup obj obj prop))
(defmethod lookup (obj prop)
  (declare (ignore prop))
  (js-error :type-error "~a has no properties." (to-string obj)))

(defun index-in-range (index len)
  (if (and (typep index 'fixnum) (>= index 0) (< index len))
      index
      (let ((index (to-string index)) index-int)
        (if (and (every #'digit-char-p index) ;; TODO faster check
                 (progn (setf index-int (parse-integer index)) (>= index-int 0) (< index-int len)))
            index-int
            nil))))

(defmethod lookup ((obj aobj) prop)
  (let* ((vec (aobj-arr obj))
         (index (index-in-range prop (length vec))))
    (if index
        (aref vec index)
        (do-lookup obj obj prop))))
(defmethod lookup ((obj argobj) prop)
  (let ((lst (argobj-list obj))
        (index (index-in-range prop (argobj-length obj))))
    (if index
        (nth index lst)
        (do-lookup obj obj prop))))

(defvar *not-found* :undefined)
(defmacro if-not-found ((var lookup) &body then/else)
  (unless var (setf var (gensym)))
  `(let ((,var (let ((*not-found* :not-found)) ,lookup)))
     (declare (ignorable ,var))
     (if (eq ,var :not-found) ,@then/else)))

;; Used for non-cached lookups
(defun simple-lookup (this start prop)
  (loop :for obj := start :then (cls-prototype cls) :while obj
        :for cls := (obj-cls obj) :for vals := (obj-vals obj) :do
     (macrolet ((maybe-active (slot value)
                  `(if (logtest (cdr ,slot) +slot-active+)
                       (dcall (car ,value) this)
                       ,value)))
       (if (hash-table-p vals)
           (let ((slot (gethash prop vals)))
             (when slot
               (return (maybe-active slot (car slot)))))
           (let ((slot (lookup-slot cls prop)))
             (when slot
               (return (maybe-active slot (svref vals (car slot))))))))
     :finally (return *not-found*)))

(defun cache-miss (val obj cache)
  (multiple-value-bind (fn a1 a2 result) (meta-lookup val obj (cache-prop cache))
    (setf (cache-op cache) fn (cache-a1 cache) a1 (cache-a2 cache) a2 (cache-cls cache) (obj-cls obj))
    result))

(defun %direct-lookup (val obj cache)
  (if (eq (cache-cls cache) (obj-cls obj))
      (svref (obj-vals obj) (cache-a1 cache))
      (cache-miss val obj cache)))

(defun %direct-lookup-d (val obj cache)
  (if (eq (cache-cls cache) (obj-cls obj))
      (dcall (car (svref (obj-vals obj) (cache-a1 cache))) val)
      (cache-miss val obj cache)))

(defun %direct-lookup-m (val obj cache)
  (if (eq (cache-cls cache) (obj-cls obj))
      *not-found*
      (cache-miss val obj cache)))

(defun %direct-lookup-h (val obj cache)
  (if (hash-table-p (obj-vals obj))
      (simple-lookup val obj (cache-prop cache))
      (cache-miss val obj cache)))

(defun %proto-lookup (val obj cache)
  (let* ((cls (obj-cls obj))
         (proto (cls-prototype cls)))
    (if (and (eq (cache-cls cache) cls)
             (eq (cache-a2 cache) (obj-cls proto)))
        (svref (obj-vals proto) (cache-a1 cache))
        (cache-miss val obj cache))))

(defun %proto-lookup-d (val obj cache)
  (let* ((cls (obj-cls obj))
         (proto (cls-prototype cls)))
    (if (and (eq (cache-cls cache) cls)
             (eq (cache-a2 cache) (obj-cls proto)))
        (dcall (car (svref (obj-vals proto) (cache-a1 cache))) val)
        (cache-miss val obj cache))))

(defun %proto-lookup-m (val obj cache)
  (let ((cls (obj-cls obj)))
    (if (and (eq (cache-cls cache) cls)
             (eq (cache-a2 cache) (obj-cls (cls-prototype cls))))
        *not-found*
        (cache-miss val obj cache))))

(defun %proto-lookup-h (val obj cache)
  (let ((cls (obj-cls obj)))
    (if (eq (cache-cls cache) cls)
        (simple-lookup val (cls-prototype cls) (cache-prop cache))
        (cache-miss val obj cache))))

(defun %deep-lookup (val obj cache)
  (let* ((cls (obj-cls obj))
         (proto-cls (obj-cls (cls-prototype cls))))
    (if (and (eq (cache-cls cache) cls)
             (eq (cache-a2 cache) proto-cls))
        (simple-lookup val (cls-prototype proto-cls) (cache-prop cache))
        (cache-miss val obj cache))))

(defun meta-lookup (this obj prop)
  (macrolet ((ret (&rest vals) `(return-from meta-lookup (values ,@vals))))
    (let ((cls (obj-cls obj)) (vals (obj-vals obj)))
      (when (hash-table-p vals)
        (ret #'%direct-lookup-h nil nil (simple-lookup this obj prop)))
      (let ((slot (lookup-slot cls prop)))
        (when slot
          (if (logtest (cdr slot) +slot-active+)
              (ret #'%direct-lookup-d (car slot) nil (dcall (car (svref vals (car slot))) this))
              (ret #'%direct-lookup (car slot) nil (svref vals (car slot))))))
      (let ((proto (cls-prototype cls)))
        (unless proto (ret #'%direct-lookup-m nil nil *not-found*))
        (let ((proto-cls (obj-cls proto)) (proto-vals (obj-vals proto)))
          (when (hash-table-p proto-vals)
            (ret #'%proto-lookup-h nil nil (simple-lookup this proto prop)))
          (let ((slot (lookup-slot proto-cls prop)))
            (when slot
              (if (logtest (cdr slot) +slot-active+)
                  (ret #'%proto-lookup-d (car slot) proto-cls (dcall (car (svref proto-vals (car slot))) this))
                  (ret #'%proto-lookup (car slot) proto-cls (svref proto-vals (car slot))))))
          (let ((proto2 (cls-prototype proto-cls)))
            (unless proto2 (ret #'%proto-lookup-m nil proto-cls *not-found*))
            (ret #'%deep-lookup nil proto-cls (simple-lookup this proto2 prop))))))))

(defun expand-cached-lookup (obj prop)
  `(static-lookup ,obj (load-time-value (make-cache (intern-prop ,prop)))))
(defmacro cached-lookup (obj prop)
  (expand-cached-lookup obj prop))

;; Writing

;; TODO should there also be support for dynamic setters? (stdlib seems to be okay with ro+dynread)

(defun update-class-and-set (obj new-cls slot val)
  (setf (obj-cls obj) new-cls)
  (unless (< slot (length (obj-vals obj)))
    (let ((vals (make-array (max 4 (* 2 (length (obj-vals obj)))))))
      (replace vals (obj-vals obj))
      (setf (obj-vals obj) vals)))
  (setf (svref (obj-vals obj) slot) val))

(defstruct (wcache (:constructor make-wcache (prop)))
  (op #'wcache-miss) cls prop slot a1)

(defun %simple-set (obj wcache val)
  (if (eq (obj-cls obj) (wcache-cls wcache))
      (setf (svref (obj-vals obj) (wcache-slot wcache)) val)
      (wcache-miss obj wcache val)))

(defun %active-set (obj wcache val)
  (if (eq (obj-cls obj) (wcache-cls wcache))
      (progn (dcall (wcache-a1 wcache) obj val) val)
      (wcache-miss obj wcache val)))

(defun %change-class-set (obj wcache val)
  (if (eq (obj-cls obj) (wcache-cls wcache))
      (update-class-and-set obj (wcache-a1 wcache) (wcache-slot wcache) val)
      (wcache-miss obj wcache val)))

(defun %ignored-set (obj wcache val)
  (if (eq (obj-cls obj) (wcache-cls wcache))
      val
      (wcache-miss obj wcache val)))

(defun %hash-set (obj wcache val)
  (if (hash-table-p (obj-vals obj))
      (hash-set obj (wcache-prop wcache) val)
      (wcache-miss obj wcache val)))

(defun %hash-then-set (obj wcache val)
  (if (eq (obj-cls obj) (wcache-cls wcache))
      (progn (hash-obj obj (scls-children (obj-cls obj)))
             (setf (gethash (wcache-prop wcache) (obj-vals obj)) (cons val +slot-dflt+))
             val)
      (wcache-miss obj wcache val)))

(defun hash-set (obj prop val)
  (let* ((table (obj-vals obj))
         (exists (gethash prop table)))
    (if exists
        (setf (car exists) val)
        ;; Check prototypes for read-only or active slots
        (if (loop :for cur := (cls-prototype (obj-cls obj)) :then (cls-prototype curc) :while cur
                  :for curc := (obj-cls cur) :for curv := (obj-vals cur) :for hash := (hash-table-p curv) :do
               (let ((slot (if hash (gethash prop curv) (lookup-slot curc prop))))
                 (when slot
                   (when (logtest (cdr slot) +slot-ro+) (return t))
                   (when (logtest (cdr slot) +slot-active+)
                     (let ((func (cdr (if hash (car slot) (svref curv (car slot))))))
                       (when func (dcall func obj val))
                       (return t)))
                   (return nil))))
            val
            (progn (setf (gethash prop table) (cons val +slot-dflt+)) val)))))

(defun wcache-miss (obj wcache val)
  (setf (wcache-cls wcache) (obj-cls obj))
  (multiple-value-bind (fn slot a1) (meta-set obj (wcache-prop wcache) val)
    (setf (wcache-op wcache) fn (wcache-slot wcache) slot (wcache-a1 wcache) a1)
    val))

;; This makes the assumption that the read-only flag of a property is
;; final, and doesn't change at runtime. If we add code to allow
;; twiddling of this flag, we can no longer cache the check.
(defun meta-set (obj prop val)
  (macrolet ((ret (&rest vals) `(return-from meta-set (values ,@vals))))
    (let ((cls (obj-cls obj)) (vals (obj-vals obj)))
      (when (hash-table-p vals)
        (hash-set obj prop val)
        (ret #'%hash-set))
      (let ((slot (lookup-slot cls prop)))
        (when slot
          (when (logtest (cdr slot) +slot-ro+)
            (ret #'%ignored-set))
          (when (logtest (cdr slot) +slot-active+)
            (let ((func (cdr (svref vals (car slot)))))
              (when func
                (dcall func obj val)
                (ret #'%active-set func))
              (ret #'%ignored-set)))
          (setf (svref vals (car slot)) val)
          (ret #'%simple-set (car slot))))
      ;; Look for a read-only or active slot in prototypes
      (loop :for cur := (cls-prototype cls) :then (cls-prototype curc) :while cur
            :for curc := (obj-cls cur) :for curv := (obj-vals cur) :for hash := (hash-table-p curv) :do
         (let ((slot (if hash (gethash prop curv) (lookup-slot curc prop))))
           (when slot
             (when (logtest (cdr slot) +slot-ro+) (ret #'%ignored-set))
             (when (logtest (cdr slot) +slot-active+)
               (let ((func (cdr (if hash (car slot) (svref curv (car slot))))))
                 (when func
                   (dcall func obj val)
                   (ret #'%active-set func))
                 (ret #'%ignored-set)))
             (return))))
      ;; No direct slot found yet, but can write. Add slot.
      (scls-add-slot obj cls prop val +slot-dflt+))))
  
(defun scls-add-slot (obj cls prop val flags)
  ;; Setting scls-children to a hash class means hash, using that class, when adding slots
  (unless (listp (scls-children cls))
    (hash-obj obj (scls-children cls))
    (setf (gethash prop (obj-vals obj)) (cons val flags))
    (return-from scls-add-slot #'%hash-then-set))
  (let ((new-cls (cdr (assoc prop (scls-children cls) :test #'eq))) slot)
    ;; We switch to a hash table if this class has 8 'exits' (probably
    ;; being used as a container), and it is not one of the reused classes.
    (when (and (not new-cls) (> (length (scls-children cls)) 8)
               (not (find cls (gobj-common-cls *global*) :test #'eq)))
      (setf (scls-children cls) (make-hcls (cls-prototype cls)))
      (hash-obj obj (scls-children cls))
      (setf (gethash prop (obj-vals obj)) (cons val flags))
      (return-from scls-add-slot #'%hash-then-set))
    (if new-cls
        (setf slot (lookup-slot new-cls prop))
        (progn
          (setf slot (cons (length (scls-props cls)) flags)
                new-cls (make-scls (cons (cons prop slot) (scls-props cls)) (cls-prototype cls)))
          (push (cons prop new-cls) (scls-children cls))))
    (update-class-and-set obj new-cls (car slot) val)
    (values #'%change-class-set (car slot) new-cls)))

(defun ensure-slot (obj prop val &optional (flags +slot-dflt+))
  (setf prop (intern-prop prop))
  (let ((vals (obj-vals obj)))
    (if (hash-table-p vals)
        (setf (gethash prop vals) (cons val flags))
        (let* ((cls (obj-cls obj)) (slot (lookup-slot cls prop)))
          (if slot
              (setf (svref (obj-vals obj) (car slot)) val)
              (scls-add-slot obj cls prop val flags))))))

(defmethod (setf static-lookup) (val (obj obj) wcache)
  (funcall (the function (wcache-op wcache)) obj wcache val))
(defmethod (setf static-lookup) (val obj wcache)
  (declare (ignore wcache val))
  (js-error :type-error "~a has no properties." (to-string obj)))

(defmethod (setf lookup) (val (obj obj) prop)
  ;; Uses meta-set since the overhead isn't big, and duplicating all
  ;; that logic is error-prone.
  (meta-set obj (intern-prop (if (stringp prop) prop (to-string prop))) val)
  val)
(defmethod (setf lookup) (val obj prop)
  (declare (ignore prop val))
  (js-error :type-error "~a has no properties." (to-string obj)))
;; TODO sparse storage, clever resizing
(defmethod (setf lookup) (val (obj aobj) prop)
  (let ((index (index-in-range prop most-positive-fixnum)))
    (if index
        (let ((arr (aobj-arr obj)))
          (when (>= index (length arr))
            (adjust-array arr (1+ index) :fill-pointer (1+ index) :initial-element :undefined))
          (setf (aref arr index) val))
        (call-next-method val obj prop))))
(defmethod (setf lookup) (val (obj argobj) prop)
  (let ((lst (argobj-list obj))
        (index (index-in-range prop (argobj-length obj))))
    (if index
        (setf (nth index lst) val)
        (call-next-method val obj prop))))

(defun expand-cached-set (obj prop val)
  `(setf (static-lookup ,obj (load-time-value (make-wcache (intern-prop ,prop)))) ,val))
(defmacro cached-set (obj prop val)
  (expand-cached-set obj prop val))

;; Optimized global-object access

(define-condition undefined-variable (js-condition) ()) ;; TODO proper contents
(defun undefined-variable (name)
  (let ((err (make-js-error :reference-error "Undefined variable: ~a" name)))
    (error 'undefined-variable :value err)))

(defun gcache-lookup (gcache obj)
  (let ((slot (car gcache))
        (cache (cdr gcache)))
    (macrolet ((read-slot ()
                 `(if (logtest (cdr slot) +slot-active+)
                      (if (eq (car slot) :deleted)
                          (progn (setf (car gcache) nil)
                                 (return-from gcache-lookup (gcache-lookup gcache obj)))
                          (dcall (car slot) obj))
                      (car slot))))
      (cond (slot (read-slot))
            ((setf slot (gethash (cache-prop cache) (obj-vals obj)))
             (setf (car gcache) slot)
             (read-slot))
            (t (if-not-found (value (static-lookup obj cache))
                 (undefined-variable (cache-prop cache))
                 value))))))

(defun expand-global-lookup (prop)
  `(gcache-lookup (load-time-value (cons nil (make-cache (intern-prop ,prop)))) ,*global*))

(defun global-lookup (prop)
  (if-not-found (value (lookup *global* prop))
    (undefined-variable prop)
    value))

(defun gcache-set (gcache obj val)
  (let ((slot (car gcache))
        (prop (cdr gcache)))
    (when (cond (slot t)
                ((setf slot (gethash prop (obj-vals obj))) (setf (car gcache) slot))
                (t (hash-set obj prop val) nil))
      (cond ((logtest (cdr slot) +slot-active+)
             (if (eq (car slot) :deleted)
                 (progn (setf (car gcache) nil)
                        (return-from gcache-set (gcache-set gcache obj val)))
                 (when (cdar slot) (dcall (cdar slot) obj val))))
            ((not (logtest (cdr slot) +slot-ro+))
             (setf (car slot) val))))
    val))

(defun expand-global-set (prop val)
  `(gcache-set (load-time-value (cons nil (intern-prop ,prop))) ,*global* ,val))

;; Enumerating

(defun enumerate-properties (obj)
  (let ((set ()))
    (flet ((enum (obj)
             (let ((vals (obj-vals obj)))
               (if (hash-table-p vals)
                   (with-hash-table-iterator (next vals)
                     (loop
                        (multiple-value-bind (more key val) (next)
                          (unless more (return))
                          (unless (logtest (cdr val) +slot-noenum+) (pushnew key set)))))
                   (loop :for (name nil . flags) :in (scls-props (obj-cls obj)) :do
                      (unless (logtest flags +slot-noenum+) (pushnew name set)))))))
      (loop :for cur := obj :then (cls-prototype (obj-cls cur)) :while cur :do (enum cur))
      set)))

(defmethod list-props ((obj obj))
  (enumerate-properties obj))
(defmethod list-props (obj)
  (declare (ignore obj))
  ())

;; Registering prototypes for string, number, and boolean values

(defmacro declare-primitive-prototype (specializer proto-id)
  `(progn
     (defmethod static-lookup ((obj ,specializer) cache)
       (funcall (the function (cache-op cache)) obj (find-proto ,proto-id) cache))
     (defmethod lookup ((obj ,specializer) prop)
       (do-lookup obj (find-proto ,proto-id) prop))
     (defmethod (setf static-lookup) (val (obj ,specializer) wcache)
       (declare (ignore wcache))
       val)
     (defmethod (setf lookup) (val (obj ,specializer) prop)
       (declare (ignore prop))
       val)
     (defmethod list-props ((obj ,specializer))
       (enumerate-properties (find-proto ,proto-id)))))

;; Utilities

(defun obj-from-props (proto props &optional (make #'make-obj))
  (let* ((vals (make-array (max 2 (length props))))
         (cls (make-scls (loop :for off :from 0 :for (name value . flags) :in props
                               :do (setf (svref vals off) value)
                               :collect (cons (intern-prop name) (cons off flags)))
                         proto)))
    (funcall make cls vals)))

(defun expand-static-obj (proto props)
  (let ((cls (gensym)))
    `(let ((,cls (load-time-value (make-scls ',(loop :for off :from 0 :for (name) :in props :collect
                                                  (cons (intern-prop name) (cons off +slot-dflt+)))
                                             ,proto))))
       (make-obj ,cls (vector ,@(mapcar #'cdr props))))))

(defun js-new (func &rest args) ;; TODO check standard
  (unless (fobj-p func)
    (js-error :type-error "~a is not a constructor." (to-string func)))
  (let* ((this (make-obj (ensure-fobj-cls func)))
         (result (apply (the function (proc func)) this args)))
    (if (obj-p result) result this)))

(defun simple-obj ()
  (make-obj (find-cls :object)))

(defun ensure-fobj-cls (fobj)
  (let ((proto (lookup fobj "prototype"))) ;; Active property in function prototype ensures this is always bound
    (unless (obj-p proto)
      (setf proto (simple-obj))
      (setf (lookup proto "constructor") fobj))
    (unless (and (fobj-new-cls fobj) (eq (cls-prototype (fobj-new-cls fobj)) proto))
      (setf (fobj-new-cls fobj) (make-scls () proto)))
    (fobj-new-cls fobj)))

(defun find-slot (obj prop)
  (let ((vals (obj-vals obj)))
    (if (hash-table-p vals)
        (gethash (intern-prop prop) vals)
        (lookup-slot (obj-cls obj) (intern-prop prop)))))

(defun delete-prop (obj prop)
  (if (obj-p obj)
      (let ((slot (find-slot obj prop)))
        (cond ((not slot) t)
              ((logtest (cdr slot) +slot-nodel+) nil)
              (t (cond ((not (hash-table-p (obj-vals obj)))
                        (hash-obj obj (make-hcls (cls-prototype (obj-cls obj))))) ;; TODO reuse?
                       ((eq obj *global*) ;; Global slots can be cached, so we have to flag them as deleted
                        (setf (car slot) :deleted (cdr slot) +slot-active+)))
                 (remhash (intern-prop prop) (obj-vals obj)))))
      t))

)
