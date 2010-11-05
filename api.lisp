(in-package :js)

(defmacro js-eval (str)
  `(wrap-js ,(translate-ast (parse-js-string str))))


(defun run (str &key (compile t))
  (let ((form `(wrap-js ,(translate-ast (parse-js-string str)))))
    (if compile
        (compile-eval form)
        (eval form))))

(defun js-load-file (fname &optional optimize)
  (let ((code (with-open-file (str fname) `(wrap-js ,(translate-ast (parse-js str))))))
    (compile-eval (if optimize `(locally (declare (optimize speed (safety 0))) ,code) code))))

;; TODO support multiline input
(defun js-repl ()
  (format t "~%JS repl (#q to quit)~%> ")
  (loop :for line := (read-line) :do
     (when (equal line "#q") (return))
     (handler-case
         (let ((result (compile-eval (translate-ast (parse/err line)))))
           (unless (eq result :undefined)
             (format t "~a~%" (to-string result))))
       (js-condition (e)
         (format t "! ~a~%" (to-string (js-condition-value e))))
       (error (e)
         (format t "!! ~a~%" e)))
     (format t "> ")))

(defun find-user-proto (id)
  (or (cdr (assoc id (gobj-user-protos *env*)))
      (error "No prototype ~a defined." id)))
(defun add-user-proto (id obj)
  (let ((found (assoc id (gobj-user-protos *env*))))
    (if found
        (setf (cdr found) obj)
        (push (cons id obj) (gobj-user-protos *env*)))))

(defmacro integrate-type (specializer &body defs)
  `(locally ()
     ,@(loop :for (id arg . body) :in defs :collect
          (multiple-value-bind (arg body)
              (if body (values (car arg) body) (values (gensym) (list arg)))
            (if (eq id :proto-id)
                `(locally ()
                   (defmethod static-lookup ((,arg ,specializer) cache)
                     (funcall (the function (cache-op cache)) ,arg (find-user-proto (progn ,@body)) cache))
                   (defmethod lookup ((,arg ,specializer) prop)
                     (do-lookup ,arg (find-user-proto (progn ,@body)) prop))
                   (defmethod (setf static-lookup) (val (obj ,specializer) wcache)
                     (declare (ignore wcache)) val)
                   (defmethod (setf lookup) (val (obj ,specializer) prop)
                     (declare (ignore prop)) val)
                   (defmethod js-for-in ((,arg ,specializer) func)
                     (js-for-in (find-user-proto (progn ,@body)) func)))
                `(defmethod ,(ecase id (:string 'js-to-string) (:number 'js-to-number)
                                       (:boolean 'js-to-boolean) (:typeof 'js-type-of))
                     ((,arg ,specializer)) ,@body))))))

(defmacro js-def (name value)
  `(setf (lookup *env* ,name) ,value))
(defmacro js-defun (name args &body body)
  `(setf (lookup *env* ,name) (build-func (js-lambda ,args ,@body))))

(defun default-constructor-name (structname)
  (intern (format nil "%make-new-~a-~a" (symbol-name structname) (package-name (symbol-package structname))) :js))

(defmacro defobjstruct (name &body slots)
  (multiple-value-bind (name opts)
      (if (consp name) (values (car name) (cdr name)) (values name ()))
    `(defstruct (,name (:include obj) (:constructor ,(default-constructor-name name) (cls)) ,@opts) ,@slots)))

(defmacro defconstructor (name args &body defs)
  (let ((proto (gensym))
        proto-props cons-props proto-id type
        (body (pop defs)))
    (loop :for (id . def) :in defs :do
       (ecase id
         (:prototype (setf proto-props def))
         (:proto-id (setf proto-id (car def)))
         (:properties (setf cons-props def))
         (:type (setf type (car def)))))
    `(let ((-self- ,(if type
                        `(make-cfobj nil nil nil #',(default-constructor-name type) nil)
                        '(make-fobj nil nil nil nil)))
           (,proto (ensure-proto (list ,@proto-props))))
       (build-constructor -self- ,proto (list ,@cons-props) ,(wrap-js-lambda args (list body)))
       (setf (lookup *env* ,name) -self-)
       ,@(when proto-id `((add-user-proto ,proto-id ,proto))))))

(defmacro defobject (name &body props)
  `(setf (lookup *env* ,name) (obj-from-props (find-proto :object) (list ,@props))))

(defmacro defproto (id &body props)
  `(add-user-proto ,id (obj-from-props (find-proto :object) (list ,@props))))

(defmacro .method (name args &body body)
  `(list ,name (build-func ,(wrap-js-lambda args body))))

(defmacro .prop (name &body value) `(list ,name ,value))

(defmacro .activeprop (name &body read/write)
  (let (read write)
    (loop :for (id args . body) :in read/write :do
       (ecase id
         (:read (setf read (wrap-js-lambda args body)))
         (:write (setf write (wrap-js-lambda args body)))))
    `(list* ,name (cons ,read ,write) +slot-active+)))
