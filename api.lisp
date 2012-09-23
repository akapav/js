(in-package :cl-js)

(defmacro void (&body body)
  `(progn ,@body :undefined))

(defun reset ()
  (setf *env* (create-env *printlib*)))

(defvar *enable-Function.caller* nil
  "If T, enables support for arguments.callee.caller/Function.caller property in newly compiled JavaScript code")

(defun run-js (str &key (compile t) (wrap-parse-errors nil) (optimize nil) (wrap-as-module nil))
  (unless (boundp '*env*) (reset))
  (let* ((ast (handler-bind ((js-parse-error
                              (lambda (e) (when wrap-parse-errors
                                            (js-error :syntax-error (princ-to-string e))))))
                (parse str)))
         (ast (if wrap-as-module
                  `(:function nil ("exports") (,@(second ast) (:return (:name "exports"))))
                  ast))
         (form `(wrap-js ,(translate-ast ast)))
         (form (if optimize `(locally (declare (optimize speed (safety 0))) ,form) form)))
    (if compile
        (compile-eval form)
        (eval form))))

(defun run-js-file (file &key (compile t) (wrap-parse-errors nil) (optimize nil) (wrap-as-module nil)
                    (external-format :default))
  (with-open-file (in file :external-format external-format)
    (run-js in :compile compile :wrap-parse-errors wrap-parse-errors :optimize optimize
            :wrap-as-module wrap-as-module)))

(defun js-repl (&key (handle-errors t))
  (unless (boundp '*env*) (reset))
  (format t "~%JS repl (#q to quit)~%> ")
  (let ((accum "") continue)
    (flet ((handle-js-condition (e)
             (let ((str (to-string (js-condition-value e))))
               (cond ((and (> (length str) 35) (equal (subseq str 0 35) "SyntaxError: Unexpected token 'EOF'"))
                      (setf continue t) (throw 'err nil))
                     (handle-errors
                      (format t "! ~a~%" (to-string (js-condition-value e))) (throw 'err nil)))))
           (handle-error (e)
             (when (eq handle-errors t)
               (format t "!! ~a~%" e)
               (throw 'err nil))))
      (loop :for line := (read-line) :do
         (when (equal line "#q") (return))
         (if continue
             (setf line (setf accum (concatenate 'string accum '(#\newline) line))
                   continue nil)
             (setf accum line))
         (catch 'err
           (handler-bind ((js-condition #'handle-js-condition)
                          (error #'handle-error))
             (let ((result (compile-eval (translate-ast (parse/err line)))))
               (unless (eq result :undefined)
                 (format t "~a~%" (to-string result))))))
         (format t (if continue "  " "> "))))))

(defun tests ()
  (let ((*enable-function.caller* t))
    (with-js-env (*printlib*)
      (run-js-file (asdf:system-relative-pathname :cl-js "test.js")))))

(defun js-obj (&optional proto struct-type)
  (let ((cls (etypecase proto
               (keyword (find-cls proto))
               (js-obj (make-scls nil proto))
               (null (find-cls :object)))))
    (if struct-type
        (funcall (default-constructor-name struct-type) cls)
        (make-obj cls))))

(defun js-array (vec)
  (assert (and (vectorp vec) (adjustable-array-p vec)))
  (build-array vec))

(deftype js-obj () 'obj)
(deftype js-func () 'fobj)
(deftype js-array () 'aobj)

(defun js-array-length (x) (length (aobj-arr x)))
(defun js-aref (x index) (aref (aobj-arr x) index))
(defun (setf js-aref) (val x index)
  (setf (aref (aobj-arr x) index) val))
(defun js-array-vec (x) (aobj-arr x))

(defun js-null (x)
  (or (eq x :null) (eq x :undefined)))

(deftype js-null () '(or (eql :null) (eql :undefined)))

(defmacro js-func ((&rest args) &body body)
  `(build-func (js-lambda ,args ,@body) ,(arg-count args)))

(defun js-special-number (x)
  (or (is-nan x) (eq x (infinity)) (eq x (-infinity))))
