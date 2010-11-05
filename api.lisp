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

(defmacro integrate-type (specializer &body defs)
  `(locally
       ,@(loop :for (id arg . body) :in defs :collect
            `(defmethod ,(ecase id (:string 'js-to-string) (:number 'js-to-number)
                                   (:boolean 'js-to-boolean) (:typeof 'js-type-of))
                 ((,(if arg (car arg) (gensym)) ,specializer))
               ,@body))))

(defmacro js-defun (name args &body body)
  `(setf (lookup *env* ,name) (js-lambda ,args ,@body)))
