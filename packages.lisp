(defpackage :net.svrg.javascript
  (:nicknames :js)
  (:use :cl :parse-js)
  (:export #:js-eval #:js-load-file #:js-repl #:run
           #:*env* #:with-js-env #:reset
           #:to-number #:to-string #:to-integer #:to-boolean
           #:js-to-string #:js-to-number #:js-to-boolean #:js-type-of
           #:this #:-self- #:js-lambda #:js-def #:js-defun #:js-error #:js-new
           #:integrate-type #:defproto #:defobjstruct
           #:defconstructor #:defobject #:.method #:.prop #:.activeprop))

(defpackage :net.svrg.js-var
  (:nicknames :js-var)
  (:use))
