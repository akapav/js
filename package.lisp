(defpackage :cl-js
  (:use :cl :parse-js)
  (:export #:run-js-file #:run-js #:js-repl
           #:*env* #:with-js-env #:create-env #:add-to-env

           #:*printlib*

           #:empty-lib #:add-to-lib #:.prototype #:.constructor
           #:.object #:.value #:.func #:.active #:.active-r
           #:integrate-type #:define-js-obj

           #:to-number #:to-string #:to-integer #:to-boolean
           #:this
           #:js-error #:js-condition #:js-condition-value

           #:void #:js-null #:js-number #:js-special-number
           #:js-func #:js-call #:js-method
           #:js-array #:js-array-length #:js-aref #:js-array-vec
           #:js-obj #:js-prop #:js-for-in

           #:nan #:infinity #:-infinity #:is-nan))
