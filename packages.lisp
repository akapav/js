(defpackage :net.svrg.javascript
  (:nicknames :js)
  (:shadow :array)
  (:use :cl :parse-js)
  (:export #:!eval #:js-load-file #:js-repl #:run
           #:*env* #:with-js-env #:reset))

(defpackage :net.svrg.js-user
  (:nicknames :js-user)
  (:use))
