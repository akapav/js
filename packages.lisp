(defpackage :net.svrg.javascript
  (:nicknames :js)
  (:shadow :array)
  (:use :cl :parse-js)
  (:export #:!eval #:js-load-file #:js-repl #:run
           #:*global* #:with-js-env #:reset))

(defpackage :net.svrg.js-user
  (:nicknames :js-user)
  (:use))
