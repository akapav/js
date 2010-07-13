(defpackage :net.svrg.javascript
  (:nicknames :js)
  (:shadow :array)
  (:use :cl :parse-js)
  (:export #:!eval #:js-load-file #:js-repl))

(defpackage :net.svrg.js-user
  (:nicknames :js-user)
  (:use))
