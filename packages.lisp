(defpackage :net.svrg.reader-macro
  (:use :cl)
  (:export :define-reader
	   :find-reader
	   :remove-reader
	   :read-line-stream))

(defpackage :net.svrg.javascript
  (:nicknames :js)
  (:shadow :array)
  (:use :cl :parse-js :net.svrg.reader-macro)
  (:export native-hash
	   native-function
	   global-object
	   !eval
	   javascript js
           js-load-file
	   define-js-function))

(defpackage :net.svrg.js-user
  (:nicknames :js-user)
  (:use :cl :js :net.svrg.reader-macro :parse-js))
