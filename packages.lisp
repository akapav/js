(defpackage :net.svrg.javascript
  (:nicknames :js)
  (:shadow :array)
  (:use :cl :parse-js #+sbcl :sb-mop)
  (:export native-hash
	   native-function
	   global-object
	   !eval
	   javascript js
           js-load-file
	   define-js-function))

(defpackage :net.svrg.js-user
  (:nicknames :js-user)
  (:use :cl :js :parse-js))
