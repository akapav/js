(defpackage :net.svrg.reader-macro
    (:use :cl)
  (:export :define-reader
		   :find-reader
		   :remove-reader
		   :read-line-stream))

(defpackage :net.svrg.javascript
	(:nicknames :js)
  (:use :cl :parse-js :net.svrg.reader-macro
		:ptester)
  #+nil (:export ...))
