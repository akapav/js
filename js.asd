(defpackage :net.svrg.js-system (:use :cl :asdf))
(in-package :net.svrg.js-system)

(defsystem :js
  :depends-on (:parse-js :cl-ppcre)
  :serial t
  :components
  ((:file "packages")
   (:file "utils")
   (:file "jsos")
   (:file "js")
   (:file "runtime")
   (:file "infer")
   (:file "inline-op")
   (:file "translate")
   (:file "stdlib")
   (:file "operators"))
  :perform (load-op :after (op js)
		    (with-input-from-string (str "(js:js-load-file (asdf:system-relative-pathname 'js \"stdlib.js\"))")
		      (eval (read str)))))
