(defpackage :net.svrg.js-system (:use :cl :asdf))
(in-package :net.svrg.js-system)

(defsystem :js
  :depends-on (:ptester :parse-js)
  :serial t
  :components
  ((:file "packages")
   (:file "utils")
   (:file "reader")
   (:file "proc")
   (:file "js")
   (:file "runtime")
   (:file "stdlib")
   (:file "operators"))
  :perform (load-op :after (op js)
		    (load (system-relative-pathname 'js "js.stdlib.lisp"))))
