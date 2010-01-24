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
   (:file "stdlib")
   (:file "operators")))
