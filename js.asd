(defpackage :net.svrg.js-system (:use :cl :asdf))
(in-package :net.svrg.js-system)

(defsystem :js
  :depends-on (:parse-js :cl-ppcre :local-time)
  :serial t
  :components
  ((:file "packages")
   (:file "utils")
   (:file "js")
   (:file "jsos")
   (:file "url-encode")
   (:file "runtime")
   (:file "infer")
   (:file "inline-op")
   (:file "translate")
   (:file "operators")))
