(when (asdf:find-system :local-time nil) (pushnew :js-dates *features*))

(asdf:defsystem :cl-js
  :description "JavaScript-to-CL compiler and runtime"
  :author "Marijn Haverbeke <marijnh@gmail.com>"
  :license "MIT"
  :depends-on (:parse-js :cl-ppcre #+js-dates :local-time)
  :serial t
  :components
  ((:file "package")
   (:file "utils")
   (:file "js")
   (:file "jsos")
   (:file "url-encode")
   (:file "json")
   (:file "deflib")
   (:file "runtime")
   (:file "infer")
   (:file "inline-op")
   (:file "translate")
   (:file "operators")
   (:file "api")))
