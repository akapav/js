(when (asdf:find-system :local-time nil) (push :js-dates *features*))

(asdf:defsystem :js
  :depends-on (:parse-js :cl-ppcre #+js-dates :local-time)
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
