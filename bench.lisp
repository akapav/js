(in-package :cl-js)

(defun ray ()
  (with-js-env (*printlib*)
    (run-js-file (asdf:system-relative-pathname :cl-js "bench/ray.js"))
    (time (wrap-js (js-funcall (js-prop *env* "renderScene"))))))

(defun slurp-file (file)
  (with-open-file (in file)
    (apply #'concatenate 'string
           (loop :for buf := (make-string 4096) :for chars := (read-sequence buf in)
                 :if (< chars 4096) :collect (subseq buf 0 chars) :into all :and :do (return all)
                 :else :collect buf :into all))))

(defun codemirror ()
  (with-js-env (*printlib*)
    (let ((file (asdf:system-relative-pathname :cl-js "bench/codemirror.js")))
      (run-js-file file)
      (let ((code (slurp-file file)))
        (time (wrap-js (js-funcall (js-prop *env* "codemirrorBench") code)))))))
