(in-package :js)

(defun ray ()
  (with-js-env
    (js-load-file (asdf:system-relative-pathname :js "bench/ray.js"))
    (time (js-funcall (lookup *global* "renderScene")))))

(defun slurp-file (file)
  (with-open-file (in file)
    (apply #'concatenate 'string
           (loop :for buf := (make-string 4096) :for chars := (read-sequence buf in)
                 :if (< chars 4096) :collect (subseq buf 0 chars) :into all :and :do (return all)
                 :else :collect buf :into all))))

(defun codemirror ()
  (with-js-env
    (let ((file (asdf:system-relative-pathname :js "bench/codemirror.js")))
      (js-load-file file)
      (let ((code (slurp-file file)))
        (time (js-funcall (lookup *global* "codemirrorBench") code))))))
