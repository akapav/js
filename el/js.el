(defun js-load-file (file)
  (interactive
   (let ((file (expand-file-name (read-file-name "File: " nil nil t))))
     `(,file)))
  (slime-eval-async
      `(js::js-load-file ,file)
    (lambda (ret) (message (format "js: %s" ret)))))
