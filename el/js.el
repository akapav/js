(defun js-load-file (file)
  (interactive
   (let ((file (expand-file-name (read-file-name "File: " nil nil t))))
     `(,file)))
  (slime-eval-with-transcript
   `(js::js-load-file ,file)))

(defun js-eval-region (start end)
  (interactive "r")
  (slime-eval-with-transcript
   `(js::!eval ,(buffer-substring-no-properties start end))))
