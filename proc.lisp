(in-package :js)

(defparameter *label-name* nil)
(defparameter *lexenv-chain* nil)

;;
(defun transform-tree (form)
  (cond ((null form) nil)
	((atom form)
	 (if (keywordp form)
	     (js-intern form) form))
	(t (apply-transform-rule (car form) form))))

(defmacro define-transform-rule ((rule form) &body body)
  `(defmethod apply-transform-rule ((,(gensym) (eql ,rule)) ,form)
     (declare (special env locals obj-envs lmbd-forms *toplevel*))
     ,@body))

(defgeneric apply-transform-rule (keyword form)
  (:method (keyword form)
    (declare (ignore keyword))
    (mapcar #'transform-tree form)))

(define-transform-rule (:var form)
    (cons (js-intern (car form))
	  (list (mapcar
		 (lambda (var-desc)
		   (let ((var-sym (->sym (car var-desc))))
		     (set-add env var-sym)
		     (set-add locals var-sym)
		     (cons (->sym (car var-desc))
			   (transform-tree (cdr var-desc)))))
		 (second form)))))

(define-transform-rule (:label form)
    (let ((*label-name* (->sym (second form))))
      (transform-tree (third form))))

(define-transform-rule (:for form)
    (let* ((label *label-name*)
	   (*label-name* nil))
      (list (js-intern (car form)) ;for
	    (transform-tree (second form)) ;init
	    (transform-tree (third form))  ;cond
	    (transform-tree (fourth form)) ;step
	    (transform-tree (fifth form)) ;body
	    label)))

(define-transform-rule (:while form)
    (transform-tree
     (list (js-intern :for)
	   nil (second form)
	   nil (third form) *label-name*)))

(define-transform-rule (:do form)
    (let* ((label *label-name*)
	   (*label-name* nil))
      (list (js-intern (car form))
	    (transform-tree (second form))
	    (transform-tree (third form))
	    label)))

(define-transform-rule (:name form)
    (list (js-intern (car form)) (->sym (second form))))

(define-transform-rule (:dot form)
    (list (js-intern (car form)) (transform-tree (second form))
	  (->sym (third form))))

(define-transform-rule (:with form)
    (let* ((*lexenv-chain* (cons :obj *lexenv-chain*))
	   (placeholder (copy-list *lexenv-chain*)))
      (push placeholder obj-envs)
      (list (js-intern (car form))
	    placeholder
	    (transform-tree (second form))
	    (transform-tree (third form)))))

(define-transform-rule (:defun form)
    (unless *toplevel*
      (when (second form)
	(let ((fun-name (->sym (second form))))
	  (set-add env fun-name)
	  (set-add locals fun-name))))
  (let ((placeholder (list (car form))))
    (queue-enqueue lmbd-forms
		   (list form env placeholder (copy-list *lexenv-chain*)))
    placeholder))

(define-transform-rule (:function form)
    (let ((placeholder (list (car form))))
      (queue-enqueue lmbd-forms
		     (list form env placeholder (copy-list *lexenv-chain*)))
      placeholder))

(define-transform-rule (:toplevel form)
    (let ((*lexenv-chain* (cons :obj *lexenv-chain*)))
      (list (js-intern (car form))
	    (copy-list *lexenv-chain*)
	    (transform-tree (second form)))))
;;

(flet ((dump (el)
	 (or (and (symbolp el) el) (set-elems el))))
  (defun dump-lexenv-chain ()
    (mapcar #'dump *lexenv-chain*))
  (defun transform-obj-env (place)
    (setf (car place) (dump (car place)))
    (setf (cdr place) (mapcar #'dump (cdr place)))))

(defparameter *toplevel* nil)
(defun shallow-process-toplevel-form (form)
  (let* (*lexenv-chain*
	 (env (set-make))
	 (locals (set-make))
	 (obj-envs nil)
	 (*toplevel* t)
	 (new-form (transform-tree form)))
    (declare (special env locals obj-envs))
    (mapc #'transform-obj-env obj-envs)
    (let ((toplevel-vars (set-elems env)))
      (set-remove-all env)
      (append (list (car new-form) toplevel-vars)
	      (cdr new-form)))))

(defun lift-defuns (form)
  (let (defuns oth)
    (loop for el in form do
	 (if (eq (car el) :defun) (push el defuns)
	     (push el oth)))
    (append (reverse defuns) (reverse oth))))

(defun shallow-process-function-form (form old-env lexenv-chain)
  (let* ((env (set-copy old-env))
	 (*lexenv-chain* (cons env lexenv-chain))
	 (locals (set-make))
	 (arglist (mapcar #'->sym (third form)))
	 (obj-envs nil)
	 (new-form (transform-tree (fourth form)))
	 (name (and (second form) (->sym (second form)))))
    (declare (special env locals obj-envs))
    (mapc (lambda (arg) (set-add env arg)) arglist)
    (mapc #'transform-obj-env obj-envs)
    (set-add env name) ;;inject function name (if any) into it's lexical environment
    (list (js-intern (first form)) ;;defun or function
	  (dump-lexenv-chain) ;;
	  name arglist (set-elems locals) (lift-defuns new-form))))

(defun process-ast (ast)
  (assert (eq :toplevel (car ast)))
  (let ((lmbd-forms (queue-make)))
    (declare (special lmbd-forms))
    (let ((toplevel (shallow-process-toplevel-form ast)))
      (loop until (queue-empty? lmbd-forms)
	 for (form env position lexenv-chain) = (queue-dequeue lmbd-forms) do
	   (let ((funct-form (shallow-process-function-form form env lexenv-chain)))
	     (setf (car position) (car funct-form)
		   (cdr position) (cdr funct-form))))
      toplevel)))
