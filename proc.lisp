(in-package :js)
;;
(defmacro with-new-lexical-environment ((var new-env) &body body)
  `(let* ((lexenv-chain (cons ,new-env lexenv-chain))
	  (,var (copy-list lexenv-chain)))
     (declare (special lexenv-chain)) ; ... later
     (progn (push ,var environments) ,@body)))

(defun transform-tree (form)
  (cond ((null form) nil)
	((atom form)
	 (if (keywordp form)
	     (js-intern form) form))
	(t (apply-transform-rule (car form) form))))

(defmacro define-transform-rule ((rule form) &body body)
  `(defmethod apply-transform-rule ((,(gensym) (eql ,rule)) ,form)
     (declare (special locals environments lexenv-chain toplevel))
     ,@body))

(defgeneric apply-transform-rule (keyword form)
  (:method (keyword form)
    (declare (ignore keyword))
    (mapcar #'transform-tree form)))

(define-transform-rule (:var form)
  (cons (js-intern (car form))
	(list (mapcar
	       (lambda (var-desc)
		 (let ((var-sym (car var-desc)))
		   (set-add locals var-sym)
		   (cons var-sym
			 (transform-tree (cdr var-desc)))))
	       (second form)))))

(define-transform-rule (:object form)
  (list (js-intern (car form))
	(mapcar (lambda (el)
		  (list (car el) (transform-tree (cdr el)))) (second form))))

(defparameter *label-name* nil)

(define-transform-rule (:label form)
  (let ((*label-name* (->usersym (second form))))
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

(define-transform-rule (:try form)
  (let* ((var (car (third form))))
    (with-new-lexical-environment (env (list var))
      (list (js-intern (car form))
	    env
	    (transform-tree (second form)) ;body
	    var ;catch var
	    (transform-tree (cdr (third form))) ;catch body
	    (transform-tree (fourth form)))))) ;finally block

#+nil (define-transform-rule (:name form)
  (list (js-intern (car form)) (->sym (second form))))

#+nil (define-transform-rule (:dot form)
  (list (js-intern (car form)) (transform-tree (second form))
	(->sym (third form))))

(define-transform-rule (:with form)
  (with-new-lexical-environment (env :obj)
    (list (js-intern (car form))
	  env
	  (transform-tree (second form))
	  (transform-tree (third form)))))

(defun transform-function (form)
  (flet ((lift-defuns (form)
	   (let (defuns oth)
	     (loop for el in form do
		  (if (eq (car el) '!defun) (push el defuns)
		      (push el oth)))
	     (append (reverse defuns) (reverse oth)))))
    (let* (toplevel
	   (locals (set-make))
	   (arglist (third form))
	   (name (second form)))
      (declare (special locals environments lexenv-chain toplevel))
      (with-new-lexical-environment (env (list name arglist locals))
	(let ((new-form (lift-defuns (transform-tree (fourth form)))))
	  (list (js-intern (first form))
		env
		name
		arglist
		(set-elems locals)
		new-form))))))

(define-transform-rule (:defun form)
  (let ((fun-name (second form)))
    (unless toplevel
      (set-add locals fun-name))
    (transform-function form)))

(define-transform-rule (:function form)
  (transform-function form))

(define-transform-rule (:toplevel form)
  (declare (special exsisting-env))
  (let ((lexenv-chain (append exsisting-env lexenv-chain)))
    (declare (special lexenv-chain))
    (list (js-intern (car form))
	  (copy-list lexenv-chain)
	  (transform-tree (second form)))))
;;
(labels ((dump-decl-env (env)
	   (let ((rest (append (second env)
			       (and (third env) (set-elems (third env))))))
	     (if (first env) (cons (first env) rest) rest)))
	 (dump (el)
	   (if (listp el) (dump-decl-env el) el)))
  (defun transform-obj-env (place)
    (setf (car place) (dump (car place)))
    (setf (cdr place) (mapcar #'dump (cdr place)))))

(defun process-ast (ast &optional (exsisting-env '(:obj)))
  (assert (eq :toplevel (car ast)))
  (let* ((lexenv-chain nil)
	 (exsisting-env (mapcar (lambda (el)
				  (if (listp el) (list nil el nil) el)) exsisting-env))
	 (locals (set-make))
	 (environments nil)
	 (toplevel t)
	 (new-form (transform-tree ast)))
    (declare (special locals environments lexenv-chain toplevel exsisting-env))
    (format t ">>> ~A~%" environments)
    (mapc #'transform-obj-env environments)
    (let ((toplevel-vars (set-elems locals)))
      (append (list (car new-form) toplevel-vars)
	      (list (mapcar (lambda (el) (if (listp el) (second el) el)) (cadr new-form)))
	      (cddr new-form)))))
