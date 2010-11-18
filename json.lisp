(in-package :cl-js)

(defvar *reading-slot-name* nil)

(defun is-whitespace (char)
  (member char '(#\space #\newline #\return #\tab)))

(defun ends-atom (char)
  (or (is-whitespace char) (member char '(#\) #\] #\} #\, #\:))))

(defun skip-whitespace (stream)
  (loop :while (is-whitespace (peek-char nil stream nil))
        :do (read-char stream)))

(defun at-eof (stream)
  (eql (peek-char nil stream nil :eof) :eof))

(defun json-error (message &rest args)
  (apply 'js-error :syntax-error message args))

(defun parse-json (string)
  (with-input-from-string (in string)
    (let ((value (read-json in)))
      (skip-whitespace in)
      (unless (at-eof in)
        (json-error "Unused characters at end of input."))
      value)))

(defun read-json (stream)
  (skip-whitespace stream)
  (case (peek-char nil stream nil :eof)
    (:eof (json-error "Unexpected end of input."))
    ((#\" #\') (read-json-string stream))
    (#\[ (read-json-array stream))
    (#\{ (read-json-object stream))
    (t (read-json-atom stream))))

(defun read-json-string (stream)
  (labels ((interpret (char)
             (if (eql char #\\)
                 (let ((escaped (read-char stream)))
                   (case escaped
                     (#\u (read-unicode))
                     (#\b #\backspace) (#\n #\newline) (#\r #\return)
                     (#\t #\tab) (#\f #\page) (t escaped)))
                 char))
           (read-unicode ()
             (code-char (loop :for pos :from 0 :below 4
                              :for weight :of-type fixnum := #.(expt 16 3) :then (ash weight -4)
                              :for digit := (digit-char-p (read-char stream) 16)
                              :do (unless digit (json-error "Invalid unicode constant in string."))
                              :sum (* digit weight)))))
    (with-output-to-string (out)
      (handler-case
          (loop :with quote :of-type character := (read-char stream)
                :for next :of-type character := (read-char stream)
                :until (eql next quote)
                :do (write-char (interpret next) out))
        (end-of-file () (json-error "Encountered end of input inside string constant."))))))

(defun gather-comma-separated (stream end-char obj-name gather-func)
  (declare (type character end-char)
           (type function gather-func))
  ;; Throw away opening char
  (read-char stream)
  (let ((finished nil))
    (loop
     (skip-whitespace stream)
     (let ((next (peek-char nil stream nil #\nul)))
       (declare (type character next))
       (when (eql next #\nul)
         (json-error "Encountered end of input inside ~A." obj-name))
       (when (eql next end-char)
         (read-char stream)
         (return))
       (when finished
         (json-error "Comma or end of ~A expected, found '~A'" obj-name next)))
     (funcall gather-func)
     (skip-whitespace stream)
     (if (eql (peek-char nil stream nil) #\,)
         (read-char stream)
         (setf finished t)))))

(defun read-json-array (stream)
  (let ((accum (empty-fvector 20 0)))
    (gather-comma-separated
     stream #\] "list"
     (lambda ()
       (vector-push-extend (read-json stream) accum)))
    (build-array accum)))

(defun read-json-object (stream)
  (let ((obj (js-obj)))
    (gather-comma-separated 
     stream #\} "object literal"
     (lambda ()
       (let ((slot-name (let ((*reading-slot-name* t)) (read-json stream))))
         (unless (or (typep slot-name 'string) (typep slot-name 'number))
           (json-error "Invalid slot name in object literal: ~A" slot-name))
         (skip-whitespace stream)
         (when (not (eql (read-char stream nil) #\:))
           (json-error "Colon expected after '~a'." slot-name))
         (setf (js-prop obj slot-name) (read-json stream)))))
    obj))

(defun looks-like-a-number (string)
  (let ((string (coerce string 'simple-string)))
    (every (lambda (char)
             (or (digit-char-p char)
                 (member char '(#\e #\E #\. #\- #\+))))
           string)))

(defun read-json-atom (stream)
  (let ((accum (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t)))
    (loop
     (let ((next (peek-char nil stream nil :eof)))
       (when (or (ends-atom next) (eql next :eof))
         (return))
       (vector-push-extend next accum)
       (read-char stream)))
    (let ((number-val (and (looks-like-a-number accum)
                           (ignore-errors (read-from-string accum)))))
      (cond ((numberp number-val) number-val)
            ((string= accum "false") nil)
            ((string= accum "true") t)
            ((string= accum "null") :null)
            ((string= accum "undefined") :null)
            ((and *reading-slot-name*
                  (every (lambda (c)
                           (declare (type character c))
                           (or (alphanumericp c) (eql c #\_) (eql c #\$)))
                         accum))
             accum)
            (t (json-error "Unrecognized value in JSON data: ~A" accum))))))

(defvar *replacer*)

(defun process-replacer (repl)
  (typecase repl
    (js-func (lambda (key val)
               (let ((result (js-call repl *env* key val)))
                 (values result (not (eq result :undefined))))))
    (js-array (let ((vec (js-array-vec repl)))
                (lambda (key val)
                  (values val
                          (loop :for elt :across vec :do (when (js== elt key) (return t)))))))
    (js-null (lambda (key val) (declare (ignore key)) (values val t)))
    (t (js-error :range-error "The second argument to JSON.stringify should be either a function or an array."))))

(defun stringify-json (value replacer)
  (let ((*replacer* (process-replacer replacer)))
    (with-output-to-string (out)
      (write-json value out))))

(defun write-json (element stream)
  (typecase element
    (aobj (write-json-array element stream))
    (obj (write-json-obj element stream))
    (string (write-json-string element stream))
    (js-number (write-json-number element stream))
    (boolean (write-string (if element "true" "false") stream))
    (js-null (write-string "null" stream))
    (t (write-json-string (to-string element) stream))))

(defun write-json-string (string stream)
  (declare (stream stream))
  (let ((string (coerce string 'simple-string)))
    (write-char #\" stream)
    (loop :for ch :of-type character :across string :do
       (let ((code (char-code ch)))
         (declare (fixnum code))
         (cond ((< code 14) (princ (case ch (#\backspace "\\b") (#\newline "\\n") (#\return "\\r")
                                         (#\page "\\f") (#\tab "\\t") (t ch)) stream))
               ((eq code 92) (write-string "\\\\" stream))
               ((eq code 34) (write-string "\\\"" stream))
               (t (write-char ch stream))))))
  (write-char #\" stream))

(defun write-json-number (number stream)
  (typecase number
    (integer (write number :stream stream))
    (double-float (format stream "~,,,,,,'eE" number))
    (t (write-string (ecase number (:NaN "NaN") (:Inf "Infinity") (:-Inf "-Infinity")) stream))))

(defun write-json-obj (obj stream)
  (write-char #\{ stream)
  (let ((first t))
    (flet ((write-prop (key)
             (multiple-value-bind (val include) (funcall *replacer* key (js-prop obj key))
               (when include
                 (if first (setf first nil) (write-char #\, stream))
                 (write-json (to-string key) stream)
                 (write-char #\: stream)
                 (write-json val stream)))))
      (js-for-in obj #'write-prop t)))
  (write-char #\} stream))

(defun write-json-array (arr stream)
  (write-char #\[ stream)
  (loop :for i :from 0 :for val :across (js-array-vec arr) :for first := t :then nil :do
     (unless first (write-char #\, stream))
     (write-json (funcall *replacer* i val) stream))
  (write-char #\] stream))
