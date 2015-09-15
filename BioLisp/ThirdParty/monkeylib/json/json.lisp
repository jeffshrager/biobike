(in-package :com.gigamonkeys.json)

;; JSON: The top-level function for converting Lisp objects into a
;; string in the JSON format. It can convert any object that can be
;; converted to a json-exp.

;; TO-JSON: generic function for converting an arbitrary Lisp object
;; into a json-exp that the JSON function knows how to render into
;; JSON format. To make an arbitrary class convertable to JSON, add a
;; method to this generic function that generates a json-exp.

;; TO-JAVASCRIPT: Converts a json-exp into FOO Javascript language.
;; json-exps consist 

#+(or)(defun json (data)
  "This should only be used with sexps containing types that can
  be encoded in JSON syntax via the to-json generic function.
  By default this include hashtables, sequences, numbers,
  strings, NIL, T, and the keywords :true, :false, and :null."
  (with-output-to-string (out)
    (with-foo-output (out :pretty t)
      (let ((javascript (make-instance 'javascript)))
	(process 
	 javascript
	 (new-pretty-printer)
	 (to-javascript (to-json data))
	 (new-env 'statement-or-expression :expression (top-level-environment javascript)))))))

(defun json (data)
  (with-output-to-string (out)
    (emit-json (to-json data) out)))

(defgeneric emit-json (object stream))

(defmethod emit-json ((object t) stream)
  (declare (ignore stream))
  (error "Can't encode ~a in JSON." (class-name (class-of object))))

(defmethod emit-json ((object (eql nil)) stream)
  (write-string "{}" stream))

(defmethod emit-json ((object cons) stream)
  (write-char #\{ stream)
  (loop for (key value . rest) on object by #'cddr do 
       (emit-json (json-stringify key) stream)
       (write-char #\: stream)
       (emit-json (to-json value) stream)
       when rest do (write-char #\, stream))
  (write-char #\} stream))

(defmethod emit-json ((object vector) stream)
  (write-char #\[ stream)
  (loop with len = (length object)
     for i from 0 below len
     do (emit-json (to-json (aref object i)) stream)
     when (< i (1- len)) do (write-char #\, stream))
  (write-char #\] stream))

(defmethod emit-json ((object symbol) stream)
  (write-string (json-stringify object) stream))
				 
(defmethod emit-json ((object string) stream)
  (write-char #\" stream)
  (loop for char across object do (emit-json-char char stream))
  (write-char #\" stream))

(defun emit-json-char (char stream)
  (case char
    (#\" (write-string "\\\"" stream))
    (#\\ (write-string "\\\\" stream))
    (#\/ (write-string "\\/" stream))
    (#.(code-char 8) (write-string "\\b" stream))
    (#.(code-char 9) (write-string "\\t" stream))
    (#.(code-char 10) (write-string "\\n" stream))
    (#.(code-char 12) (write-string "\\f" stream))
    (#.(code-char 13) (write-string "\\r" stream))
    (t
     (cond
       ((<= 0 (char-code char) #x1f)
	(format stream "\\u~4,'0x" (char-code char)))
       (t (write-char char stream))))))

(defmethod emit-json ((object number) stream)
  (write-string (json-stringify object) stream))

(defmethod emit-json ((object (eql t)) stream)
  (emit-json :true stream))
  
(defmethod json-stringify (object)
  (error "Can't stringify ~a" object))

(defmethod json-stringify ((object string)) object)

(defmethod json-stringify ((object symbol))
  (unless (keywordp object)
    (error "Only keywords allowed in JSON-EXPs. Got ~a in package ~a" object (package-name (symbol-package object))))
  (string-downcase (symbol-name object)))

(defmethod json-stringify ((object number))
  (let ((*read-default-float-format* 'double-float))
    (let ((float (float object 0d0)))
      (if (= (round float) float)
	  (prin1-to-string (round float))
	  (prin1-to-string float)))))
    

(defgeneric to-json (thing)
  (:documentation "Convert an arbitrary Lisp object to a json-exp.
  This method is probably the right thing for "))

(defmethod to-json ((thing t)) thing)

(defmethod to-json ((thing hash-table))
  (loop for k being the hash-keys of thing using (hash-value v) collect k collect v))

;;; This function converts a json-exp to the format understood by 

(defgeneric to-javascript (thing)
  (:documentation "Convert data to the an sexp that can be converted to Javascript code (i.e. JSON)."))

(defmethod to-javascript ((thing t))
  (error "JSON doesn't support encoding objects of class ~a." (class-name (class-of thing))))

(defmethod to-javascript ((thing string)) thing)

(defmethod to-javascript ((thing number)) thing)

(defmethod to-javascript ((thing (eql t))) (to-javascript :true))

(defmethod to-javascript ((thing symbol))
  (cond
    ((keywordp thing) (string-downcase (symbol-name thing)))
    (t (error "Only keyword symbols allowed in json-exps: ~s." thing))))

(defmethod to-javascript ((thing vector)) 
  `(array ,@(mapcar #'(lambda (x) (to-javascript (to-json x))) (coerce thing 'list))))

(defmethod to-javascript ((thing cons))
  `(object ,@(loop for (k v) on thing by #'cddr collect (intern (string-downcase k) :keyword) collect (to-javascript (to-json v)))))

(defmethod to-javascript ((thing (eql nil))) '(object))


