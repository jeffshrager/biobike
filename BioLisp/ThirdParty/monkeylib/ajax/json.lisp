(in-package :com.gigamonkeys.ajax.json)

(defun json (sexp)
  "This should only be used with sexps containing types that can
  be encoded in JSON syntax via the sexp->json generic function.
  By default this include hashtables, sequences, numbers,
  strings, NIL, T, and the keywords :true, :false, and :null."
  (with-output-to-string (out)
    (with-foo-output (out :pretty t)
      (process 
       *javascript*
       (new-pretty-printer)
       (downcase-symbols (check-symbols (sexp->json sexp)))
       (new-env 'statement-or-expression :expression (top-level-environment *javascript*))))))

(defgeneric sexp->json (sexp))

(defmethod sexp->json ((sexp t))
  (error "JSON doesn't support encoding objects of class ~a." (class-name (class-of sexp))))

(defmethod sexp->json ((sexp number)) sexp)
(defmethod sexp->json ((sexp string)) sexp)
(defmethod sexp->json ((sexp (eql t))) :true)
(defmethod sexp->json ((sexp (eql nil))) :false)
(defmethod sexp->json ((sexp (eql :true))) :true)
(defmethod sexp->json ((sexp (eql :false))) :false)
(defmethod sexp->json ((sexp (eql :null))) :null)

(defmethod sexp->json ((sexp sequence))
  `(array ,@(map 'list #'sexp->json sexp)))

(defmethod sexp->json ((sexp hash-table))
  `(object ,@(loop for k being the hash-keys of sexp using (hash-value v) collect k collect (sexp->json v))))

(defun dictionary (&rest key-values)
  (let ((h (make-hash-table)))
    (loop for (k v) on key-values by #'cddr do (setf (gethash k h) v))
    h))

(defun boolify (object) (not (not object)))

(defun check-symbols (sexp)
  (typecase sexp
    (cons 
     (check-symbols (car sexp))
     (check-symbols (cdr sexp)))
    ((member sexp object array nil) t)
    (keyword t)
    (symbol 
     (unless (member sexp '(true false null) :test #'string=)
       (error "Bad JSON symbol ~a" sexp))))
  sexp)

(defun downcase-symbols (sexp)
  (typecase sexp
    ((member object array nil)  sexp)
    (cons (cons (downcase-symbols (car sexp)) (downcase-symbols (cdr sexp))))
    (symbol (intern (string-downcase (symbol-name sexp)) (symbol-package sexp)))
    (t sexp)))

