(in-package :com.gigamonkeys.asdf-extensions)

(defclass concat-fasls (operation)
  ((concatenator :initarg :concatenator :initform #'(lambda (f) (format t "~&~a~%" f)))))

(defun build-one-fasl (system &optional (output (output-fasl system)))
  (with-open-file (out output :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
    (flet ((add-file (file)
	     (format t "~&;; Appending ~a~%" (truename file))
	     (with-open-file (in file :element-type '(unsigned-byte 8))
	       (loop with buffer = (make-array 4096 :element-type '(unsigned-byte 8))
		  for read = (read-sequence buffer in)
		  while (plusp read) do
		    (write-sequence buffer out :end read)))))
      (oos 'concat-fasls :triple-store :concatenator #'add-file))))

(defun output-fasl (system-name)
  (make-pathname :name (string-downcase system-name) :type (fasl-type)))

(defun fasl-type ()
  (pathname-type (compile-file-pathname (make-pathname :type "lisp"))))

(defmethod perform ((o concat-fasls) (c cl-source-file))
  (with-slots (concatenator) o
    (map nil concatenator (input-files o c))))

(defmethod perform ((operation concat-fasls) (c static-file)) nil)

(defmethod operation-done-p ((operation concat-fasls) (c cl-source-file)) nil)
(defmethod operation-done-p ((operation concat-fasls) (c static-file)) t)

(defmethod output-files ((operation concat-fasls) (c component)) nil)

(defmethod component-depends-on ((operation concat-fasls) (c cl-source-file))
  (cons (list 'compile-op (component-name c))
        (call-next-method)))

;; Nicked from load-source-op more or less.
(defmethod component-depends-on ((o concat-fasls) (c component))
  (let ((what-would-load-op-do (cdr (assoc 'load-op
                                           (slot-value c 'asdf::in-order-to)))))
    (mapcar #'(lambda (dep)
		(if (eql (car dep) 'load-op)
		    (cons 'concat-fasls (cdr dep))
		    dep))
            what-would-load-op-do)))
