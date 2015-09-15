(require 'asdf)
(in-package :sb-impl)
(defun string-hash (str)
  (sxhash (string-upcase str)))
(define-hash-table-test 'string-equal #'string-equal #'string-hash)

(defun in-biodir (path)
  (format nil "~A/~A" (posix-getenv "BIODIR") path))

(let ((biodir (posix-getenv "BIODIR")))
  (push (in-biodir "uffi/") asdf:*central-registry*)
  (push (in-biodir "clsql/") asdf:*central-registry*))

(asdf:operate 'asdf:load-op :uffi)
(asdf:operate 'asdf:load-op :clsql)

;(asdf:operate 'asdf:load-op :clsql-mysql)
(load (in-biodir "clinit.cl"))
(load (in-biodir "portableaserve/INSTALL.lisp"))
(load (in-biodir "clocc/load-xml.lisp"))

