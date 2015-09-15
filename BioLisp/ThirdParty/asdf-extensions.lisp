(in-package :cl-user)
(defpackage :com.gigamonkeys.asdf-extensions
  (:use :cl :asdf)
  (:export :register :include))

(in-package :com.gigamonkeys.asdf-extensions)

(defun load-directory ()
  (make-pathname :directory (pathname-directory *load-truename*)))

(defun register (dir)
  (let ((*default-pathname-defaults* (load-directory)))
    (push (merge-pathnames dir) *central-registry*)))

(defun include (dir)
  (let ((*default-pathname-defaults* (load-directory)))
    (load (merge-pathnames (merge-pathnames (parse-namestring ".asdf") dir)))))

(defun register-and-include (dir)
  (register dir)
  (include dir))

