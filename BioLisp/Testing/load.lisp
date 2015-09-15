;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: user; -*-

(in-package :cl-user)

(defparameter *test-mechanism-files*
  `(
    "package"
    "test-utils"
    "test-mechanism-code"
    ))

;; Make it so the test system can be loaded independently.

(let ((actual-load-path nil))
  (handler-case
      ;; this will error if no logical host named 'websrc' is defined.
      (setq actual-load-path (cl-user:translate-simple-lp "websrc:Testing;"))
    ;; Catch the error and try to load from the current directory.
    (error
     ()
     (format t "~%;; Warning: No logical host WEBSRC exists!")
     (format t "~%;; Loading from current directory.")
     (if *load-pathname*
         (setq actual-load-path 
               (namestring (dirpath-from-path *load-pathname*)))
       (error "No logical host named WEBSRC and *LOAD-PATHNAME* is NIL!")
       )))
  (load-system* actual-load-path *test-mechanism-files*)
  )

(when (fboundp 'provides) (funcall 'provides :test-mechanism))


