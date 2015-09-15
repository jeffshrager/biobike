(in-package :cl-user)

#-asdf (load (make-pathname :name "asdf" :type "lisp" :defaults *load-truename*))
(load (make-pathname :name "asdf-extensions" :type "lisp" :defaults *load-truename*))

(com.gigamonkeys.asdf-extensions:include "monkeylib/")
(com.gigamonkeys.asdf-extensions:register "twas/")
(com.gigamonkeys.asdf-extensions:register "split-sequence/")
(com.gigamonkeys.asdf-extensions:register "slime/")

(when (fboundp 'provides) (funcall 'provides :third-party))

