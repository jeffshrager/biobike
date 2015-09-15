;;
;; Copyright (c) 2005, Gigamonkeys Consulting All rights reserved.
;;

;; Indentation for javascript sexp language.

(put 'destructure 'common-lisp-indent-function (get 'destructuring-bind 'common-lisp-indent-function))
(put 'dokids 'common-lisp-indent-function (get 'dotimes 'common-lisp-indent-function))

(put 'with-dialog-values 'common-lisp-indent-function (get 'with-slots 'common-lisp-indent-function))