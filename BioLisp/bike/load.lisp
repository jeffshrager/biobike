;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: user; -*-

(in-package :cl-user)

(load 
 (merge-pathnames 
  "bbl-package.lisp"
  *load-pathname*
  ))

(defparameter *bbl-language-files* 
  '(
    ;; "bbl-package.lisp"
    "bbl-data-load.lisp"
    "bbl-instance.lisp"
    #-:sframes
    "non-sframes.lisp"
    #+:sframes
    "sframes.lisp"
    ;;  "tours.lisp"  (inactive live-tours)       
    "bbi-definitions.lisp"
    "1level.lisp"
    "assign.lisp"
    "increment.lisp"
    "aux2level.lisp"
    "2level.bike"
    "postaux2level.lisp"
    "sequence-of.lisp"
    "replace.lisp"
    "aux3level.lisp"
    "3level.bike"
    "aux4level.lisp"
    "4level.bike"
    "postaux4level.lisp"
    ;; Moved to .../ncbi/contig-extraction.lisp
    ;; "ncbi-extraction.lisp"
    "display-context-of.bike"
    "microarray-data.lisp"
    "aux5level.lisp"
    "blast-aux.lisp"
    "5level.bike"
    "postaux5level.lisp"
    "6level.bike"
    "aux7level.lisp"
    "7level.bike"
    "load-private-organisms-aux.lisp"
    "load-private-organisms.bike"
    "bbi-data-table-functions.bike"
    "bbi-pssm-functions.bike"
    "markov-functions-aux.lisp"
    "bbi-markov-functions.bike"
    "bbi-rnaz-functions.bike"
    "phylip-aux.lisp"
    "bbi-phylip-functions.bike"
    "phylip-post-aux.lisp"
    "kegg-aux.lisp"
    "bbi-KEGG-APIs-functions.bike"
    "pm.lisp"
    "share.lisp"
    "annotation-functions.bike"
    "parse-code-aux.lisp"
  ; "parse-code.bike"
    "parse-code.lisp"
    ))

(setq utils::*safety* nil)

(with-compilation-unit ()
  (let ((cp *load-pathname*))
    (flet ((doit (x) 
             (let ((fp (merge-pathnames x cp)))
               (cond
                ((string-equal (pathname-type fp) "lisp") (utils:c/l fp))
                ((string-equal (pathname-type fp) "bike") 
                 (bbi::bbcl fp)
                 )
                (t (error "Don't know how to load ~S" fp))
                ))))
      (mapcar #'doit *bbl-language-files*)
      )))

(setq utils::*safety* t)

(when (fboundp 'provides) (funcall 'provides :bbl))
