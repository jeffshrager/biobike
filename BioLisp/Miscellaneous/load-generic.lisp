(in-package :cl-user)

;;; +=========================================================================+
;;; | Copyright (c) 2002, 2003, 2004 JP Massar, Jeff Shrager, Mike Travers    |
;;; |                                                                         |
;;; | Permission is hereby granted, free of charge, to any person obtaining   |
;;; | a copy of this software and associated documentation files (the         |
;;; | "Software"), to deal in the Software without restriction, including     |
;;; | without limitation the rights to use, copy, modify, merge, publish,     |
;;; | distribute, sublicense, and/or sell copies of the Software, and to      |
;;; | permit persons to whom the Software is furnished to do so, subject to   |
;;; | the following conditions:                                               |
;;; |                                                                         |
;;; | The above copyright notice and this permission notice shall be included |
;;; | in all copies or substantial portions of the Software.                  |
;;; |                                                                         |
;;; | THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,         |
;;; | EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF      |
;;; | MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  |
;;; | IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY    |
;;; | CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,    |
;;; | TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE       |
;;; | SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                  |
;;; +=========================================================================+

;;;; *****>  THIS FILE BELONGS IN THE TOPLEVEL CLOCC DIRECTORY  <*****

;;; Provides a mechanism to load the files needed to access the XML
;;; functionality within CLOCC/CLLIB.

#+:ALLEGRO
;; Hack to make REQUIRE load a .fasl extension.
;; (And make Corman not blow up encountering #p)
(eval (read-from-string
       "(push #p(:type \"fasl\") sys:*require-search-list*)"))


(defvar *clocc-root* 
  (namestring
   (make-pathname
    :host (pathname-host *load-truename*)
    :device (pathname-device *load-truename*)
    :directory (pathname-directory *load-truename*)
    :name nil :type nil :version nil
    )))

(let ((*compile-print* nil) (*load-print* nil) (*load-verbose* t))

  ;; This defines the CLOCC logical host, and translations for it.

  (load (progn (concatenate 'string *clocc-root* "clocc.lisp")))

  ;; Load DEFSYSTEM

  (let* ((defsystem-source-file 
	     (translate-logical-pathname 
	      "clocc:src;defsystem;defsystem.lisp"))
	 (defsystem-binary-file
	     (compile-file-pathname defsystem-source-file)))
    (declare (ignorable defsystem-binary-file))
    #-:CORMANLISP
    (if (probe-file defsystem-binary-file)
	(load defsystem-binary-file)
      (load (compile-file defsystem-source-file)))
    #+:CORMANLISP
    (load defsystem-source-file)
    ))

;; * compile some systems
(setq mk::*central-registry*
      (nconc
       (list
        ;; select the systems you want to use
        (translate-logical-pathname "clocc:src;cllib;")
        (translate-logical-pathname "clocc:src;ext;queues;")
        (translate-logical-pathname "clocc:src;ext;union-find;")
        (translate-logical-pathname "clocc:src;f2cl;packages;")
        (translate-logical-pathname "clocc:src;f2cl;")
        (translate-logical-pathname "clocc:src;port;")
        (translate-logical-pathname "clocc:src;port;configuration;")
        (translate-logical-pathname "clocc:src;port;environment;")
        (translate-logical-pathname "clocc:src;tools;metering;"))
       mk::*central-registry*))





