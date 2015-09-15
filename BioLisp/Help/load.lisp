;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: user; -*-

(in-package :cl-user)

;;; +=========================================================================+
;;; | Copyright (c) 2005 Peter Seibel, JP Massar                              |
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

(pushnew 
 (make-pathname :name nil :type nil :defaults *load-truename*) 
 asdf:*central-registry* :test #'equal)

(if cl-user::*force-load-system*-recompile*
    (asdf:oos 'asdf:load-op :com.biobike.help :force t)
  (asdf:operate 'asdf:load-op :com.biobike.help))

(defparameter *weblistener-help-files* 
  '(
    "packages"
    "doc-definer"
    "doc-objects"
    "document-function"
    "live-tutorial"
    "doc-utils"
    "see-also"
    "examples"
    "externaldf"
    "doc-html"
    "see-also-html"
    "module-html"
    "document-function-html"
    "glossary-html"
    "live-tutorial-html"
    "jhelp-control-panel"
    "jhelp-structs"
    "jhelp-stop-words"
    "jhelp-english-suffixes"
    "jhelp-stemmer"
    "help"
    "help-utils"
    "help-single"
    "help-multiple"
    "help-html"
    "lisp-urls"
    "jhelp-multiple"
    "jhelp-engine-run"
    "jhelp-engine-monitor"
    "jhelp-engine-build"
    "jhelp-init"
    "jhelp-html"
    ;;  "jhelp-handle-box-wksp-req" 
    ;;forward symbols in let macro require delayed loading.
    "jhelp-load-oldhelp"
    "jhelp-pages"
    "jhelp-load-jhelp"	
    ))

(load-system* "websrc:Help;" *weblistener-help-files*)

(when (fboundp 'provides) (funcall 'provides :Help))


