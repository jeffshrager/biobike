;;; -*- Package: help; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :help)

;;; +=========================================================================+
;;; | Copyright (c) 2002-2006 JP Massar, Jeff Shrager, Peter Seibel           |
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

;;; Authors:  JP Massar, Peter Seibel.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; HTML for module and module list display 

(publish
 :path *help-modules-url*
 :content-type cl-user::*html-publish-content-type* 
 :function
 (lambda (req ent)
   (let* ((input (request-query req))
          (user-package-name (url-parameter-value :pkg input))
          (user-package-symbol (keywordize user-package-name)))
     (execute-with-standard-weblistener-environment
      req ent user-package-symbol
      (lambda () (html-for-help-modules))
      ))))

(defun html-for-help-modules ()
  (html
   (with-standard-weblistener-page-header ("Modules" :listener? nil)
     (:table
      (:tr (:td (:b "Name")) (:td (:b "Description")))
      (loop for module in (sort (modules) 'string< :key 'help:name) 
            for name = (help:name module)
            for url = (make-help-module-url :name name)
            when (and (module-is-to-be-displayed? module) (toplevel? module))
            do
            (html (:tr 
                   (:td ((:a :href  url) 
                         (:princ-safe (formatn "~(~a~)" (help:name module)))))
                   (:td (:princ-safe (help:docstring module))))))))))

(defun module-is-to-be-displayed? (module)
  (let ((modes (help::display-modes module)))
    (or (member :all modes :test 'symbol=)
        (null modes)
        (case (forward-funcall 'wb::user-mode)
          (:bbl (intersection '(:bbl :biobike) modes :test 'symbol=))
          (:biolisp (intersection '(:lisp :biolisp) modes :test 'symbol=))
          (t nil)
          ))))

(publish 
 :path *help-module-url*
 :content-type cl-user::*html-publish-content-type*  
 :function
 (lambda (req ent)
   (let* ((input (request-query req))
          (package-name (url-parameter-value :pkg input))
          (package-symbol (keywordize package-name))
          (name (url-parameter-value :name input))
          (docobj (help::find-documentation name 'help:module))
          )
     (execute-with-standard-weblistener-environment
      req ent package-symbol
      (lambda ()
        (com.gigamonkeys.foo:with-html-output ((request-reply-stream req))
          (html-for-help-module-listing name docobj))
        )))))

(defun html-for-help-module-listing (name module)
  (if (null module)
      (oops-no-docobj name "Module")
    (with-slots 
        (name docstring text keywords see-also 
              referred-to-by functions variables macros submodules author)
        module
      (let ((title (formatn "Module ~a" name)))
        (html
         (with-standard-weblistener-page-header (title)
           (html-for-module-help-links)
           (html (:big ((:font :color "green") (:p (:princ-safe docstring)))))
           (html-for-text-information
            text :size :h2 :paragraph-after-title? nil)
           (when submodules 
             (html (:h2 "Submodules"))
             (submodule-table submodules))
           (when functions
             (html (:h2 "Functions"))
             (thing-table functions 'function-documentation))
           (when variables
             (html (:h2 "Variables"))
             (thing-table variables 'variable-documentation))
           (when macros
             (html (:h2 "Macros"))
             (thing-table macros 'macro-documentation))
           (html-for-keyword-information keywords)
           (html-for-see-also-information see-also t)
           (html-for-referred-to-by-information referred-to-by)
           (html-for-author-information author)
           ))))))

(defun html-for-module-help-links (&optional symbol)
  (let ((all-modules-url (make-help-modules-url)))
    (html
     ((:a :href all-modules-url) "All Modules"))
    (when symbol
      (let ((containing-modules
             (lmaphashnn 
              (lambda (key module) 
                (declare (ignore key))
                (and (member symbol (help:functions module)) module))
              (gethash 'help:module *documentation*)
              )))
        (when containing-modules 
          (html " | Modules: ")
          (loop for modules on containing-modules 
                as module = (first modules) do
                (multiple-value-bind (url label)
                    (docobj->url&label module)
                  (html 
                   ((:a :href url) (:princ-safe label))
                   (when (cdr modules) (html ", "))
                   ))))))))


(defun thing-table (names type)
  (if names
      (html 
       (:table
        (:tr (:td (:b "Name")) (:td (:b "Description")))
        (loop for name in (sort (copy-seq names) #'string<)
              as alias-of = (get name :alias-of)
              for url = (forward-package-funcall :wb :symbol-doc-url name type)
              for docs = (find-documentation name type) 
              do
              (when (and (eq type 'function-documentation) alias-of)
                (setq docs (find-documentation alias-of type)))
              (html 
               (:tr 
                (:td ((:a :href (or url "NONE")) (:princ-safe name)))
                (:td (:princ-safe 
                      (or (and docs (docstring docs)) "Undocumented"))))))))
    (html (:p (:i "None")))))

(defun submodule-table (submodule-symbols)
  (html 
   (:table
    (:tr (:td (:b "Name")) (:td (:b "Description")))
    (loop for name in submodule-symbols
          as sname = (string name)
          for url = (make-help-module-url :name sname)
          for docs = (find-documentation sname 'module)
          do
          (html 
           (:tr 
            (:td ((:a :href url) (:princ-safe sname)))
            (:td (:princ-safe 
                  (or (and docs (docstring docs)) "Undocumented")))))))))


