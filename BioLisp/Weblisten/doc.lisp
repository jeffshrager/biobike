;; -*- mode: Lisp; Syntax: Common-Lisp; Package: wb; -*-

(in-package :wb)

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

;;; Author:  JP Massar.

;;; Access via the Weblistener to the Documentation directory files.

(defparameter *doc-directory-url* "/weblistenerdocs/")

(publish-directory
 :prefix *doc-directory-url*
 :destination 
 help::*doc-directory-path*
 )

;; This is a hack so we can record when a user goes to the main documentation 
;; page from the weblistener.  Unfortunately, there is no way to record
;; the other documentation pages he visits...

(publish 
 :path *weblistener-toplevel-doc-page-url*
 :content-type cl-user::*html-publish-content-type*
 :function
 (lambda (req ent)
   (let* ((input (request-query req))
          (package-name (url-parameter-value :pkg input))
          (package-symbol (keywordize package-name))
          )
     (execute-with-standard-weblistener-environment
      req ent package-symbol
      (lambda () 
        (html (:princ (html-for-indirect-to-url (make-doc-directory-url))))
        )))))


(defun make-doc-directory-url (&optional name type) 
  (s+
   *doc-directory-url*
   (cond
    ((and (null name) (null type)) "index.html")
    ((and name (null type)) name)
    ((and name type) (s+ name "." type))
    ((and (null name) type) (s+ "." type))
    )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defparameter *fdef-hyperlinks* t "Controls whether hyperlinking is enabled")

(defun pretty-api-name (api-symbol)
  (if (eq api-symbol :other)
      "Other Symbols"
    (formatn "~A API" 
	     (string-capitalize
	     (subseq (symbol-name api-symbol)
		     1
		     (search "-API-SYMBOL" (symbol-name api-symbol)))))))
			       
(defparameter *doc-no-description* "*** No description available ***")
(defparameter *doc-hierarchical-mode* "hierarchical")
(defparameter *doc-alphabetical-mode* "alphabetical")
(defparameter *doc-expanded-state* "expanded")
(defparameter *doc-full-state* "full")
(defparameter *doc-collapsed-state* "collapsed")

;;; The 'doc state' is, for each API, the symbols included in that API
;;; and whether to display those symbols (or just the name of the API)
;;; and how.  A DOC-STATE is created for each user when they request
;;; display of BioLingua primitives the first time.

(defun doc-state-api-symbol (x) (first x))
(defun doc-state-api-state (x) (second x))
(defun doc-state-api-members (x) (third x))
(defun change-doc-state-api-state (record new-value)
  (setf (second record) new-value))

(defun create-default-primitives-doc-state ()
  (let* ((api-symbols (apropos-list "-API-SYMBOLS*"))
         (all-api-members 
          (mapcan (lambda (x) (copy-list (symbol-value x))) api-symbols))
         (state 
          (mapcar 
           (lambda (x) (list x :collapsed (symbol-value x))) api-symbols))
         (all-externals nil))
    (loop for package in (application-apropos-packages cl-user:*ai*) do
          (do-external-symbols (sym package) (push sym all-externals)))
    (cons
     (list :OTHER :collapsed (set-difference all-externals all-api-members))
     state
     )))
        

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; STANDARDIZED PUBLISHED PAGE CODE FOR DOC FUNCTIONALITY

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(publish 
 :path *application-primitives-url*
 :content-type cl-user::*html-publish-content-type*
 :function
 (lambda (req ent)
   (let* ((input (request-query req))
          (package-name (url-parameter-value :pkg input))
          (package-symbol (keywordize package-name))
          (mode (url-parameter-value :mode input))
          (symbol (url-parameter-value :symbol input))
          (package (url-parameter-value :package input))
          (state (url-parameter-value :state input))
          )
     (execute-with-standard-weblistener-environment
      req ent package-symbol
      (lambda () (html-for-application-primitives mode symbol package state))
      ))))

(publish 
 :path *doc-alpha-listing-url*
 :content-type cl-user::*html-publish-content-type*
 :function
 (lambda (req ent)
   ;; retrieve the user's primitives doc state or create one
   ;; for him if he doesn't have one yet.
   (let* ((input (request-query req))
          (package-name (url-parameter-value :pkg input))
          (package-symbol (keywordize package-name))
          (doc-state (get package-symbol :primitives-doc-state)))
     (execute-with-standard-weblistener-environment
      req ent package-symbol
      (lambda ()
        (when (null doc-state)
          (setq doc-state (create-default-primitives-doc-state))
          (setf (get package-symbol :primitives-doc-state) doc-state))
        (html-for-alphabetical-primitives-listing doc-state)
        )))))

(publish 
 :path *doc-find-function-source-url*
 :content-type cl-user::*html-publish-content-type*
 :function
 (lambda (req ent)
   (let* ((input (request-query req))
          (user-package-name (url-parameter-value :pkg input))
          (user-package-symbol (keywordize user-package-name))
          (symbol-package-name (url-parameter-value :package input))
          (symbol-package-symbol (keywordize symbol-package-name))
          (symbol-name (url-parameter-value :symbol input))
          )
     (execute-with-standard-weblistener-environment
      req ent user-package-symbol
      (lambda () 
        (html-for-find-function-source symbol-package-symbol symbol-name)
        )))))

(publish 
 :path *function-source-to-edit*
 :content-type cl-user::*html-publish-content-type*
 :function
 (lambda (req ent)
   (let* ((input (request-query req))
          (user-package-name (url-parameter-value :pkg input))
          (user-package-symbol (keywordize user-package-name))
          (package-name (url-parameter-value :package input))
          (package-symbol (keywordize package-name))
          (symbol-name (url-parameter-value :symbol input))
          )
     (execute-with-standard-weblistener-environment
      req ent user-package-symbol
      (lambda () 
        (html-for-function-source-to-edit package-symbol symbol-name)
        )))))

(publish 
 :path *doc-find-function-file-source-url*
 :content-type cl-user::*html-publish-content-type*
 :function
 (lambda (req ent)
   (let* ((input (request-query req))
          (user-package-name (url-parameter-value :pkg input))
          (user-package-symbol (keywordize user-package-name))
          (package-name (url-parameter-value :package input))
          (package-symbol (keywordize package-name))
          (symbol-name (url-parameter-value :symbol input))
          )
     (execute-with-standard-weblistener-environment
      req ent user-package-symbol
      (lambda ()
        (html-for-find-function-file-source package-symbol symbol-name)
        )))))

(publish 
 :path *doc-find-variable-source-url*
 :content-type cl-user::*html-publish-content-type*
 :function
 (lambda (req ent)
   (let* ((input (request-query req))
          (user-package-name (url-parameter-value :pkg input))
          (user-package-symbol (keywordize user-package-name))          
          (package-name (url-parameter-value :package input))
          (package-symbol (keywordize package-name))
          (symbol-name (url-parameter-value :symbol input))
          )
     (execute-with-standard-weblistener-environment
      req ent user-package-symbol
      (lambda () (html-for-find-variable-source package-symbol symbol-name))
      ))))


(publish
 :path *help-function-url*
 :content-type cl-user::*html-publish-content-type*
 :function
 (lambda (req ent)
   (let* ((input (request-query req))
          (user-package-name (url-parameter-value :pkg input))
          (user-package-symbol (keywordize user-package-name))          
          (package-name (url-parameter-value :package input))
          (package-symbol (keywordize package-name))
          (symbol-name (url-parameter-value :symbol input)))
     (execute-with-standard-weblistener-environment
      req ent user-package-symbol
      (lambda () 
	(com.gigamonkeys.foo:with-html-output ((request-reply-stream req))
          (help::html-for-help-function package-symbol symbol-name) 
          ))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; IMPLEMENTATION CODE FOR DOC PUBLISHED PAGE FUNCTIONALITY

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun html-for-application-primitives 
       (mode api-symbol-name api-symbol-package-name state)
  (let* ((mode (or mode *doc-hierarchical-mode*))
         (api-symbol
          (when api-symbol-name 
            (intern 
             api-symbol-name (find-package api-symbol-package-name))))
         ;; Retrieve the user's DOC STATE.
         (doc-state (get (wb::user-session-id) :primitives-doc-state))
         )
    ;; Create a DOC STATE for the user if one does not exist.
    (when (null doc-state)
      (setq doc-state (create-default-primitives-doc-state))
      (setf (get (wb::user-session-id) :primitives-doc-state) doc-state))
    ;; Determine what to show based on MODE
    (cond
     ((string= mode *doc-alphabetical-mode*)
      (html-for-alphabetical-primitives-listing doc-state))
     ((string= mode *doc-hierarchical-mode*)
      ;; Change the API symbol's mode if one was referred to.
      (when api-symbol
        (let ((doc-state-record
               (find api-symbol doc-state 
                     :key 'doc-state-api-symbol)))
          (change-doc-state-api-state
           doc-state-record
           (cond
            ((string= state "collapsed") :collapsed)
            ((string= state "expanded") :expanded)
            ((string= state "full") :full)
            (t :collapsed)
            ))))
      (html-for-hierarchical-primitives-api-listing doc-state)
      ))))


(defun html-for-alphabetical-primitives-listing (doc-state)
  (let ((sorted-symbols
         (sort-symbols-smartly
          (mapcan
           (lambda (api-record) 
             (copy-list (doc-state-api-members api-record)))
           doc-state))))
    (with-standard-weblistener-page-header
        ("Documented Primitives: Alphabetical Listing." :listener? nil)
     :br 
     (:table
      ((:tr :align "left")
       (:th "Name")
       (:th "Type")
       ;; Two dummy columns for padding
       (:th "") (:th "")
       (:th "Arglist/Value"))
      (loop for symbol in sorted-symbols do
            (html-for-function-description-table-row symbol)
            (html-for-variable-description-table-row symbol)
            )))))

(defun html-for-hierarchical-primitives-api-listing (doc-state)
  (let* ((sorted-api-symbols-list
          (sort-symbols-smartly 
           (copy-list doc-state) :key 'doc-state-api-symbol)))
    (with-standard-weblistener-page-header
        ("Documented Primitives: Hierarchical Listing." :listener? nil)
      ((:a :href (make-doc-alpha-listing-url))
       "Alphabetical Listing of All Primitives")
      :br :br
      (loop for (symbol state members) in sorted-api-symbols-list
            as pretty-name = (pretty-api-name symbol) do
            (flet ((api-symbol-line (f1 f2 s1 s2)
                     (html
                      ((:a :href (funcall f1 symbol)
                        :style "text-decoration: none") (:princ-safe s1))
                      ((:a :href (funcall f2 symbol)
                        :style "text-decoration: none") (:princ-safe s2))
                      ((:a :name pretty-name)
                       (:big (:b (:princ-safe pretty-name))))
                      )))
              (ecase state
                (:collapsed
                 (api-symbol-line 
                  'expanded-listing-url 'full-listing-url "[+] " "[*] "))
                (:expanded
                 (api-symbol-line
                  'collapsed-listing-url 'full-listing-url "[-] " "[*] ")
                 (html :br)
                 (loop for symbol in (sort-symbols-smartly (copy-list members))
                       do
                       (html-for-indented-function-name symbol)
                       (html-for-indented-variable-name symbol)
                       ))
                (:full
                 (api-symbol-line
                  'collapsed-listing-url 'expanded-listing-url "[-] " "[+] ")
                 (html :br)
                 (loop for symbol in (sort-symbols-smartly (copy-list members))
                       do
                       (html-for-full-function-description symbol)
                       (html-for-full-variable-description symbol)
                       )
                 (html :hr)
                 )))
            (html :p)
            ))))


(defun html-for-find-function-source (package-symbol symbol-name)

  (let* ((symbol (intern symbol-name (find-package package-symbol)))
         (package-name (symbol-name package-symbol))
         (doc (documentation symbol 'function))
         (source-file (system-specific-source symbol :function))
         (source-dir 
          (and source-file 
               (dirpath-of-filepath 
                (translate-logical-pathname source-file))))
         (source-dir-url 
          (and source-file (directory-listing-url source-dir)))
         (source-name 
          (string-downcase
           (if source-file (file-namestring source-file) " ??? ")))
         (source-file-url 
          (and source-file (function-source-file-url symbol)))
	 (new-style-p (new-style-function-help-p symbol)))
          
    (with-standard-weblistener-page-header
        ("BioBike Function Listing")

      (multiple-value-bind (name kind arglist)
          (briefly-describe-symbol-as symbol :function)
        (html
         (:table
	  (unless new-style-p
	    (html 
	      (:tr (:td (:h4 "Name:")) (:td (:h4 (:princ-safe name))))
	      (:tr (:td (:h4 "Type:")) (:td (:h4 (:princ-safe kind))))
	      (:tr (:td (:h4 "Arglist:")) (:td (:h4 (:princ-safe arglist))))))
          (:tr (:td (:h4 "Source File:"))
           (if (null source-file)
               (html (:td (:h4 (:princ-safe source-name))))
             (html (:td (:h4 ((:a :href source-file-url)
                              (:princ-safe source-name)))))))
          (:tr (:td (:h4 "Source Dir:"))
           (when source-file 
             (html (:td (:h4 ((:a :href source-dir-url) 
                              (:princ-safe source-dir)))))
             )))
         :p
         ((:a :href 
           (make-editfunction-url 
            :function symbol-name :package package-name))
          "Source code -> Editor")
         "&nbsp;&nbsp;"
         ((:a :href (make-function-source-to-edit 
                     :symbol symbol-name
                     :package package-name))
          "Source Code -> Listener")
         :hr
	 (unless new-style-p
	   (html 
	     (:h3 "Documentation:")
	     (:pre (:princ-safe (or doc "*** No description available ***")))
	     :p
	     :hr
	     :br))
         (let ((text (function-definition-source-text symbol :function)))
           (if *fdef-hyperlinks*
               (html 
                (:pre 
                 (form-string-to-html-with-function-hyperlinks text)))
             (html (:pre (:princ-safe text)))
             ))
         )))

    ))

(defun html-for-function-source-to-edit (package-symbol symbol-name)
  (let* ((symbol (intern symbol-name (find-package package-symbol)))
         (text (function-definition-source-text symbol :function)))
    (setq *multiline-form-data* 
          (one-string-nl
           (formatn ";; Package: ~A" package-symbol)
           text))
    (html 
     (:princ 
      ;; (indirect-to-redisplay (incf *user-display-id*) wb:*username*)
      (indirect-to-redisplay (incf *user-display-id*) (wb::user-session-id))
      ))))

(defun html-for-find-function-file-source (package-symbol symbol-name)
  (let* ((symbol (intern symbol-name (find-package package-symbol)))
         (source-file (system-specific-source symbol :function)))
    (with-standard-weblistener-page-header 
        ("BioLingua Source File Listing")
      (if source-file
          (html-for-hyperlinked-lisp-file source-file)
        (html (:princ-safe "*** No source file available ***"))
        ))))

(defun html-for-find-variable-source (package-symbol symbol-name)
  (let* ((symbol (intern symbol-name (find-package package-symbol)))
         (source-file (system-specific-source symbol :variable))
         (doc (documentation symbol 'variable)))
    (declare (ignore source-file))
    (with-standard-weblistener-page-header
        ("Variable Listing")
      (multiple-value-bind (name kind value)
          (briefly-describe-symbol-as symbol :variable)
        (html
         (:table
          (:tr (:td (:h3 "Name:")) (:td (:h3 (:princ-safe name))))
          (:tr (:td (:h4 "Type:") ) (:td (:h4 (:princ-safe kind))))
          (:tr (:td (:h4 "Value:")) (:td (:h4 (:princ-safe value))))
          )
         (:pre (:princ-safe (or doc "*** No description available ***")))
         )))))

(defun html-for-hyperlinked-lisp-file (source-file &key (publish-name nil))
  (let* ((file-string (file-to-string source-file))
         (fullpath (translate-logical-pathname source-file))
         (dirpath (dirpath-of-filepath fullpath))
         (dirtag (canonicalize-pathname-namestring (namestring dirpath)))
         (filename (file-namestring fullpath)))
    (html 
     :br
     (:table
      (:tr
       (:td
        ((:a :href (directory-listing-url dirpath))
         (:small (:princ-safe dirtag)))
        (:small (:princ-safe filename) (:small "&nbsp;&nbsp;")))
       (:td
        (if publish-name 
            (html ((:a :href publish-name) (:small "Download source"))
                  (:small "&nbsp;&nbsp;")))
          "") 
       (:td 
        ((:a :href 
          (forward-funcall 
           'make-editanyfile-url 
           :filepath (canonicalize-pathname-namestring (namestring fullpath))))
         (:small "Edit source")))
        ))
     :hr :p
     (cond
      ((null file-string)
       (html (:princ-safe "<<<<< FILE EXCEEDS 1,000,000 CHARACTERS! >>>>>")))
      ((zerop (length file-string))
       (html (:princ-safe "<<<<< FILE IS EMPTY >>>>>")))
      ((and (< (length file-string) 1000) (every 'whitespacep file-string))
       (html (:princ-safe "<<<<< FILE CONTAINS NOTHING BUT WHITESPACE >>>>>")))
      (t
       (html
        (:pre
         (form-string-to-html-with-function-hyperlinks file-string)
         )))))))


(defun briefly-describe-symbol-as (s as &key (mode :strings))
  #.(one-string-nl
     "Returns information about a symbol."
     "AS may be either :FUNCTION a :VARIABLE."
     "If MODE is :STRINGS, the information is returned as strings, or"
     "as symbols and lists if MODE is :FORMS"
     "If :FUNCTION is specified and the symbol is FBOUNDP,"
     "then the name of the function, whether it is a FUNCTION, MACRO"
     "or SPECIAL form, and its argument list are returned as three values."
     "If :VARIABLE is specified and the symbol is BOUNDP,"
     "then the name of the variable, VARIABLE, and the variables  value"
     "are returned as three values."
     "NIL is returned if the symbol is not fboundp or boundp, as appropriate.")
  (multiple-value-bind (type arglist/value)
      (ecase as
        ((:function 'function)
         (when (fboundp s)
           (values 
            (cond
             ((special-operator-p s) 'special)
             ((macro-function s) 'macro)
             (t 'function))
            (system-specific-arglist-of s)
            )))
        ((:variable 'variable)
         (when (boundp s) (values 'variable (symbol-value s)))))
    (when type
      (ecase mode
        (:strings
         (values 
          (symbol-name s)
          (string-capitalize (string type))
          (ecase as
            (:function (string-downcase (formatn "~A" arglist/value)))
            (:variable (formatn "~S" arglist/value)))))
        (:forms (values s type arglist/value)
         )))))
     

;;; HTML for the listing of functions and variables on 
;;; on the alphabetical listing page.  (Shows clipped arglist, no doc)

(defun html-for-function-description-table-row (symbol)
  (html-for-symbol-table-row
   symbol :function 'function-source-url  " ...)"))

(defun html-for-variable-description-table-row (symbol)
  (html-for-symbol-table-row
   symbol :variable 'full-variable-url  "..."))

(defun html-for-symbol-table-row 
       (symbol as full-url clipped-suffix
               &key (name-limit 35) (arglist/var-limit 50))
  (multiple-value-bind (name kind arglist/var)
      (briefly-describe-symbol-as symbol as)
    (when name
      (multiple-value-bind (name name-clipped?)
          (maybe-clip-string name name-limit)
        (declare (ignore name-clipped?))
        (multiple-value-bind (arglist/var arglist/var-clipped?)
            (maybe-clip-string arglist/var arglist/var-limit clipped-suffix)
          (html
           (:tr
            (:td ((:a :href (funcall full-url symbol)) (:princ-safe name)))
            (:td (:princ-safe kind))
            ;; Two dummy values
            (:td (:princ-safe "")) (:td (:princ-safe ""))
            (:td (if arglist/var-clipped? 
                     (html
                      ((:a :href (funcall full-url symbol))
                       (:princ-safe arglist/var)))
                   (html (:princ-safe arglist/var))
                   )))))))))

;;; HTML for function and variable listing on hierarchical page 
;;; when MODE is FULL (shows full argument list and full doc string).

(defun html-for-full-function-description (symbol)
  (html-for-full-description 
   symbol :function 'function (function-source-url symbol)))
        
(defun html-for-full-variable-description (symbol)
  (html-for-full-description 
   symbol :variable 'variable (full-variable-url symbol)))

(defun html-for-full-description (symbol as doctype url)
  (multiple-value-bind (name kind arglist/var)
      (briefly-describe-symbol-as symbol as)
    (when name
      (let ((doc (or (documentation symbol doctype) *doc-no-description*)))
        (html
         :hr
         :br
         (:b 
          ((:a :href url) (:princ-safe name))
          (:princ-safe (formatn " [~A] ~A" kind arglist/var)))
         :p
         (:princ-safe doc)
         )))))


;;; HTML for hierarchical page when MODE = EXPANDED
;;; (just shows function or variable name)

(defun html-for-indented-function-name (symbol)
  (when (fboundp symbol) (html-for-indented-name symbol 'function-source-url)))

(defun html-for-indented-variable-name (symbol)
  (when (boundp symbol) (html-for-indented-name symbol 'full-variable-url)))

(defun html-for-indented-name (symbol url-function)
  (html
   :br (:princ "&nbsp;&nbsp;&nbsp;")
   ((:a :href (funcall url-function symbol))
    (:princ-safe (symbol-name symbol))
    )))
  

;;; HTML for function and variable names with a link to display
;;; complete information on that function or variable.

(defun new-style-function-help-p (symbol)
  (let ((doc (help:find-documentation symbol 'help::function-documentation)))
    (and doc (help:explicitly-documented-p doc))))

(defgeneric symbol-doc-url (symbol type))

(defmethod symbol-doc-url 
           ((symbol symbol) (type (eql 'help::function-documentation)))
  (when (symbol-package symbol) 
    (function-doc-url symbol)
    ))

(defmethod symbol-doc-url ((symbol t) (type t)) nil)

(defun function-doc-url (symbol)
  (cond
   ((symbolp symbol)
    (if (eql (symbol-package symbol) (find-package :cl))
        (common-lisp-external-symbol-url symbol)
      (function-source-url symbol)))
   ((stringp symbol)
    (let ((p&s (string-split symbol #\:)))
      (unless (= 2 (length p&s))
        (error "See-also string is not of the form 'package:symbol'"))
      (let ((pkg (string-upcase (first p&s)))
            (sym (string-upcase (second p&s))))
        (when (find-package pkg)
          (function-source-url (intern sym (find-package pkg)))
          ))))
   (t (error "Illegal entry in see-also list: ~A" symbol))
   ))
                 

(defun function-source-url (symbol)
  (url-with-package-name-and-symbol-name 
   (if (new-style-function-help-p symbol)
       'make-help-function-url
       'make-doc-find-function-source-url)
   symbol))

(defun function-source-file-url (symbol)
  (url-with-package-name-and-symbol-name
   'make-doc-find-function-file-source-url symbol))

(defun full-variable-url (symbol)
  (url-with-package-name-and-symbol-name 
   'make-doc-find-variable-source-url symbol))

(defun url-with-package-name-and-symbol-name (url-constructor symbol)
  (funcall url-constructor
           :package
           (url-safe-string (package-name (symbol-package symbol)))
           :symbol
           (url-safe-string (symbol-name symbol))))


;;; HTML for the [-], [+] and [*] links next to the API symbols.

(defun expanded-listing-url (api-symbol)
  (primitives-listing-url 
   *doc-hierarchical-mode* api-symbol *doc-expanded-state*))
(defun full-listing-url (api-symbol)
  (primitives-listing-url *doc-hierarchical-mode* api-symbol *doc-full-state*))
(defun collapsed-listing-url (api-symbol)
  (primitives-listing-url 
   *doc-hierarchical-mode* api-symbol *doc-collapsed-state*))

(defun primitives-listing-url (mode api-symbol api-symbol-state)
  (one-string
   (make-application-primitives-url
    :mode mode
    :package
    (url-safe-string (package-name (symbol-package api-symbol)))
    :symbol
    (url-safe-string (symbol-name api-symbol))
    :state
    api-symbol-state
    )
   (formatn "#~A" (pretty-api-name api-symbol))
   ))


;;; Sort a list of symbols (destructively) such that initial
;;; non-alphanumeric characters are ignored by the sort ordering algorithm.
;;; So *XYZZY* sorts next to XYZZY.

(defun sort-symbols-smartly (symbol-list &key (key 'identity))
  (sort symbol-list 'string-lessp
        :key (lambda (s)
               (let* ((name (symbol-name (funcall key s))))
                 (first-alphabetic-sort-key name)
                 ))))

(defun first-alphabetic-sort-key (string)
  (let ((pos (position-if 'alpha-char-p string)))
    (cond
     ((null pos) string)
     ((zerop pos) string)
     (t (subseq string pos))
     )))


(defun show-system-defun (name &optional (type :function))
  #.(one-string-nl
     "Prints the definition of a BioLingua function named NAME, or a suitable "
     "message if NAME has no definition or the source cannot be found.")
  ;; Allow you to type a symbol or a string.
  (setq name (intern (string name)))
  (let ((source (function-definition-source-text name type)))
    (formatt "~A" source)
    ))

(defun find-def-form-in-file (file name type)

  (setq name (string name))

  (let* ((type-strings 
          '("deftype" "defclass" "defstruct"))
         (variable-strings 
          '("defparameter" "defvar" "defconstant"))
         (function-strings 
          '("defun" "defmacro" "defgeneric" "defmethod" "define-function" 
                    "define-macro"))
         (defform-strings
          (ecase type
            (:type type-strings)
            (:variable variable-strings)
            (:function function-strings)
            ((nil t :any) 
             (append type-strings variable-strings function-strings))
            ))
         (search-strings 
          (append 
           ;; handle define-function forms with aliases
           (when (eq type :function)
             (list (one-string "(define-function (" name)))
           (mapcar 
            (lambda (x) (one-string "(" x " " name)) 
            defform-strings)
           (mapcar
            (lambda (x) (one-string "(lisp:" x " " name)) 
            defform-strings)))
         (found nil)
         (start-position nil)
         (end-position nil)
         )

    (with-open-file (s file)
      (setq start-position (file-position s))

      (block found

        ;; Read lines until we find a match.  Then try to find the
        ;; end of the form, either by using READ or by reading
        ;; successive lines in until we find a line that appears to
        ;; terminate the definition.

        (do ((line (read-line s nil nil) (read-line s nil nil)))
            ((null line))

          (when (match-defstring-set-on-line search-strings line)
            
            ;; This line matches.  Return to the beginning of the line
            ;; so we can do a READ.
            (file-position s start-position)

            (handler-case
                ;; If the READ executes successfully, by definition
                ;; we must be at the end of the form we read.
                (progn
                  (read s nil nil)
                  (setq found t)
                  (setq end-position (file-position s)))
              ;; Ruh roh.  READ didn't work.
              (error 
               ()
               ;; Go back to the start of the match line and starting
               ;; reading lines in one by one until we think we might
               ;; have reached the end of this form (or end-of-file).
               ;; Keep track of the end position of the previous line
               ;; we read so when we find a line that indicates that
               ;; the form is finished, we mark the end as back there.
               (file-position s start-position)
               (read-line s)
               (let ((previous-line-end-pos (file-position s)))
                 (do ((line (read-line s nil nil) (read-line s nil nil)))
                     ((null line))
                   (when (and (plusp (length line)) (find (char line 0) "(;#"))
                     (return))
                   (setq previous-line-end-pos (file-position s)))
                 (setq found t)
                 (setq end-position previous-line-end-pos)
                 )))
              (return-from found t)
              )
          (setq start-position (file-position s))
          ))

      (if (not found)

          (formatn
           "*** Could not find definition for ~A of type ~A in file ~A"
           name type file)

        ;; Read all the chars between START-POSITION and END-POSITION
        ;; tacking on a Newline at the end if one is not present.
        
        (let ((sx (make-array 0 :element-type 'character
                              :adjustable t :fill-pointer 0)))
          (file-position s start-position)
          (do ((ch (read-char s nil nil) (read-char s nil nil)))
              ((null ch))
            (vector-push-extend ch sx)
            (when (> (file-position s) end-position) (return)))
          (let ((last (1- (fill-pointer sx))))
            (unless (eql (aref sx last) #\Newline)
              (setf (aref sx last) #\Newline))
            (coerce (subseq sx 0 (1+ last)) 'simple-string)
            )))

      )))

;; Find a match followed either by a Space or the end of the line.
(defun match-defstring-set-on-line (defstring-list line)
  (block exit
    (let ((linelen (length line)))
      (dolist (defstring defstring-list)
        (let ((pos (search defstring line :test 'char-equal)))
          (when pos
            (let* ((nextpos (+ pos (length defstring))))
              (when (or (= nextpos linelen) (eql #\Space (char line nextpos)))
                (return-from exit t)
                ))))))))
                      

(defun function-definition-source-text (symbol &optional (type :function))
  (let ((source-file (system-specific-source symbol type))
        (fsymbol? (fboundp symbol))
        (vsymbol? (boundp symbol))
        (lisppkg (find-package :lisp))
        (sympkg (symbol-package symbol))
        )
    (if source-file
        (find-def-form-in-file 
         (namestring source-file) (symbol-name symbol) type)
      (flet ((clp (symbol descriptor)
               (formatn 
                "*** ~A is a Common Lisp ~A. Its source is not available."
                symbol descriptor))
             (no-source (symbol descriptor)
               (formatn
                "*** ~A is defined as a ~A but its source is not available." 
                symbol descriptor))
             (not-defined (symbol descriptor)
               (formatn
                (one-string
                 "*** There is no accessible symbol named ~A defined as a "
                 "~A, and therefore no source to display. "
                 "Perhaps it is misspelled?")
                symbol descriptor)))
        (ecase type
          (:function
           (cond
            ((and fsymbol? (eq sympkg lisppkg)) (clp symbol "primitive"))
            (fsymbol?
             (no-source 
              symbol (if (macro-function symbol) "macro" "function")))
            (t (not-defined symbol "function or macro"))
            ))
          (:variable
           (cond
            ((and vsymbol? (eq sympkg lisppkg)) (clp symbol "variable"))
            (vsymbol? (no-source symbol "variable"))
            (t (not-defined symbol "variable"))
            )))))))
            
(defun file-to-hyperlinked-string (file)
  (let ((file-string (file-to-string file)))
    (form-string-to-html-with-function-hyperlinks file-string)))

;; XXX implement for SBCL - mas 9/22/04
(defun system-specific-source (symbol type)             
  "Returns a pathname designating the source file, or NIL"
  (let ((result
         (or (get symbol :df-source-file)
             (block nil
               #+:allegro
               (return
                (ignore-errors
                  (ecase type
                    (:function (excl:source-file symbol :operator))
                    (:variable (excl:source-file symbol :variable))
                    )))
               #+:lispworks
               (return
                (second 
                 (first
                  (dspec:find-dspec-locations
                   (ecase type
                     (:function `(lisp:defun ,symbol))
                     (:variable `(lisp:defvar ,symbol))
                     )))))
               (warn "SYSTEM-SPECIFIC-SOURCE not defined!")
               nil
               ))))
    (cond
     ((pathnamep result)
      ;; Return nil if any problem
      ;; Hack because Allegro sometimes returns pathname with 
      ;; just name if file is compiled without full pathname
      ;; i.e. (compile-file "foo.lisp") different than
      ;; (compile-file "C:/Lispcode/foo.lisp") in terms of what
      ;; excl:source-file returns.
      (ignore-errors
        (cond
         ((null (pathname-directory result))
          (let ((possible-result-file 
                 (merge-pathnames result *default-pathname-defaults*)
                 ))
            (and (probe-file possible-result-file) possible-result-file)
            ))
         (t (and (probe-file result) result))
         )))
     (t nil)
     )))

;; XXX implement for SBCL - mas 9/22/04
(defun system-specific-arglist-of (s)
  (block nil
    #+:allegro
    (return (excl:arglist s))
    #+lispworks
    (return (lispworks:function-lambda-list s))
    (warn "SYSTEM-SPECIFIC-ARGLIST-OF not defined!")
    nil
    ))

