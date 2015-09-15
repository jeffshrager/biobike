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

(defparameter *doc-directory-path* 
  (namestring (cl-user:translate-simple-lp "websrc:Doc;")))

(defgeneric html-page-for-new-help-entry (doc-item)
  (:documentation
   "Generates a page displaying all the information about a documented object."
   ))

(defmethod html-page-for-new-help-entry ((doc-item t))
  (html (:h2 "Not implemented yet.")))

(defun maybe-para? (flag) (when flag (html :p)))

(defun html-for-help-section-title 
       (title size color paragraph-after?)
  (labels ((title-text () (html (:b (:princ-safe (s+ title ":")))))
           (title-ctext ()
             (if color (html ((:font :color color) (title-text))) (title-text))
             ))
    (ecase size
      (:h2 (html (:h2 (title-ctext))))
      (:h3 (html (:h3 (title-ctext))))
      (:h4 (html (:h4 (title-ctext))))
      )
    (maybe-para? paragraph-after?)
    ))

(defun html-for-comma-separated-text-list (list)
  (loop for rest on list 
        as elem = (first rest)
        do 
        (html (:princ-safe (string elem)))
        (when (cdr rest) (html ", "))
        ))

(defun html-for-summary-information 
       (summary
        &key
        (title-text "Summary")
        (size :h3) 
        (paragraph-before? t) 
        (paragraph-after-title? nil)
        (paragraph-after? nil)
        (color nil))
  (when summary
    (maybe-para? paragraph-before?)
    (when title-text 
      (html-for-help-section-title
       title-text size color paragraph-after-title?))
    (html (:princ-safe summary))
    (maybe-para? paragraph-after?)
    ))

(defun html-for-keyword-information 
       (keywords
        &key
        (title-text "Keywords")
        (size :h3) 
        (paragraph-before? t) 
        (paragraph-after-title? nil)
        (paragraph-after? nil)
        (color nil))
  (when keywords
    (maybe-para? paragraph-before?)
    (when title-text 
      (html-for-help-section-title
       title-text size color paragraph-after-title?))
    (html-for-comma-separated-text-list keywords)
    (maybe-para? paragraph-after?)
    ))

(defun html-for-vpl-syntax-information (vpl-syntax)
  (when vpl-syntax 
    (eval `(com.gigamonkeys.foo:html ,@vpl-syntax))
    (maybe-para? t)
    ))

(defun html-for-text-information 
       (text
        &key
        (title-text "Description")
        (size :h3) 
        (paragraph-before? t) 
        (paragraph-after-title? t)
        (paragraph-after? nil)
        (color nil))
  (when text
    (maybe-para? paragraph-before?)
    (when title-text 
      (html-for-help-section-title
       title-text size color paragraph-after-title?))
    (eval `(com.gigamonkeys.foo:html ,@text))
    (maybe-para? paragraph-after?)
    ))

(defun html-for-referred-to-by-information 
       (referrals
        &key
        (title-text "Referred to by")
        (size :h3) 
        (paragraph-before? t) 
        (paragraph-after-title? nil)
        (paragraph-after? nil)
        (color nil))
  (when referrals
    (maybe-para? paragraph-before?)
    (when title-text
      (html-for-help-section-title
       title-text size color paragraph-after-title?))
    (loop for rest on referrals 
          as docobj = (first rest)
          do 
          (html-for-doc-object-reference docobj)
          (when (cdr rest) (html ", "))
          )
    (maybe-para? paragraph-after?)
    ))

(defun html-for-author-information 
       (author
        &key
        (title-text "Author")
        (size :h3) 
        (paragraph-before? t) 
        (paragraph-after-title? nil)
        (paragraph-after? nil)
        (color nil))
  (when author
    (maybe-para? paragraph-before?)
    (when title-text
      (html-for-help-section-title
       title-text size color paragraph-after-title?))
    (html-for-comma-separated-text-list author)
    (maybe-para? paragraph-after?)
    ))

(defgeneric docobj->url (docobj)
  (:documentation 
   #.(one-string-nl
      "Returns a URL which points at a (possibly computed) page displaying"
      "information about DOCOBJ, or what DOCOBJ refers to (e.g. a file)."
      )))

(defmethod docobj->url ((docobj t))
  (error "Not implemented: Docobj->url for type ~S" (type-of docobj)))

(defmethod docobj->url ((docobj documented))
  (let ((type (type-of docobj)))
    (funcall 
     ;; Something like 'make-help-module-url'
     (intern (s+ "MAKE-HELP-" type "-URL") :help) 
     :name (url-safe-string (help:name docobj))
     )))

(defmethod docobj->url ((docobj help:function-documentation))
  (let ((name (help:name docobj)))
    (make-help-function-documentation-url
     :name (url-safe-string (symbol-name name))
     :package (url-safe-string (package-name (symbol-package name)))
     )))

(defmethod docobj->url ((docobj help:symbol-doc))
  (let ((name (help:name docobj)))
    (if (eq (symbol-package name) (find-package :cl))
        (wb::common-lisp-external-symbol-url name)
      (make-help-symbol-doc-url 
       :name (url-safe-string (symbol-name name)) 
       :package (url-safe-string (package-name (symbol-package name)))
       :type (url-safe-string (string (help:dtype docobj)))
       ))))

;;J. Myers Feb 12 '13.  Hotlinks to .pdf etc. now go directly to file, no intermediate page.
;; Comment out this entire method to reset to previous behavior, 
;; it's a self-contained override with no unusual extra hooks.
;;  Note this does this for all objects of type documentation-file.
;;
;;  There is an unusual "intern" side-effect going on in the original example.
;;  I believe this is not needed for what I'm doing here...
;;
(defmethod docobj->url ((docobj help:documentation-file))

    (source-url-for-filedoc docobj)

)


(defgeneric docobj->label (docobj &optional symbol-type?)
  (:documentation 
   "Returns a string which identifies the DOCOBJ in a user-friendly way"
   ))

(defmethod docobj->label ((docobj t) &optional (symbol-type? nil))
  (declare (ignore symbol-type?))
  (error "Not implemented: Docobj->label for type ~S" (type-of docobj)))

(defmethod docobj->label ((docobj documented) &optional (symbol-type? nil))
  (declare (ignore symbol-type?))
  (help:name docobj))
  
(defmethod docobj->label 
           ((docobj help:documentation-file) &optional (symbol-type? nil))
  (declare (ignore symbol-type?))
  (or (help:label docobj) (file-namestring (help:name docobj))))

(defmethod docobj->label 
           ((docobj help:symbol-doc) &optional (symbol-type? nil))
  (let ((name (help:name docobj)))
    (if (null symbol-type?) 
        name 
      (ecase (help:dtype docobj)
        (:function name)
        (:variable (s+ "VAR: " name))
        (:type (s+ "TYPE: " name))
        ))))

(defun docobj->url&label (docobj &optional (symbol-type? nil))
  (values (docobj->url docobj) (docobj->label docobj symbol-type?))
  )

(defun html-for-doc-object-reference (doc-object &optional (symbol-type? nil))
  (multiple-value-bind (url label)
      (docobj->url&label doc-object symbol-type?)
    (html ((:a :href url) (:princ-safe label)))
    ))

(defun html-for-elhai-doc-object-reference 
       (doc-object &optional (symbol-type? nil))
  (multiple-value-bind (url label)
      (docobj->url&label doc-object symbol-type?)
    (html ((:a :href url :target "_blank")
           (emit-limited-match-with-search-term-bolded 
            (substitute #\Space #\Newline (string label))
            (get *sessionid* :current-search-string) 
            :color "red")))
    ))
      
    

(defun emit-limited-match-with-search-term-bolded 
       (matchline search-term &key (color "green") (limit 30))
  (let* ((pos (search search-term matchline :test 'string-equal)))
    (if (null pos)
        (html (:princ-safe (ellipsis-string matchline limit)))
      (let* ((before (subseq matchline 0 pos))
             (szbefore (length before))
             (endpos (+ pos (length search-term)))
             (term (subseq matchline pos endpos))
             (szterm (length term))
             (after (subseq matchline endpos))
             (szafter (length after))
             (szbefore&term (+ szbefore szterm)))
        (cond
         ((>= szbefore limit) 
          (html (:princ-safe (ellipsis-string before limit))))
         ((> limit (+ szbefore&term 3))
          (html 
           (:princ-safe before)
           ((:font :color color) (:princ-safe term)))
          (when (plusp szafter) 
            (emit-limited-match-with-search-term-bolded 
             after search-term :color color :limit (- limit szbefore szterm)))
          )
         (t 
          (let* ((amount-of-term-to-show (- limit szbefore 3)))
            (if (not (plusp amount-of-term-to-show))
                (html (:princ-safe (ellipsis-string matchline limit)))
              (let ((tterm (subseq term 0 amount-of-term-to-show)))
                (html 
                 (:princ-safe before)
                 (:b ((:font :color color) (:princ-safe tterm)))
                 (:princ-safe "...")
                 ))))))))))
           
          
         

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun oops-no-docobj (name type-string)
  (html 
   ((:font :color "red") 
    (:h2 (:princ-safe (formatn "No ~A for '~A' found!" type-string name))))))


(publish 
 :path *help-topic-url* 
 :content-type cl-user::*html-publish-content-type*
 :function
 (lambda (req ent)
   (let* ((input (request-query req))
          (package-name (url-parameter-value :pkg input))
          (package-symbol (keywordize package-name))
          (name (url-parameter-value :name input))
          (docobj (help::find-documentation name 'help::topic))
          )
     (execute-with-standard-weblistener-environment
      req ent package-symbol
      (lambda () 
        (com.gigamonkeys.foo:with-html-output ((request-reply-stream req))
          (html-for-help-topic-display name docobj)))
      ))))

(defun html-for-help-topic-display (name doc-item)
  (if (null doc-item) 
      (oops-no-docobj name "Topic")
    (with-slots (name text keywords see-also referred-to-by author)
        doc-item
      (let* ((summary (help:docstring doc-item))
             (title (formatn "~A" name)))
        (with-standard-weblistener-page-header
            (title :listener? nil)
          (html-for-summary-information summary)
          :p
          :hr
          (html-for-text-information text :title-text nil)
          :p
          :hr
          (html-for-keyword-information keywords)
          (html-for-see-also-information see-also t)
          (html-for-referred-to-by-information referred-to-by)
          (html-for-author-information author)
          )))))




(publish 
 :path *help-documentation-file-url*
 :content-type cl-user::*html-publish-content-type*
 :function
 (lambda (req ent)
   (let* ((input (request-query req))
          (package-name (url-parameter-value :pkg input))
          (package-symbol (keywordize package-name))
          (name (url-parameter-value :name input))
          (docobj (help::find-documentation name 'help::documentation-file))
          )
     (execute-with-standard-weblistener-environment
      req ent package-symbol
      (lambda () 
        (com.gigamonkeys.foo:with-html-output ((request-reply-stream req))
          (html-for-help-documentation-file-description name docobj)))
      ))))

(defun html-for-help-documentation-file-description (name doc-item)
  (if (null doc-item) 
      (oops-no-docobj name "File Documentation Description")
    (with-slots (name text keywords see-also referred-to-by author)
        doc-item
      (let* ((summary (help:docstring doc-item))
             (title (formatn "Description of '~A'" name))
             (source-url (source-url-for-filedoc doc-item)))
        (with-standard-weblistener-page-header
            (title :listener? nil)
          (if (or summary keywords see-also referred-to-by author)
              (html 
               :p
               (:big (:b ((:a :href source-url) "View the actual file")))
               (html-for-summary-information summary)
               (html-for-keyword-information keywords)
               (html-for-see-also-information see-also t)
               (html-for-referred-to-by-information referred-to-by)
               (html-for-author-information author)
               (html 
                :p
                (:big 
                 (:b ((:a :href source-url) "View the actual file"))
                 )))
            (html
             :p 
             (:i 
              (:princ-safe 
               "A documentation file with no summary or associated keywords"))
             :p
             (:big (:b ((:a :href source-url) "View the actual file")))
             )))))))

(defun source-file-for-filedoc (docobj)
  (merge-pathnames 
   (or (source-file docobj) (name docobj))
   *doc-directory-path*
   ))

(defun txt-file-for-filedoc (docobj)
  (let ((sfile (source-file docobj)))
    (if (null sfile)
        nil
      (vif (tfile (associated-text-file docobj))
           (merge-pathnames tfile *doc-directory-path*)
           (pathname-of-new-type sfile "txt")
           ))))

(defun associated-filedoc-url (docobj filepath type)
  (if *new-text-extraction-mode*
      (new-associated-filedoc-url docobj filepath type)
    (let* ((fpp (pathname filepath))
           (ftype (pathname-type fpp)))
      ;; Create a url that goes through apache for .ppt and .pps
      ;; files on linux in the externaldf directory so that they get opened
      ;; in the right mode.  
      (if (and 
           (not (eq (user::os?) :windows))
           (or (string-equal "pps" ftype) (string-equal "ppt" ftype))
           (member "externaldf" (pathname-directory fpp) :test 'string-equal))
          (s+ 
           user::*host-machine-apache-url*
           "biobike-doc/" 
           "externaldf/"
           (pathname-name fpp)
           "."
           ftype
           )
        (let* ((urlname 
                (string-downcase (remove-if-not 'alphanumericp (name docobj))))
               (urlpath (s+ "/doc-" type "-" urlname)))
          (publish-file :path urlpath :file filepath)
          urlpath
          )))))

(defun new-associated-filedoc-url (docobj filepath type)
  (let* ((fpp (pathname filepath))
         (ftype (pathname-type fpp)))
    ;; Create a url that goes through apache for Unix systems
    ;; so that .ppt and .pps files get opened in the right mode,
    ;; and uses File:/// notation for windows systems.
    (if (and 
         (not (eq (user::os?) :windows))
         (or (string-equal "pps" ftype) (string-equal "ppt" ftype))
         (member "externaldf" (pathname-directory fpp) :test 'string-equal))
        (s+ 
         user::*host-machine-apache-url*
         "biobike-doc/" 
         "externaldf/"
         (pathname-name fpp)
         "."
         ftype
         )
      (let* ((urlname 
              (string-downcase (remove-if-not 'alphanumericp (name docobj))))
             (urlpath (s+ "/doc-" type "-" urlname)))
        (publish-file :path urlpath :file filepath)
        urlpath
        ))))

(defun source-url-for-filedoc (docobj)
  (let ((sfile (source-file docobj)))
    (if (null sfile)
        (forward-funcall 'wb::make-doc-directory-url (name docobj))
      (let ((source-path (source-file-for-filedoc docobj)))
        (associated-filedoc-url docobj source-path "source")
        ))))

(defun txt-url-for-filedoc (docobj)
  (let ((txt-path (txt-file-for-filedoc docobj)))
    (when txt-path
      (associated-filedoc-url docobj txt-path "txt")
      )))

(publish 
 :path *help-symbol-doc-url*
 :content-type cl-user::*html-publish-content-type* 
 :function
 (lambda (req ent)
   (let* ((input (request-query req))
          (package-name (url-parameter-value :pkg input))
          (package-symbol (keywordize package-name))
          (name (url-parameter-value :name input))
          (package (url-parameter-value :package input))
          (type (url-parameter-value :type input))
          )
     (execute-with-standard-weblistener-environment
      req ent package-symbol
      (lambda () 
        (com.gigamonkeys.foo:with-html-output ((request-reply-stream req))
          (html-for-symbol-doc name package type)
          ))))))

(defun html-for-symbol-doc (name package type &aux symbol)
  (flet ((oops (format-string &rest args)
           (html ((:font :color "red") 
                  (:princ-safe (apply 'format nil format-string args))))))
    (cond
     ((null (find-package package))
      (oops 
       (one-string-nl
        "Internal problem.  Reference to symbol ~A in package ~A"
        "but package ~A does not exist.")
       name package package
       ))
     ((null (setq symbol (find-symbol name (find-package package))))
      (oops 
       (one-string-nl
        "Internal problem.  Reference to symbol ~A in package ~A"
        "but no such symbol exists in that package.")
       name package
       ))
     (t 
      ;; this could be rewritten so that information from the docobject
      ;; representing the symbol (if it exists) is printed out along with
      ;; the information displayed by the below functions 
      (case (keywordize type) 
        (:function 
         (forward-funcall 
          'wb::html-for-find-function-source (keywordize package) name))
        (:variable 
         (forward-funcall 
          'wb::html-for-find-variable-source (keywordize package) name))
        (:type
         (let ((docstring (lisp:documentation symbol 'type)))
           (html 
            (:h3 (:princ-safe
                  (formatn "Type documentation for ~A::~A" 
                           (package-name (symbol-package symbol))
                           (symbol-name symbol))))
            :p
            (if docstring (html (:princ-safe docstring)) (html "None"))
            )))
        (otherwise 
         (oops 
          (one-string-nl
           "Internal problem.  Reference to symbol ~A in package ~A"
           "for documentation type ~A, but that documentation type is unknown.")
          name package type
          ))))))) 
           
          
