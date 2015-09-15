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

(wb::define-url&pkg&args complete-help-listing-url
                         "/new-help/complete-help-listing")

(wb::define-url&pkg&args new-help-entry-url "/new-help/new-help-entry" 
                         :htype :name :npackage)

(wb::define-url&pkg&args new-help-options-url "/new-help/new-help-options")


(defun display-single-word-help-results (results)

  (let* ((exact-matches (single-word-help-results-exact-matches results))
         (near-matches (single-word-help-results-near-matches results))
         (keyword-matches (single-word-help-results-keyword-matches results))
         (text-matches (single-word-help-results-text-matches results))
         (all-matches 
          (append exact-matches near-matches keyword-matches text-matches))
         (remaining-matches all-matches)
         (n-exact (length exact-matches))
         (first-match (first exact-matches))
         (second-match (second exact-matches))
         (n-displayed 0))

    ;; Display algorithm. All matches come with a link to a page which will
    ;; display the documentation for entire item (or the HTML page, etc),
    ;; the type of documentation item and a 1/2 line summary 
    ;; (maybe with a tooltip to show the entire summary string?).

    ;; If there are one or two exact matches, we display the exact matches
    ;; along with up to four total examples and a link to a page that will
    ;; show all other matches, if there are any.

    ;; Then we display more matches up to five total, and if there are
    ;; more than five, we say how many more exact and partial matches
    ;; there are, along with a link that will display a page showing
    ;; all the matches.

    (html 
     (:html
       (:head (:title "Single Word Help Results")
  	((:link  rel "shortcut icon"  href "favicon.ico"))
 	((:link  rel "stylesheet"      type "text/css"   href "vpl.css" ))
  	((:script type "text/javascript"  src "jquery-1.3.2.min.js"))
 	((:script type "text/javascript"  src "json2.js"))
        ;; (:script type='text/javascript' src='queue.js')
        ;; (:script type='text/javascript' src='ajax.js')
        ;; (:script type='text/javascript' src='http.js')
        ;; (:script type='text/javascript' src='vpl.js')
        ;; (:script type='text/javascript' src='vpl-version.js')
        ;; (:script type='text/javascript' src='sexp.js')
        ;; (:script type='text/javascript' src='patches.js')
	((:script type "text/javascript"  src "help_window_Boxes.js"))
       ) ;; end head
       (:body

     (:table 

      (cond
       ((zerop n-exact) nil)
       ((= 1 n-exact)
        (display-match-with-examples first-match 2)
        (setq n-displayed 1)
        (pop remaining-matches))
       ((= 2 n-exact)
        (let ((max (if (and (match-examples first-match)
                            (match-examples second-match)) 
                       2 2)))
          (display-match-with-examples first-match max)
          (display-match-with-examples second-match max)
          (pop remaining-matches) (pop remaining-matches))
        (setq n-displayed 2))
       ((> 2 n-exact) 
        (loop for j from 1 to 5 
              for match in exact-matches 
              do
              (display-match-with-examples match 0)
              (incf n-displayed)
              (pop remaining-matches)
              )))
    
      (when (< n-displayed 5) 
        (loop for j from (1+ n-displayed) to 5
              for match in remaining-matches
              do 
              (display-match-with-examples match 0)
              (incf n-displayed)
              (pop remaining-matches)
              ))
    
      (when remaining-matches 
        (link-for-rest-of-matches 
         (nthcdr 5 exact-matches) 
         remaining-matches
         (length all-matches)))

      )  ;table

      )  ;body

))))      
            


(defun link-for-rest-of-matches (exact-matches all-remaining-matches total)
  (let ((n-exact (length exact-matches))
        (n-remaining (length all-remaining-matches)))
    (html 
     (:tr 
      ((:td :colspan 3) 
       ;; The information to do a complete help listing is passed
       ;; via the property list of the user's sessionID -- the list of
       ;; all matches and the original search string.
       ((:a :href (make-complete-help-listing-url) :target "_blank")
        (:princ-safe 
         (if exact-matches 
             (formatn 
              (one-string
               "... view all ~D matches "
               "(including ~D more exact and ~D partial matches)")
              total n-exact (- n-exact n-remaining))
           (formatn
            "... view all ~D matches (including ~D remaining partial matches)"
                    total n-remaining)
           ))))))))

(defun display-match-with-examples (match n)
  (with-match-data
   (match) 
   (multiple-value-bind (url label)
       (docobj->url&label ref)
     (declare (ignorable url))
     (let ((source-url 
            (typecase ref 
              (help::documentation-file (source-url-for-filedoc ref))
              (otherwise url)
              )))
       (html 
        (:tr 
         (:td ((:a :href source-url :target "_blank") (:princ-safe label)))
         (:td 
          "&nbsp;&nbsp;" 
          (:princ-safe (s+ "[" (abbr-for-help-item match) "]"))
          "&nbsp;&nbsp;")
         (:td 
          (:i (:princ-safe
               (vif (doc (help:docstring ref))
                    (limited-string doc 40) 
                    (if (and (symbolp name)
                             (eq (symbol-package name) (find-package :cl)))
                        "Common Lisp" 
                      "No documentation"
                      ))))))
        (when (plusp n) 
          (vwhen (examples (match-examples match))
                 (html (:tr (:td "")))
                 (loop for j from 1 to n 
                       for example in examples 
                       do 
                       (html 
                        (:tr 
                         ((:td :colspan 3) 
                          (html-for-help-item-example example)
                          )))))))))))

(publish 
 :path *complete-help-listing-url* 
 :content-type cl-user::*html-publish-content-type* 
 :function
 (lambda (req ent)
   (let* ((input (request-query req))
          (package-name (url-parameter-value :pkg input))
          (package-symbol (keywordize package-name))
          )
     (execute-with-standard-weblistener-environment
      req ent package-symbol
      (lambda () (html-for-complete-help-listing))
      ))))

(publish 
 :path *new-help-entry-url*
 :content-type cl-user::*html-publish-content-type* 
 :function
 (lambda (req ent)
   (let* ((input (request-query req))
          (package-name (url-parameter-value :pkg input))
          (package-symbol (keywordize package-name))
          (htype (url-parameter-value :htype input))
          (name (url-parameter-value :name input))
          (npackage (url-parameter-value :npackage input))
          )
     (execute-with-standard-weblistener-environment
      req ent package-symbol
      (lambda (&aux doc-type key doc-obj)
        (block exit
          (flet ((oops (&rest format-args)
                   (html 
                    (:h3 (:b (:princ-safe (apply 'format nil format-args)))))
                   (return-from exit nil)
                   ))
            (setq doc-type (intern htype :help))
            (unless (member doc-type help::*documentation-types*)
              (oops "Unknown documentation type ~S" doc-type))
            (when (and npackage (not (string-equal npackage "NIL"))) 
              (unless (find-package npackage) 
                (oops "Unknown package ~A" npackage)))
            (setq key (if npackage (intern name (find-package npackage)) name))
            (setq doc-obj (help::find-documentation key doc-type))
            (unless doc-obj 
              (oops "Cannot find doc object of type ~S named ~S" doc-type key))
            (html-page-for-new-help-entry doc-obj)
            )))))))

(publish 
 :path *new-help-options-url*
 :content-type cl-user::*html-publish-content-type* 
 :function
 (lambda (req ent)
   (let* ((input (request-query req))
          (package-name (url-parameter-value :pkg input))
          (package-symbol (keywordize package-name))
          )
     (execute-with-standard-weblistener-environment
      req ent package-symbol
      (lambda () (html-for-new-help-options))
      ))))

(defun html-for-new-help-options ()
  (with-standard-weblistener-page-header
      ("BioBike Help Options" :listener? nil)
    (html-for-explain-system-help-options :independent-page? t :full? t)
    ))

(defun text-file-match-exists? (docobj)
  (and 
   (typep docobj 'documentation-file)
   (current-text-file-match docobj)
   ))

(defun current-text-file-match (docobj)
  (find wb::*sessionid* (matches docobj) :key 'first))

(defun string-for-text-match-line (matchline search-term &optional (context 20))
  (let* ((startpos (search search-term matchline :test 'string-equal))
         (endpos nil)
         (linelen nil))
    (cond
     ((<= startpos context) nil)
     (t (setq matchline (s+ "..." (subseq matchline (- startpos context))))))
    (setq startpos (search search-term matchline :test 'string-equal))
    (setq endpos (+ startpos (length search-term)))
    (setq linelen (length matchline))
    (cond
     ((>= (+ endpos context) linelen) nil)
     (t (setq matchline (s+ (subseq matchline 0 (+ endpos context)) "..."))))
    (s+ "'" matchline "'")
    ))

(defun emit-match-with-search-term-bolded 
       (matchline search-term &optional (color "green"))
  (let* ((pos (search search-term matchline :test 'string-equal)))
    (if (null pos)
        (html (:princ-safe matchline))
      (let* ((before (subseq matchline 0 pos))
             (endpos (+ pos (length search-term)))
             (term (subseq matchline pos endpos))
             (after (subseq matchline endpos)))
        (html 
         (:princ-safe before)
         (:b ((:font :color color) (:princ-safe term))))
        (when (plusp (length after)) 
          (emit-match-with-search-term-bolded after search-term)
          )))))

(defgeneric html-for-doc-item-arglist (doc-item)
  (:documentation 
   "Generates html if a doc item has an arglist, otherwise does nothing."))

(defmethod html-for-doc-item-arglist ((doc-item t)) nil)

(defmethod html-for-doc-item-arglist ((doc-item help:function-documentation))
  nil
  #+obsolete
  (let* ((name (help:name doc-item))
         (flavor (help:flavor doc-item)))
    (cond
     ((eq flavor :define-function)
      (emit-syntax 
       (help:syntax doc-item)
       (help:parameters doc-item)
       :define-function
       :para? nil
       :name? nil
       ))
     ((eq flavor :defun)
      (vwhen (arglist 
              (forward-package-funcall :wb :system-specific-arglist-of name))
        (html 
         (:princ-safe 
          (arglist-to-help-display-string 
           arglist (remaining-space-for-arglist name)
           )))))
     (t nil)
     ))
  )

(defmethod html-for-doc-item-arglist ((doc-item help::symbol-doc))
  (let ((name (help:name doc-item)))
    (when (eq (help::dtype doc-item) :function)
      (vwhen (arglist (forward-package-funcall 
                       :wb :system-specific-arglist-of name))
        (html 
         (:princ-safe 
          (arglist-to-help-display-string 
           arglist (remaining-space-for-arglist name)
           )))))))

(defun remaining-space-for-arglist (name) (- 80 (+ (length (string name)) 2) 8))


(defun html-for-help-item-example (example)
  (create-html-from-raw-example example))

(defun abbr-for-help-item (help-item)
  (abbr-for-doc-item (help-match-ref help-item)))



(defun html-for-complete-help-listing ()
  (let* ((matches (get *sessionid* :help-matches))
         (exact-matches (single-word-help-results-exact-matches matches))
         (near-matches (single-word-help-results-near-matches matches))
         (text-matches (single-word-help-results-text-matches matches)) 
         (keyword-matches (single-word-help-results-keyword-matches matches))
         (n-matches (+ (length exact-matches) (length near-matches) 
                       (length keyword-matches) (length text-matches)))
         (search-string (get *sessionid* :current-search-string))
         (title 
          (formatn "Search for '~A' (~D results)" search-string n-matches))
         (all-matches
          (append exact-matches near-matches text-matches keyword-matches)))
    (macrolet ((green (&body body) `((:font :color "green") ,@body)))
      (flet ((header (s) (html (:td (:tt (green (:b (:princ s))))))))
        (html 
         (:head (:title (:princ-safe title)))
         :br
         (:h2 (:center (green (:princ-safe title))))
         :br
         (:table 
          (:tr 
           (header "DOC TYPE")
           (header "&nbsp;&nbsp;NAME OF RESOURCE")
           (header "&nbsp;&nbsp;DESCRIPTION/CONTEXT")
           )
          (loop for match in all-matches do
                (html-for-elhai-formatted-search-result match)
                )))))))

(defun html-for-elhai-formatted-search-result (match)
  (html-for-elhai-formatted-search-result-docobj (help-match-ref match) match))

(defun emit-descriptor-and-docobj-link-tds (match docobj)
  (html
   (:td (:tt (:princ-safe (string-capitalize (abbr-for-help-item match)))))
   :newline
   (:td (:tt "&nbsp;&nbsp;" (html-for-elhai-doc-object-reference docobj)))
   :newline
   ))

(defun emit-descriptor-and-docobj-link-tds-for-docfile (match docobj)
  (let ((source-url (source-url-for-filedoc docobj)))
    (multiple-value-bind (url label)
        (docobj->url&label docobj nil)
      (declare (ignore url))
      (html
       (:td (:tt (:princ-safe (string-capitalize (abbr-for-help-item match)))))
       :newline
       (:td (:tt "&nbsp;&nbsp;" 
             ((:a :href source-url :target "_blank") (:princ-safe label))
             ))
       :newline
       ))))

;; (source-url (source-url-for-filedoc doc-item))

(defun emit-matchline-td (matchline search-term)
  (html
   (:td
    (:tt
     "&nbsp;&nbsp;"
     (emit-limited-match-with-search-term-bolded
      (string-for-text-match-line 
       matchline search-term)
      search-term :color "red" :limit 65
      )))))


(defmethod html-for-elhai-formatted-search-result-docobj ((docobj t) match)
  (let ((search-term (get wb::*sessionid* :current-search-string)))
    (with-match-data (match)
      (html
       (:tr 
        (emit-descriptor-and-docobj-link-tds match docobj)
        (:td
         (:tt
          "&nbsp;&nbsp;"
          (vif (doc (help:docstring docobj))
               (emit-limited-match-with-search-term-bolded 
                doc search-term :color "red" :limit 65)
               (html
                (:princ-safe 
                 (if (and (symbolp name)
                          (eq (symbol-package name) 
                              (find-package :cl)))
                     "Common Lisp symbol" 
                   ""
                   ))))))
        )))))

(defmethod html-for-elhai-formatted-search-result-docobj 
           ((docobj documentation-file) match)
  (let ((search-term (get wb::*sessionid* :current-search-string)))
    (with-match-data (match)
      (if (text-file-match-exists? docobj)
          (with-open-file 
              (p (or (associated-text-file docobj) (source-file docobj))
                 :direction :input)
            (loop 
             while t 
             with match-count = 0
             do
             (let ((line (read-line p nil nil)))
               (when (null line) 
                 (when (>= match-count 6)
                   (html 
                    (:tr 
                     (:td)
                     (:td)
                     (:td 
                      (:i "&nbsp;&nbsp;&nbsp;"
                       (:princ-safe
                        (formatn "(and ~D further lines with matches)" 
                                 (- match-count 5)
                                 )))))))
                 (return nil))
               (when (search search-term line :test 'string-equal)
                 (incf match-count)
                 (cond 
                  ((= 1 match-count)
                   (html
                    (:tr 
                     (emit-descriptor-and-docobj-link-tds-for-docfile
                      match docobj)
                     (:td
                      (:tt
                       "&nbsp;&nbsp;"
                       (emit-limited-match-with-search-term-bolded
                        (string-for-text-match-line 
                         line search-term)
                        search-term :color "red" :limit 65
                        ))))))
                  ((> match-count 5) nil)
                  (t 
                   (html 
                    (:tr (:td) (:td) (emit-matchline-td line search-term))
                    )))))))
        (html 
         (:tr 
          (emit-descriptor-and-docobj-link-tds-for-docfile match docobj)
          ;; Only emit a matchline if in fact the doc matches.  
          (vif (doc (help:docstring docobj))
               (if (search search-term doc :test 'string-equal)
                   (emit-matchline-td doc search-term)
                 (html (:td)))
               (html (:td))
               )))
        
        ))))
                     
         
