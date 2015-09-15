;;; -*- Package: help; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :help)

;;; +=========================================================================+
;;; | Copyright (c) 2002-2006 JP Massar, Jeff Shrager, Mike Travers           |
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

;;; Authors:  JP Massar, Jeff Shrager.

(defvar *help-debug* nil)

(defvar *current-search-string* nil)

(defstruct help-match doc-type name ref score type)

#+obsolete
(defun help-init ()
  #+not-yet
  (process-externaldf-directories)
  (process-externaldf-directory)
  (create-symbol-doc-entries)
  (see-also-second-pass)
  (unless *dont-even-init-jhelp*    ;;in jhelp-control-panel, natch.
    (jhelp-init)) ;; JKM.  Must get called last, after *documentation* set up.
  )

(Defun help-init ()
  (if *new-text-extraction-mode*
      (process-externaldf-directories)
    (process-externaldf-directory))
  (create-symbol-doc-entries)
  (see-also-second-pass)
  (unless *dont-even-init-jhelp*    ;;in jhelp-control-panel, natch.
    (jhelp-init)) ;; JKM.  Must get called last, after *documentation* set up.
  )

;;; New Help command.  If any tokens are keywords, assume it and
;;; all tokens beyond are keyword/value pairs which specify how
;;; HELP is to function.  All previous tokens must be symbols or
;;; strings and constitute the topic that HELP is trying to find
;;; information about.

(define-symbol-macro help (help))

(defmacro help (&rest args)
  (vif (kpos (position-if 'keywordp args))
       `(help-function ',(subseq args 0 kpos) ,@(subseq args kpos))
       `(help-function ',args)
       ))

;;; HELP is divided into two.
;;; One set of functionality for requests for information about
;;; a single word -- HELP tries to locate documentation objects whose
;;; names match or nearly match the single word, or whose keywords match
;;; or nearly match those names.
;;; The other for requests about multiple words or phrases -- here HELP
;;; tries to location documentation objects whose name, keywords, 
;;; textual description and/or other textual fields seem relevant to the phrase.

(defun use-jhelp? () (and *use-jhelp* wb::*vpl-executing?*))

(defun help-function (search-tokens &rest keys &key &allow-other-keys)
  (block exit
    ;; Must be an object that returns.
    (loop for token in search-tokens do
          (unless (or (stringp token) (symbolp token))
            (help-oops token)
            (return-from exit (values))
            ))
    (let ((len (length search-tokens)))
      (cond
       ((zerop len) (explain-system-help-options))
       ((= 1 len)
        (if (string-equal "HELP" (string (first search-tokens)))
            (explain-help-itself)
          (progn
            (when (> *jhelp-debug* 0)
            (formatt "help-function LENGTH 1: ~s~%" search-tokens))
          (if (use-jhelp?)
              (apply 'multiple-word-jhelp search-tokens keys)  ;;K I S S.
           (apply 'single-word-help (first search-tokens) keys)))))
            ;;  "keys" is a rest-list of unknown length, therefore the apply.
       (t 
          (progn
            (when (> *jhelp-debug* 0)
            (formatt "help-function LENGTH N: ~s~%" search-tokens))
        (if (use-jhelp?)
            (apply 'multiple-word-jhelp search-tokens keys) 
          (apply 'multiple-word-help search-tokens keys))))
 
       ))))

(defparameter *help-basic-examples*
  (list
   ";;   (help string)"
   ";;   (help \"cond\")"
   ";;   (help orthologs of a gene)"
   ";;   (help \"operating system\")"
   ))


(defun help-oops (obj)
  (formatt 
   (one-string-nl
    ""
    ";; Don't know how to get HELP for ~S."
    ";; (HELP expects to get symbols or strings.)"
    ";; Here are a few examples of HELP:"
    (apply 's+ *help-basic-examples*)
    ";;"
    ";; Type (help help) for more examples and usage information."
    )
   obj))

(defun explain-system-help-options (&optional (full? nil))
  (forward-package-funcall 
   :wb 
   :make-function-for-html
   :f
   (lambda () (html-for-explain-system-help-options :full? full?))
   ))

(defun help-for-bbl-users-url ()
  (make-help-topic-url :name "HELP for BBL users"))

(defun html-for-explain-system-help-options
       (&key (independent-page? nil) (full? nil))
  (html 
   (:h3 "Some Biobike Help facilities.")
   (:ul 
    (:li 
     "Send " (:b ((:font :color "green") "Email")) " to:"
     :br
     (:i (:princ-safe cl-user::*default-support-email-address*))
     :br
     "(You should receive a reply within 24 hours, and often much sooner!)"
     )
    :p
    (:li (:b (:big ((:font :color "red") "BIOLOGISTS and BBL users")))
     (:big ", " 
      ((:a :href (help-for-bbl-users-url)) "click here")) ".")
    :p
    (:li 
     "Use the " (:b ((:font :color "green") "Help Menu")) " at the bottom of "
     (:princ-safe (if independent-page? "the Weblistener" "this page"))
     " to peruse the main BioBike documentation page, and "
     :br
     (:ul 
      (:li 
       "find information about BioBike's programming languages, BioLisp and BBL"
       )
      (:li "run through tutorials about BioBike and Bioinformatics")
      (:li "browse the BioBike glossary")
      (:li "find out what functions are available for your use")
      (:li "send feedback to the BioBike developers")
      ))
    :p
    (:li 
     "Use the " (:b ((:font :color "green") "(help ...) ")) 
     "command to find functions and other relevant information"
     :br
     "on any topic you choose.  Type " 
     (:b ((:font :color "green") "(help help)")) " to see some examples.")
    :p
    (:li 
     "Type " 
     (:b ((:font :color "green") ":?"))
     " (type a colon, then a question mark, then return)"
     " to see a list of system commands.")
    (if full? 
        nil 
      (html 
       :p
       (:li ((:a :href (make-new-help-options-url)) "More Help Options"))))
    )))
      

(defun explain-help-itself ()
  (forward-package-funcall 
   :wb 
   :make-function-for-html
   :f
   (lambda ()
     (mapcar
      (lambda (s) (html (:princ-safe s) :br))
      (append
       (list
        ""
        ";; The HELP command helps you find documentation that could be"
        ";; relevant to the subject you tell it to search for."
        ";; Relevant documentation may include functions, variables,"
        ";; tutorials, essays, web pages, and/or glossary entries."
        ";; HELP will list out for you what it thinks are the best matches"
        ";; to your topic, along with a link for you to examine all possibly"
        ";; relevant documents."
        ";;"
        ";; Here are a few examples of how to use HELP:"
        ";;"
        )
       *help-basic-examples*
       ))
     (html 
      :p
      ((:a :href (make-new-help-options-url) :target "_blank")
       "Other ways to get help.")
      :p
      ((:a :href (make-help-function-documentation-url  
                  :name "HELP" :package "HELP") :target "_blank")
       (:princ-safe "Complete documentation for the HELP command.")
       )))))

(defun help-about-specific-symbol (symbol)
  (forward-package-funcall 
   :wb 
   :make-function-for-html
   :f
   (lambda ()
     (let ((symbol-matches 
            (find-doc-items-if
             (lambda (docobj)
               (and (or (typep docobj 'help:symbol-doc) 
                        (typep docobj 'help:function-documentation))
                    (eq symbol (help:name docobj))
                    0
                    ))
             :type :exact-doc
             )))
       (loop for match in symbol-matches do
             (html (:table (display-match-with-examples match 2)))
             )))))


(defmacro with-match-data ((match) &body body)
  (let ((obj (gensym "MATCH-")))
    `(let* ((,obj ,match)
            (doc-type (help-match-doc-type ,obj))
            (name (help-match-name ,obj))
            (ref (help-match-ref ,obj))
            (score (help-match-score ,obj))
            (type (help-match-type ,obj))
            )
       (declare (ignorable doc-type name ref score type))
       ,@body
       )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun apropos+ 
       (search-term 
        &key
        (package-or-packages (cons (find-package :common-lisp) cl-user::*biobike-packages*))
        (limit 50))
  "Finds near matches based on edit distance or word homology"
  (setq search-term (string-trim utils:*whitespace* (string search-term)))
  (let ((pkgs (ensure-list package-or-packages))
        (term (string-upcase search-term)))
    (if (find #\Space search-term)
        (apropos+-via-word-homology term pkgs limit)
      (apropos+-via-edit-distance term pkgs limit)
      )))

(defvar *apropos+-hits* nil)

(defun apropos+-via-edit-distance (s pkgs limit)
  (let ((*apropos+-hits* nil) (slen (length s)))
    (flet ((add-symbol (sym score) (add-to-apropos+-matches sym score)))
      (loop 
       for pkg in pkgs
       as pkg-object = (find-package pkg)
       do
       (if pkg-object
           (do-symbols (sym pkg-object)
             (when (eq pkg-object (symbol-package sym))
               (let* ((name (symbol-name sym)) (nlen (length name)))
                 (cond
                  ((and (>= nlen slen) (search s name))
                   (add-symbol sym (- 1.0 (/ slen nlen))))
                  ((and (< nlen slen) (search name s))
                   (add-symbol sym (- 1.0 (/ nlen slen))))
                  (t
                   (let ((ed (near-name-match s name))) 
                     (when ed (add-symbol sym (score->fraction ed)))
                     ))))))
         (cformatt "No package named ~A..." pkg)
         ))
      (forward-funcall
       'wb::make-function-for-html 
       :f 
       (lambda (matching-symbols) (two-column-display matching-symbols limit))
       :args
       (list
        (let ((results 
               (mapcar 
                (lambda (x) (list (first x) (third x)))
                (sort *apropos+-hits* '< :key 'second)
                )))
          (if (> (length results) limit) (subseq results 0 limit) results)
          ))))))

(defun apropos+-via-word-homology (search-term pkgs limit)
  (let ((all-symbols-with-docstrings 
         (loop 
          for pkg in pkgs
          with documented-symbols = nil
          as pkg-object = (find-package pkg)
          do
          (if pkg-object
              (do-symbols (sym pkg-object)
                (when (eq pkg-object (symbol-package sym))
                  (let* ((type nil)
                         (docstring 
                          (or (prog1 (documentation sym 'function)
                                (setq type :function))
                              (prog1 (documentation sym 'variable)
                                (setq type :variable))
                              (prog1 (documentation sym 'type)
                                (setq type :type))
                              )))
                    (when docstring 
                      (push (list sym docstring type) documented-symbols)
                      )))))
          finally
          (return documented-symbols))))
    (let ((matches 
           (word-homology-fast 
            search-term all-symbols-with-docstrings limit 'second)
           ))
      (forward-funcall
       'wb::make-function-for-html 
       :f 
       (lambda (matching-symbols) (two-column-display matching-symbols limit))
       :args
       (list
        (mapcar 
         (lambda (x)
           (destructuring-bind (score (symbol docstring type)) x
             (declare (ignore score docstring))
             (list symbol type)))
         matches
         ))))))

(defun two-column-display (matching-symbols limit) 
  (html
   (:table
    (loop for remaining on matching-symbols by #'cddr
          for count from 0 by 2
          as m1 = (first remaining)
          as m2 = (second remaining)
          until (and limit (>= count limit))
          do
          (html
           (:tr
            (loop for match in (list m1 m2) 
                  as symbol = (first match)
                  as type = (second match)
                  when symbol
                  do
                  (let ((url
                         (make-help-symbol-doc-url
                          :name (url-safe-string (symbol-name symbol))
                          :package 
                          (package-name (symbol-package symbol))
                          :type (string type)
                          )))
                    (html
                     (:td 
                      ((:a :href url)
                       (:princ-safe (s+ (symbol-name symbol) " ")))
                      (:princ-safe (type-abbrev type)))
                     )))))))))

(defun type-abbrev (type)
  (ecase type (:function "[F]") (:variable "[V]") (:type "[T]")))

(defun add-to-apropos+-matches (sym score)
  (when (fboundp sym) (push (list sym score :function) *apropos+-hits*))
  (when (boundp sym) (push (list sym score :variable) *apropos+-hits*))
  (ignore-errors
    (when (subtypep sym t) (push (list sym score :type) *apropos+-hits*))
    ))
