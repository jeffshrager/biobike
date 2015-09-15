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

(publish 
 :path *help-glossary-entry-url* 
 :content-type cl-user::*html-publish-content-type*
 :function
 (lambda (req ent)
   (let* ((input (request-query req))
          (package-name (url-parameter-value :pkg input))
          (package-symbol (keywordize package-name))
          (name (url-parameter-value :name input))
          (docobj (help::find-documentation name 'help::glossary-entry))
          )
     (execute-with-standard-weblistener-environment
      req ent package-symbol
      (lambda () 
        (com.gigamonkeys.foo:with-html-output ((request-reply-stream req))
          (html-for-glossary-entry-page name docobj))
        )))))

(publish 
 :path *help-glossary-url* 
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
        (com.gigamonkeys.foo:with-html-output ((request-reply-stream req))
          (html-for-entire-glossary)
          ))))))

#+does-not-work-but-why?
(publish 
 :path *help-glossary-url* 
 :content-type cl-user::*html-publish-content-type*
 :function
 (lambda (req ent)
   (let* ((input (request-query req))
          (package-name (url-parameter-value :pkg input))
          (package-symbol (keywordize package-name))
          )
     (com.gigamonkeys.foo:with-html-output ((request-reply-stream req))
       (execute-with-standard-weblistener-environment
        req ent package-symbol
        (lambda ()
          (html-for-entire-glossary)
          ))))))

(defun html-for-glossary-entry-page (name doc-item)
  (if (null doc-item)
      (oops-no-docobj name "Glossary entry")
    (with-slots (name text keywords see-also referred-to-by) doc-item
      (let ((title (formatn "Glossary entry for '~A'" name)))
        (with-standard-weblistener-page-header
            (title :listener? nil)
          (html 
           :br 
           (html-for-glossary-entry doc-item :single-item)
           (html-for-keyword-information keywords)
           (html-for-see-also-information see-also t)
           (html-for-referred-to-by-information referred-to-by)
           :p
           ((:a :href (make-help-glossary-url)) 
            ((:font :color "green") "Show entire glossary."))
           ))))))

(defun html-for-entire-glossary ()
  (let ((title "BioBike System Glossary")
        (alphabet  "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
        (glossary-entries
         (sort  
          (hash-table-contents 
           (gethash 'help:glossary-entry *documentation*))
          'string-lessp
          :key (lambda (x) (forward-package-funcall 
                            :wb :first-alphabetic-sort-key (first x)))
          )))
    (with-standard-weblistener-page-header
        (title :listener? nil)
      (html
       :p
       (loop for char across alphabet do
             (html ((:a :href (formatn "#TAG-~A" char))
                    (:princ-safe (string char)))
                   " "
                   :newline
                   ))
       :p
       (loop
        with previous-tag = (code-char (1- (char-code #\A)))
        for (name entry) in glossary-entries
        as current-tag = 
        (char (forward-package-funcall :wb :first-alphabetic-sort-key name) 0)
        do
        (when (char/= previous-tag current-tag)
          (loop for tagindex 
                from (1+ (char-code previous-tag)) to (char-code current-tag)
                do 
                (html ((:p :id (formatn "TAG-~A" (code-char tagindex))))))
          (setq previous-tag current-tag))
        (html-for-glossary-entry entry :multiple-items)
        (html :newline)
        )
       :p
       ))))

(defun html-for-glossary-entry (glossary-doc-item mode)
  (let ((name (help:name glossary-doc-item))
        (summary (help:docstring glossary-doc-item))
        (text (help:text glossary-doc-item))
        (url (docobj->url glossary-doc-item))) 
    (ecase mode 
      (:multiple-items 
       (html 
        (:big (:b ((:a :href url) "+") " " (:i (:princ-safe (s+ name ": ")))))
        (when summary (html (:i (:princ-safe summary))))
        :p
        ))
      (:single-item 
       (html
        (:big (:b (:i ((:font :color "green") (:princ-safe (s+ name ": "))))))
        (when summary (html (:i (:princ-safe summary))))
        (html-for-text-information text)
        )))))


