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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This used to emit a link to the source code if the doc-object was 
;; a symbol with a function binding.  But that belongs not at this level
;; but in the code for displaying DOCUMENT-FUNCTION objects.  

(defun html-for-see-also-information (see-also-entries &optional header?)
  (when see-also-entries
    (when header? (html (:h3 (:b "See also: "))))
    (multiple-value-bind (cl-stuff symbol-doc df the-rest)
        (separate-into-lists 
         see-also-entries
         (lambda (x) (eq (see-also-category x) 'help:common-lisp))
         (lambda (x) (eq (see-also-category x) 'help:symbol-doc))
         (lambda (x) (eq (see-also-category x) 'help:function-documentation)))
      (labels ((title (string) 
                 (html "&nbsp;&nbsp; " (:princ-safe string) ": "))
               (emit-list (title-string list)
                 (html 
                  (title title-string)
                  (loop for rest on list 
                        as see-also = (first rest)
                        do
                        (html-for-see-also-reference see-also nil)
                        (when (cdr rest) (html ", ")))                        
                  :br)))
        ;; Print out see-also entries for everything having to do with symbols
        (multiple-value-bind (sdf sdv sdt others)
            (separate-into-lists 
             symbol-doc
             (lambda (x) (eq (third (see-also-id x)) :function))
             (lambda (x) (eq (third (see-also-id x)) :variable))
             (lambda (x) (eq (third (see-also-id x)) :type)))
          (when others 
            (error "Symbol-doc entry not one of :function, :variable, :type!"))
          (when cl-stuff (emit-list "Common Lisp" cl-stuff))
          (when (or df sdf)
            (emit-list "System functions" (append df sdf)))
          (when sdv (emit-list "System variables" sdv))
          (when sdt (emit-list "System types" sdt))
          )
        ;; print out see-also entries for everything else 
        (multiple-value-bind 
            (files glossaries modules topics tutorials frames others urls rest)
            (separate-into-lists
             the-rest
             (lambda (x) (eq (see-also-category x) 'help:documentation-file))
             (lambda (x) (eq (see-also-category x) 'help:glossary-entry))
             (lambda (x) (eq (see-also-category x) 'help:module))
             (lambda (x) (eq (see-also-category x) 'help:topic))
             (lambda (x) (eq (see-also-category x) 'help:tutorial))
             (lambda (x) (eq (see-also-category x) 'help:frame))
             (lambda (x) (eq (see-also-category x) 'help:other))
             (lambda (x) (eq (see-also-category x) 'help:url)))
          (declare (ignore rest))
          (when glossaries (emit-list "Glossary entries" glossaries))
          (when topics (emit-list "Topics" topics))
          (when modules (emit-list "Modules" modules))
          (when tutorials (emit-list "Tutorials" tutorials))
          (when files (emit-list "Files" files))
          (when frames (emit-list "Frames" frames))
          (when urls (emit-list "Web links" urls))
          (when others (emit-list "Other information" others))
          )))))

(defun html-for-see-also-reference (see-also &optional (symbol-type? nil))
  (multiple-value-bind (url label)
      (see-also->url&label see-also symbol-type?)
    (if url 
        (html 
         ((:a :href url) (:code (:princ-safe label))))
      (if label 
          (html (:princ-safe label))
        (html 
         (:princ-safe 
          (s+ "<Unknown Reference: " 
              (let ((id (see-also-id see-also)))
                (if (listp id) (first id) id))
              ">"
              )))))))

;; Returns NIL if no URL is defined or a string
(defun see-also->url&label (see-also &optional (symbol-type? nil))
  (if (see-also-refers-to-docobj? see-also)
      (vif (docobj (see-also->docobj see-also))
           (docobj->url&label docobj symbol-type?)
           (values nil nil))
    (let ((category (see-also-category see-also))
          (id (see-also-id see-also)))
      (ecase category
        (help:common-lisp 
         (values (wb::common-lisp-external-symbol-url (find-symbol id :cl)) id))
        (help:frame
         (values (wb::make-weblistener-frames-url :name id) (s+ "#$" id)))
        (help:other (values nil id))
        (help:url (values (first id) (second id)))
        ))))

 


          
