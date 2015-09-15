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


;; Loop through all existing documentation items and find those that
;; satisfy a function F, returning for each satisfied entry a list of 
;; the documentation type (e.g., help::module), the name of the documentation
;; item (e.g., "commands"), the documentation instance itself, and 
;; the result of calling F on the documentation instance (this might
;; be a score instead of just T or NIL).  Then sort the results based on
;; this score if a SORT-PREDICATE is provided.  

(defun find-doc-items-if (f &key (sort-predicate nil) (type nil))
  (let ((results nil))
    (maphash 
     (lambda (key subhash)
       (maphash 
        (lambda (name doc-item) 
          (flet ((doit (x) 
                   (vwhen (result (funcall f x))
                     (push 
                      (make-help-match 
                       :doc-type key :name name :ref x 
                       :score result :type type)
                      results
                      ))))
            (setq doc-item (ensure-list doc-item))
            (if (not *help-debug*) 
                (mapcar #'doit doc-item)
              (mapcar 
               (lambda (x) 
                 (handler-case (doit x) 
                   (error 
                    (c) 
                    (print 
                     (list 'name name 'doc-item (help:name doc-item) 'key key))
                    (error c))))
               doc-item
               ))))
        subhash
        ))
     help::*documentation*
     )
    (if (null sort-predicate) 
        (nreverse results)
      (sort results sort-predicate :key 'fourth)
      )))

;; Loop through a subset of symbols as defined by SCOPE, find all such
;; symbols that satisfy a function F.  For each such symbol, create
;; some number of HELP-MATCH records for it, depending on how it is used.
;; Remove any duplicate HELP-MATCH records, then sort the results by the
;; result value from F.

;; This should be obsolete, but might still be useful someday.

#+obsolete
(defun find-symbol-items-if 
       (f &key (scope :user-external) (sort-predicate nil) (type nil))
  (let ((results nil))
    (labels ((doit (symbol) 
               (vwhen (result (funcall f symbol))
                 (loop for symdoc in (maybe-create-symbol-docs symbol) do
                       (push
                        (make-help-match 
                         :doc-type :symbol :name symbol :ref symdoc 
                         :score result :type type)
                        results
                        ))))
             (maybe-push (symbol) 
               (if (not *help-debug*) 
                   (doit symbol)
                 (handler-case (doit symbol)
                   (error 
                    (c)
                    (print (list 'symbol symbol))
                    (error c)))))
             (search-external (packages)
               (loop for p in packages do 
                     (do-external-symbols (s p) (maybe-push s))))
             (search-internal (packages)
               (loop for p in packages do (do-symbols (s p) (maybe-push s)))))
      (ecase scope
        (:user-external 
         (let ((user-package (find-package wb::*username*)))
           (search-internal (list user-package))
           ))
        ((:system-external :biobike-external) 
         (search-external cl-user::*biobike-packages*))
        ((:system-all :biobike-all) 
         (search-internal cl-user::*biobike-packages*)))
      (setq results 
            (purge-duplicates 
             results
             :key 
             (lambda (x) 
               (cons (help-match-name x) (help::dtype (help-match-ref x))))
             :test 'equal
             :hash-threshold 20
             )))
    (if (null sort-predicate) 
        (nreverse results)
      (sort results sort-predicate :key 'help-match-score)
      )))

(defun define-function-p (symbol) (get symbol :define-function-parse))

(defun split-at-whitespace (s)
  (loop for ch in *whitespace* do (setq s (substitute #\Space ch s)))
  (remove-if 
   (lambda (x) (zerop (length x))) 
   (string-split s)))

(defun arglist-to-help-display-string (arglist &optional (limit 40))
  (labels ((all-keywords (x)
             (cond 
              ((null x) nil)
              ((symbolp x) (keywordize x))
              ((listp x) (mapcar #'all-keywords x))
              (t x)
              )))
    (if (null arglist)
        "()"
      (let ((s (limited-string (formatn "~A" (all-keywords arglist)) limit)))
        (if (eql (lastelem s) #\)) s (one-string s ")"))
        ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun symbol->doc-url 
       (symbol 
        &key 
        (hyperspec-first? nil)
        (if-no-documentation? nil)
        (if-only-docstring? nil)
        )
  (let ((fdocobj (find-documentation symbol 'function-documentation))
        (sdocobj (find-documentation symbol 'symbol-doc)))
    (cond 
     ((and (eq (find-package :common-lisp) (symbol-package symbol))
           (external-symbol-of-package? symbol :common-lisp))
      (if hyperspec-first? 
          (wb::common-lisp-external-symbol-url symbol)
        (if fdocobj 
            (docobj->url fdocobj)
          (wb::common-lisp-external-symbol-url symbol)
          )))
     (fdocobj (docobj->url fdocobj))
     (sdocobj (docobj->url (first sdocobj)))
     ((documentation symbol 'function)
      (ecase if-only-docstring? 
        ((nil) nil)
        ))
     ((documentation symbol 'variable)
      (ecase if-only-docstring? 
        ((nil) nil)
        ))
     (t 
      (ecase if-no-documentation? 
        ((nil) nil)
        )))))
