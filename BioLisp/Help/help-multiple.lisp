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


;;; Do the single word help algorithm on each word, gathering up the matches.
;;; Then combine the results, sorting the hits by figuring out how many
;;; times each hit occurs: This takes priority over the score, but within
;;; each group of equal numbers of hits, they are still sorted by score.


(defun multiple-word-help (words &rest keys &key &allow-other-keys)
  (setq words (mapcar 'string words))
  (setq words (remove-if (lambda (x) (gethash x *stop-table*)) words))
  (setq words (remove-if (lambda (x) 
                           (and (stringp x) 
                                (or (zerop (length x))
                                    (every 'whitespacep x))))
                         words))

  ;;(print "Multiple-word-help ")(prin1 words)(princ " ")(prin1 keys)(terpri)

  (let* ((*current-search-string* (string-join words #\Space))
         (multiple-results 
          (loop for word in words 
                as matches = (apply 'single-word-help word keys)
                when matches 
                collect matches
                ))
         (merged-match-lists 
          ;; (print (list 'len (length multiple-results)))
          (loop
           for match-accessor in '(single-word-help-results-exact-matches
                                   single-word-help-results-near-matches
                                   single-word-help-results-keyword-matches)
           as docobj-hash = (make-hash-table :test 'eq)
           collect
           (loop for match-record in multiple-results 
                 as matches = (funcall match-accessor match-record)
                 do 
                 (loop for match in matches 
                       as docobj = (help-match-ref match)
                       do 
                       (if (gethash docobj docobj-hash)
                           (incf (first (gethash docobj docobj-hash)))
                         (setf (gethash docobj docobj-hash) 
                               (list 1 match)
                               )))
                 finally 
                 (return 
                  (mapcar 
                   'second
                   (sort 
                    (hash-table-values docobj-hash) 
                    (lambda (x y) 
                      (cond
                       ((> (first x) (first y)) t)
                       ((< (first x) (first y)) nil)
                       (t (> (help-match-score (second x)) 
                             (help-match-score (second y))))))
                    ))))
           )))
    
    ;; return NIL if there are no matches instead of a null help object
    (if (every 'null merged-match-lists)
        nil
      (let ((results 
             (make-single-word-help-results 
              :exact-matches (first merged-match-lists)
              :near-matches (second merged-match-lists)
              :keyword-matches (third merged-match-lists)
              )))
        (setf (get wb::*sessionid* :help-matches) results)
        (setf (get wb::*sessionid* :current-search-string) 
              *current-search-string*)
#||
(princ "Results: ")(print results)
(princ "Exact: ")(print (single-word-help-results-exact-matches results))
(princ "Near:  ")(print (single-word-help-results-near-matches results))
(princ "Keywd: ")(print (single-word-help-results-keyword-matches results))
||#

        results
        ))))
      
    
