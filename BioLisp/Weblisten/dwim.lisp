;;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: wb; -*-

(in-package :wb) 

;;; +=========================================================================+
;;; | Copyright (c) 2005 JP Massar, Jeff Elhai, Mark Slupesky, Peter Seibel   |
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

;;; Author: JP Massar. 

(defparameter *default-near-match-threshold* 3)

(defun edit-distance-threshold (symbol-length)
  ;; completely arbitrary, but seems reasonable
  (cond
   ((< symbol-length 3) 0)
   ((< symbol-length 6) 1)
   ((< symbol-length 9) 2)
   ((< symbol-length 12) 3)
   ((< symbol-length 16) 4)
   ((< symbol-length 20) 5)
   (t 6)))
  

;;; %%% Needs to be memoized and optimized thoughtfully to dynamically
;;; gen the user's own symbols but not the standard ones all the time %%%

(defun candidate-symbols 
       (&key (packages (cons *package* (package-use-list *package*)))
             (preferred-package *package*))
  (unless (or (null preferred-package) (member preferred-package packages))
    (error "In candidate-symbols: Preferred package isn't in packages!"))
  (purge-duplicates 
   (loop for package in packages
         as pkg = (find-package package)
         when pkg 
         nconc 
         (remove-if-not
          (lambda (x) 
            (multiple-value-bind (symbol type)
                (find-symbol (string x) pkg)
              (and (or (boundp symbol) (fboundp symbol))
                   (or (eq preferred-package (symbol-package symbol))
                       (eq :external type)))
              ))
          (apropos-list "" pkg)
          ))))

(defun near-matches 
       (symbol 
        &key
        (preferred-package *package*)
        (packages (cons *package* (package-use-list *package*)))
        (frames? nil)
        (threshold (edit-distance-threshold (length (string symbol)))))
  (when (plusp threshold)
    (let* ((candidates 
            (candidate-symbols 
             :packages packages :preferred-package preferred-package))
           (symbol-name (string symbol))
           (ms-max-length (+ (length symbol-name) threshold))
           (ms-min-length (- (length symbol-name) threshold)))
      (let ((matches 
             ;; Calculate the edit-distance of those symbols which could
             ;; possibly be transformed into SYMBOL in no more than
             ;; THRESHOLD steps; collect all the symbols and their 
             ;; thresholds if and only if they are below THRESHOLD
             (loop with ed = 0
                   for c in candidates 
                   as cstring = (fstring c)
                   as clen = (length cstring)
                   when (and (within-threshold 
                              clen ms-min-length ms-max-length)
                             (setq
                              ed 
                              (compute-edit-distance 
                               cstring symbol-name))
                             (<= ed threshold))
                   collect
                   (list c ed)
                   )))
        (values 
         (sort matches '< :key 'second) 
         (when frames? (sort (frame-near-matches symbol) '< :key 'second))
         )))))
        

;;; Check symbol against all frame names for edit distance similarity.  
;;; If a frame name is not similar, if the frame has a '.' prefix
;;; check also for the string after the '.' being similar.  
;;; Return all those frames which match either criterion.  

(defun frame-near-matches (symbol)
  (let* ((symbol-name (string-upcase (string symbol)))
         (threshold (edit-distance-threshold (length symbol-name)))
         (ms-max-length (+ (length symbol-name) threshold))
         (ms-min-length (- (length symbol-name) threshold)))
    (nmapframes 
     (lambda (f) 
       (let* ((fname (fname f))
              (flen (length fname))
              (pos (position #\. fname))
              (foo nil))
         (cond
          ((within-threshold flen ms-min-length ms-max-length)
           (when foo (break))
           (let ((ed (compute-edit-distance 
                      (string-upcase fname) symbol-name)))
             (when foo (break "~D" ed))
             (when (<= ed threshold) (list f ed))
             ))
          (pos 
           (let ((sublen (- flen (1+ pos))))
             (when (within-threshold sublen ms-min-length ms-max-length)
               (let* ((substr (subseq fname (1+ pos)))
                      (ed (compute-edit-distance 
                           (string-upcase substr) symbol-name)))
                 (when (<= ed threshold) (list f ed))
                 ))))))))))

(defun within-threshold (value min max)
  (and (>= value min) (<= value max)))
          
(defun candidate-packages-of-symbol (symbol)
  (cons (symbol-package symbol) (package-use-list (symbol-package symbol))))
(defun candidate-packages-of-package (&optional (package *package*))
  (cons package (package-use-list package)))

(defun partial-match-dashed-symbol (dashed-symbol pieces)
  (let ((s (string dashed-symbol)))
    (if (null (find #\- s))
        nil 
      (let ((symbol-pieces (simple-string-split s #\-))
            (match-count 0))
        (loop for p in symbol-pieces do 
              (when (find p pieces :test 'string-equal) (incf match-count)))
        (>= match-count (1- (min (length pieces) (length symbol-pieces))))
        ))))
            
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

(defvar *frames-nicknames-hashtable* nil)


(defun frame-nicknames-hashtable ()
  (let ((ht (make-hash-table :test 'equalp)))
    (flet ((maybe-add-nickname (nickname frame) 
             (when (or (stringp nickname) (symbolp nickname))
               (setq nickname (string nickname))
               (let ((existing? (gethash nickname ht)))
                 (if existing? 
                     (let ((ef (if (listp existing?) 
                                   (first existing?)
                                 existing?)))
                       (cformatt 
                        "Two frames have the same nickname, ~A and ~A"
                        ef frame)
                       (cformatt "Neither frame's nickname will be stored.")
                       (setf (gethash nickname ht) (list frame))
                       )
                   (setf (gethash nickname ht) frame)
                   )))))
      (for-all-frames (f) 
        (vwhen (nickname (#^nickname f)) (maybe-add-nickname nickname f))
        (vwhen (nicknames (#^nicknames f))
          (mapcar (lambda (x) (maybe-add-nickname x f)) nicknames))))
    (maphash 
     (lambda (key value) 
       (when (listp value) (remhash key ht)))
     ht)
    
    ht))
      
|#                      
                
#|                         
(defun tery-test ()
  (let ((scanner (ppcre:create-scanner 
                  "Tery\\.Te\\?(([0-7][0-9][0-9][0-9])|(80[0-3][0-9]))"))
        (max 8039)
        (genes (#^genes tery)))
    (let ((names-matching-pattern-but-not-genes 
           (loop for n from 1 to max
                 as name = (formatn "Tery.Te?~4,'0D" n)
                 when (null (frame-fnamed name))
                 collect name
                 ))
          (genes-not-matching-pattern
           (loop for gene in genes
                 as name = (fname gene)
                 when (null (ppcre:all-matches scanner name))
                 collect name
                 )))
      (pprint names-matching-pattern-but-not-genes)
      (pprint genes-not-matching-pattern)
      (values names-matching-pattern-but-not-genes genes-not-matching-pattern)
      )))
|#      


#|

To find out whether a symbol names a frame, given that frames can have
nicknames and gene and protein frames can be named using an abbreviation
like 'Te?7749' instead of 'Terry.Te?7749': 

  -- First, use frame-fnamed with nil as second argument.  
If it returns a frame, we're done.  

  -- Second, look in nicknames hash table.  If the symbol name
matches, then we're done.  

  -- Third, we iterate through all the organisms, each of which has
associated with it 1) a pattern, 2) a 'matching but not really a gene' table
and 3) a 'not matching but really a gene' table.  Also similar tables
for the proteins.  

If the symbol name matches the pattern then we look in table 2, and if
we find an entry we know the symbol does not designate a frame, and
we're done.  If we don't find an entry, we know the symbol does
designate a frame (although we could check to make sure by prepending
the organism prefix and using frame-fnamed).  

If the symbol name doesn't match the pattern, then we look in table 3, 
and if we find a match, we know the symbol designates a frame and we are done.
If we don't find an entry, then the symbol does not designate a frame and 
we're done.  

|#

#|

(defun symbol-names-a-frame? (symbol)
  "Returns the frame named (or nicknamed) or NIL"
  (let ((name (symbol-name symbol)))
    (or (frame-fnamed name nil) 
        (find-in-frame-nicknames-table name)
        (gene-or-protein-abbreviation? name)
        )))

(defun find-in-frame-nicknames-table (name)
  (when (null *frames-nicknames-hashtable*)
    (setq *frames-nicknames-hashtable* (frame-nicknames-hashtable)))
  (gethash name *frames-nicknames-hashtable*))

(defun gene-or-protein-abbreviation? (name)
  (loop for orgf in *loaded-organisms*
        as prefix = (#^organism-prefix orgf)
        as frame-name = (one-string prefix name)
        as frame = (frame-fnamed frame-name nil)
        do 
        (when frame
          (flet ((oops ()
                   (error 
                    (one-string-nl
                     "The name ~S, when prefixed with the organism prefix ~S,"
                     "denotes a frame, ~A,"
                     "but that frame is not a gene or a protein.")
                    name prefix frame
                    )))
            (ecase *frame-system-version*
              (:old 
               (unless (member (#^Organism-Entity-Type frame) 
                               '(#$gene #$protein))
                 (oops)))
              (:new
               (unless (or (typep frame 'frames::bio.gene)
                           (typep frame 'frames::bio.protein))
                 (oops)))))
          (return frame))))
        
|#
        
#|
        (multiple-value-bind 
            (gene-pattern 
             names-matching-gene-pattern-but-not-genes
             gene-names-not-matching-pattern
             protein-pattern
             names-matching-protein-pattern-but-not-proteins
             protein-names-not-matching-pattern
             )
            (organism-abbreviation-info orgf)
          (if (ppcre:all-matches gene-pattern name) 
              (if (gethash name names-matching-gene-pattern-but-not-genes)
                  nil
                (let ((frame (frame-fnamed frame-name nil)))
                  (unless frame
                    (error
                     (one-string-nl
                      "Ruh Roh.  ~S is supposed to be an abbreviation for"
                      "a gene in the organism ~A, but there is no frame"
                      "named ~S.")
                     name orgf frame-name))
                  (return frame)))
            (vwhen (gene (gethash name gene-names-not-matching-pattern))
              (return gene)))

          (if (ppcre:all-matches protein-pattern name) 
              (if (gethash name names-matching-protein-pattern-but-not-proteins)
                  nil
                (let ((frame (frame-fnamed frame-name nil)))
                  (unless frame
                    (error
                     (one-string-nl
                      "Ruh Roh.  ~S is supposed to be an abbreviation for"
                      "a protein in the organism ~A, but there is no frame"
                      "named ~S.")
                     name orgf frame-name))
                  (return frame)))
            (vwhen (protein (gethash name protein-names-not-matching-pattern))
              (return protein))
            ))
        finally (return nil)
        ))
           
|#           
                

        
