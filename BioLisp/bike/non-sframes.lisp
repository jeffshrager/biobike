;;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: bbi; -*-

(in-package :bbi) 

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


;; Revised symbol -> frame conversion which checks for duplicate possibilities 
;; with respect to genes, proteins, and contigs across organisms
(defun in-bbl-frame-mapping? (symbol)
  (block exit
    (when (or (null symbol) (eq t symbol)) (return-from exit nil))
    (or (gethash (symbol-name symbol) *bbl-frame-mapping*)
        (let ((possibilities nil))
          (loop 
           with name = (string symbol)
           for organism in biolisp:*loaded-organisms* 
           as prefix = (#^organism-prefix organism)
           as nicknames = (#^nicknames organism)
           do 
           (cond 
            ((member name nicknames :test 'string-equal) 
             (push organism possibilities))
            (t 
             ;; If the symbol actually names the organism, return
             ;; the organism frame.  
             (vwhen (f (frame-fnamed name))
               (when (is-organism? f) 
                 (return-from exit f)
                 ))
             ;; If the symbol with the organism prefix prepended 
             ;; names a gene, protein, or contig frame, store that
             ;; frame as a candidate.  
             (let ((frame (frame-fnamed (one-string prefix name))))
               (when (and frame (or (is-gene? frame) 
                                    (is-protein? frame)
                                    (is-contiguous-sequence? frame)
                                    ))
                 (push frame possibilities)
                 )))))
          (cond
           ((null possibilities) nil)
           ((= (length possibilities) 1) (first possibilities))
           (t (frame-name-ambiguity (string symbol) possibilities))
           )))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; deftype and defconverion forms for this bbl instance

#||
(deftype gene () `(satisfies biobike-gene?))
(deftype protein () `(satisfies biobike-protein?))
(deftype organism () `(satisfies biobike-organism?))
(deftype contiguous-sequence () `(satisfies biobike-contiguous-sequence?))
||#

(defun biobike-frame-nsf? (f) (typep f 'frames::%frame))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bbl-printed-type-of-frame-or-other (type) 
  (cond
   ((eq type 'frames::%frame) :frame)
   (t type)
   ))
  

      
     
      
      