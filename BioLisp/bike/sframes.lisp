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

;; the *bbl-frame-mapping* consists of the gids of all master list
;; organisms with underscores converted to dashes mapped to the 
;; corresponding organism frame, plus the name of all the aliases
;; of the organisms

;; To avoid looping over all organisms to see if symbols name frames, 
;; except in unusual circumstances, we implement the heuristic
;; that any symbol that doesn't have 2 digits in its name
;; is almost certain not to designate a protein, gene, or contig frame.  
;; This isn't strictly true because some contig names have 0 or 1
;; digits, but only about 50 out of 50000.  So if you insist on typing
;; one of those contig names you're just going to lose.  

(defun in-bbl-frame-mapping? (symbol)
  (block exit
    (when (or (null symbol) (eq t symbol)) (return-from exit nil))
    (vif (fname (gethash (string symbol) *bbl-frame-mapping*))
         (frame-fnamed fname)
         (let ((name (string symbol)))
           ;; If the symbol actually names an organism, return
           ;; the organism frame.  
           (vwhen (f (frame-fnamed name))
             (when (is-organism? f) 
               (return-from exit f)
               ))
           ;; Efficiency hack.  See above comments.  
           (if (< (count-if 'digit-char-p (symbol-name symbol)) 2)
               (return-from exit nil)
             (loop 
              for organism in biolisp:*loaded-organisms* 
              as prefix = (#^organism-prefix organism)
              do 
              ;; If the symbol with the organism prefix prepended 
              ;; names a gene, protein, or contig frame, return 
              ;; the frame so found immediately
              (let ((frame (frame-fnamed (one-string prefix name))))
                (when (and frame (or (is-gene? frame) 
                                     (is-protein? frame)
                                     (is-contiguous-sequence? frame)
                                     ))
                  (return-from exit frame)
                  ))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun biobike-frame-sf? (f) (typep f 'frames::frame))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bbl-printed-type-of-frame-or-other (type) 
  (cond
   ((subtypep type 'frames::aframe) :frame)
   (t type)
   ))

      
     
      
      