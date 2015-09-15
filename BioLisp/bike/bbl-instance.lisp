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

;;; This file includes all the code necessary to 'personalize' a biobike
;;; instance.  At the moment this means that the system definer
;;; needs to define which symbols get converted to frames, using the
;;; hashtable and the IN-BBL-FRAME-MAPPING? function.  

(setq *bbl-frame-mapping* 
  (create-hash-table nil :test 'equalp))

(defun frame-name-ambiguity (name p)
  (error 
   (s+ 
    "Attempting to translate ~A into a frame, but ~A is ambiguous.~%"
    "There are ~D possibilities: ~%"
    (apply 
     's+
     (loop for x in p collect (formatn "  ~A~%" (frames:fname x))))
    "Please use (or type in) one of the possibilities given above.~%"
    )
   name name (length p)
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; deftype and defconverion forms for this bbl instance

(defun biobike-gene? (g)
  (ecase cl-user::*frame-system-version*
    (:old
     (and (frames::isframe? g) (eq #$gene (#^organism-entity-type g))))
    (:sframes (typep g 'bio::gene))))

(defun biobike-protein? (p)
  (ecase cl-user::*frame-system-version*
    (:old 
     (and (frames::isframe? p) (eq #$protein (#^organism-entity-type p))))
    (:sframes (typep p 'bio::protein))))
            
(defun biobike-contiguous-sequence? (c)
  (ecase cl-user::*frame-system-version*
    (:old
     (and (frames::isframe? c)
          (eq #$contiguous-sequence (#^organism-entity-type c))))
    (:sframes (typep c 'bio::contiguous-sequence))))

(defun biobike-organism? (o)
  (ecase cl-user::*frame-system-version*
    (:old
     (and (frames::isframe? o) (not (null (#^organism-prefix o)))))
    (:sframes (typep o 'bio::organism))
    ))

(defun biobike-metagenome? (m)
  (ecase cl-user::*frame-system-version*
    (:old nil)
    (:sframes (typep m 'bio::metagenome))
    ))

(defun biobike-metagenome-read? (m)
  (ecase cl-user::*frame-system-version*
    (:old nil)
    (:sframes (typep m 'bio::metagenome-read))
    ))

(defun biobike-frame? (f)
  (ecase cl-user::*frame-system-version*
    (:old (forward-funcall 'biobike-frame-nsf? f))
    (:sframes (forward-funcall 'biobike-frame-sf? f))
    ))

(deftype frame () `(satisfies biobike-frame?))

(deftype gene () `(satisfies biobike-gene?))
(deftype protein () `(satisfies biobike-protein?))
(deftype organism () `(satisfies biobike-organism?))
(deftype contiguous-sequence () `(satisfies biobike-contiguous-sequence?))
(deftype metagenome () `(satisfies biobike-metagenome?))
(deftype metagenome-read () `(satisfies biobike-metagenome-read?))

(defun is-gene? (frame)
  "Checks to see if the frame represents a gene"
 (biobike-gene? frame))

(defun is-protein? 
       (frame)
  "Checks to see whether a frame is a protein"
  (biobike-protein? frame))

(defun is-contiguous-sequence? (frame) (biobike-contiguous-sequence? frame))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bbl-printed-type-of (x)
  (let ((type (type-of x)))
    (cond
     ((eq type 'cons) 'list)
     ((and (listp type) 
           (eq (first type) 'simple-array)
           (eq (second type) 'character))
      :string)
     (t (bbl-printed-type-of-frame-or-other type))
     )))
     
      
      