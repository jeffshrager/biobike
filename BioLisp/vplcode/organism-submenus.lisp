;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: nvpl; -*-

(in-package :nvpl)

;;; +=========================================================================+
;;; | Copyright (c) 2006 JP Massar, John Myers                                |
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

;; Author: JP Massar.



(defparameter *dummy-microarray-names* 

(loop for org-marray-data IN bbi::*microarrays*
  collect org-marray-data)

)

(defparameter *dummy-pathway-names* 
  '(
    bbi::*Carbohydrate-Metabolism*
    bbi::*Energy-Metabolism*
    bbi::*Lipid-Metabolism*
    bbi::*Nucleotide-Metabolism*
    bbi::*Amino-Acid-Metabolism*
    bbi::*Metabolism-of-Other-Amino-Acids*
    bbi::*Glycan-Biosynthesis-and-Metabolism*
    bbi::*Biosynthesis-of-Polyketides-and-Nonribosomal-Peptides*
    bbi::*Metabolism-of-Cofactors-and-Vitamins*
    bbi::*Biosynthesis-of-Secondary-Metabolites*
    bbi::*Xenobiotics-Biodegradation-and-Metabolism*
    bbi::*Transcription*
    bbi::*Translation*
    bbi::*Folding-Sorting-and-Degradation*
    bbi::*Replication-and-Repair*
    bbi::*Membrane-Transport*
    bbi::*Signal-Transduction*
    bbi::*Cell-Motility*
    ))

;;; Utility functions

(defun shortest-org-symbol (orgf)
  (let ((shortest-symbol nil))
    (loop for s in (#^organism-symbols orgf) 
          as len = (length (symbol-name s))
          do
          (when (or (null shortest-symbol) 
                    (> (length (symbol-name shortest-symbol)) len))
            (setq shortest-symbol s)
            ))
    shortest-symbol
    ))

(defun hack-organism-related-name-and-add-to-data-hash (name)
  (menu-item
   (string-capitalize
    (remove
     #\*
     (substitute #\Space #\- (string name))))
   (get-id-and-add-vpl-data-to-hash name)
   ))

;;; Generic functions for organism submenu creation

(defgeneric create-organism-submenus (descriptor) 
  (:documentation "returns a list of submenus, none of which can be NIL"))

(defmethod create-organism-submenus ((d t))
  (list 
   (create-palette-submenu 
    "organisms" 
    ;;;;; [no longer] SHORT CIRCUITED create-loaded-organism-submenus
    ;;;;;   because VPL fails when it is executed, unless 
    ;;;;;   the descriptor is NIL
    (create-loaded-organism-submenus cl-user::*organisms-descriptor*)
    (create-other-organism-options)
    )))

(defgeneric create-loaded-organism-submenus (descriptor)
  (:documentation "returns a list of submenus specific to loaded organisms"))

(defmethod create-loaded-organism-submenus ((organism-descriptor t))
  ;; the reverse is to make it consistent with the preloaded order
  (let ((loaded (sort (reverse (bio::loaded-organisms)) 'string-lessp
                      :key 'frames::fname)))
    (loop for orgf in loaded
          as shortest = (shortest-org-symbol orgf)
          as orgname = (frames:fname orgf)
          as gene-operator-name = (numeric-opcode-operator 
                                   'add-gene-hole 
                                   'create-add-gene-hole-operator
                                   orgname)
          as protein-operator-name =  (numeric-opcode-operator 
                                       'add-protein-hole 
                                       'create-add-protein-hole-operator
                                       orgname)
          collect
          (create-palette-submenu
           (frames::fname orgf)
           ()
           (create-menu-items 
            (let* ((org-symbol-entry 
                    (menu-item 
                     (symbol-name shortest)
                     (get-id-and-add-vpl-data-to-hash shortest)))
                   (add-gene-entry 
                    (menu-item "Add a gene" gene-operator-name))
                   (add-protein-entry 
                    (menu-item "Add a protein" protein-operator-name))
                   (contigs (#^contiguous-sequences orgf))
                   (contig-entries 
                    ;; only do 10 contigs per organism to avoid 
                    ;; problems with menu display slowness in browser
                    (loop for contig in contigs 
                          for j from 1 to 10
                          collect
                          (menu-item
                           (frames::fname contig)
                           (get-id-and-add-vpl-data-to-hash contig)
                           ))))
              (append (list org-symbol-entry)
                      (list add-gene-entry)
                      (list add-protein-entry)
                      (if (<= (length contigs) 10)
                          contig-entries
                        (append contig-entries (list (menu-item "..." 'noop)))
                        ))))))))

(defgeneric organism-subset-names (descriptor))

(defmethod organism-subset-names ((d t)) (copy-list bbi::*organism-subsets*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun create-subset-of-organism-submenus (organism-descriptor subset)
  ;; the reverse is to make it consistent with the preloaded order
  (declare (ignore organism-descriptor))
  (let* ((sorted-org-list (sort (reverse subset) 'string-lessp
                      :key 'frames::fname)))
    (loop for orgf in sorted-org-list
          as shortest = (shortest-org-symbol orgf)
          as orgname = (frames:fname orgf)
          as gene-operator-name = (numeric-opcode-operator 
                                   'add-gene-hole 
                                   'create-add-gene-hole-operator
                                   orgname)
          as protein-operator-name =  (numeric-opcode-operator 
                                       'add-protein-hole 
                                       'create-add-protein-hole-operator
                                       orgname)
          do 
          (ignore-errors 
            (import (list shortest) :bbl)
            (export (list shortest) :bbl))
          collect
          (create-palette-submenu
           (frames::fname orgf)
           ()
           (create-menu-items 
            (let* ((org-symbol-entry 
                    (menu-item 
                     (symbol-name shortest)
                     (get-id-and-add-vpl-data-to-hash shortest)))
                   (add-gene-entry 
                    (menu-item "Add a gene" gene-operator-name))
                   (add-protein-entry 
                    (menu-item "Add a protein" protein-operator-name))
                   (contigs (#^contiguous-sequences orgf))
                   (contig-entries 
                    ;; only do 10 contigs per organism to avoid 
                    ;; problems with menu display slowness in browser
                    (loop for contig in contigs 
                          for j from 1 to 10
                          collect
                          (menu-item
                           (frames::fname contig)
                           (get-id-and-add-vpl-data-to-hash contig)
                           ))))
              (append (list org-symbol-entry)
                      (list add-gene-entry)
                      (list add-protein-entry)
                      (if (<= (length contigs) 10)
                          contig-entries
                        (append contig-entries (list (menu-item "..." 'noop)))
                        ))))))))

(defun create-subset-of-microarray-submenus (organism-descriptor subset) 
  ;; subset = bbi:microarrays
  ;; the reverse is to make it consistent with the preloaded order
  (declare (ignore organism-descriptor))
  (let* ((sorted-org-list (sort (reverse subset) 'string-lessp
                                :key 'frames::fname)))
    (loop for orgf in sorted-org-list
          ;; as shortest = (shortest-org-symbol orgf)
          ;; as orgname = (frames:fname orgf)
          ;; do 
          ;; (ignore-errors 
          ;; (import (list shortest) :bbl)
          ;; (export (list shortest) :bbl))
          collect
          (create-palette-submenu
           (frames::fname orgf)
           ()
           (create-menu-items 
            (let* (
                   ;; (org-symbol-entry 
                   ;; (menu-item 
                   ;; (symbol-name shortest)
                   ;; (get-id-and-add-vpl-data-to-hash shortest))
                   ;; )
                   (contigs (#^Experiments orgf))
                   (contig-entries 
                    ;; only do 10 contigs per organism to avoid 
                    ;; problems with menu display slowness in browser
                    (loop for contig in contigs 
                          for j from 1 to 10
                          collect
                          (menu-item
                           (frames::fname contig)
                           (get-id-and-add-vpl-data-to-hash contig)
                           ))))
              ;; was (LIST org-symbol-entry)
              (append NIL 
                      (if (<= (length contigs) 10)
                          contig-entries
                        (append contig-entries (list (menu-item "..." 'noop)))
                        ))
              ))))))

(defun create-other-organism-options ()
  (loop for subset-name in 
        (organism-subset-names cl-user::*ORGANISMS-DESCRIPTOR*)
        collect 
        (hack-organism-related-name-and-add-to-data-hash subset-name)
        ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when *load-pathname*
  (let* ((od-source-file-name
          (s+ (string-downcase cl-user::*organisms-descriptor*) 
              "-submenus.lisp"))
         (od-source-path (merge-pathnames od-source-file-name *load-pathname*)))
    (if (probe-file od-source-path) 
        (handler-case 
            (cl-user::compile/load od-source-path)
          (error () (load od-source-path)))
      (formatt ";; No VPL organisms data file for *organisms-descriptor* ~S" 
               cl-user::*organisms-descriptor*
               ))))

(defun generate-seed-organism-selection-operators ()
  `(progn
     ,@(mapcan
        (lambda (frame)
          `((defun-opcode 
                ,(forward-package-funcall 
                  :bio :seed-organism-frame-operator frame) (sid)
              (declare (ignore sid))
              (forward-funcall 'seed-organism-menu-select ,frame)
              )))
        (if user::*master-list-enabled*
            (bio::available-organisms)
          bio::*seed-genome-frames*
          ))))

(defun handle-organism-id (id)
  (let ((orgf (find id (bio::available-organisms)
                    :key #^organism-id :test 'equal)))
    (case user::*organisms-descriptor*
      (:cyanobacteria 
       (forward-funcall 'cyano-organism-menu-select orgf))
      (:seed-organisms
       (forward-funcall 'seed-organism-menu-select orgf))
      (otherwise (vpl-internal-error "Organism ID from menu but no handler!"))
      )))
  

