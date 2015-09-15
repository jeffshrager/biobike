;;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: bbi; -*-

(IN-PACKAGE :bbi)

(defmethod vpl-data-menu-variables 
           ((instance cl-user:biolingua)
            (organism-descriptor (eql :viruses)))
  (append 
   *common-vpl-data-menu-variables* 
   ;; put any variables specific to viruses in a quoted list here 
   ;; E.g., '(*frob* *virus-goo*) 
   nil
   ))

(defmethod organism-subset-variables ((descriptor (eql :viruses)))
  (copy-list
   '(*all-sequences* *metagenomes* *marine-metagenomes* *known-viruses* 
     *eukaryotic-viruses* *prokaryotic-viruses* *dsDNA-viruses* 
     *ssRNA-viruses* *ssDNA-viruses* *dsRNA-viruses* 
     *hot-springs* *hot-springs-edited*
     )))


;;; Stuff specific to the :viruses *organisms-descriptor* value

(define-symbol-macro *all-sequences* (biolisp::loaded-organisms))

(define-symbol-macro *metagenomes* (compute-metagenomes))

(define-symbol-macro *marine-metagenomes*
   (compute-marine-metagenomes))

(define-symbol-macro *known-viruses* (compute-known-viruses))

(define-symbol-macro *eukaryotic-viruses* (compute-eukaryotic-viruses))

(define-symbol-macro *prokaryotic-viruses* (compute-prokaryotic-viruses))

(define-symbol-macro *dsDNA-viruses* (compute-dsDNA-viruses))

(define-symbol-macro *ssRNA-viruses* (compute-ssRNA-viruses))

(define-symbol-macro *ssDNA-viruses* (compute-ssDNA-viruses))

(define-symbol-macro *dsRNA-viruses* (compute-dsRNA-viruses))

(define-symbol-macro *hot-springs* (compute-hot-springs-viruses))

(define-symbol-macro *hot-springs-edited* (compute-hot-springs-edited-viruses))

(DEFUN Compute-metagenomes ()
   (LOOP FOR seq IN *all-sequences*
         WHEN (EQUAL (SLOTV seq #$class ) "metagenome")
           COLLECT seq)) 

(DEFUN Compute-marine-metagenomes ()
   (LOOP FOR set IN *metagenomes*
         AS habitat = (SLOTV set #$habitat)
         WHEN (AND (> (LENGTH habitat) 5)
                   (EQUALP (SUBSEQ habitat 0 6) "marine"))
           COLLECT set))

 (DEFUN Compute-known-viruses ()
   (SET-DIFFERENCE *all-sequences* *metagenomes*))

 (DEFUN Compute-eukaryotic-viruses ()
   (LOOP FOR virus IN *known-viruses*
         AS class = (SLOTV virus #$class)
         WHEN (AND (> (LENGTH class) 2)
                   (EQUALP (SUBSEQ class 0 3) "euk"))
           COLLECT virus))

 (DEFUN Compute-prokaryotic-viruses ()
   (SET-DIFFERENCE *known-viruses* *eukaryotic-viruses*))

 (DEFUN Compute-dsDNA-viruses ()
   (LOOP FOR virus IN *known-viruses*
         AS type = (SLOTV virus #$molec-type)
         WHEN (AND (> (LENGTH type) 2)
                   (OR (EQUALP type "DNA")
                       (EQUALP type "ds-DNA")))
           COLLECT virus))

 (DEFUN Compute-ssDNA-viruses ()
   (LOOP FOR virus IN *known-viruses*
         AS type = (SLOTV virus #$molec-type)
         WHEN (AND (> (LENGTH type) 2)
                   (EQUALP type "ss-DNA"))
           COLLECT virus))

 (DEFUN Compute-dsRNA-viruses ()
   (LOOP FOR virus IN *known-viruses*
         AS type = (SLOTV virus #$molec-type)
         WHEN (AND (> (LENGTH type) 2)
                   (EQUALP type "ds-RNA"))
           COLLECT virus))

 (DEFUN Compute-ssRNA-viruses ()
   (LOOP FOR virus IN *known-viruses*
         AS type = (SLOTV virus #$molec-type)
         WHEN (AND (> (LENGTH type) 2)
                   (EQUALP type "ss-RNA"))
           COLLECT virus))

(defun compute-hot-springs-viruses ()
  (organism-symbol-values '(:octopus :bearpaw)))

(defun compute-hot-springs-edited-viruses ()
  (organism-symbol-values '(:octopus-e :bearpaw-e)))

