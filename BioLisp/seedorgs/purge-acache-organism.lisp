;;; -*- Package: bio; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :bio)

;;; +=========================================================================+
;;; | Copyright (c) 2009 JP Massar
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

;;; Author:  JP Massar

(defmethod purge-organism 
           ((org organism)
            &rest args
            &key (delete-frame? nil) (commit? t) (verbose? t)
            &allow-other-keys)
  (declare (ignorable delete-frame?))
  (call-next-method)
  (when commit? 
    (when verbose? (formatt "Database updated!"))
    (db.ac::commit)
    ))

(defmethod purge-organism 
           ((org seed-organism) 
            &rest args &key (delete-frame? nil) (verbose? t)
            &allow-other-keys)
  (declare (ignorable verbose? delete-frame?))
  (apply 'purge-acache-seed-organism org args)
  )

(defun purge-acache-seed-organisms (orglist)
  (let ((count 0))
    (if (eq orglist :all) 
        (progn
          (cformatt "Removing ALL organisms from acache database...")
          (db.ac::doclass* 
           (orgf 'bio::seed-organism)
           (purge-acache-seed-organism orgf :commit? nil)
           (incf count))
          (db.ac::commit))
      (progn
        (dolist (orgf orglist) 
          (purge-acache-seed-organism orgf :commit? nil)
          (incf count))
        (db.ac::commit)
        ))
    (cformatt "Removed ~D organisms from acache database!" count)
    ))

(defun purge-acache-seed-organism 
       (frame 
        &key 
        (delete-frame? nil) (commit? t) (remove-info-slots? delete-frame?)
        (verbose? t))
  (when (and delete-frame? (not remove-info-slots?))
    (error "Cannot delete the frame and not remove the info slots!"))
  (let ((*disable-seed-organism-autoload* t)
        (seed-id (#^seed-id frame)))
    ;; kill all the block frames associated with the contigs of the organism
    (when (#^organism-loaded? frame)
      (when verbose? (cformatt "Deleting contig block frames..."))
      (loop for c in (#^contiguous-sequences frame)
            do
            (vwhen (gv (attached-frame c #$genes-sorted-by-position))
              (setf (slotv gv #$data) nil)
              (frames::delete-frame gv))
            (vwhen (fv (attached-frame c #$fragments-sorted-by-position))
              (setf (slotv fv #$data) nil)
              (frames::delete-frame fv))
            (let* ((len (#^sequence-length c))
                   (nblocks (ceiling len *contig-sequence-segment-size*)))
              (loop for j from 0 below nblocks 
                    as block = (nth-contig-sequence-block c j)
                    do
                    (frames::delete-frame block)
                    ))))
    ;; kill the contig, gene, and protein data associated with the organism
    ;; if it exists.  The noncoding genes data should be a subset of the
    ;; genes data and therefore would be redundant (and potentially cause
    ;; an error) to delete.
    (flet ((clear-attached-frame (f)
             (loop for d in (#^data f) do (frames::delete-frame d))
             (setf (slotv f #$data) nil)
             (frames::delete-frame f)
             ))
      (loop for slot in *attached-organism-slots* do
            (unless (eq slot #$noncoding-genes)
              (vwhen (af (attached-frame frame slot))
                (clear-attached-frame af)
                ))))
    (when (frames::frame-has-slot? frame #$chromosome)
      (setf (#^chromosome frame) nil))
    (setf (#^organism-loaded? frame) nil)
    (when verbose?
      (cformatt 
       "Organism ~A download info (contigs, genes, proteins, and sequence)"
       frame)
      (cformatt "deleted.  Organism no longer considered loaded."))
    (when remove-info-slots? 
      ;; kill any frames which are values of any direct slots of the organism.
      ;; Clear out all the slot values 
      (loop for x in (frames::aframe-slots frame)
            as v = (cdr x)
            do
            (when (isframe? v) (frames::delete-frame v))
            (setf (cdr x) nil)
            )
      ;; for good measure wipe out all the slots of the organism
      (setf (frames::aframe-slots frame) nil)
      (when delete-frame?
        (setq *available-organisms* (delete frame *available-organisms*))
        (frames::delete-frame frame)
        ;; Since we've removed an organism, we need to reset the 
        ;; data that is used to create the organism menu.
        (set-up-seed-genome-types)
        (when verbose? (cformatt "Organism frame removed."))
        (when *gid->frames-map* (remhash seed-id *gid->frames-map*))
        )
      (setq *seed-genome-frames* (delete frame *seed-genome-frames*))
      )
    (setq *loaded-organisms* (delete frame *loaded-organisms*))
    (when commit? 
      (db.ac::commit)
      (when verbose? (cformatt "Database updated."))
      )))

(defun remove-org-download-info (frame)
  (purge-acache-seed-organism frame :delete-frame? nil :remove-info-slots? nil))


