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

(def-aframe-type organism (aframe))

(def-aframe-type seed-organism (organism))

(def-aframe-type contiguous-sequence (aframe))

(def-aframe-type seed-contiguous-sequence (contiguous-sequence))

(def-aframe-type gene (aframe))

(def-aframe-type seed-gene (gene))

(def-aframe-type protein (aframe))

(def-aframe-type seed-protein (protein))

(def-aframe-type contig-sequence-block (aframe))

(def-aframe-type metagenome (aframe))

(def-aframe-type seed-metagenome (metagenome))

(def-aframe-type metagenome-read (aframe))

(def-aframe-type seed-metagenome-read (metagenome-read))

(defparameter *ncbi-taxonomy-id-root* "1")
(defparameter *ncbi-taxonomy-root-name* "ncbi-taxonomy-root")
(defvar *ncbi-taxonomy-root-frame* 
  (frames::frame-fnamed *ncbi-taxonomy-root-name*))

;;; Special organism methods dealing with attached frames.

(defmethod slotv ((frame seed-organism) (slot t))
  (if (null (member slot *attached-organism-slots*))
      (call-next-method)
    (progn
      (when (and (not (frames::%slotv frame #$organism-loaded?))
                 (null *disable-seed-organism-autoload*))
        (let ((*disable-seed-organism-autoload* t))
          (load-seed-organism (frames::%slotv frame #$seed-id))
          (db.ac::commit)
          ))
      (case *acache-organism-structure*
        (:internal-lists (frames::internal-slotv frame slot))
        (:external-lists
         (attached-frame-value
          frame slot :if-not-attached?
          (if *disable-seed-organism-autoload* nil :warn)
          ))))))

(defmethod frames::set-slotv ((frame seed-organism) (slot t) value)
  (if (member slot *attached-organism-slots*)
      (case *acache-organism-structure*
        (:internal-lists (call-next-method))
        (:external-lists 
         (maybe-create-and-set-attached-frame frame slot value)
         value
         ))
    (call-next-method)
    ))

;; Don't load the organism data if we're just displaying the frame contents
(defmethod frames::describe-frame ((frame seed-organism))
  (let ((*disable-seed-organism-autoload* t))
    (call-next-method)
    ))

(defmethod frames::call-for-each-frame-slot-body-on-attached-slots 
           ((frame seed-organism) function call-with-slot-value?)
  (case *acache-organism-structure*
    (:internal-lists nil)
    (:external-lists
     (loop for slot in *attached-organism-slots* do
           (if call-with-slot-value?
               (funcall function slot (slotv frame slot))
             (funcall function slot)
             )))))

;;; Special contiguous sequence methods dealing with attached frames.

(defmethod frames::set-slotv ((frame seed-contiguous-sequence) (slot t) value)
  (if (member slot *attached-contiguous-sequence-slots*)
      (case *acache-organism-structure*
        (:internal-lists (call-next-method))
        (:external-lists 
         (maybe-create-and-set-attached-frame frame slot value)
         value
         ))
    (call-next-method)
    ))

(defmethod slotv 
           ((frame seed-contiguous-sequence) 
            (slot (eql #$genes-sorted-by-position)))
  (case *acache-organism-structure*
    (:internal-lists (call-next-method))
    (:external-lists (attached-frame-value frame slot))
    ))

(defmethod slotv 
           ((frame seed-contiguous-sequence) 
            (slot (eql #$fragments-sorted-by-position)))
  (case *acache-organism-structure*
    (:internal-lists (call-next-method))
    (:external-lists (attached-frame-value frame slot))
    ))

(defmethod frames::call-for-each-frame-slot-body-on-attached-slots
           ((frame contiguous-sequence) function call-with-slot-value?)
  (case *acache-organism-structure*
    (:internal-lists nil)
    (:external-lists
     (loop for slot in *attached-contiguous-sequence-slots* do
           (if call-with-slot-value?
               (funcall function slot (slotv frame slot))
             (funcall function slot)
             )))))

;;; Special method to extract a DNA sequence using contiguous-sequence
;;; data sequence blocks using acache.  If we're bootstrapping use the 
;;; old code that obtains the sequence from a file; otherwise the entire DNA
;;; sequence has been broken up into subsequences and stored in frames 
;;; indirectly attached to the contig (via a naming convention).

(defmethod extract-contig-sequence 
           ((contig seed-contiguous-sequence) from to direction 
            &key
            (complement-backwards? t)
            (if-wrapped-but-not-circular :error) 
            (safely? t))
  (declare (ignore if-wrapped-but-not-circular safely?))
  (if *extract-sequence-info-bootstrap* 
      (call-next-method)
    (extract-seed-contig-sequence 
     contig from to direction complement-backwards?
     )))

;;; Special method to extract a gene sequence using contiguous-sequence
;;; data sequence blocks, using acache.

(defmethod slotv ((frame seed-gene) (slot (eql #$sequence)))
  (let ((from (slotv frame #$from))
        (to (slotv frame #$to))
        (direction (slotv frame #$direction))
        (contig (slotv frame #$contiguous-sequence))
        )
    (extract-seed-contig-sequence contig from to direction t)
    ))

;;; Special method to obtain a protein sequence using acache.
;;; The sequence is simply stored directly in the frame in the
;;; #$sequence slot.  If we're bootstrapping we use the old code
;;; that obtains the sequence from the indexed fasta file.

(defmethod extract-protein-sequence 
           ((protein seed-protein) &key (strip-trailing-star? t) (cache? nil))
  (declare (ignore strip-trailing-star? cache?))
  (if *extract-sequence-info-bootstrap*
      ;; make all seed sequences uppercase using acahe as per elhai request
      (string-upcase (call-next-method))
    (copy-seq (slotv protein #$sequence))
    ))


;;; Code to extract a DNA sequence from a set of sequence data blocks
;;; associated with a contig frame.

(defun extract-seed-contig-sequence 
       (contig from to direction complement-backwards?)
  (flet ((block-sequence (blocknum)
           (#^sequence (nth-contig-sequence-block contig blocknum))
           ))
    (let* ((blocksize *contig-sequence-segment-size*)
           (raw-sequence
            (if (<= from to)
                ;; From and To are 1-based
                (multiple-value-bind (from-block from-offset)
                    (floor (1- from) blocksize)
                  (multiple-value-bind (to-block to-offset)
                      (floor (1- to) blocksize)
                    (if (= from-block to-block)
                        (subseq 
                         (block-sequence from-block) 
                         from-offset (1+ to-offset))
                      (apply
                       's+
                       (loop 
                        for blocknum from from-block to to-block
                        as sequence = (block-sequence blocknum)
                        collect
                        (cond
                         ((= blocknum from-block) 
                          (subseq sequence from-offset))
                         ((/= blocknum to-block) sequence)
                         ((= blocknum to-block) 
                          (subseq sequence 0 (1+ to-offset)))
                         ))))))
              (let ((seqlen (#^Sequence-length contig)))
                (s+ 
                 (extract-seed-contig-sequence contig from seqlen :f nil)
                 (extract-seed-contig-sequence contig 1 to :f nil)
                 )))))
      (case direction
        (:f raw-sequence)
        (:b 
         (if complement-backwards?
             (ncomplement-base-pairs raw-sequence)
           (nreverse raw-sequence)
           ))))))
          

(defun break-contig-sequence-into-blocks (contig contig-sequence)
  (let ((sequence-blocks
         (break-string-into-blocks-of-size 
          contig-sequence
          *contig-sequence-segment-size*
          )))
    (loop for j from 1 
          for block in sequence-blocks
          as blockname = (contiguous-sequence-block-name contig j)
          as block-frame = 
          (frame-fnamed blockname t 'contig-sequence-block)
          do
          (setf (slotv block-frame #$sequence) block)
          collect block-frame
          )))

(defun contiguous-sequence-block-name (contig block)
  (s+ (#^fname contig) (formatn "-sb-~D" block)))

(defun nth-contig-sequence-block (contig blocknum)
  (let ((blockframe 
         (frame-fnamed (contiguous-sequence-block-name contig (1+ blocknum)))))
    (unless blockframe 
      (error "Internal error!  Block ~D of contig ~D does not exist!" 
             blocknum contig))
    blockframe
    ))
                                         
(defun maybe-create-new-frame-names-for-contig (orgf contig block-data)
  (vif (new-contig-name (new-contig-name-from-table orgf contig))
       (let ((new-frame 
              (frame-fnamed new-contig-name t 'seed-contiguous-sequence)))
         (copy-slots-except 
          contig new-frame (cons #$fname *attached-contiguous-sequence-slots*))
         (loop for block-frame in block-data 
               for j from 1
               as new-block-frame = 
               (frame-fnamed 
                (contiguous-sequence-block-name new-frame j)
                t 'contig-sequence-block)
               do 
               (copy-slots-except block-frame new-block-frame)
               )
         (loop for block-frame in block-data do 
               (frames::delete-frame block-frame))
         (frames::delete-frame contig)
         new-frame
         )
       contig
       ))

(defun copy-slots-except (source-frame dest-frame &optional (except '(#$fname)))
  (frames::for-each-frame-slot (slot) source-frame 
    (cond
     ((member slot except) nil)
     (t (setf (slotv dest-frame slot) (slotv source-frame slot)))
     )))

(defun new-contig-name-from-table (orgf contig)
  (declare (ignore orgf contig))
  nil
  )

(defmethod slotv ((frame seed-gene) (slot (eql #$description)))
  (if (null *enable-descriptions-map*)
      (call-next-method)
    (let* ((mapkey (descriptions-map-key frame))
           (map-value (db.ac::map-value *features-descriptions-map* mapkey)))
      (and map-value (description-from-map-value map-value))
      )))

(defmethod slotv ((frame seed-gene) (slot (eql #$genetic-name)))
  (if (null *enable-descriptions-map*)
      (call-next-method)
    (let* ((mapkey (descriptions-map-key frame))
          (map-value (db.ac::map-value *features-descriptions-map* mapkey)))
      (and map-value (genetic-name-from-map-value map-value))
      )))

(defmethod slotv ((frame seed-gene) (slot (eql #$subsystem-role)))
  (if (null *enable-descriptions-map*)
      (call-next-method)
    (let* ((mapkey (descriptions-map-key frame))
          (map-value (db.ac::map-value *features-descriptions-map* mapkey)))
      (and map-value (ssrole-from-map-value map-value))
      )))

(defmethod frames::set-slotv 
           ((frame seed-gene) (slot (eql #$description)) value)
  (if (null *enable-descriptions-map*)
      (call-next-method)
    (let ((gn (#^genetic-name frame))
          (sr (#^subsystem-role frame)))
      (change-description-map-value frame value gn sr)
      value
      )))

(defmethod frames::set-slotv 
           ((frame seed-gene) (slot (eql #$genetic-name)) value)
  (if (null *enable-descriptions-map*)
      (call-next-method)
    (let ((desc (#^description frame))
          (sr (#^subsystem-role frame)))
      (change-description-map-value frame desc value sr)
      value
      )))

(defmethod frames::set-slotv 
           ((frame seed-gene) (slot (eql #$subsystem-role)) value)
  (if (null *enable-descriptions-map*)
      (call-next-method)
    (let ((desc (#^description frame))
          (gn (#^genetic-name frame)))
      (change-description-map-value frame desc gn value)
      value
      )))

(defun change-description-map-value (frame desc gn sr)
  (let ((mapkey (descriptions-map-key frame))
        (new-value (pack-gene-description-info desc gn sr)))
    (setf (db.ac::map-value *features-descriptions-map* mapkey) new-value)
    ))

(defmethod frames::slotv ((frame seed-organism) (slot (eql #$ncbi-taxonomy)))
  (ncbi-taxonomy-strings frame))

(defmethod frames::slotv ((frame seed-organism) (slot (eql #$host-phylogeny)))
  (ncbi-host-phylogeny-strings frame)
  )

(defun orgname-for-taxonomy (orgf as)
  (case as 
    (:fname (#^fname orgf))
    (:gname (or (#^gname orgf) (#^fname orgf)))
    (otherwise
     (substitute #\Space #\- (#^fname orgf)))
    ))

(defun ncbi-taxonomy-parent-strings (parent)
  (let ((list nil))
    (loop until (eq parent *ncbi-taxonomy-root-frame*)
          do 
          (push (#^ncbi-taxonomy-string parent) list)
          (setq parent (#^parent parent))
          finally (return list)
          )))

(defun ncbi-taxonomy-strings 
       (orgf &key (include-org? nil) (as :fname-with-spaces))
  (vwhen (parent (#^ncbi-taxonomy-parent orgf))
    (let ((list (when include-org? (list (orgname-for-taxonomy orgf as)))))
      (append (ncbi-taxonomy-parent-strings parent) list)
      )))

(defun ncbi-host-phylogeny-strings 
       (orgf &key (include-org? nil) (as :fname-with-spaces))
  (vwhen (host (#^host orgf))
    (vwhen (host-frame (frame-fnamed host))
      (let ((parent (#^parent host-frame))
            (list
             (append 
              (list host) 
              (when include-org? (list (orgname-for-taxonomy orgf as)))
              )))
        (append (ncbi-taxonomy-parent-strings parent) list)
        ))))
      

(defun ncbi-taxonomy-single-string (orgf &optional (include-organism-name? nil))
  (vif (strings (ncbi-taxonomy-strings orgf include-organism-name?))
       (string-join strings "; ")
       ""
       ))
   
