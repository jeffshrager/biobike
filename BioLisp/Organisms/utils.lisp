;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: bio; -*-

(in-package :bio)

;;; +=========================================================================+
;;; | Copyright (c) 2002, 2003, 2004 JP Massar, Jeff Shrager, Mike Travers    |
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

;;; Author:  JP Massar. 


(defvar *loaded-organisms* nil 
  #.(one-string-nl
     "All organisms currently available to BioLingua."
     "Use (AVAILABLE-ORGANISMS) to see the list all accessible organisms."
     "Use (LOAD-ORGANISM ...) to make other organisms available."))

(defvar *crossblast-organisms* nil 
  #.(one-string-nl
     "All organisms whose genes are currently available in the"
     "cross blast table."))

(DEFVAR *common-ortholog-organisms* nil 
  #.(one-string-nl
     "All organisms whose genes are currently available in the"
     "common ortholog table."))

(defun loaded-organisms () (copy-list *loaded-organisms*))

(defun organism-toplevel-directory ()
  (cl-user:translate-simple-lp "bioetc:data;"))

(defun organism-data-directory (orgn &key (dir nil))
  (if dir
      dir
      ;; (append-subdir dir (make-pathname :directory `(:relative ,orgn)))
    (append-subdir
     (organism-toplevel-directory)
     (make-pathname :directory `(:relative ,orgn))
     )))

;; Convert a frame, a symbol, or a string to a lowercase string.
(defun lcstring (x)
  (string-downcase
   (cond 
    ((isframe? x) (slotv x #$Fname))
    (t (string x))
    )))

(defun directory-of-dir (directory-path)
  (directory-with-subdirs-in-directory-form
   (pathname-in-directory-form directory-path)))

(defun valid-architecture-list (arch-list)
  (and (listp arch-list)
       (every
        (lambda (x)
          (and (listp x)
               (eql (length x) 2)
               (let ((from (first x)) (to (second x)))
                 (and (integerp from) (plusp from) (integerp to) (plusp to))
                 )))
        arch-list
        )))

(defun all-slots-of (frame-list)
  (let ((slots-ht (make-hash-table :test 'eq))
        (unique-slots nil))
    (dolist (frame frame-list)
      (for-each-frame-slot (slot value) frame
          (setf (gethash (car slot) slots-ht) t)))
    (maphash 
     #'(lambda (key value) (declare (ignore value)) (push key unique-slots))
     slots-ht)
    unique-slots
    ))

(defun canonicalize-direction (d)
  (cond
   ((or (eq d :f) (eq d :b)) d)
   ((stringp d)
    (cond
     ((string-equal d "f") :f)
     ((string-equal d "b") :b)
     (t (error "Unrecognized direction string: ~S" d))))
   ((symbolp d) (canonicalize-direction (symbol-name d)))
   (t (error "Unrecognized direction object: ~S" d))
   ))

(defun canonicalize-circular? (c)
  (cond
   ((or (eq c t) (eq c nil)) c)
   ((eq c :t) t)
   ((eq c :nil) nil)
   ((stringp c)
    (cond
     ((string-equal c "T") t)
     ((string-equal c "F") nil)
     ((string-equal c "TRUE") t)
     ((string-equal c "FALSE") nil)
     ((string-equal c "NIL") nil)
     (t (error "Unrecognized 'circular?' string: ~S" c))))
   (t (error "Unrecognized 'circular?' object: ~S" c))
   ))

(defun canonicalize-organism-entity-designator (f type create?)
  (let ((result (canonicalize-frame-designator f create?)))
    (unless (eq (#^Organism-Entity-Type result) type)
      (error "Frame ~A should be a ~A, but it is not!" result (fname type)))
    result))

(defun canonicalize-contig-designator (f &optional (create? nil))
  "Turns F into a frame and insures that it is a Contiguous-Sequence frame"
  (canonicalize-organism-entity-designator f #$Contiguous-Sequence create?))

(defun canonicalize-protein-designator (f &optional (create? nil))
  "Turns F into a frame and insures that it is a Protein frame"
  (canonicalize-organism-entity-designator f #$Protein create?))

(defun canonicalize-gene-designator (f &optional (create? nil))
  "Turns F into a frame and insures that it is a Gene frame"
  (canonicalize-organism-entity-designator f #$Gene create?))

(defun genes-of-organism (org)
  "Returns the genes of organism ORG, a frame, symbol or string"
  (slotv (canonicalize-loaded-organism-designator org) #$Genes))

(defun proteins-of-organism (org)
  "Returns the proteins of organism ORG, a frame, symbol or string"
  (slotv (canonicalize-loaded-organism-designator org) #$Proteins))

(defun contigs-of-organism (org)
  "Returns the contigs of organism ORG, a frame, symbol or string"
  (slotv (canonicalize-loaded-organism-designator org) #$Contiguous-Sequences))

(defun canonicalize-organism-name (orgn)
  (string-downcase orgn))

(defun canonicalize-loaded-organism-designator (orgd)
  "Convert an organism designator to a loaded organism's frame."
  (cond
   ((isframe? orgd) orgd)
   (t 
    (let ((orgf (canonicalize-organism-designator orgd)))
      (unless (or (#^organism-loaded? orgf) (#^organism-genes-loaded orgf))
        (error "Organism is not loaded!")
        )))))


(defun canonicalize-organism-designator (orgd)
  #.(one-string-nl
     "Convert an organism designator to an organism frame.  "
     "Search all preloaded organisms, including all their nicknames,"
     "for possible matches.")
  (cond 
   ((isframe? orgd)
    (if (member #$organism (#^isa orgd))
        orgd
      (error "Frame is not an organism frame!")))
   ;; Patch to allow loading private organism to work
   ((frame-fnamed (string orgd))
    (let ((f (frame-fnamed (string orgd))))
      (unless (member #$organism (#^isa f))
        (error "~A names a frame, ~A, which is not an organism frame!"
               orgd f))
      f))
   (t
    (let ((name-or-nickname orgd))
      (block exit
        (loop for orgframe in (available-organisms) do
              (if (string-equal name-or-nickname (fname orgframe))
                  (return-from exit orgframe)
                (loop for nickname in (#^nicknames orgframe) do
                      (when (string-equal name-or-nickname (string nickname))
                        (return-from exit orgframe)
                        )))))))))


(defun wrap-non-positive-index (i seqlen)
  #.(optimization-declaration)
  (declare (fixnum i seqlen))
  (let ((index (the fixnum (+ seqlen i))))
    (declare (fixnum index))
    (if (plusp index) index (wrap-non-positive-index index seqlen))))

(defun wrap->seqlen-index (i seqlen)
  #.(optimization-declaration)
  (declare (fixnum i seqlen))
  (let ((index (the fixnum (- i seqlen))))
    (declare (fixnum index))
    (if (<= index seqlen) index (wrap->seqlen-index index seqlen))))

(defun wrap-sequence-index (i seqlen)
  #.(optimization-declaration)
  (declare (fixnum i seqlen))
  (cond
   ((> i seqlen) (wrap->seqlen-index i seqlen))
   ((< i 1) (wrap-non-positive-index i seqlen))
   (t i)
   ))

(defun wrap-gene-index (i gene)
  (let* ((contig (slotv gene #$Contiguous-Sequence))
         (seqlen (slotv contig #$Sequence-Length)))
    (when (null seqlen)
      (error "Gene ~A's contig has no SEQUENCE-LENGTH property!" gene))
    (wrap-sequence-index i seqlen)
    ))
  
(defun orgf-element-prefix (orgf element)
  (if (eq element :organism)
      (or (slotv orgf #$Organism-Prefix) "")
    (let ((element-property (fff (one-string (string element) "-PREFIX"))))
      (or (slotv orgf element-property)
          (slotv orgf #$Organism-Prefix)
          ""
          ))))

(defun add-prefix-if-not-present (obj prefix)
  (let* ((s (fstring obj))
         (index (search prefix s)))
    (if (and index (zerop index)) s (one-string prefix s))
    ))

#||

#-(or :jpmtf :sframes)
(def-always-computed-slot (#$geneid frame)
  (and (eq (#^Organism-Entity-Type frame) #$Gene)
         (second (string-split (fname frame) #\.))))

||#

(defun make-appropriate-frame-of-type (orgn force? type)
  (unless (stringp type) 
    (error "make-appropriate-xxx must be called with TYPE being a string!")) 
  (cond
   ((member :sframes *features*)
    (frame-fnamed orgn force? (read-from-string type)))
   ((member :nframes *features*)
    (error "Not implemented!"))
   (t 
    (frame-fnamed orgn force?)
    )))

(defun make-appropriate-organism-frame 
       (orgn force? &optional (type "bio::organism"))
  (make-appropriate-frame-of-type orgn force? type)
  )

(defun make-appropriate-contig-frame 
       (orgn force? &optional (type "bio::contiguous-sequence"))
  (make-appropriate-frame-of-type orgn force? type)
  )

(defun make-appropriate-gene-frame 
       (orgn force? &optional (type "bio::gene"))
  (make-appropriate-frame-of-type orgn force? type)
  )

(defun make-appropriate-protein-frame 
       (orgn force? &optional (type "bio::protein"))
  (make-appropriate-frame-of-type orgn force? type)
  )

(defun appropriate-extant-organism-set ()
  (cond
   ((member :sframes *features*) (available-organisms))
   ((member :nframes *features*) (error "Not implemented!"))
   (t *loaded-organisms*)
   ))


(defparameter *non-ascii-char-mapping*

  #||

  ;; don't have any non-standard ascii characters as lisp readable
  ;; text in the file

  '(
    (#\Ä "Ae")
    (#\Ö "Oe")
    (#\Ü "Ue")
    (#\ä "ae")
    (#\ö "oe")
    (#\ü "ue")
    (#\À "A")
    (#\Á "A")
    (#\È "E")
    (#\É "E")
    (#\Ì "I")
    (#\Í "I")
    (#\Ò "O")
    (#\Ó "O")
    (#\Ù "U")
    (#\Ú "U")
    (#\à "a")
    (#\á "a")
    (#\è "e")
    (#\é "e")
    (#\ì "i")
    (#\í "i")
    (#\ò "o")
    (#\ó "o")
    (#\ù "u")
    (#\ú "u")
    )

||#

  `(
    (,(code-char 196) "Ae")
    (,(code-char 214) "Oe")
    (,(code-char 220) "Ue")
    (,(code-char 228) "ae")
    (,(code-char 246) "oe")
    (,(code-char 252) "ue")
    (,(code-char 192) "A")
    (,(code-char 193) "A")
    (,(code-char 200) "E")
    (,(code-char 201) "E")
    (,(code-char 204) "I")
    (,(code-char 205) "I")
    (,(code-char 210) "O")
    (,(code-char 211) "O")
    (,(code-char 217) "U")
    (,(code-char 218) "U")
    (,(code-char 224) "a")
    (,(code-char 225) "a")
    (,(code-char 232) "e")
    (,(code-char 233) "e")
    (,(code-char 236) "i")
    (,(code-char 237) "i")
    (,(code-char 242) "o")
    (,(code-char 243) "o")
    (,(code-char 249) "u")
    (,(code-char 250) "u")
    )
  )

(defun check-and-replace-org-frame-strings-with-bad-chars
       (org &key (string-list-slots nil) (mapping *non-ascii-char-mapping*))
  (let ((changed-frames nil))
    (flet ((check-frame-for-non-ascii-strings (f)
             (for-each-frame-slot (slot value) f 
               (when (stringp value)
                 (multiple-value-bind (new-string modified?)
                     (replace-chars value mapping)
                   (when modified? 
                     (setf (slotv f slot) new-string)
                     (push (list f slot) changed-frames)
                     ))))
             (loop for slot in string-list-slots 
                   as string-list = (slotv f slot)
                   do
                   (loop for strings on string-list 
                         as string = (first strings)
                         do
                         (when (stringp string)
                           (multiple-value-bind (new-string modified?)
                               (replace-chars string mapping)
                             (when modified?
                               (setf (first strings) new-string)
                               (pushnew (list f slot)
                                        changed-frames :test 'equalp)
                               )))))))
      (check-frame-for-non-ascii-strings org)
      (loop for gene in (#^genes org) 
            do
            (check-frame-for-non-ascii-strings gene))
      (loop for protein in (#^proteins org) 
            do
            (check-frame-for-non-ascii-strings protein))
      (loop for contig in (#^contiguous-sequences org) 
            do
            (check-frame-for-non-ascii-strings contig))
      changed-frames
      )))