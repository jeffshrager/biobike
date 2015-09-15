;; -*- mode: Lisp; Syntax: Common-Lisp; Package: biolisp; -*-

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

;;; Authors:  Jeff Shrager & JP Massar

(defun run-phylip (entities &key 
			    (type :dna)
			    (seqfn #'identity)
			    (labelfn #'identity)
			    (delete-temp-file? *delete-temp-files*)
			    )
  #.(one-string-nl
     "Calls phylip, passing it the sequences that you offer, and returning"
     "the resulting tree in BioTree format."
     "(You can use, for example, SEEGRAPH to visualize the tree.)"
     "The keyword argument :TYPE (default = :DNA) tells which program to use."
     "(Usually, you'll just want either :DNA (i.e., dnapars) or :PROTEIN"
     "(i.e., protpars).)  SEQFN and LABELFN enable you to name the resulting"
     "data in any desired manner by defining accessor functions into each"
     "ENTITY."
     "For example:"
     "(run-phylip '((gene-name1 #$first-gene) (gene-name2 #$second-gene)" 
     "(gene-name3 #$sthird-gene))"
     "            :labelfn #'first :seqfn #'second)"
     "This tells the code to get the sequence information from the second"
     "element of an ENTITY, and the label information (to pass to PHYLIP"
     "as the name of the sequence) from the first element of an ENTITY."
     ""
     "Note: At least three sequences must be given; phylip itself"
     "produces no output if it is given only two sequences."
     "WARNING: The sequences must be pre-aligned, and should be of the same"
     "length -- they are trimmed to be the length of the shortest one before"
     "being passed to PHYLIP."
     )
  (block exit 
    (unless (>= (length entities) 3) 
      (error "You must provide at least three sequences!"))
    (with-entity-encoding (infn outfn)
      (let* ((keys-and-sequence 
              (loop for entity in entities
                    as key = (funcall #'infn entity)
                    as seq = (extract-sequence+ (funcall seqfn entity))
                    collect (cons key seq)))
             (program-string (ecase type
                               (:dna "dnapars")
                               ((:protein :proteins) "protpars")
                               ((:protdist :protdist) "protdist")))
             )
        ;; Find the shortest one for trimming and header formation
        (let ((trim-limit (reduce #'min keys-and-sequence 
                                  :key (lambda (a) (length (cdr a))))))
          (with-temp-directory (dir "tmp:" :delete? delete-temp-file?)
            (with-temp-file-in 
                (infile dir :name "phyin" :delete? delete-temp-file?)
              (with-temp-file-in 
                  (cmdfile dir :name "phycmd" :delete? delete-temp-file?)
                (with-temp-file-in 
                    (outfile dir :name "phyout" :delete? delete-temp-file?)
                  (with-open-file (phyin infile :direction :output)
                    (format phyin "~a ~a~%" 
                            (length keys-and-sequence) trim-limit)
                    (loop for (key . seq) in keys-and-sequence
                          with seq-format-string = 
                          (format nil "~~~a@a~~%" trim-limit)
                          do
                          (format phyin "~10a" key)
                          (format phyin seq-format-string 
                                  (subseq seq 0 trim-limit))))
                  (with-open-file (phycmd cmdfile :direction :output)
                    (format phycmd "~a~%Y%" infile))
                  (let ((phylip-command 
                         (formatn "~a/~a < ~a > ~a" 
                                  cl-user::*phylip-executable-dir*
                                  program-string cmdfile outfile)))
                    (case (protected-shell-command 
                           phylip-command :directory (namestring dir))
                      (:timeout (return-from exit nil))
                      (otherwise nil))))))
            (let ((result (parse-phylip-outtree-file 
                           (merge-pathnames "outtree" dir) type)))
              (loop for triple in result
                    as (a nil b) = triple
                    do 
                    (let ((newa (funcall labelfn (funcall #'outfn a))))
                      (when newa (setf (first triple) newa)))
                    (let ((newb (funcall labelfn (funcall #'outfn b))))
                      (when newb (setf (third triple) newb))))
              result)
            ))))))

(defun parse-phylip-outtree-file (filepath type)
  (let ((result 
         (read-from-string 
          (substitute 
           #\space #\, 
           (string-join 
            (string-split (file-to-string filepath) #\:) 
            " :colon ")))))
    (ecase type
      (:dna (decode-horrific-phylip-dna-tree-format result))
      ((:protein :proteins)
       (decode-horrific-phylip-protein-tree-format result))
      )))

(defmethod decode-horrific-phylip-dna-tree-format (tree)
  (let ((result nil))
    (labels ((recurse (tree node-name)
               (loop for (child nil weight) on tree by #'cdddr
                     do 
                     (cond 
                      ((listp child) 
                       (let ((new-node (keywordize (gensym "node"))))
                         (push (list node-name weight new-node) result)
                         (recurse child new-node)))
                      (t (push (list node-name weight child) result))))))
      (recurse tree :top)
      result)))

(defmethod decode-horrific-phylip-protein-tree-format (tree)
  (let ((result nil))
    (labels ((recurse (tree node-name)
               (loop for (child) on tree by #'cdr
                     do 
                     (cond 
                      ((listp child) 
                       (let ((new-node (keywordize (gensym "node"))))
                         (push (list node-name :-> new-node) result)
                         (recurse child new-node)))
                      (t (push (list node-name :-> child) result))))))
      (recurse tree :top)
      result)))
