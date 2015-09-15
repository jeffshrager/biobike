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

(defun sdsd (x) (second (second x)))

(defun all-gids-in-domain-soap (domain &key (complete? nil) (restrictions nil))
  (map
   'list
   (lambda (x) (utils::string-split x #\Tab))
   (sdsd
    (funcall 
     'seed::genomes
     :complete (if complete? "complete" "")
     :restrictions (if restrictions restrictions "")
     :domain domain
     ))))

(defun get-seed-annotation-info-soap (gid)
  (declare (ignore gid))
  nil
  )

(defun contig-info-soap (gid)
  (let ((contigs (sdsd (funcall 'seed::contigs-of :genomeid gid))))
    (loop for c across contigs collect 
          (list c (sdsd (funcall 'seed::contig-ln :genomeid gid :contig c)))
          )))

;; 1 based
(defun contig-sequence (gid contig from to)
  (sdsd 
   (funcall 'seed::dna-sequence
            :genomeid gid :location1 (format nil "~A_~D_~D" contig from to))
   ))

(defun all-pegs-of-a-genome-soap (gid) 
  (sdsd (funcall 'seed::pegs-of :genomeid gid))
  )

;; this takes a gene (peg) and returns contig_from_to
(defun peg-location-info (peg)
  (sdsd (funcall 'seed::feature-location :peg peg)))

(defun gene-annotation (peg)
  (sdsd (funcall 'seed::function-of :peg peg)))

(defun get-protein-sequence (peg)
  (sdsd (funcall 'seed::get-translation :peg peg)))

(defun peg-contig-from-and-to (peg) 
  (let* ((location-info (peg-location-info peg)))
    (parse-seed-location-info location-info)
    ))

(defun peg-sequence-of (gid peg)
  (destructuring-bind (contig-name from to)
      (peg-contig-from-and-to peg)
    (contig-sequence gid contig-name from to)
    ))

(defun seed-gene-sequence-of (seed-gene)
  (let ((pegid (#^seed-id seed-gene))
        (gid (#^seed-id (#^organism seed-gene))))
    (peg-sequence-of gid pegid)
    ))

(defun check-for-bad-chars (gid)
  (let ((contigs-and-lengths (contig-info gid)))
    (loop for (contig length) in contigs-and-lengths
          as sequence = (contig-sequence gid contig 1 length)
          as bad-positions = nil
          nconc
          (progn
            (loop for j from 0 below length do
                  (unless (member 
                           (char sequence j) 
                           '(#\a #\g #\c #\t) :test 'char-equal)
                    (push (list j (char sequence j)) bad-positions)
                    ))
            (when bad-positions (list (list gid contig bad-positions)))
            ))))