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


;;; Creates lists of frames of the genomes of each type found in the SEED.
;;; A genome can have multiple versions, so we also create lists of frames
;;; of the genomes which are the latest version for each type.
;;; The type of each genome is pulled from the SEED and stored in the
;;; #$Maindomain slot of the corresponding organism frame.

(defun set-up-seed-genome-types ()
  (let* ((real-domains 
          (sort 
           (purge-duplicates 
            (loop for orgf in (available-organisms) 
                  collect (#^real-domain orgf))
            :test 'string-equal
            )
           'string-lessp
           ))
         (domain-variables 
          (mapcar 
           (lambda (domain-name) 
             (setq 
              domain-name
              (string-upcase (substitute #\- #\Space domain-name)))
             (intern (formatn "*SEED-~A-FRAMES*" domain-name) :bio))
           real-domains
           ))
         (data (mapcar 'list real-domains domain-variables)))
    (loop for (nil domain-variable) in data
          do (setf (symbol-value domain-variable) nil))
    (loop for orgf in (available-organisms) 
          as org-real-domain = (#^real-domain orgf)
          do
          (loop for (domain-category domain-variable) in data
                do
                (when (string-equal org-real-domain domain-category)
                  (let ((list (symbol-value domain-variable)))
                    (setf (symbol-value domain-variable) (cons orgf list))
                    (return nil)
                    ))
                finally 
                (error "Huh?  Could not find real-domain ~A!" org-real-domain)
                ))
    (loop for (nil domain-variable) in data
          do
          (setf 
           (symbol-value domain-variable) 
           (sort (symbol-value domain-variable) 'string-equal :key #^fname)
           ))
    (setq *seed-genome-types* data)
    (forward-package-funcall :vpl "INSTANTIATE-SEED-CATEGORY-OPERATORS")
    ))

