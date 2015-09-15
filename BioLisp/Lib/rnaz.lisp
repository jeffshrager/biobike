;; -*- mode: Lisp; Syntax: Common-Lisp; Package: biolisp; -*-

(in-package :bio)

;;; +=========================================================================+
;;; | Copyright (c) 2002-2005 JP Massar, Jeff Shrager, Mike Travers           |
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

;;; Routines to deal with RNAz 

(defun run-rnaz 
       (data 
        &key
        (flags "")
        (verbose? t)
        (view-input-file? nil)
        (view-output-file? nil))
  #.(one-string-nl 
     "Runs the RNAz unix program and returns a temp-frame with 2 slots, "
     "#$parameters and #$results.  The #$parameters value is an assoc list"
     "of names and values; each name and value are strings.  The #$results"
     "value is a list of lists, each sublist being a list of four strings:"
     "identifier, sequence, configuration, and any other information on the"
     "configuration line after the configuration itself."
     "The input, DATA, is a list of (key sequence) pairs, where the keys and"
     "the sequences must be strings which do not contain blanks."
     "The sequences must all be the same length as well."
     "FLAGS is a string which is intended to be a set of flags and arguments"
     "for the RNAz command, and is simply passed through.  By default, RNAz"
     "is run without flags (flags = \"\").")
  (block exit
    (let ((rnaz-executable-path (external-executable-path "RNAz")))
      (with-temp-file-in (rnaz-output-file user:*tmp-directory*)
        (with-temp-file-in (rnaz-input-file user:*tmp-directory* :delete? nil)
          (when verbose? 
            (cformatt "RNAz-executable path: ~A" rnaz-executable-path)
            (cformatt "RNAz input file path: ~A" rnaz-input-file)
            (cformatt "RNAz output file path: ~A" rnaz-output-file)
            ;; (cformatt "RNAz models directory: ~A" rnaz-models-directory)
            )
          (create-rnaz-input-file data rnaz-input-file)
          (when view-input-file? 
            (list-file-contents rnaz-input-file :line-limit nil))
          ;; This no longer seems to be necessary.  The models
          ;; directory doesn't even exist anymore.  
          ;; #+:Allegro
          ;; (setf (sys:getenv "RNAZDIR") (namestring rnaz-models-directory))
          (let ((rnaz-command 
                 (formatn 
                  "~A~A ~A > ~A"
                  (namestring rnaz-executable-path)
                  (if (zerop (length flags)) flags (one-string " " flags))
                  (namestring rnaz-input-file)
                  (namestring rnaz-output-file)
                  )))
            (when verbose? (cformatt "RNAz shell command: ~A" rnaz-command))
            (case (protected-shell-command rnaz-command) 
              (:timeout (return-from exit nil))
              (otherwise nil))))
        (when view-output-file? 
          (list-file-contents rnaz-output-file :line-limit nil))
        (parse-rnaz-output-file rnaz-output-file)
        ))))
    

;;; This function creates clustal-style output.  That is, blocks which
;;; consist of a key naming the sequence followed by 50 characters of
;;; the sequence, repeated for each key-sequence pair.  Blocks are
;;; then repeated until the sequences (which must all be the same
;;; length) are exhausted.  An arbitrary header line is written as
;;; well.

(defun create-rnaz-input-file (data input-file &key (section-width 50))
  (loop for (key sequence) in data do
        (unless (and (stringp key) (stringp sequence) (plusp (length key)))
          (error "Invalid key-sequence pair: ~A" (list key sequence)))
        (when (some 'whitespacep sequence)
          (error "Invalid data sequence! Input must not contain whitespace!")))
  (let* ((seqlen (length (second (first data))))
         (max-keylen (reduce 'max data :key (lambda (x) (length (first x)))))
         (key-format (formatn "~~~DA" (1+ max-keylen))))
    (unless (every (lambda (x) (= (length (second x)) seqlen)) data)
      (error "All the sequences must be the same length!"))
    (let ((first-sequence (second (first data))))
      (unless (<= (length first-sequence) 400)
        (error 
         (one-string-nl
          "The latest version of RNAz does not allow you to use"
          "sequences of more than 400 base pairs!"
          ))))
    (with-open-file (s input-file :direction :output :if-exists :supersede)
      (format s "CLUSTAL W (executed from biolingua) ~%~%")
      (loop for start = 0 then (+ start section-width)
            until (>= start seqlen)
            do
            (loop for (key sequence) in data 
                  as end = (min (+ start section-width) seqlen)
                  do
                  (format s key-format key)
                  (write-sequence sequence s :start start :end end)
                  (terpri s))
            (terpri s)
            ))))


(defun parse-rnaz-output-file (file)
  (flet ((trim (x) (string-trim *whitespace* x)))
    (let* ((lines (file-to-string-list file))
           (parameter-lines nil)
           (parameter-data nil)
           (results-lines nil)
           (results-data nil)
           (temp-frame (make-temp-frame)))
      (loop for line in lines
            with pound-sign-line-count = 0
            do
            (cond 
             ((or (null line) (zerop (length line))) nil)
             ((every 'whitespacep line) nil)
             ((char= #\# (char line 0)) 
              (incf pound-sign-line-count))
             ((= pound-sign-line-count 1) 
              (push line parameter-lines))
             ((= pound-sign-line-count 2)
              (push line results-lines))
             (t (error "Unrecognized line in RNAz output file: ~S" line))))
      (setq parameter-lines (nreverse parameter-lines))
      (setq results-lines (nreverse results-lines))
      (setq 
       parameter-data 
       (loop for line in parameter-lines collect
             (let ((colonpos (position #\: line)))
               (when (null colonpos) 
                 (error
                  "Unknown format for parameter line in RNAz output file: ~S" 
                  line))
               (let ((parameter-name 
                      (substitute #\- #\Space (trim (subseq line 0 colonpos))))
                     (parameter-data 
                      (trim (subseq line (1+ colonpos)))))
                 (list parameter-name parameter-data)
                 ))))
      (setq 
       results-data 
       (loop for rest-of-data on results-lines by #'cdddr 
             as identity-line = (first rest-of-data)
             as sequence-line = (second rest-of-data)
             as structure-line = (third rest-of-data)
             as structure-end-pos = (position #\Space structure-line)
             do
             (unless (and (char= #\> (char identity-line 0))
                          (> (length identity-line) 1))
               (error "Invalid identity line in RNAz output file: ~S" 
                      identity-line))
             (unless structure-end-pos
               (error 
                "No space separating structure from numeric data: ~S" 
                structure-line))
             collect 
             (list 
              (trim (subseq identity-line 1))
              (trim sequence-line) 
              (subseq structure-line 0 structure-end-pos)
              (subseq structure-line (1+ structure-end-pos))
              )))
      (setf (slotv temp-frame #$parameters) parameter-data)
      (setf (slotv temp-frame #$results) results-data)
      temp-frame
      )))
            
#+data
(setq *rnaz-test-data* 
  '(
    ("AF041468.1-40566_40494"      
     "GGGGGTATAGCTCAGT-TGGTAGAGCGCTGCCTTTGCACGGCAGATGTCAGGGGTTCGAGTCCCCTTACCTCCA")
    ("X54300.1-105_177"            
     "GGGGGTATAGCTTAGT-TGGTAGAGCGCTGCTTTTGCAAGGCAGATGTCAGCGGTTCGAATCCGCTTACCTCCA")
    ("L00194.1-685_756"           
     "GGGGCCATAGCTCAGT-TGGTAGAGCGCCTGCTTTGCAAG-CAGGTGTCGTCGGTTCGAATCCGTCTGGCTCCA")
    ("AY017179.1-1528_1601"        
     "GGGCCGGTAGCTCAGCCTGGGAGAGCGTCGGCTTTGCAAGCCGAAGGCCCCGGGTTCGAATCCCGGCCGGTCCA")
    ))


