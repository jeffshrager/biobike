;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: bbi; -*-

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

(defun read-from-string-many-values (string &aux result-forms)
  #.(one-string-nl
     "Reads all the forms in STRING, not just the first one"
     "like READ-FROM-STRING does.  A list of the forms read is"
     "returned (so if only one form is read, say, (foo), the result"
     "will be ((foo)).")
  (catch :eof-ok       
    (loop with position = 0 
          with slen = (length string)
          while (< position slen)
          do
          (multiple-value-bind (form new-position)
              (handler-case 
                  (read-from-string string t nil :start position)
                (end-of-file 
                 (c)
                 (handle-read-from-string-many-values-eof c string position))
                #+allegro
                (excl::extra-right-paren-error 
                 ()
                 (when (= (length result-forms) 1) (return))
                 (error "Unexpected right parenthesis encountered!")
                 ))
            (loop until (or (= new-position slen)
                            (not (whitespacep (char string new-position))))
                  do 
                  (incf new-position))
            (setq position new-position)
            (push form result-forms)
            )))
  (nreverse result-forms))

(defun handle-read-from-string-many-values-eof (c string position)
  (cond 
   ;; If there's a paren as the last character, send the error up
   ;; to the weblistener to let it do paren completion.  
   ((eql #\) (find-if-not 'whitespacep string :from-end t))
    (signal c))
   ((last-characters-are-comment? string position)
    (throw :eof-ok nil))
   (t   
    (error
     (formatn 
      (one-string-nl
       "Attempt to read off the end of the input string.  This"
       "usually means you forgot a paren, bracket, or closing"
       "quote somewhere.  The failure occured at or after position"
       "~D in the input string, at or after the characters:"
       "  ~A.")
      (1+ position) (limited-string (subseq string position) 10)
      )))))

(defun last-characters-are-comment? (string pos)
  (or 
   ;; a single line with a ';' as the first non-whitespace character
   ;; after or at POS
   (and (null (position #\Newline string :start pos))
        (eql #\; (find-if-not 'whitespacep string :start pos)))
   ;; a block comment, possibly over multiple lines, with no 
   ;; characters other than whitespace between POS and the beginning
   ;; of the block comment, and no characters other than whitespace
   ;; between the end of the block comment and the end of STRING.  
   (let ((block-start (search "#|" string :start2 pos))
         (block-end (search "|#" string :start2 pos)))
     (and block-start 
          block-end 
          (> block-end block-start)
          (every 'whitespacep (subseq string pos block-start))
          (or (= (length string) (+ block-end 2))
              (every 'whitespacep (subseq string (+ block-end 2)))
              )))))

(defun bracket-hacking (form)
  (flet ((oops (format-string &rest format-args)
           (let ((form-string (limited-form-string form 60))
                 (real-format-string 
                  (s+ "~%The form ~A" #\Newline "  " format-string)))
             (error
              (formatn real-format-string form-string format-args)
              ))))
    (cond
     ((or (eq form +lbs+) (eq form +rbs+))
      (error 
       (formatn "Bracket \"~A\" encountered in an illegal position." form)
       ))
     ((not (consp form)) form)
     ;; ((eq 'quote (first form)) form)
     ((not (find-if (lambda (x) (or (eq x +lbs+) (eq x +rbs+))) form))
      (mapcar 'bracket-hacking form))
     (t 
      (let ((lbs-count (count +lbs+ form)) 
            (rbs-count (count +rbs+ form)))
        (unless (= lbs-count rbs-count)
          (let ((amount (abs (- lbs-count rbs-count))))
            (if (> lbs-count rbs-count)
                (oops 
                 "contains ~D more left brackets than right brackets." amount)
              (oops 
               "contains ~D more right brackets than left brackets." amount)
              ))))
      (let* ((lbs-pos (position +lbs+ form))
             (first-rbs-pos (position +rbs+ form))
             (rbs-pos (find-balancing-bracket-pos form lbs-pos)))
        (unless (> first-rbs-pos lbs-pos)
          (oops "contains a right bracket before a left bracket."))
        (when (zerop lbs-pos)
          (oops
           (one-string-nl
            "contains a left bracket as the first element of the form."
            "This is not legal syntax.")))
        (when (= lbs-pos (1- rbs-pos))
          (oops "contains matching brackets with nothing in between."))
        (when (eq +lbs+ (nth (1+ lbs-pos) form))
          (oops 
           "contains two adjacent left brackets. This is not legal syntax."))
        (bracket-hacking 
         (let ((pre-lbs (subseq form 0 (1- lbs-pos)))
               (target (elt form (1- lbs-pos)))
               (within-brackets (subseq form (1+ lbs-pos) rbs-pos))
               (post-rbs (subseq form (1+ rbs-pos))))
           `(,@pre-lbs 
             (ref ,target ,@within-brackets)
             ,@post-rbs
             ))))))))
            
(defun find-balancing-bracket-pos (form lbs-pos) 
  (loop with count = 0
        for x on (nthcdr (1+ lbs-pos) form)
        for pos from (1+ lbs-pos)
        as obj = (first x)
        do 
        (cond
         ((eq obj +lbs+) (incf count))
         ((eq obj +rbs+) 
          (if (zerop count) (return pos) (decf count)))
         (t nil)
         )))
          
(defun bbl-read-from-string (x) 
  (let ((*readtable* *bbl-readtable*))
    (let ((forms (read-from-string-many-values x)))
      (cond 
       ((eq +lbs+ (second forms)) 
        (first (bracket-hacking forms)))
       ((find +lbs+ forms) 
        (error
         (formatn 
          "Unrecognized syntax.  Bracket in unexpected position: ~%  ~A"
          (limited-string x 70)))
        )
       (t (bracket-hacking (first forms)))
       ))))


;;; For use from a Lisp Listener toplevel; not used by Weblistener.

(defun ebr (x) 
  (eval 
   (let ((*readtable* *bbl-readtable*))
     (let ((forms (read-from-string-many-values x)))
       (cond 
        ((eq +lbs+ (second forms)) 
         (first (bracket-hacking forms)))
        ((find +lbs+ forms) 
         (error "Unrecognized syntax: ~A" x))
        (t (bracket-hacking (first forms)))
        )))))



