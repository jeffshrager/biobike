;;; -*- Package: bio; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

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


;;; Author: Mark Slupesky, JP Massar.

#|
<6>> (setq p (ppcre:create-scanner \"attempt to call .* which is an undefined function.")) 
:: #<Closure (:INTERNAL CL-PPCRE::CREATE-SCANNER-AUX 4) @ #x921e5c1a>
<7>> (ppcre:all-matches p "attempt to call `BLAH' which is an undefined function.")  
:: (0 54)
<8>> (ppcre:all-matches p "sdfa.")
:: NIL
<9>> (ppcre:all-matches p "attempt.")
:: NIL
|#

;; 
;; open each file once, do the regexps on each file
;; all the error strings end up in teh string vector
;; clean up the error strings with extra spaces.  

                 
                 
(length (setq m (remove-if (lambda (x) (second x)) *matching-strings-array*)))


(defvar *matching-strings-array* nil)
(defvar *string-counter* 0)

(defparameter *error-lists-2*
  ".* does not exist, cannot load"
  "[iI]llegal keyword given"
  "can't figure this out"
  "[iI]f-true statement missing body"
  "comma not inside a backquote"
  "figure this out"
  "not a list of organisms")

  


(defparameter *error-lists*
  (list
   "[aA]ttempt to call .* which is an undefined function"
   "[eE]of encountered on stream"
    "[aA]ttempt to take the value of the unbound variable"
    ".* found where a LOOP keyword or LOOP type keyword expected"
    "[fF]unction position must contain a symbol or lambda expression"
    "is an unknown keyword"
    "[bB]adly formed let"
    "[Ii]llegal argument to"
    "is not of the expected type"
    "expects .* arguments?"  
    "[aA]ttempt to access the name field"
    "[dD]uplicated .* iteration variable"
    "does not introduce .* that can follow"
    "[aA]rgument must be a list"
    "[sS]ource code ran out when another token was expected"
    "[aA]ttempt to take the length of a non-sequence"
    "[gG]ot .* args?, wanted .* args?"
    "where a form was expected"
    "is not an even number of items"
    "[tT]rying to access never set position"
    "[aA]ttempt to divide .* by (0|(zero))"
    "[aA]ttempt to take the (car)|(cdr) of .* which is not listp"
    "Compound form expected, but found"
    "Missing keyword or value"
    "Package .* not found"
    "Attempt to store the wrong type of a value"
    "is not the name or nickname of an available organism"
    "Destructuring type pattern .* contains unrecognized type keyword"
    "[iI]nvalid key-word"
    "[Oo]bject of function must be a list"
    "found where LOOP keyword expected"
    "[iI]teration in LOOP follows body code"
    "not a list, gene, protein, replicon, contig, organism, or string"
    "[Ee]ntity .* does not exist or is not a valid frame"
    ))


(defvar *errors-contexts*
  (list
   "/home/biovcu/bioetc/saved-sl2/logs1.txt" "/home/biovcu/bioetc/saved-sl2/logs2.txt"
   "/home/biovcu/bioetc/saved-sl2/logs3.txt" "/home/biovcu/bioetc/saved-sl2/logs4.txt"
   "/home/biovcu/bioetc/saved-sl2/logs5.txt" "/home/biovcu/bioetc/saved-sl2/logs6.txt"
   "/home/biovcu/bioetc/saved-sl2/logs7.txt" "/home/biovcu/bioetc/saved-sl2/logs8.txt"
   "/home/biovcu/bioetc/saved-sl2/logs8.txt" "/home/biovcu/bioetc/saved-sl2/logs9.txt"
   "/home/biovcu/bioetc/saved-sl2/logs10.txt" "/home/biovcu/bioetc/saved-sl2/logs11.txt"
   "/home/biovcu/bioetc/saved-sl2/logs12.txt"))


(defun in-search-out (thing in-file)
  (let* ((stringed-file (file-to-string in-file :max 80000000))
         (string-vec (coerce (simple-string-split stringed-file #\Newline) 'vector)))
    (loop for string across string-vec
          for count from 0
          when (search thing string)
          collect
          (let* ((actual-error-start-index (+ count 1))
                 (actual-error-start-string (elt string-vec actual-error-start-index)))
            
            (if (position #\> actual-error-start-string)
                actual-error-start-string
              
              (let ((too-long-error nil))
                (do ((i 1 (+ i 1))
                     (str actual-error-start-string (elt string-vec (+ i actual-error-start-index))))
                    ((position #\> str) 
                     (setq too-long-error (reverse (push str too-long-error))))
                  (push str too-long-error))
                (string-join too-long-error #\Space)))))))


(Defun errors-loop (dir-with-logs out-file)
  (with-open-file (o out-file :direction :output :if-exists :supersede :if-does-not-exist :create)
    (with-standard-io-syntax
      (pprint 
       (remove-if 'null (loop for log in (directory dir-with-logs)
             collect
             (in-search-out "Actual error:" log)))
      o)
    )))
    
(defun pesky-spaces (string)
  (string-join 
   (remove 
    "" 
    (string-split string #\Space) :test 'string-equal) #\Space))
    


(defun get-error-stats (&optional (regexps *error-lists*))
  (let ((lots-of-errors nil))
    (loop for file in *errors-contexts* do
          (with-open-file (p file)
            (let ((these-errors (flatten (read p))))
              (setq lots-of-errors (nconc lots-of-errors these-errors))
              )))
    (let ((percentage-matched 
           (loop for regexp in regexps collect
                 (progn 
                   (setq *string-counter* 0)
                   (list 
                    regexp 
                    (regexp-in-list regexp 
                                    (mapcar 'pesky-spaces
                                            (flatten lots-of-errors))))))))
      (cformatt "Total percentage matched ~$~%" 
                (reduce '+ percentage-matched :key 'second))
      (setq percentage-matched (sort percentage-matched '> :key 'second))
      (loop for (regexp pct) in percentage-matched do
            (cformatt "Regexp ~S matched ~$%~%"
                      regexp pct)))))
  
          

(defun regexp-in-list (regexp ls)
  (let ((scanner (ppcre:create-scanner regexp))
        (how-many 0))
    (loop for error in ls do
          (when (ppcre:all-matches scanner error)
            (progn 
              (setf
               (second (aref *matching-strings-array* *string-counter*)) T)
              (incf how-many)))
          (incf *string-counter*))
    (let ((percentage (* 100 (/ how-many (length *matching-strings-array*)))))
      percentage)))

  
(defun number-of-errors (&optional (error-files *errors-contexts*))
  (let ((string-list nil))
    (loop for file in error-files 
          sum
          (with-open-file (p file :direction :input)
            (let ((strings (flatten (read p))))
              (prog1 
                  (length strings)
                (setq string-list (nconc string-list strings))
                )))
          finally
          (setq *matching-strings-array* 
                (make-array (length string-list) 
                            :initial-contents 
                            (mapcar (lambda (x) (list x nil)) (mapcar 'pesky-spaces string-list)))))))
  


(defun top-level ()
  (setq *string-counter* 0)
  (number-of-errors)
  (cformatt "Number of errors: ~D" (length *matching-strings-array*))
  (get-error-stats))
          

      








(defun collect-errors-from-logs ()
  (progn 
    (errors-loop "/home/biovcu/bioetc/saved-sl2/g/"  "/home/biovcu/bioetc/saved-sl2/logs1.txt")
    (errors-loop "/home/biovcu/bioetc/saved-sl2/h/"  "/home/biovcu/bioetc/saved-sl2/logs2.txt")
    (errors-loop "/home/biovcu/bioetc/saved-sl2/i/"  "/home/biovcu/bioetc/saved-sl2/logs3.txt")
    (errors-loop "/home/biovcu/bioetc/saved-sl2/j/"  "/home/biovcu/bioetc/saved-sl2/logs4.txt")
    (errors-loop "/home/biovcu/bioetc/saved-sl2/l/"  "/home/biovcu/bioetc/saved-sl2/logs5.txt")
    (errors-loop "/home/biovcu/bioetc/saved-sl2/m/"  "/home/biovcu/bioetc/saved-sl2/logs6.txt")
    (errors-loop "/home/biovcu/bioetc/saved-sl2/n/"  "/home/biovcu/bioetc/saved-sl2/logs7.txt")
    (errors-loop "/home/biovcu/bioetc/saved-sl2/o/"  "/home/biovcu/bioetc/saved-sl2/logs8.txt")
    (errors-loop "/home/biovcu/bioetc/saved-sl2/p/"  "/home/biovcu/bioetc/saved-sl2/logs9.txt")
    (errors-loop "/home/biovcu/bioetc/saved-sl2/q/"  "/home/biovcu/bioetc/saved-sl2/logs10.txt")
    (errors-loop "/home/biovcu/bioetc/saved-sl2/r/"  "/home/biovcu/bioetc/saved-sl2/logs11.txt")
    (errors-loop "/home/biovcu/bioetc/saved-sl2/s/"  "/home/biovcu/bioetc/saved-sl2/logs12.txt")))




