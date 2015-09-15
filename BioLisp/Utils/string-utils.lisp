;;; -*- Package: utils; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :utils)

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

;;; Authors:  JP Massar, Jeff Shrager, Mike Travers.

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defparameter *utility-string-user-symbols*
    '(
      nullstring?
      one-string
      one-string-sp
      one-string-nl
      one-string-nli
      doc
      formatt
      formatn
      cformatt
      string-split
      break-string-into-blocks-of-size
      string-join
      surround
      ntranslate-string
      translate-string
      limited-string
      ellipsis-string
      limited-form-string
      all-strings-of-length
      character-counts
      s+
      s+join
      string+-join
      remove-all-whitespace
      whitespacep
      center-in
      string-to-lines
      string-after-first 
      string-before-first 
      replace-chars
      nth-position
      sbutlast
      ))
    
  (defparameter *utility-string-api-symbols*
    (append *utility-string-user-symbols*
            '(
              *whitespace*
              quoted-symbol-p
              ierror
              terpprint
              simple-string-split
              maybe-clip-string
              anything-stringlike-to-string
              compile-word
              score-homology
              word-homology
              word-homology-fast
              float-to-mysql-float-string
              precede-with-header-and-indent
              )))

  (export *utility-string-api-symbols* (find-package :utils)))

(defun nullstring? (x) (and (stringp x) (zerop (length x))))

;;; Use this to compile code that must be fast with no safety by default.
;;; If a problem occurs, by changing the value of the below variable to
;;; T, the compilation will take place at full safety instead of optimizing
;;; for speed.  See documentation below.  

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defvar cl-user::*wb-optimized-compilation-safety* t)

  (defmacro optimization-declaration ()
    #.(concatenate 
       'string
       "By including this form where a declaration would go at the"
       "beginning of a function, as in:"
       "(defun foo (x)"
       "  #.(optimization-declaration)"
       "  (declare (single-float x))"
       "  (* x x))"
       "the body of the function will be compiled according to the value of"
       "user::*wb-optimized-compilation-safety*, which when its value is NIL"
       "(the default), :NIL, :NONE, :NO-SAFETY or :DISABLED"
       "denotes compilation with no safety: SPEED 3 and SAFETY 0."
       "By changing the value to T, :FULL, :FULL-SAFETY or :DISABLED,"
       "the compilation will take place at full safety: SPEED 0 and SAFETY 3."
       "Any other value denotes compilation at current compilation settings."
       "This would generally be used in functions that need to be compiled"
       "for speed, realizing that if there is a system problem potentially"
       "caused by passing in an illegal value to one of these functions or some"
       "other potential inconsistency, the entire system can be recompiled"
       "with these functions compiled at high safety to catch such a problem.")
    (cond
     ((and (boundp 'cl-user::*wb-optimized-compilation-safety*) 
           (member (symbol-value 'cl-user::*wb-optimized-compilation-safety*)
                   '(t :full :full-safety :enabled)))
      ''(declare (optimize (speed 0) (safety 3) (debug 3))))
     ((and (boundp 'cl-user::*wb-optimized-compilation-safety*) 
           (member (symbol-value 'cl-user::*wb-optimized-compilation-safety*)
                   '(nil :nil :none :no-safety :disabled)))
      ''(declare (optimize (speed 3) (safety 0) (debug 0))))
     (t ''(declare))
     ))

  )

;;; A little macro that lets us type in a single string over multiple lines
;;; without line wrap or extension beyond the edge of the window.
;;; So we can do:
;;; (one-string  "This is the first part of a long string, "
;;;               and this is the second part of a long string, "
;;;               and this is the last part of a long string.")
;;; and the ONE-STRING macro will replace this by a single string of
;;; the obvious content.
;;; It will also do things like
;;; (one-string "ABC" 'foobar #\Y) --> "ABCFOOBARY")
;;; (one-string "abc" "def" a-variable "ghi" "jkl") -->
;;;    (concatenate 'string "abcdef" a-variable "ghijkl")
;;; Cute, eh?

(eval-when (:compile-toplevel :load-toplevel :execute)

(defun quoted-symbol-p (q)
  "Is a list of the form (quote <symbol>) ?"
  (and (listp q) (eql (length q) 2) 
       (eq 'quote (first q)) (symbolp (second q))))

)

(eval-when (:compile-toplevel :load-toplevel :execute)

(defmacro one-string (&rest string-designators)
  "Creates a single string (if its arguments are all constant strings)
   or a form that creates a single string (if some arguments are variables)"
  (flet ((string-designator-p (x)
           (or (stringp x) (characterp x) (quoted-symbol-p x)))
         (to-string (x)
           (cond
            ((stringp x) x)
            ((characterp x) (string x))
            ((quoted-symbol-p x) (string (second x))))))
    (cond
     ((every 'stringp string-designators)
      (apply 'concatenate 'string string-designators))
     ((every #'string-designator-p string-designators)
      (apply 'concatenate 
        'string (mapcar #'to-string string-designators)))
     (t
      (let ((reversed-form (reverse '(concatenate 'string)))
            (merged-constant-strings ""))
        (dolist (x string-designators)
          (if (not (string-designator-p x))
              (progn
                (when (> (length merged-constant-strings) 0)
                  (push merged-constant-strings reversed-form)
                  (setq merged-constant-strings ""))
                (push x reversed-form))
            (setq merged-constant-strings
                  (concatenate 'string 
                    merged-constant-strings (to-string x)))
            ))
        (when (> (length merged-constant-strings) 0)
          (push merged-constant-strings reversed-form))
        (reverse reversed-form)
        )))))

(defmacro one-string-sp (&rest string-designators)
  "Inserts spaces after every argument except the last, and calls ONE-STRING"
  (let ((sp (string #\Space)))
    `(one-string 
      ,@(loop for remaining-strings on string-designators 
              as s = (first remaining-strings)
              nconc 
              (if (cdr remaining-strings) (list s sp) (list s))
              ))))

(defmacro one-string-nl (&rest string-designators)
  "Inserts newlines after every argument, and calls ONE-STRING"
  (let ((nl (string #\Newline)))
    `(one-string ,@(loop for s in string-designators nconc (list s nl)))
    ))

(defmacro one-string-nli (&rest string-designators)
  "Inserts newlines after every argument except the last, and calls ONE-STRING"
  (let ((nl (string #\Newline)))
    `(one-string
      ,@(loop for strings on string-designators 
              as s = (first strings)
              nconc 
             (if (cdr strings) (list s nl) (list s))))))

(defmacro doc (&rest strings) `(one-string-nl ,@strings))

)




(defmacro formatt (format-string &rest format-args)
  "Shorthand for (format t ...) and it indents better"
  `(format t ,format-string ,@format-args))
(defmacro formatn (format-string &rest format-args)
  "Shorthand for (format nil ...) and it indents better"
  `(format nil ,format-string ,@format-args))
(defun cformatt (format-string &rest format-args)
  #.(one-string-nl
     "Writes FORMAT-STRING to standard ouput, first prepending ';; ' "
     "and postpending a newline.  (The 'c' is for 'comment'.)")
  (apply 'format t (one-string "~&;; " format-string "~%") format-args))
(defun ierror (format-string &rest format-args)
  "Signals an error whose string message begins with 'Internal error. '"
  (apply 'error (one-string "Internal error. " format-string) format-args))
(defun terpprint (form &optional (p *standard-output*))
  "Pretty prints FORM and then prints a #\Newline"
  (pprint form p) (terpri p))

;;; Mike Travers' string split doesn't copy the string, 
;;; and so is pretty efficient!
;;; BE FOREWARNED ABOUT POSSIBLE SIDE-EFFECT CONFUSION!

(defun string-split 
       (string &optional (delimiter #\space) (method :new-strings))
  #.(one-string-nl 
     "Returns a list of strings which are the substrings of STRING "
     "separated by DELIMITER."
     "DELIMITER must be either a character or a string of length 1."
     "When METHOD is :IN-PLACE the strings returned"
     "share 'string space' with the orginal string STRING -- modifying any"
     "character in one of the substrings will modify the original string,"
     "and modifying a character in the original string may modify one of the"
     "substrings. Also, the substrings returned are not of type SIMPLE-STRING,"
     "since they are really displaced arrays."
     "When METHOD is :NEW-STRINGS (the default) the strings returned are"
     "newly created strings which do not share content with the original"
     "input string.  Also, the substrings returned are of type SIMPLE-STRING."
     "Use :IN-PLACE when you want to avoid creating long substrings"
     "which are copies of subparts of the original string."
     "Use :NEW-STRINGS when the substrings are short and/or the input string"
     "is not very long (or if you need to modify the returned strings and"
     "do not want the input string modified).")
  (when (and (stringp delimiter) (= 1 (length delimiter)))
    (setq delimiter (char delimiter 0)))
  (ecase method
    (:new-strings
     (simple-string-split string delimiter))
    (:in-place
     (let ((substrings '())
           (length (length string))
           (string-char-type (array-element-type string))
           (last 0))
       (flet ((add-substring (i)
                (push (make-array (- i last)
                                  :element-type string-char-type
                                  :displaced-to string
                                  :displaced-index-offset last)
                      substrings)))
         (dotimes (i length)
           (when (eq (char string i) delimiter)
             (add-substring i)
             (setq last (1+ i))))
         (add-substring length)
         (nreverse substrings)
         )))))


(defun simple-string-split (string &optional (delimiter #\space))
  #.(one-string-nl
     "Returns a list of simple strings which are the substrings of STRING"
     "separated by DELIMITER.  These substrings are true copies, and may"
     "therefore be modified without affecting the original string STRING."
     "This function is most efficient if STRING is of type simple-string.")
  (cond
   ((not (simple-string-p string))
    (mapcar
     (lambda (s) (coerce s 'simple-string))
     (string-split string delimiter)
     ))
   (t (simple-string-split-fast string delimiter))
   ))
    
(defun simple-string-split-fast (sstring delimiter)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (simple-string sstring) (character delimiter))
  (let* ((len (the fixnum (length sstring)))
         (result (list nil))
         (ptr result)
         (start 0))
    (declare (fixnum len start))
    (block exit
      (cond
       ((zerop len) 
        (return-from exit (list "")))
       ((= len 1) 
        (return-from exit
          (if (char= (schar sstring 0) delimiter)
              (list "" "")
            (list (copy-seq sstring))
            ))))
      (if (char= (schar sstring 0) delimiter)
          (progn (setq start 1) (setf (cdr ptr) (list "")) (pop ptr))
        (setq start 0))
      (loop for j fixnum from 1 below len 
            as ch = (schar sstring j)
            do
            (when (char= ch delimiter) 
              (let* ((size (the fixnum (- j start)))
                     (s (make-string size)))
                (declare (fixnum size))
                (declare (simple-string s))
                (loop for k fixnum from 0 
                      for i fixnum from start below j 
                      do
                      (setf (schar s k) (schar sstring i)))
                (setf (cdr ptr) (list s))
                (pop ptr)
                (setq start (the fixnum (1+ j)))
                ))
            finally 
            (let ((last-string (if (= start len) "" (subseq sstring start))))
              (setf (cdr ptr) (list last-string))
              ))
      (cdr result)
      )))          

(defun break-string-into-blocks-of-size (string size)
  #.(one-string-nl
     "If SIZE >= LENGTH(STRING), returns a list of one element, a"
     "copy of the STRING."
     "Otherwise, a list of subsections of the string, each SIZE long"
     "is returned in order, save that the last subsequence may be shorter."
     )
  (declare (fixnum size))
  (let ((len (length string)))
    (if (<= len size) 
        (list (copy-seq string))
      (let ((nblocks (ceiling len size)))
        (declare (fixnum nblocks))
        (loop for j from 0 by size
              for k fixnum from 1 to nblocks
              collect
              (if (= k nblocks)
                  (subseq string j)
                (subseq string j (+ j size))
                ))))))


;;; Inverse of STRING-SPLIT.  Creates a single string from the
;;; list of strings, with the characters of SEP in between.

(defun string-join (string-list &optional (sep #\Space))
  "Concatenates strings together and puts SEP between each joined substring"
  (setq sep (string sep))
  (when (null string-list) (return-from string-join ""))
  (let* ((total-length 0)
         (sep-length (length sep))
         (no-sep (zerop sep-length)))
    (dolist (s string-list) (incf total-length (+ (length s) sep-length)))
    (decf total-length sep-length)
    (let ((result-string (make-string total-length))
          (current-pos 0))
      (dolist (s string-list)
        (replace result-string s :start1 current-pos)
        (incf current-pos (length s))
        (unless (or no-sep (>= current-pos total-length))
          (replace result-string sep :start1 current-pos)
          (incf current-pos sep-length)
          ))
      result-string
      )))

       
(defun surround (string prefix &optional (suffix prefix))
  #.(one-string-nl
     "Prepends PREFIX to STRING if it is a string and postpends SUFFIX"
     "to STRING if it is a string.  Returns a copy of STRING if any appending"
     "is done otherwise returns STRING itself. If PREFIX or SUFFIX are not"
     "strings but are non-nil an attempt is made to convert them to strings"
     "using (STRING ...")
  (cond
   ((and (null prefix) (null suffix)) string)
   ((null prefix) (one-string string (string suffix)))
   ((null suffix) (one-string (string prefix) string))
   (t (one-string (string prefix) string (string suffix)))
   ))


;;; Word Homology package. Orignally Jeff's; Side-graded by Mike.
;;; 'Fast' versions by JP.
       
;;; Now prepends a null so that single char names work (and start of term 
;;; is more significant.)
       
       
(defconstant null-char (code-char 0) "#\Null, except it's not portable")

(defun compile-word (word)
  "Turns a word into a useful representation for word homology"
  (if (simple-string-p word)
      (compile-word-fast word)
    (loop for p from 0 to (- (length word) 1)
	  as l1 = (char-upcase (aref word p))
	  as l2 =  (if (zerop p) null-char (char-upcase (aref word (1- p))))
	  as ln1 = (char-code l1)
	  as ln2 = (char-code l2)
	  collect (+ ln1 (* ln2 128))
	  )))

(defun compile-word-fast (word)
  (unless (simple-string-p word)
    (error "COMPILE-WORD-FAST only accepts *SIMPLE* strings!"))
  (let ((word word))
    #.(optimization-declaration)
    (declare (simple-string word))
    (let ((prev-cc (char-code null-char)))
      (declare (fixnum prev-cc))
      (loop for p fixnum from 0 to (the fixnum (1- (length word)))
	    as next-cc fixnum = (char-code (char-upcase (schar word p)))
	    as result = (the fixnum (+ next-cc (the fixnum (ash prev-cc 7))))
	    do (setq prev-cc next-cc)
	    collect result
	    ))))

#+test
(defun time-cw (w n)
  (time (dotimes (j n) (compile-word w)));
  (time (dotimes (j n) (compile-word-fast w))))

;;; Takes two words or compiled words, returns the score of how close
;;; they matched based on co-occuring pairs.

(defun score-homology (word1 word2)
  "Ranks two strings as to how similar they are according to a homology metric"
  (if (or (and (simple-string-p word1) (simple-string-p word2))
          (and (not (stringp word1)) (not (stringp word2))))
      (score-homology-fast word1 word2)
    (let* ((w1 (if (listp word1) word1 (compile-word word1)))
	   (w2 (if (listp word2) word2 (compile-word word2)))
	   (o1 (loop for l1 in w1 if (member l1 w2) sum 1))
	   (o2 (loop for l2 in w2 if (member l2 w1) sum 1))
	   (l1 (length w1))
	   (l2 (length w2)))
      (if (and (> l1 1) (> l2 1))
	  (/ (+ (/ o1 l1) (/ o2 l2)) 2.0)
        0.0				;if either word is of length <2, 
        ))))

(defvar *word-homology-array* nil)

(defun score-homology-fast (word1 word2)
  #.(optimization-declaration)
  (unless (and (or (listp word1) (typep word1 'simple-string))
               (or (listp word2) (typep word2 'simple-string)))
    (error "SCORE-HOMOLOGY-FAST only accepts simple strings."))
  (when (null *word-homology-array*)
    (setq *word-homology-array* 
          (make-array (list #.(expt 2 14))
            :element-type '(unsigned-byte 8)
            :initial-element 0
            )))
  (let ((wha *word-homology-array*) 
        (x (if (listp word1) word1 (compile-word-fast word1)))
        (y (if (listp word2) word2 (compile-word-fast word2)))
        (cx 0) (cy 0) (lx 0) (ly 0))
    (declare (type (simple-array (unsigned-byte 8) 1) wha))
    (declare (fixnum cx cy lx ly) (list x y))
    (dolist (xe x) 
      (locally (declare (fixnum xe)))
      (setf (aref wha xe) 1)
      (setq lx (the fixnum (1+ lx))))
    (dolist (ye y) 
      (locally (declare (fixnum ye)))
      (setq ly (the fixnum (1+ ly)))
      (if (plusp (aref wha ye)) (setq cy (the fixnum (1+ cy)))))
    (dolist (xe x) 
      (locally (declare (fixnum xe)))
      (setf (aref wha xe) 0))
    (dolist (ye y) 
      (locally (declare (fixnum ye)))
      (setf (aref wha ye) 1))
    (dolist (xe x)
      (locally (declare (fixnum xe)))
      (if (plusp (aref wha xe)) (setq cx (the fixnum (1+ cx)))))
    (dolist (ye y) 
      (locally (declare (fixnum ye)))
      (setf (aref wha ye) 0))
    (if (or (< lx 2) (< ly 2))
        0.0
      (/ (+ (/ (float cx 0.0) (float lx 0.0))
           (/ (float cy 0.0) (float ly 0.0)))
        2.0))
    ))

#+test
(defun time-sh (w1 w2 n)
  (let ((l1 (compile-word-fast w1)) (l2 (compile-word-fast w2)))
    (time (dotimes (j n) (score-homology l1 l2)))
    (time (dotimes (j n) (score-homology-fast l1 l2)))))

;;; More flexible version using an accessor into frames that
;;; is a function on the elements of list that yields a word.

(defun word-homology (word list &optional (n 3) (accessor #'identity))
  #.(one-string-nl
     "Score how WORD compares with the words in LIST using character-pair "
     "homology and return the top N words in LIST along with their scores")
  ;; list of (score word) elements, smallest first
  (let* ((topn '())			
	 (compiled-word (compile-word word)))
    ;; If the new word is more than the lowest of the ones in the set, 
    ;; replace that one with the new one.
    (dolist (entry list)
      (let* ((score (score-homology compiled-word (funcall accessor entry))))
	(cond 
         ((< (length topn) n)
          (push (list score entry) topn)
          (setf topn (sort (cons (list score entry) (cdr topn)) 
                           #'< :key #'car)))
         ((> score (car (car topn)))
          (setf topn (sort (cons (list score entry) (cdr topn)) 
                           #'< :key #'car))))))
    (nreverse topn)))


(defun word-homology-fast (word list &optional (n 3) (accessor #'identity))
  #.(one-string-nl
     "Score how WORD compares with the words in LIST using character-pair "
     "homology and return the top N words in LIST along with their scores")
  #.(optimization-declaration)
  (declare (fixnum n))
  (let ((topn nil) 
        (topn-length 0)
        (compiled-word (if (listp word) word (compile-word word))))
    (declare (fixnum topn-length))
    (dolist (entry list)
      (let ((score 
             (score-homology-fast compiled-word (funcall accessor entry))))
        (cond
         ((< topn-length n) 
          (incf topn-length)
          (setq topn (insert-into-ordered-list 
                      (list score entry) #'< topn :key #'first)))
         ((> score (caar topn))
          (setq topn (insert-into-ordered-list
                      (list score entry) #'< (cdr topn) :key #'first))))))
    (nreverse topn)))
         

(defun float-to-mysql-float-string (f)
  "Returns a string acceptable to MYSQL.  E.g, 1.05d-25 -> \"1.05e-25\""
  (if (numberp f) 
      (setq f (float f)) ; In case an integer or something was passed...
    (error "In float-to-mysql-float-string: Non-numeric argument (~s)" f))
  (cond
   ((typep f 'single-float) (format nil "~A" f))
   ((typep f 'double-float) (substitute #\e #\d (format nil "~A" f)))
   ))


(defun ntranslate-string-fast (string from to)
  #.(optimization-declaration)
  (declare (simple-string string from to))
  (let ((ls (length string)) (lf (length from)))
    (declare (fixnum ls lf))
    ;; Completely arbitrary test for using hash algorithm.
    (if (and (> lf 10) (> ls 100))
        (let ((ht (make-hash-table :test 'eql)))
          (loop for i fixnum below lf do
                (setf (gethash (schar from i) ht) (schar to i)))
          (loop for i fixnum below ls 
                as translation = (gethash (schar string i) ht)
                when translation
                do (setf (schar string i) translation)
                ))
      (loop for i fixnum below ls
            as pos = (position (schar string i) from)
            when pos
            do (setf (schar string i) (schar to pos)))))
  string)

(defun ntranslate-string (string from to)
  #.(one-string-nl
     "Destructively changes the characters in a string from one set to "
     "another.  For example: (ntranslate-string \"This\" "
     "\"abcdefghijklmnopqrstuvwxyz\" \"ABCDEFGHIJKLMNOPQRSTUVWXYZ\") " 
     "will change the string to THIS and return it. "
     "NOTE THAT THIS ACTUALLY MODIFIES THE ORIGNAL STRING; "
     "If you want to preserve the string, use TRANSLATE-STRING."
     )
  (if (and (simple-string-p string) 
           (simple-string-p from) 
           (simple-string-p to))
      (ntranslate-string-fast string from to)
    (loop for i below (length string)
          as pos = (position (aref string i) from)
          when pos
          do (setf (aref string i) (aref to pos))))
  string)

(defun translate-string (string from to)
  #.(one-string-nl
     "Changes the characters in a string from one set to another. "
     "See the documentation for NTRANSLATE-STRING.")
  (ntranslate-string (copy-seq string) from to))



(defun limited-string (value &optional (limit 100))
  #.(one-string-nl
     "If VALUE is a string longer than LIMIT, returns the initial "
     "subsequence of the string postpended with '...'")
  (cond
   ((not (stringp value)) value)
   ((<= (length value) limit) value)
   (t (one-string (subseq value 0 limit) "..."))
   ))

(defun ellipsis-string (s &optional (limit 100))
  #.(one-string-nl
     "If is a string longer than LIMIT, returns the initial LIMIT-3"
     "characters with '...' postpended.  If LIMIT is 3 or less, it"
     "does not postpend the '...', it just returns the first LIMIT"
     "characters."
     "If the string is less than or equal to LIMIT in size, the"
     "original string is returned.")
  (let ((slen (length s)))
    (cond
     ((<= slen limit) s)
     ((zerop limit) "")
     ((< limit 4) (subseq s 0 limit))
     (t (one-string (subseq s 0 (- limit 3)) "..."))
     )))

(defun limited-form-string 
       (form 
        limit
        &key 
        (format-mode "~A")
        (print-pretty *print-pretty*)
        (single-line? nil)
        (strip-indentation? nil)
        )
  #.(one-string-nl
     "Takes FORM and creates a string representation of that form,"
     "then truncates the string to at most LIMIT characters and returns"
     "the possibly truncated string."
     "FORMAT-MODE defaults to \"~A\" ; the other reasonable value"
     "is \"~S\"."
     "Example: "
     "(limited-form-string '(This is a long form that must be truncated) -->"
     "\"(THIS IS A LONG FORM THAT MUST...\"")
  (let ((*print-pretty* print-pretty))
    (let ((form-string (formatn format-mode form)))
      (cond
       ((null single-line?)
        (limited-string form-string limit))
       ((null strip-indentation?)
        (limited-string (substitute #\Space #\Newline form-string) limit))
       (t
        (limited-string (strip-newlines-and-indentation form-string) limit)
        )))))

(defun strip-newlines-and-indentation (s)
  (declare (string s))
  (if (null (position #\Newline s))
      (copy-seq s)
    (let* ((len (length s))
           (v (make-array len :initial-element t)))
      (declare (fixnum len))
      (loop
       with j fixnum = -1
       until (>= j (the fixnum (1- len)))
       do
       (incf j)
       (when (eql #\Newline (char s j))
         (loop for k fixnum from (the fixnum (+ j 1)) below len
               until (not (whitespacep (char s k)))
               do (setf (aref v k) nil)
               finally (setf j (the fixnum (1+ k)))
               )))
      (nsubstitute #\Space #\Newline (select-sequence-elements s v))
      )))


(defun maybe-clip-string (string limit &optional (clip-suffix "..."))
  #.(one-string-nl
     "Returns two values."
     "If STRING is <= LIMIT in length, returns STRING and NIL."
     "Otherwise, returns as the first value a string of length LIMIT"
     "whose last characters are CLIP-SUFFIX and whose initial characters"
     "are the initial characters of STRING, and as a second string value"
     "the clipped characters of STRING."
     "(maybe-clip-string \"xyzzyfoozr\" 8 \"***\") -> \"xyzzy***\" \"foozr\" ")
  (let ((len (length string)))
    (if (<= len limit)
        (values string nil)
      (let ((clip-pos (- limit (length clip-suffix))))
        (values
         (one-string (subseq string 0 clip-pos) clip-suffix)
         (subseq string clip-pos)
         )))))


(defun all-strings-of-length (alphabet length &KEY as-odometer)
  #.(one-string-nl
     "Generates all possible strings of length LENGTH from an alphabet"
     "of characters.  Example:  (all-strings-of-length \"ACGT\" 2) -->"
     "(\"AA\" \"CA\" \"GA\" \"TA\" \"AC\" \"CC\" \"GC\" \"TC\""
     "\"AG\" \"CG\" \"GG\" \"TG\" \"AT\" \"CT\" \"GT\" \"TT\"))")
  (UNLESS as-odometer
    (setq alphabet (sort (copy-seq alphabet) 'string-lessp)))
  (if (zerop length)
      (list "")
    (progn
      (setq alphabet (coerce alphabet 'simple-string))
      (let ((number-of-strings (expt (length alphabet) length)))
        (when (> (* number-of-strings length) (expt 2 31))
          (error 
           (one-string-nl
            "Ruh roh. Generating all these strings would consume more memory"
            "than is addressable on a 32-bit machine!!")))
        (let ((seed-strings 
               (loop for j fixnum from 0 below (length alphabet) collect
                     (make-string length :initial-element (aref alphabet j))
                     )))
        ; (loop for pos fixnum from 1 below length do
          (loop for pos fixnum from (- length 2) DOWNTO 0 do
                (setq seed-strings 
                      (copy-strings-and-add-alphabet-at-position
                       seed-strings alphabet pos
                       )))
          seed-strings
          )))))

(defun copy-strings-and-add-alphabet-at-position (strings alphabet position)
  #.(optimization-declaration)
  (declare (simple-string alphabet) (fixnum position))
  (let* ((alength (length alphabet))
         (replication-factor alength)
         (replications
          (loop for j fixnum from 0 below replication-factor collect
                (mapcar 'copy-seq strings)
                )))
    (declare (fixnum alength replication-factor))
    (loop for replication-set in replications
          for aindex fixnum from 0 below alength
          as char = (schar alphabet aindex) do
          (loop for s in replication-set do 
                (setf (schar (the simple-string s) position) char)))
    (apply 'nconc replications)
    ))
  

(defun character-counts (s &key (sort-by :char-code) &aux strings)
  #.(one-string-nl
     "Returns a list of (character count) elements, where CHARACTER is a"
     "character found in S and COUNT is the number of times it occurs."
     "The list is sorted. If SORT-BY is :CHAR-CODE it is sorted in ascending"
     "order on the CHAR-CODE of each found character. If SORT-BY is anything"
     "else the list is sorted by the count, most frequent occurences first."
     "If S is a list, it is assumed to be a list of strings, and the"
     "operation is performed as if the strings were concatenated together.")
  (setq strings (ensure-list s))
  (let ((h (make-hash-table :test 'eql :size 256)))
    (loop for s in strings do
          (loop for ch across s do (incf (gethash ch h 0))))
    (let ((list (hash-table-contents h)))
      (case sort-by
        (:char-code
         (sort list '< :key (lambda (x) (char-code (first x)))))
        (otherwise
         (sort list '> :key 'second)
         )))))


;;; To convert some other type (like a frame), define this method
;;; for the type in question.

(defmethod anything-stringlike-to-string ((x t))
  (anything-stringlike-to-string-or-error x))

(defun anything-stringlike-to-string-or-nil (x)
  (cond
   ((stringp x) x)
   ((characterp x) (string x))
   ((symbolp x) (string x))
   ((numberp x) (format nil "~S" x))
   ((pathnamep x) (namestring x))
   (t nil)
   ))

(defun anything-stringlike-to-string-or-error (x)
  (or (anything-stringlike-to-string-or-nil x)
      (error "Cannot use s+ to convert ~S to string form." x)))


(defun s+ (&rest args)
  #.(one-string-nl
     "First converts all ARGS to a string representation and then concatenates"
     "all the strings together. Uses the generic function"
     "ANYTHING-STRING-LIKE-TO-STRING to do the conversion.  The default method"
     "knows about strings, characters, symbols, numbers, and pathnames."
     "You can define your own method for any other type (such as a frame)."
     "Example: (s+ 1 \"abc\" 'foo #\x) --> \"1abcfoox")
  (cond
   ((null args) "")
   ((null (cdr args)) (anything-stringlike-to-string (first args)))
   ((null (cddr args)) 
    (concatenate
     'string 
     (anything-stringlike-to-string (first args))
     (anything-stringlike-to-string (second args))
     ))
   (t
    (string-join (mapcar 'anything-stringlike-to-string args) "")
    )))


(defun s+join (join &rest args)
  #.(one-string-nl
     "Converts all args to a string representation as with S+,"
     "then joins all the strings together as with STRING-JOIN using"
     "JOIN as STRING-JOIN's second argument.") 
  (setq join (anything-stringlike-to-string-or-error join))
  (string-join (mapcar 'anything-stringlike-to-string args) join)
  )

(defun string+-join (item-list &optional (sep #\Space))
  #.(one-string-nl
     "Like STRING-JOIN, except that that the ITEM-LIST elements"
     "need not be strings.  Each element is converted to a string"
     "if necessary using ANYTHING-STRINGLIKE-TO-STRING, and then"
     "STRING-JOIN is called.")
  (if (every 'stringp item-list)
      (string-join item-list sep)
    (string-join (mapcar 'anything-stringlike-to-string item-list) sep)
    ))

(defun precede-with-header-and-indent (string header &key (indent 2))
  #.(one-string-nl
     "Takes STRING, splits it into lines, adds INDENT spaces to"
     "the beginning of each line, rejoins the prepended lines and"
     "finally concatenates HEADER to the beginning of the rejoined string."
     "If HEADER is NIL, no header is appended, otherwise HEADER must"
     "be a string.")
  (let ((spaces (make-string indent :initial-element #\Space)))
    (s+
     (if header header "")
     (if header (string #\Newline) "")
     spaces
     (string-join
      (string-split string #\Newline)
      (one-string (string #\Newline) spaces)
      ))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *whitespace* 
    '(#\Space #\Tab #\Return #\Newline 
              #\Page #\Null #\Linefeed
              #+:MCL #\312
              )))

(defun whitespacep (x)
  #.(one-string-nl
     "If X is a character, returns T if X is a whitespace char."
     "If X is a string, returns T if every char of X is a whitespace char."
     "Whitespace characters are: " 
     (format nil "~S" *whitespace*))
  (if (stringp x)
      (every #'fast-whitespacep x)
    (not (null (fast-whitespacep x)))))

(defun fast-whitespacep (char)
  #.(optimization-declaration)
  (member char *whitespace* :test #'eql))

(defun remove-all-whitespace (string)
  #.(one-string-nl
     "Remove all whitespace (as defined by WHITESPACEP) from STRING, returning"
     "a new string.  STRING itself is unchanged.")
  (remove-all-of string *whitespace*))

(defun center-in (string length)
  #.(one-string-nl
     "Creates a new string with whitespace to the left and right such that"
     "the original string is centered in a field of LENGTH characters"
     "The extra padding character, if any, is to the right"
     )
  (if (<= length (length string))
      string
    (let* ((excess (- length (length string)))
           (left-padding 
            (if (evenp excess) (/ excess 2) (floor excess 2)))
           (centered-string (make-string length :initial-element #\Space)))
      (replace centered-string string :start1 left-padding)
      centered-string
      )))

(defun string-to-lines (string line-width &key (max-lines nil))
  #.(one-string-nl
     "Creates a list of strings each LINE-WIDTH long containing the characters"
     "of STRING.  STRING is split up into words (things separated by spaces)"
     "and words are not split up between lines unless some words are longer"
     "than LINE-WIDTH."
     )
  (flet ((newline () (make-string line-width :initial-element #\Space)))
    (let ((words (string-split string #\Space))
          (current-pos 0)
          (current-line (newline))
          (lines nil))
      (loop for word in words 
            as wordlen = (length word)
            as remaining-width = (- line-width current-pos)
            do
            (cond
             ((< wordlen remaining-width)
              (replace current-line word :start1 current-pos)
              (incf current-pos (1+ wordlen))
              )
             ((= wordlen remaining-width)
              (replace current-line word :start1 current-pos)
              (push current-line lines)
              (setq current-line (newline))
              (setq current-pos 1)
              )
             ((and (> wordlen remaining-width) (< wordlen line-width))
              (push current-line lines)
              (setq current-line (newline))
              (replace current-line word :start1 0)
              (setq current-pos (1+ wordlen))
              )
             ((and (> wordlen remaining-width) (= wordlen line-width))
              (push current-line lines)
              (setq current-line (newline))
              (replace current-line word :start1 0)
              (push current-line lines)
              (setq current-line (newline))
              (setq current-pos 1)
              )
             (t
              (push current-line lines)
              (let ((lines-needed (ceiling wordlen line-width))
                    (last-line-size (mod wordlen line-width)))
                (loop for j from 1 to (1- lines-needed)
                      for wordpos = 0 then (+ wordpos line-width)
                      do 
                      (push (subseq word wordpos (+ wordpos line-width)) lines)
                      finally
                      (if (zerop last-line-size)
                          (progn
                            (push (subseq word wordpos (+ wordpos line-width))
                                  lines)
                            (setq current-line (newline))
                            (setq current-pos 1)
                            )
                        (progn
                          (setq current-line (newline))
                          (replace 
                           current-line word :start2 (+ wordpos line-width))
                          (setq current-pos (1+ last-line-size))
                          ))))))
            finally
            (unless (every (lambda (x) (char-equal x #\Space)) current-line)
              (push current-line lines)
              ))
      (let ((result (reverse lines)))
        (cond
         ((null max-lines) result)
         ((<= (length result) max-lines) result)
         (t (subseq result 0 max-lines))
         )))))
            
           
(defun string-after-first (s char)
  (setq 
   char 
   (cond
    ((characterp char) char)
    ((stringp char) (char char 0))
    (t (error "Invalid CHAR argument, ~S, to STRING-AFTER-FIRST" char))
    ))
  (let ((pos (position char s)))
    (if pos (if (= pos (length s)) "" (subseq s (1+ pos))) "")
    ))

(defun string-before-first (s char)
  (setq 
   char 
   (cond
    ((characterp char) char)
    ((stringp char) (char char 0))
    (t (error "Invalid CHAR argument, ~S, to STRING-BEFORE-FIRST" char))
    ))
  (let ((pos (position char s)))
    (if pos (if (= pos 0) "" (subseq s 0 pos)) (copy-seq s))
    ))

(defun replace-chars 
       (old-string mapping &optional (remove-all-other-non-ascii? t))
  #.(one-string-nl
     "This function takes a string to be replaced and a mapping of characters"
     "to their replacements.  MAPPING is either a list of the form"
     "((bad-code-char replacement-string) (bad-code-char replacement-string)..)"
     "or a vector which maps character codes to replacement strings, or a"
     "hash-table whose keys are characters and whose values are replacement"
     "strings."
     ""
     "If no characters need to be replaced, the"
     "original string is returned if OLD-STRING is a simple-string (otherwise"
     "a copy of OLD-STRING is returned), and the second value is nil."
     "If characters have been replaced (or deleted), a new string is"
     "returned and the second value is T."
     )
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (when (not (stringp old-string))
    (error "First argument to REPLACE-BAD-CHARS must be a string!"))
  (unless (simple-string-p old-string)
    (setq old-string (coerce old-string 'simple-string)))
  (locally 
    (declare (simple-string old-string))
    (block exit
      (multiple-value-bind (new-length to-be-changed?)
          (bad-chars-info old-string mapping remove-all-other-non-ascii?)
        (when (not to-be-changed?) (return-from exit (values old-string nil)))
        (let ((new-string (make-string new-length)))
          (declare (simple-string new-string))
          (loop 
           for char across old-string
           for pos fixnum from 0
           as code fixnum = (char-code char)
           do
           (cond
            ((etypecase mapping
               (list 
                (loop
                 for (possible-match replacement) in mapping
                 as new-chars-length fixnum = (length replacement)
                 as delta fixnum = (the fixnum (- new-chars-length 1))
                 do
                 (etypecase possible-match
                   (number 
                    (when (= code (the fixnum possible-match))
                      (add-the-new-chars 
                       replacement new-chars-length new-string pos)
                      (incf pos delta)
                      (return t)
                      ))
                   (character 
                    (when (char= char possible-match)
                      (add-the-new-chars
                       replacement new-chars-length new-string pos)
                      (incf pos delta)
                      (return t)
                      ))
                   (simple-string 
                    (when (char= char (schar possible-match 0))
                      (add-the-new-chars
                       replacement new-chars-length new-string pos)
                      (incf pos delta)
                      (return t)
                      ))
                   (string 
                    (when (char= char (char possible-match 0))
                      (add-the-new-chars
                       replacement new-chars-length new-string pos)
                      (incf pos delta)
                      (return t)
                      )))
                 finally (return nil)
                 ))
               ;; assumes hash-table keys are characters
               (hash-table 
                (let ((replacement (gethash char mapping)))
                  (when replacement
                    (let ((new-chars-length (length replacement)))
                      (declare (fixnum new-chars-length))
                      (add-the-new-chars 
                       replacement new-chars-length new-string pos)
                      (incf pos (the fixnum (- new-chars-length 1)))
                      t
                      ))))
               (vector
                (when (< code (length mapping))
                  (let ((replacement (aref mapping code)))
                    (when replacement
                      (let ((new-chars-length (length replacement)))
                        (declare (fixnum new-chars-length))
                        (add-the-new-chars 
                         replacement new-chars-length new-string pos)
                        (incf pos (the fixnum (- new-chars-length 1)))
                        t
                        )))))))
            ((and remove-all-other-non-ascii? (> code 127)) nil)
            (t (setf (schar new-string pos) char))
            ))
          (values new-string to-be-changed?)
          )))))

(defun add-the-new-chars (new-chars new-chars-length new-string pos)
  (declare (fixnum new-chars-length pos))
  (declare (simple-string new-string))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (etypecase new-chars
    (simple-string
     (loop for new-pos fixnum from 0 below new-chars-length
           do
           (setf 
            (schar new-string (the fixnum (+ pos new-pos)))
            (schar new-chars new-pos))
           ))
    (string 
     (add-the-new-chars 
      (coerce new-chars 'simple-string) new-chars-length new-string pos)
     )))
      
(defun bad-chars-info (old-string mapping remove-all-other-non-ascii?)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (unless (simple-string-p old-string)
    (setq old-string (coerce old-string 'simple-string)))
  (locally
    (declare (simple-string old-string))
    (let ((to-be-changed? nil)
          (slen 0))
      (declare (fixnum slen))
      (loop for char across old-string 
            as code fixnum = (char-code char)
            as ilen fixnum = 
            (cond 
             ((etypecase mapping
                (list
                 (loop for (possible-match replacement) in mapping
                       do
                       (etypecase possible-match
                         (number 
                          (when (= code (the fixnum possible-match))
                            (setq to-be-changed? t)
                            (return (length replacement))
                            ))
                         (character 
                          (when (char= char possible-match)
                            (setq to-be-changed? t)
                            (return (length replacement))
                            ))
                         (simple-string 
                          (when (char= char (schar possible-match 0))
                            (setq to-be-changed? t)
                            (return (length replacement))
                            ))
                         (string 
                          (when (char= char (char possible-match 0))
                            (setq to-be-changed? t)
                            (return (length replacement))
                            )))
                       finally (return nil)
                       ))
                (hash-table 
                 (let ((replacement (gethash char mapping)))
                   (when replacement 
                     (setq to-be-changed? t)
                     (length replacement)
                     )))
                (vector
                 (when (< code (length mapping))
                   (let ((replacement (aref mapping code)))
                     (when replacement
                       (setq to-be-changed? t) 
                       (length replacement)
                       ))))))
             ((and remove-all-other-non-ascii? (> code 127))
              (setq to-be-changed? t)
              0
              )
             (t 1)
             )
            do
            (setq slen (the fixnum (+ slen ilen)))
            )
      (values
       slen
       to-be-changed?
       ))))

(defun nth-position (n string char)
  #.(one-string-nl
     "Returns the position of the Nth occurence of CHAR.  If CHAR"
     "does not appear or does not appear N times, NIL is returned.")
  (declare (fixnum n))
  (declare (character char))
  (block exit
    (if (not (simple-string-p string))
        (nth-position n (coerce string 'simple-string) char)
      (locally 
        (declare (simple-string string))
        (declare (optimize (speed 3) (safety 0) (debug 0)))
        (unless (plusp n)
          (error "Nth argument must be a positive integer!"))
        (let ((count 0))
          (declare (fixnum count))
          (loop for ch across string
                for j fixnum from 0
                do
                (when (char= ch char) 
                  (when (= (incf count) n)
                    (return-from exit j)
                    ))
                finally
                (return-from exit nil)
                ))))))
        
(defun sbutlast (string &optional (n 1))
  #.(one-string-nl
     "Returns the string minus its last N (default 1) characters."
     "If there are less than or equal to N characters in the string,"
     "the null string is returned.")
  (let ((len (length string)))
    (if (<= len n) "" (subseq string 0 (- len n)))
    ))
