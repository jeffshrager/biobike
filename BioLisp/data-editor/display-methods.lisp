 ;;; -*- Package: data-editor; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :data-editor)

;;; +=========================================================================+
;;; | Copyright (c) 2009 JP Massar                                            |
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

;;; Author: JP Massar

#||

How to truncate strings approximately correctly using caps reduction.

Problem: Create a truncated string ST, from a string S of length LEN,
which fits approximately into a space that will fit MAX average-sized
characters on a screen using a variable-width font.

If the string is in fact truncated, it must be postpended with '...'
and those three characters must be taken into account with respect
to the MAX limit.

Our approximation assumes that uppercase letters take up more space
space than other characters, while we do not account for the fact that
certain characters, like '.', take up a smaller amount of space (except
that we take that into account when we add the trailing '...' by counting
it only as 2 characters, not three).

1.  If LEN <= MAX
    Compute 'psuedo-size' of string, by summing 1 for all non-uppercase
chars, and *caps-size* for uppercase characters.  
    If psuedo-size <= MAX return original string, otherwise logically
       remove one character after another from the end of the string
       and recompute psuedo-size until it is <= MAX.  Then remove
       the last two chars and replace with '...'. 

2.  If LEN > MAX, then truncate the string to MAX, and again logically
remove one character at a time from the end until the pseudo-size is
<= MAX.  Then remove the last two chars and replace with '...'.

||#

(defparameter *caps-size* 1.5)

(defun char-pseudo-size (ch) 
  ;; To be more sophisticated, we could case on particular characters
  ;; and assign a more accurate width to each of them.
  (if (upper-case-p ch) *caps-size* 1.0))

(defun create-display-string (s max)
  (let ((len (length s)))
    (if (<= len max)
        (let ((pseudo-size (reduce '+ s :key 'char-pseudo-size)))
          (if (<= pseudo-size max)
              s
            (loop
             for index from (1- len) downto 0
             do
             (decf pseudo-size (char-pseudo-size (char s index)))
             (when (<= pseudo-size max)
               (return (s+ (subseq s 0 (- index 2)) "...")))
             finally (return "...")
             )))
      (let* ((pseudo-size (reduce '+ s :key 'char-pseudo-size :end max)))
        (if (<= pseudo-size max)
            (s+ (subseq s 0 (- max 2)) "...")
          (loop
           for index from (1- max) downto 0
           do
           (decf pseudo-size (char-pseudo-size (char s index)))
           (when (<= pseudo-size max)
             (return (s+ (subseq s 0 (- index 2)) "...")))
           finally (return "...")
           ))))))


(defgeneric limited-display-strings (obj max)
  (:documentation
   #.(doc
      "Returns as its first value a display string of at most MAX characters,"
      "adjusted for capitalized letters by a reduction in characters"
      "of up to a third if they are all uppercase."
      "Returns as its second value a tooltip one-line description string,"
      "if appropriate."
      "Returns as its third value a multiline description string," 
      "if appropriate."
      "One of the 2nd or 3rd values is always NIL."
      "MAX should be at least 12, or weird things will happen with certain"
      "types of objects."
      )))

(defmethod limited-display-strings ((obj t) max)
  (let ((objtype (type-of obj)))
    (cond
     ((and (symbolp objtype) (get objtype :object-slots))
      (let* ((s (formatn "~A" obj))
             (sl (create-display-string s max))
             (d (formatn "An object of type ~A with ~D slots" 
                         objtype (length (get objtype :object-slots))
                         )))
        (values sl d nil)))
     ((eq 'standard-class (type-of (class-of obj)))
      (let* ((s (formatn "~A" obj))
             (sl (create-display-string s max))
             (d (formatn "A CLOS object of type ~A" objtype))
             )
        (values sl d nil)))
     (t
      (let* ((s (formatn "~A" obj))
             (sl (create-display-string s max))
             (d 
              (limited-string 
               (with-output-to-string (p) (describe obj p))
               1000
               )))
        (values sl nil d)
        )))))

(defmethod limited-display-strings ((obj character) max)
  (let* ((s (formatn "~S" obj))
         (sl (create-display-string s max))
         (code (char-code obj)))
    ;; No multiline description needed.
    (cond
     ((eq s sl)
      (values s (formatn "Charcode: ~D" code) nil))
     (t
      (values sl (s+ s ", Charcode: ~D" code) nil))
     )))

(defmethod limited-display-strings ((obj integer) max)
  (let* ((s (formatn "~S" obj))
         (sl (create-display-string s max))
         (len (length s)))
    (flet ((upperhex (x) (string-upcase (formatn "~X" x))))
      (cond
       ((eq s sl)
        (if (<= len 15)
            (values s (formatn "Binary: ~B, Hex: ~A" obj (upperhex obj)) nil)
          (values s (formatn "Hex: ~A" (upperhex obj)) nil)
          ))
       (t
        (let ((len (length s)))
          (cond
           ((< len 60)
            (values 
             sl 
             nil
             (formatn (one-string-nl "Value: ~A" "Hex: ~X") s obj)
             ))
           (t
            (values
             sl
             (formatn "A ~D digit integer" (if (minusp obj) (1- len) len))
             nil
             )))))))))

(defmethod limited-display-strings ((obj ratio) max)
  ;; hack to get around hack to print ratios as floating points in bbl
  (let* ((s (formatn "~D/~D" (numerator obj) (denominator obj)))
         (sl (create-display-string s max)))
    (labels ((floatstring ()
               (cond 
                ((> 100.0 (abs obj) 0.0001) 
                 (formatn "Approximately ~6,4F" (underflow-to-zero obj)))
                (t (formatn "As a float, approximately: ~F" 
                            (underflow-to-zero obj)))
                ))
             (underflow-to-zero (f)
               (handler-case (float f)
                 (floating-point-underflow () 0.0)
                 (floating-point-overflow () "*** Overflow! ***")
                 (error (c) (signal c))
                 )))
      (cond
       ((eq s sl) (values s (floatstring) nil))
       (t
        (let* ((n (numerator obj))
               (ns (formatn "~S" n))
               (nl (create-display-string ns (floor max 2)))
               (d (denominator obj))
               (ds (formatn "~S" d))
               (dl (create-display-string ds (floor max 2))))
          (values
           (formatn "~A/~A" nl dl)
           nil
           (one-string-nl
            (floatstring)
            (formatn "Numerator  : ~D" ns)
            (formatn "Denominator: ~D" ds)
            ))))))))

(defmethod limited-display-strings ((obj complex) max)
  (let* ((s (formatn "~S" obj))
         (sl (create-display-string s max)))
    (cond
     ((eq s sl) (values s "A complex number" nil))
     (t
      (let* ((maxpart (max 3 (floor (- max 5) 2)))
             (r (realpart obj))
             (rl (limited-display-strings r maxpart))
             (i (imagpart obj))
             (il (limited-display-strings i maxpart)))
        (values
         (formatn "#C(~A ~A)" rl il)
         nil
         (one-string-nl
          (formatn "Real part : ~A" r)
          (formatn "Imag part : ~A" i)
          )))))))

(defmethod limited-display-strings ((obj number) max)
  (let* ((s (formatn "~S" obj))
         (sl (create-display-string s max)))
    (cond
     ((eq s sl) (values s nil nil))
     (t (values sl s nil))
     )))

(defmethod limited-display-strings ((obj symbol) max)
  (let* ((s (symbol-name obj))
         (sl (create-display-string s max)))
    (cond
     ((eq obj t) (values "T" "True" nil))
     ((keywordp obj)
      (if (eq s sl)
          (values (s+ ":" s) "A keyword symbol" nil)
        (values (s+ ":" sl) (formatn "A keyword named '~A'" s) nil)
        ))
     ((null obj) (values "-" "Nil (boolean false and the empty list)" nil))
     (t
      (values
       sl
       nil
       (formatn
        (one-string
         "~A"
         "A symbol in the ~A package~%"
         "Property List: ~A~%")
        (if (eq s sl) "" (formatn "Name: ~A~%" s))
        (package-name (symbol-package obj))
        (if (symbol-plist obj)
            (formatn "(~D entries)" (length (symbol-plist obj)))
          "-"
          )))))))

         
(defmethod limited-display-strings ((obj package) max)
  (let ((ecount 0)
        (scount (length (package-shadowing-symbols obj)))
        (nicknames (package-nicknames obj))
        (use-list (package-use-list obj))
        (used-by-list (package-used-by-list obj)))
    (do-external-symbols (x obj) (incf ecount))
    (let* ((s (string-capitalize (formatn "~A" obj)))
           (sl (create-display-string s max))
           (d
            (formatn
             (one-string-nl
              "Name: ~A"
              "Nicknames: ~A"
              "Uses: ~A"
              "Used by: ~A"
              "It has ~D external symbol~P"
              "And ~D shadowing symbol~P")
             (package-name obj)
             (or nicknames "-")
             (if use-list (mapcar 'package-name use-list) "-")
             (if used-by-list (mapcar 'package-name used-by-list) "-")
             ecount ecount
             scount scount
             )))
      (values sl nil d)
      )))

           
(defmethod limited-display-strings ((obj string) max)
  (let ((sl (create-display-string obj max))
        (len (length obj))
        (nchars 60))

    (cond
     ((string-equal sl obj)
      (let ((english-size (formatn "~R" len)))
        (values 
         sl 
         (formatn "~A ~A character string" (a-or-an? english-size) english-size)
         nil
         )))
     ((<= len 600)
      (values 
       sl nil
       (string-join 
        (loop for j from 0 by nchars
              until (>= j len)
              collect
              (subseq obj j (min len (+ j nchars))))
        #\Newline
        )))
     (t
      (values 
       sl 
       nil
       (string-join 
        (flatten
         (list
          (formatn "A string of length ~D" len)
          (formatn "------------------------------")
          (loop for j from 0 by nchars
                for i from 1 to 6
                collect 
                (subseq obj j (+ j nchars)))
          "..."
          (subseq obj (- len nchars))
          ))
        #\Newline
        ))
      ))))

(defmethod limited-display-strings ((obj %frame) max)
  (flet ((pname (f) (string-capitalize (fname f))))
    (let* ((s (pname obj))
           (sl (create-display-string s max))
           (nslots (length (frame-slots-of obj)))
           (type (or (first (#^isa obj)) (#^organism-entity-type obj))))
      (if (eq s sl)
          (values 
           s
           (formatn 
            "Data frame~A with ~D data slot~P"
            (if type (formatn " of type ~A" (pname type)) "")
            (1- nslots) (1- nslots))
           nil
           )
        (values
         sl
         nil
         (formatn
          (one-string-nl
           "Data frame: ~A"
           "Number of slots: ~D"
           (if type (formatn "Frame type: ~A" (pname type)) ""))
          s nslots nslots
          ))))))

(defun lds-sequence (obj max)
  (let* ((limited-sequence (first-n-elements-depth-first obj max))
         (s (formatn "~S" limited-sequence))
         (sl (limited-display-strings s max))
         (type (if (consp obj) "List" "Vector"))
         (len (length obj)))
    (values sl (formatn "A ~A of length ~D" type len) nil)
    ))

(defmethod limited-display-strings ((obj vector) max)
  (lds-sequence obj max))

(defmethod limited-display-strings ((obj cons) max)
  (lds-sequence obj max))

(defmethod limited-display-strings ((obj hash-table) max)
  (let* ((s (formatn 
             "#<~D-element, ~A Hash Table>"
             (hash-table-count obj) (hash-table-test obj)))
         (sl (create-display-string s max))
         (d (formatn
             (one-string-nl
              "Hash table currently with ~D elements."
              "Test function: ~A"
              "Current capacity: ~D"
              "Rehash multiplier: ~A"
              "Rehash threshold: ~A"
              )
             (hash-table-count obj)
             (hash-table-test obj)
             (hash-table-size obj)
             (hash-table-rehash-size obj)
             (hash-table-rehash-threshold obj)
             )))
    (values sl nil d)
    ))

(defmethod limited-display-strings ((obj utils::garray) max)
  (let* ((s (formatn "~A" obj))
         (sl (create-display-string s max))
         (d (with-output-to-string (p) 
              (describe-garray 
               obj :stream p :type (if (wb::bbl-mode?) "TABLE" "GARRAY" )
               ))))
    (values sl nil d)
    ))


(defmethod limited-display-strings ((obj array) max)
  (let* ((rank (array-rank obj))
         (dims (array-dimensions obj))
         (dimstring 
          (string-join (mapcar (lambda (x) (formatn "~D" x)) dims) ",")))
    (let* ((s (formatn "<~Dd Array (~A)>" rank dimstring))
           (sl (create-display-string s max))
           (d
            (formatn 
             (one-string-nl
              "A ~Dd Array with ~D elements with dimensions (~A)"
              "Array type: ~A")
             rank (array-total-size obj) dimstring (type-of obj)
             )))
      (values sl nil d)
      )))

(defun first-n-elements-depth-first (seq max)
  (let ((count 0))
    (labels ((doit (sequence)
               (let ((list nil))
                 (block exit
                   (map 
                    nil
                    (lambda (elem)
                      (typecase elem
                        ((or cons vector) (push (doit elem) list))
                        (t (push elem list))
                        )
                      (when (>= (incf count) max) (return-from exit nil))
                      )
                    sequence
                    ))
                 (let ((rlist (reverse list)))
                   (typecase sequence
                     (cons rlist)
                     (string (coerce rlist 'string))
                     (bit-vector (coerce rlist 'bit-vector))
                     (t (coerce rlist 'simple-vector))
                     )))))
      (doit seq)
      )))

