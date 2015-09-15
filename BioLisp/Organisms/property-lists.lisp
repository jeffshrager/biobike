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

;;; Author:  JP Massar

;;; Verify file exists, read the property list from the file,
;;; verify the property list is kosher, and return the list and
;;; it's string representation.


(defun verify-organism-plist-file (organism-name organism-directory)
  (let ((plist-file 
         (merge-pathnames 
          (make-pathname :name organism-name :type "plist")
          organism-directory))
        (plist nil)
        (plist-string nil))
    ;; Make sure file exists.
    (unless (probe-file plist-file)
      (error "The ~A file does not exist!" (namestring plist-file)))
    ;; Read the property list from the file.
    (with-standard-io-syntax
      (let ((*package* (find-package :keyword)))
        (with-open-file (p plist-file :direction :input)
          (setq plist (read p)))))
    (unless (listp plist) 
      (error "Property list file does not contain a list!"))
    (setq plist (unkeywordize-property-list plist))
    ;; Make sure there is a VERSION property.
    (unless (assoc :version plist)
      (error "Property list file must contain VERSION key and value!"))
    ;; Provide machine and human readable timestamp information for
    ;; when this organism was uploaded.
    (let ((timestamp (get-universal-time)))
      (multiple-value-bind (sec min hour date month year)
          (decode-universal-time timestamp)
        ;; If no DATE property exists, provide one.
        (unless (assoc :date plist)
          (push (list :date (list year month date)) plist))
        (push (list :timestamp timestamp) plist)
        (push (list :timestamp-string
                    (format nil "~2,'0D/~2,'0D/~2,'0D ~2,'0D:~2,'0D:~2,'0D"
                            month date year hour min sec)) 
              plist)))
    ;; Make sure the property list can be printed out readably
    (handler-case
        (setq plist-string (plist-to-plist-string plist))
      (error 
       ()
       (error 
        (one-string
         "The property list file ~A contains a property list that"
         "cannot be printed out in readable format: ~A"
         plist-file plist
         ))))
    (values plist plist-string)
    ))

;;; Convert a property list found in an organism's .tbl data files
;;; into canonical form.  See documentation for definition of
;;; canonical form.  This form may then be converted to a string
;;; representation to store into the database in a PROPERTIES column.

(defun organism-property-string-to-key-value-list (pstring)

  (let* ((lparen (char "(" 0))
         (rparen (char ")" 0))
         (colon-syntax-terminators (list lparen rparen #\Space))
         (key-value-list nil))

    (labels 
        ((trimspace (s) (string-trim '(#\Space #\Tab) s))
         (invalid-string (reason)
           (error "Illegal property string: ~S.  ~A." pstring reason))
         (invalid-key-value-pair (kvp reason)
           (invalid-string
            (format nil "Invalid key-value-pair: ~A.  ~A." kvp reason)))
         (unkeywordize (x) (unkeywordize-property-list x))
         (string-to-keyword (s)
           (handler-case
               (let ((key (read-from-string s)))
                 (unless (symbolp key)
                   (invalid-string
                    "Invalid symbol before ':' in non-list key-value syntax"))
                 (unkeywordize key)
                 )))
         (parse-colon-syntax (s)
           (let ((pos (position #\: s)) (key nil) (value nil))
             (unless pos
               (invalid-string "No colon found in non-list key-value syntax"))
             (setq key (string-to-keyword (subseq s 0 pos)))
             (setq s (trimspace (subseq s (1+ pos))))
             (cond
              ((zerop (length s))
               (invalid-string
                "No value found after ':' in non-list key-value syntax"))
              (t
               (let ((pos (position-if 
                           #'(lambda (ch) (member ch colon-syntax-terminators))
                           s)))
                 (unless pos
                   (invalid-string 
                    "No terminator found for non-list key-value syntax"))
                 (setq value (subseq s 0 pos))
                 (setq s (trimspace (subseq s pos)))
                 )))
             (push (list key value) key-value-list)
             s))
         (parse-list-syntax (s)
           (multiple-value-bind (kvp next-char-pos)
               (read-from-string s)
             (cond
              ((not (consp kvp))
               (invalid-key-value-pair kvp "Not a list"))
              ((not (eql 2 (length kvp)))
               (invalid-key-value-pair kvp "Not a two-element list."))
              ((not (symbolp (first kvp)))
               (invalid-key-value-pair kvp "Key must be a Lisp symbol"))
              (t
               (push (unkeywordize kvp) key-value-list)))
             (setq s (trimspace (subseq s next-char-pos)))
             )))

      (with-standard-io-syntax
        (let ((*read-eval* nil) 
              (*package* (find-package :keyword))
              (*readtable* (frames-readtable)))
          (let ((s (trimspace pstring)))
            (cond
             ((zerop (length s)) nil)
             ;; If the first char is not a left paren we better have
             ;; colon syntax.
             ((not (eql (char s 0) lparen))
              (setq s (parse-colon-syntax s))
              (loop
               (cond
                ((zerop (length s)) (return))
                ((not (eql (char s 0) lparen))
                 (setq s (parse-colon-syntax s)))
                (t (setq s (parse-list-syntax s)))
                )))
             (t
              ;; The first char is a left paren.  We can still have
              ;; either Lisp or colon syntax for each property, but
              ;; we need a closing right paren eventually.
              (setq s (trimspace (subseq s 1)))
              (loop
               (cond
                ((zerop (length s))
                 (invalid-string "No closing right parenthesis"))
                ((eql (char s 0) rparen) (return))
                ((not (eql (char s 0) lparen))
                 (setq s (parse-colon-syntax s)))
                (t (setq s (parse-list-syntax s)))
                )))))))

      (nreverse key-value-list)

      )))


;;; We modify the reader to read symbols as keywords for property lists.
;;; But we don't want T and NIL to be turned into :T and :NIL, so this
;;; little function undoes that.

(defun unkeywordize-property-list (x)
  (cond
   ((consp x) 
    (cons (unkeywordize-property-list (car x)) 
          (unkeywordize-property-list (cdr x))))
   ((or (null x) (not (symbolp x))) x)
   ((eq x :t) t)
   ((eq x :nil) nil)
   ((keywordp x) x)
   (t (error "Internal error!  Symbol not read as keyword! ~A" x))
   ))

(defun plist-to-plist-string (plist)
  (with-standard-io-syntax
    (let ((*print-readably* t)) (formatn "~S" plist))))


