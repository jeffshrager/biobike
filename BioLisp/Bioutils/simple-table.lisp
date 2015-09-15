;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: bioutils; -*-

(in-package :bioutils)

;;; +=========================================================================+
;;; | Copyright (c) 2002-2006 by The BioBike Team                        s    |
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

(defun simple-read-table 
       (fn &key (delimiter #\tab) (try-to-transform-numbers? t))
  #.(one-string-nl
     "Read a tab delimited table into a list of lists. The optional"
     "keyword argument :delimeter changes the "
     "delimiter from tab (the default) to any other simple character."
     "Example: (simple-read-table \"myfile\" :delimiter #\space)"
     "By default, this function tries to guess whether the entries are"
     "numerical, and to convert them if possible.  You"
     "can supress this behavior by adding \":try-to-transform-numbers? nil\""
     )
  (with-open-file (f fn :direction :input)
    (loop for line = (read-line f nil nil)
          until (null line)
          collect
          (loop for item in (string-split line delimiter)
                as number = 
                (and try-to-transform-numbers?
                     (ignore-errors 
                       (let ((n (read-from-string item)))
                         (if (numberp n) n nil))))
                collect (or number item)
                ))))

(defun simple-write-table 
       (file-name list 
                  &key 
                  (format-string "~A")
                  (print-function nil)
                  (delimiter #\Tab)
                  (if-exists :error))
  #.(one-string-nl
     "(SIMPLE-WRITE-TABLE file-name list-to-output)"
     "Converts LIST to a tab-delimited file FILE-NAME."
     "The resulting file can be downloaded and then uploaded into,"
     "for example, Excel."
     "Elements at the top level of the list are treated as separate lines."
     "Elements at the second level are separated by :DELIMITER (default Tab)."
     "If :PRINT-FUNCTION is non-nil it should be a function of two arguments,"
     "an ELEMENT and a STREAM.  It should write ELEMENT out to STREAM."
     "If :PRINT-FUNCTION is nil then :FORMAT-STRING (default \"~A\") is used"
     "with FORMAT to write the element to the file."
     "By default :IF-EXISTS is :ERROR, so that you won't accidentally overwrite"
     "an existing file. Often you'll want to say :IF-EXISTS :SUPERSEDE"
     "which will cause existing files to be silently replaced without errors."
     "(other :IF-EXISTS options are the same as with the"
     "Lisp OPEN and WITH-OPEN-FILE functions.)"
     "The result of SIMPLE-WRITE-TABLE is the number of"
     "lines written to the table."
     ""
     "- EXAMPLE:"
     "; blast-hits is the following list:"
     "; ((\"gene1\" 2E-47 34 339)(\"gene2\" 4E-29 98 240)"
     "(\"gene3\" 1E-20 (10 50) (80 230)))"
     "(SIMPLE-WRITE-TABLE \"blast-hits.txt\" blast-hits)"
     "==> \"gene1\" <tab> 2E-47 <tab> 34 <tab> 339"
     "\"gene2\" <tab> 4E-29 <tab> 98 <tab> 240"
     "\"gene3\" <tab> 1E-20 <tab> (10 50) <tab> (80 230)")
  (with-open-file (stream file-name :if-does-not-exist :create
                          :if-exists if-exists :direction :output)
    (loop for line-element-list in list 
          as nlines from 1 by 1
	  do
          (loop for item in line-element-list do
                (if print-function
                    (funcall print-function item stream)
                  (format stream format-string item))
                (format stream "~A" delimiter))
          (terpri stream)
	  finally (return nlines)
          )))
