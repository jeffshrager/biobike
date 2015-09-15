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

;;; Author:  JP Massar.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :aserve))

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defparameter *utility-html-api-symbols*
    '(
      url-safe-string
      x-url-parameter-value
      url-parameter-value
      url-and-args
      html-for-indirect-to-url
      javascript-header
      javascript-trailer
      javascript-opener-display-new-url
      *transitional-html-doctype-header*
      list-to-html-table
      ))

  (export *utility-html-api-symbols* (find-package :utils)))


;;; Escape certain special characters for URLs
;;; see http://www.w3.org/Addressing/URL/5_BNF.html
;;; Removed '&' -- JP.  Removed '+' -- MT
(defvar *url-safe-chars* "$-_@.-")

(defvar *transitional-html-doctype-header*
  "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">"
  )

(defun url-safe-string (instring)
  "Return a version of INSTRING which is safe to embed in a URL"
  (flet ((next-bad-char (start)
	   (position 
            nil instring :start start 
            :test-not 
            #'(lambda (ignore char)
                (declare (ignore ignore))
                (or (alphanumericp char)
                    (find char *url-safe-chars* :test #'char=))))))
    (do ((badfinger (next-bad-char 0) (next-bad-char (+ 1 badfinger)))
	 (lastbad 0 (+ 1 badfinger))
	 (strings nil))
	((null badfinger)
	 (push (subseq instring lastbad nil) strings)
	 (string-join (nreverse strings) ""))
      (push (subseq instring lastbad badfinger) strings)
      (push (formatn "%~X" (char-code (char instring badfinger))) strings))))


(defun x-url-parameter-value
       (key 
        url-alist
        &key
        (test 'string-equal)
        (nilstring-to-nil? t)
        (tstring-to-t? nil)
        (when-not-present nil)
        )
  #.(one-string-nl
     "Returns the value for KEY found in URL-ALIST, an association"
     "list of string keys and string values, such as is returned by"
     "the AllegroServe function REQUEST-QUERY (which parses URL parameter"
     "lists of the form 'KEY1=VALUE1&KEY2=VALUE2&...' into an assoc list)."
     "KEY may be either a string or a symbol. If a symbol, its SYMBOL-NAME"
     "is used. TEST should be either STRING-EQUAL (the default) or STRING=, "
     "determining whether the test for KEY is case-sensitive or not."
     "The value of WHEN-NOT-PRESENT (default NIL) determines what is returned"
     "if KEY cannot be found."
     "If NILSTRING-TO-NIL? is T (the default), if a KEY's value is the string"
     "'NIL' (in any case) or the string '()', the Lisp value NIL is returned"
     "instead of the string."
     "If TSTRING-TO-T? is T (default NIL), and a KEY's value isthe string 'T'"
     "(in any case), the Lisp value T is returnd instead of the string.")
  (let ((value (cdr (assoc (string key) url-alist :test test))))
    (cond
     ((null value) when-not-present)
     ((and nilstring-to-nil? 
           (or (string-equal value "nil") (string= value "()")))
      nil)
     ((and tstring-to-t? (string-equal value "t")) t)
     (t value)
     )))

(defun url-parameter-value (key url-alist &optional (nilstring-to-nil? t))
  #.(one-string-nl
     "Returns the value associated with KEY in URL-ALIST, an assoc"
     "list of string keys and string values, such as is returned by"
     "the AllegroServe function REQUEST-QUERY (which parses URL parameter"
     "lists of the form 'KEY1=VALUE1&KEY2=VALUE2&...' into assoc lists)."
     "KEY may be either a string or a symbol (the SYMBOL-NAME is used)."
     "The search is case-insensitive."
     "If NILSTRING-TO-NIL? is T (the default), if a KEY's value is the string"
     "'NIL' (in any case), the Lisp value NIL is returned instead of a string."
     )
  (let ((value (cdr (assoc (string key) url-alist :test #'equalp))))
    (cond
     ((null value) value)
     ((and nilstring-to-nil? (string-equal value "nil")) nil)
     (t value)
     )))

(defun url-and-args (urlstring &rest keyword-value-pairs)
  #.(one-string-nl
     "Constructs a URL from a base (URLSTRING) and a list of all the arguments"
     "and their values. Each VALUE can be any Lisp object, and it is turned"
     "into a string representation using FORMAT with \"~S\".  The exceptions"
     "are that string VALUES are left alone, and symbols use their symbol name"
     "(i.e., neither package information nor 'keywordness' is preserved)."
     "Once values are in string form they are all run through URL-SAFE-STRING."
     "Example:"
     "(url-and-args \"/foo\" :bar 3 :baz :xyzzy :quux 'a :description \"a toy\")"
     "--> \"/foo?BAR=3&BAZ=XYZZY&QUUX=A&DESCRIPTION=a%20toy\""
     )
  (one-string
   urlstring
   "?"
   (string-join
    (loop for key in keyword-value-pairs by #'cddr
        for value in (cdr keyword-value-pairs) by #'cddr
        collect 
        (one-string 
         (string key)
         "="
         (url-safe-string 
          (cond
           ((stringp value) value)
           ((symbolp value) (symbol-name value))
           (t (formatn "~S" value))
           ))))
    "&"
    )))

(defun html-for-indirect-to-url (url-string)
  #.(one-string-nl
     "Creates a string which, when interpreted by a browser, will cause"
     "the browser to open the URL defined by URL-STRING.")
  (one-string-nl
   #.(one-string-nl
      "<html>" 
      "<head>" 
      "<script type=\"text/javascript\">" 
      "function locate()" 
      "{" )
   (formatn "location=~S" url-string)
   #.(one-string-nl
      "}"
      "</script>"
      "</head>"
      "<body>"
      "<script type=\"text/javascript\">"
      "locate();"
      "</script>"
      "</body>"
      "</html>"
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun javascript-header ()
  #.(one-string-nl 
     "Creates the HTML needed to begin entering Javascript code.")
  #.(one-string-nl
     "<script language=\"JavaScript\">"
     "<!--"
     ))

(defun javascript-trailer ()
  #.(one-string-nl 
     "Creates the HTML needed to terminate entering of Javascript code.")
  #.(one-string-nl
     "// -->"
     "</script> "))

(defun javascript-opener-display-new-url (&key (name "openerdisplaynewurl"))
  #.(one-string-nl
     "Creates a javascript function which, when called, causes the opener"
     "of the current window to display a local URL without manual intervention."
     "If the window has no opener (that is, there was no other browser window"
     "that this window was spawned from) then the behavior is undefined. "
     "Presumably, the Javascript just errors out and nothing bad happens. ")
  (one-string-nl
   (formatn "function ~A(url) {" name)
   ;;   "   alert(window.location.host);"
   ;;   "   alert(window.location.port);"
   "  window.opener.location= 'http://' + window.location.host + url;"
   "}"
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#||

STRING
you're either going to pass a list of lists, 
a list with ncols

FILE
STREAM

||#
#||

(defun list-to-html-table 
       (list 
        &key
        (to-file? nil)
        (to-stream? nil)
        (to-lispscript? nil)
        (ncols nil)
        (add-html? t)
        (add-body? t)
        (border 0)
        (cellpadding 0)
        (cellspacing 0)
        (tralign "center")
        (tdalign "center")
        (certain-element nil))
  (unless (or (every 'listp list) ncols)
    (error "You must either pass a list of lists or specify the ncols keyword"))
  (let ((group (if (every 'listp list) list (group-into-rows list ncols))))
    (cond 
     (to-file? 
      (list-to-html-table-file 
       group to-file?
       :border border :cellpadding cellpadding :cellspacing cellspacing
       :tralign tralign :tdalign tdalign
       :add-html? add-html? :add-body? add-body? 
       :certain-element certain-element
       ))
     (to-stream?
      (list-to-html-table-stream
       group to-stream?
       :border border :cellpadding cellpadding :cellspacing cellspacing
       :tralign tralign :tdalign tdalign
       :add-html? add-html? :add-body? add-body? 
       :certain-element certain-element
       ))
     (to-lispscript?
      (list-to-html-table-lispscript
       group
       :border border :cellpadding cellpadding :cellspacing cellspacing
       :tralign tralign :tdalign tdalign
       :add-html? add-html? :add-body? add-body?
       :certain-element certain-element
       ))
     (t
      (list-of-lists-to-html-table 
       group
       :border border :cellpadding cellpadding :cellspacing cellspacing
       :tralign tralign :tdalign tdalign
       :add-html? add-html? :add-body? add-body?
       :certain-element certain-element
       )))))

(defun list-to-html-table-file  
       (list to-file
             &key
             add-html? add-body? border 
             cellpadding cellspacing tralign tdalign certain-element
             )
  (with-open-file 
      (p to-file
         :direction :output :if-does-not-exist :create :if-exists :supersede)
    (list-to-html-table-stream 
     list p :add-html? add-html? 
     :add-body? add-body? :border border :cellpadding cellpadding
     :cellspacing cellspacing :tralign tralign :tdalign tdalign
     )))

(defun list-to-html-table-stream
       (list to-stream
             &key
             add-html? add-body? border 
             cellpadding cellspacing tralign tdalign certain-element
             )
  (flet ((output (ce) (format to-stream "~A" column-entry)))
    (when add-html? (format to-stream "<html>~%"))
    (when add-body? (format to-stream "<body>~%"))
    (format to-stream "<table border=~S cellpadding=~S cellspacing=~S>~%"
            border cellpadding cellspacing)
    (loop for row-entry in list
      do
      (format to-stream "<tr align=~A>~%" tralign)
      (loop for column-entry in row-entry
        do
        (format to-stream "<td align=~A>" tdalign)
        (when (and (= (first certain-element row-entry))
                   (= (second certain-element row-entry)))
          (format to-stream (third certain-element)))
        (output)
        (when (and (= (first certain-element row-entry))
                   (= (second certain-element row-entry)))
          (format to-stream (fourth certain-element)))
        (format to-stream "</td>~%"))
      (format to-stream "</tr>~%")
      )
    (format to-stream "</table>")
    (when add-html? (format to-stream "</html>~%"))
    (when add-body? (format to-stream "</body>~%"))
    ))

(defun list-to-html-table-lispscript 
       (list 
        &key border cellpadding cellspacing
        tralign tdalign add-html? add-body? certain-element)
  (net.aserve::html
   ((:table :border border :cellpadding cellpadding :cellspacing cellspacing)
    (loop for row-entry in list
          do
          (net.aserve::html 
           ((:tr :tralign tralign)
            (loop for column-entry in row-entry
                  do
                  (net.aserve::html
                   ((:td :tdalign tdalign) 
                    (:princ-safe (formatn "~A" column-entry)))
                   ))))))))

(defun list-of-lists-to-html-table 
       (list
        &key
        add-html? add-body? border cellpadding
        cellspacing tralign tdalign certain-element
        )
  (with-output-to-string (p)
    (list-to-html-table-stream 
     list p :add-html? add-html? 
     :add-body? add-body? :border border :cellpadding cellpadding
     :cellspacing cellspacing :tralign tralign :tdalign tdalign
     )))

(defun group-into-rows (list ncols)
  (let ((result nil))
    (loop while list
          do
          (if (<= (length list) ncols)
              (progn
                (push list result)
                (setq list nil))
            (progn
              (push (subseq list 0 ncols) result)
              (setq list (subseq list ncols))
              )))
    (reverse result)
    ))


||#


#||

we have output to a string
we also want to output to a file
and to a stream

pre/post html for either all elements 
OR
for a particular element, which will require specifying which row/column

||#