;; -*- Package: help; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :help)

;;; +=========================================================================+
;;; | Copyright (c) 2002-2006 JP Massar, Jeff Shrager, Peter Seibel           |
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

;;; Authors:  JP Massar, Peter Seibel.

;;; Page for describing a tutorial

(publish 
 :path *help-tutorial-url* 
 :content-type cl-user::*html-publish-content-type* 
 :function
 (lambda (req ent)
   (let* ((input (request-query req))
          (package-name (url-parameter-value :pkg input))
          (package-symbol (keywordize package-name))
          (name (url-parameter-value :name input))
          (docobj (help::find-documentation name 'help::tutorial))
          )
     (execute-with-standard-weblistener-environment
      req ent package-symbol
      (lambda ()
        (com.gigamonkeys.foo:with-html-output ((request-reply-stream req))
          (html-for-tutorial-description name docobj))
        )))))

(defun html-for-tutorial-description (name doc-item)
  (if (null doc-item)
      (oops-no-docobj name "Tutorial")
    (with-slots (name description text keywords see-also referred-to-by
                      user-mode author)
        doc-item
      (let ((title (formatn "'~A' Tutorial" name)))
        (with-standard-weblistener-page-header
            (title :listener? nil)
          (html 
           (html-for-summary-information description)
           :p
           (:big (:b "Available: "))
           (:princ-safe
            (cond
             ((member :all user-mode) "everywhere")
             (t
              (cond
               ((member :biolisp user-mode) "in BioLisp mode only")
               ((member :bbl user-mode) "BBL mode only")
               (t "?")
               ))))
           (html-for-text-information text)
           (:h3 (:b "Using this Tutorial:"))
           :p
           (:princ-safe 
            "To work through this live tutorial, go to the main Weblistener "
            "page, make sure you are in the right mode, find the ")
           ((:font :color "green") "Help Menu")
           (:princ-safe " (bottom center) and select ")
           ((:font :color "green") "Live tutorials. ")
           (:princ-safe "Then click on the link that appears for this tutorial.")
           (html-for-keyword-information keywords)
           (html-for-see-also-information see-also)
           (html-for-referred-to-by-information referred-to-by)
           (html-for-author-information author)
           ))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; LIST-LIVE-TUTORIALS


(defstruct live-tutorial-info listings)

(defun tutorial-hash () (gethash 'help:tutorial help::*documentation*))
(defun find-tutorial (name) (gethash name (tutorial-hash)))

(defun list-live-tutorials (&key forced-user-mode)
  #.(one-string-nl
     "Display a table of available live tutorials."
     "Each tutorial name is a link which starts up the tutorial."
     "If the tutorial has a description it is printed out along with the name."
     "Those that have sort-orders come first, followed by an"
     "alphabetical listing of those without sort orders."
     )
  ;; Create the object that will get printed out below,
  ;; showing the appropriate tutorials and links to their start pages.
  (make-live-tutorial-info 
   :listings
   (let ((user-mode 
          (or forced-user-mode (forward-funcall 'wb::user-mode cl-user:*ai*))))
     (sort 
      ;; Only offer those tutorials that you're supposed to be able to
      ;; see in the current user mode.
      (lmaphashnn 
       (lambda (key obj) 
         (declare (ignore key))
         (let* ((target-modes (ensure-list (help:user-mode obj))))
           (when (or (member :all target-modes) (member user-mode target-modes))
             obj)))
       (tutorial-hash))
      'string<
      :key (lambda (obj) (help:sort-order obj))
      ))))

;; Print out all the tutorials in the LIVE-TUTORIAL-INFO structure,
;; with a link for each one to start it.
(defmethod wb::out-record-to-html
           ((form live-tutorial-info) (string string) &rest ignore)
  (declare (ignore ignore))
  (html 
   (:table 
    (:tr 
     ((:td :align "left") (:b "Name"))
     ((:td :align "left") (:b "&nbsp;&nbsp;Description")))
    (loop for tutorial in (live-tutorial-info-listings form)
	  as name = (help:name tutorial)
          as section-header = (help:section-header tutorial)
          as title = (first section-header)
          as color = (second section-header)
          as description = (or (help:description tutorial) "")
          do 
          (if section-header 
              (html 
               (:tr 
                ((:td :align "left") 
                 ((:font :color color) (:b (:princ-safe title))))
                ((:td :align "left") "&nbsp;&nbsp;"
                 ((:font :color color)
                  (:b (:princ-safe (limited-string description 80)))))))
            (html 
             (:tr 
              ((:td :align "left") 
               ((:a :href (make-begin-live-tutorial-url name tutorial)
                 :target "_blank")
                (:princ-safe name)))
              ((:td :align "left") 
               "&nbsp;&nbsp;"
               (:princ-safe (limited-string description 80)))
              )))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Invocation of a particular live tutorial


;;; URLs for live-tutorial invocation 

(wb::define-url&pkg&args 
 live-tutorial-html-file-url 
 "/live-tutorial-html-file" :tutorial-name)

(wb::define-url&pkg&args
 live-tutorial-lhtml-file-url 
 "/live-tutorial-lhtml-file" :tutorial-name)

(defun make-begin-live-tutorial-url (name tutorial) 
  (let ((file-type (help:file-type tutorial)))
    (make-live-tutorial-url file-type (url-safe-string name))
    ))

(defmethod make-live-tutorial-url ((file-type (eql :html)) name) 
  (make-live-tutorial-html-file-url :tutorial-name name))

(defmethod make-live-tutorial-url ((file-type (eql :lhtml)) name) 
  (make-live-tutorial-lhtml-file-url :tutorial-name name))

;;; Implementation of live-tutorial invocation URL's

(publish
 :path *live-tutorial-lhtml-file-url*  
 :content-type cl-user::*html-publish-content-type* 
 :function
 (lambda (req ent) 
   (let* ((input (request-query req))
          (package-name (url-parameter-value :pkg input))
          (package-symbol (keywordize package-name))
          (tutorial-name (url-parameter-value :tutorial-name input))
          )
     (wb::execute-with-standard-weblistener-environment
      req ent package-symbol
      (lambda () 
        (html-for-live-lhtml-tutorial package-symbol tutorial-name)
        )))))

(publish
 :path *live-tutorial-html-file-url* 
 :content-type cl-user::*html-publish-content-type*  
 :function 
 (lambda (req ent) 
  (let* ((input (request-query req))
         (package-name (url-parameter-value :pkg input))
         (package-symbol (keywordize package-name))
         (tutorial-name (url-parameter-value :tutorial-name input))
         )
    (wb::execute-with-standard-weblistener-environment
     req ent package-symbol
     (lambda () 
       (html-for-live-html-tutorial package-symbol tutorial-name)
       )))))

(defun html-for-live-lhtml-tutorial (package tutorial-name)
  (declare (ignore package))
  (let* ((tutorial (find-tutorial tutorial-name))
         (filepath (help:filename tutorial))
         (lhtml-function (help:lhtml-function tutorial))
         (start-function (help:start-function tutorial)))
    (load filepath)
    (funcall lhtml-function start-function t) 
    ))

(defun html-for-live-html-tutorial (package tutorial-name)
  (declare (ignore package))
  (let* ((tutorial (find-tutorial tutorial-name))
         (filepath (help:filename tutorial))
	 (directory (directory-namestring filepath)))
    ;; Everything (including, hopefully, esp. images) on the
    ;; directory is published under the name of the tutorial
    ;; so that the tutorial can refer to it. Note that this
    ;; uses only the tutorial's pathname-name, so, for a tutorial
    ;; called "c:/foo/bar/baz.html" the tutorial author only uses: 
    ;; /livetutorial-files/baz/myimage.jpg internally.
    (net.aserve:publish-directory
     :prefix (formatn "/livetutorial-files/~a/" (pathname-name filepath))
     :destination (namestring directory))
    (if (null (probe-file filepath))
        (oops-no-tutorial-file filepath)
      (html
       (:princ (make-javascript-for-live-tutorial))
       :newline
       (flet ((>>line? (line)
                (and (> (length line) 2)
                     (string-equal (subseq line 0 2) ">>"))))
         (let* ((lines (file-to-string-list filepath)))
           (loop 
            until (null lines)
            as line = (first lines)
            do 
            (if (not (>>line? line))
                (progn 
                  (pop lines)
                  (html (:princ line) :newline))
              (let* ((>>lines 
                      (loop for restlines on lines
                            as line = (first restlines)
                            until (not (>>line? line))
                            do (pop lines)
                            collect line))
                     (code (mapcar (lambda (line) (subseq line 2)) >>lines))
                     ;; make sure we preserve the newlines 
                     (expression-string
                      (if (= (length code) 1)
                          (first code)
                        (apply 
                         's+
                         (mapcar (lambda (line) (s+ line #\Newline)) code)
                         ))))
                (html
                 ((:table :BORDER 4)
                  (:tr
                   (:td
                    (:pre
                     (loop for line in >>lines
                           do (html (:princ-safe line) :br)))))
                  (:tr
                   (:td
                    (html 
                     (:princ 
                      (html-for-button-to-execute-lisp-expression 
                       (replace-newlines-for-url-with
                        (string-trim *whitespace* expression-string) 
                        "@@"
                        )))))))))))))))))

(defun oops-no-tutorial-file (filepath)
  (html
   (:big 
    (:b
     (:center
      :br
      ((:font :color "red")
       (:princ-safe 
        (formatn "Tutorial HTML file ~A" (namestring filepath))))
      :br
      (:princ-safe "does not exist (or cannot be accessed) !!!")
      (:princ-safe "Please inform the system administrators.")
      )))))

(defun make-javascript-for-live-tutorial ()
  (one-string
   (wb::javascript-header)
   (wb::javascript-opener-display-new-url :name "weblistenerexecute")
   (wb::javascript-trailer)
   ))

(defun html-for-button-to-execute-lisp-expression 
       (expression-as-string &key (button-label "Do It"))
  (formatn
   (one-string 
    "<input type=BUTTON value=\"~A\""  
    "onclick=weblistenerexecute"
    "(\"/weblistener-eval-tutorial-string.html?PKG=~a&evalstring=~a\")>") 
   button-label wb:*sessionid* (url-safe-string expression-as-string)))

;;; This can be inserted into LHTML code, but is otherwise not used.
(defun tutorial-instructions (startup-function live?)
  (if live?
      (html 
       ((:font :color "brown" :size 2)
        (:princ-safe
         (one-string-nl
          "Notice that this Window popped up separate from the Weblistener..."
          "That's entirely intentional.  When you click the \"Do It\" button"
          "next to any example on this page, the example will be executed in"
          "the Weblistener window, as if you had typed it in yourself and hit"
          "[Enter]."
          ))))
    (html
     ((:font :color "brown" :size 2)
      (:princ-safe 
       (one-string-nl 
        "It's possible to have this tutorial 'come alive' in the sense"
        "that instead of having to cut and paste the example code"
        "herein into the Weblistener to see it execute, you can have this"
        "tutorial come up with buttons associated with each example, so that"
        "when you hit the button, the code is automatically transferred"
        "to the Weblistener and executed.  Of course, this assumes you have"
        "a Weblistener running.  To get one running you can use the"
        "Demo server which you can reach"))
      ((:a :href "http://nostoc.stanford.edu:8003/biologin") "here")
      (:princ-safe
       (formatn
        (one-string-nl
         ".  Once you are logged in type in"
         "(~A) into the small box and hit [Enter].")
        startup-function))))))
  
(defun replace-newlines-for-url-with (string with)
  (when (search with string)
    (error "String ~S contains the newline replacement string ~S!"
           string with))
  (string-join (string-split string #\Newline) with))

(defun replace-something-with-newlines (string something)
  (let ((s "") (pos 0) (nl (string #\Newline)))
    (loop as found-pos = 
          (search something string :test 'char-equal :start2 pos)
          until (null found-pos)
          do 
          (setq s (one-string s (subseq string pos found-pos) nl))
          (setq pos (+ found-pos (length something)))
          finally
          (setq s (one-string s (subseq string pos)))
          )
    s))



