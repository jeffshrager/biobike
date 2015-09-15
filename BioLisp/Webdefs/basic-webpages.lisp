;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: wb; -*-

(in-package :frames)

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

(publish-file
 :path "/robots.txt"
 :file (namestring (cl-user:translate-simple-lp "websrc:Webdefs;robots.txt"))
 :content-type cl-user::*html-publish-content-type*
 )

(publish-file
 :path "/favicon.ico"
 :file (namestring (cl-user::translate-simple-lp "websrc:Doc;favicon.ico"))
 :content-type cl-user::*html-publish-content-type*
 )

(defparameter *toplevel-frames-url* "/toplevel-frames.html")

(defun make-toplevel-frames-url 
       (&key (name (forward-funcall 'wb::user-session-id)))
  (formatn "~A?PKG=~A" *toplevel-frames-url* (string name)))

(publish
 :path *toplevel-frames-url*
 :content-type cl-user::*html-publish-content-type*
 :function
 (lambda (req ent)
   (let* ((input (request-query req))
          (package-name (url-parameter-value :pkg input))
          (package-symbol (keywordize package-name))
          )
     (wb::execute-with-standard-weblistener-environment
      req ent package-symbol
      (lambda () (toplevel-frames-function))
      ))))


(defun toplevel-frames-function ()
  (let* ((title 
          (formatn "~A Toplevel Frames" (wb::application-name cl-user:*ai*))
          ))
    (wb::with-standard-weblistener-page-header (title)
      (html :p :hr :p)
      (let ((tops (wb::application-toplevel-frames cl-user:*ai*)))
        (loop for (category frames) in tops 
              as n = (length frames)
              do
              (setq frames (sort (copy-list frames) 'string-lessp :key #^fname))
              (html
               (:center 
                (:h3 (:b (:princ-safe 
                          (one-string (string category) " Frames")))))
               :p 
               (cond
                ((> n 200) (display-big-toplevel-frames-list frames))
                (t (display-small-toplevel-frames-list frames))
                )
               :p :br
               ))))))

(defun display-small-toplevel-frames-list (frames)
  (loop for f in frames 
        as fname = (#^Fname f) 
        as url =
        (forward-package-funcall :de :simple-frame-editor-url f)
        do
        (html ((:a :href url :target "_blank") 
               ((:font :size 3) (:princ-safe fname))) 
              "&nbsp;&nbsp;&nbsp;&nbsp;" " "
              )))

(defun display-big-toplevel-frames-list (sorted-frames)
  (let ((alphalists nil)
        (charlist nil))
    (flet ((firstchar (f) (char (#^fname f) 0)))
      (loop for flist on sorted-frames
            as first = (first flist)
            as second = (second flist)
            do
            (cond
             ((null second) 
              (push first charlist)
              (setq charlist (nreverse charlist))
              (push charlist alphalists)
              (setq alphalists (nreverse alphalists))
              (return)
              )
             ((not (char-equal (firstchar first) (firstchar second)))
              (push first charlist)
              (setq charlist (nreverse charlist))
              (push charlist alphalists)
              (setq charlist nil)
              )
             (t (push first charlist))
             )))
    (loop 
     for flist in alphalists
     do
     (display-small-toplevel-frames-list flist)
     (html :p :br)
     )))

