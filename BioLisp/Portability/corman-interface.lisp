;;; -*- Package: user; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :cl-user)

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

;;; In Corman Lisp, *default-pathname-defaults* indicates the
;;; Corman installation directory.  So this should work.  If
;;; the file we want isn't there, blow up and tell user to find it.

(defparameter *corman-internet-file-path* "examples/internet.lisp")

(unless (probe-file *corman-internet-file-path*)
  (error "The file '~A' doesn't seem to exist.  You need to find it ~
          and change *CORMAN-INTERNET-FILE-PATH* appropriately."
	 ))

(load *corman-internet-file-path*)

;;; The internet.lisp file doesn't have this interface, which the
;;; Miscrosoft documentation for the other functions says should
;;; be called when done with the handles.

(in-package :win32)

#! (:library "WinInet" :export t :pascal "WINAPI")
BOOL WINAPI InternetCloseHandle (HINTERNET hFile);
!#

;;; Web page data is apparently kept in a cache.
;;; We have to tell it not to use the cache.

(defconstant INTERNET_FLAG_RELOAD #x80000000)

(in-package :cl-user)

(defmacro with-open-web-page 
    ((stream-symbol url &optional (port 80)) &body body)
  (let ((web-page-string-symbol (gensym "WEB-PAGE-STRING-")))
    `(let ((,web-page-string-symbol (web-page-contents ,url :port ,port)))
       (with-input-from-string (,stream-symbol ,web-page-string-symbol)
	 ,@body
	 )))) 

(defun web-page-contents (url &key (port 80))
  (declare (ignore port))

  (unless (stringp url) 
    (error "URL argument to WEB-PAGE-CONTENTS must be a string."))

  ;; The Microsoft documentation for InternetOpenUrl says
  ;; 'Only URLs beginning with ftp:, http:, or https: are supported.'

  (flet ((if-zero (x) (and (numberp x) (zerop x))))
    (unless (or (if-zero (search "http:" url))
		(if-zero (search "https:" url))
		(if-zero (search "ftp:" url)))
      (error 
       "Invalid URL '~A'.~% ~A" url 
       ";; The URL must begin with 'http:', 'https:' or 'ftp:'"
       )))
  
  (let ((ihandle
	 (win32::InternetOpen 
	  (ct:create-c-string "cormanlisp")
	  win32::INTERNET_OPEN_TYPE_PRECONFIG
	  ct:null
	  ct:null
	  0)))
    (when (ct:cpointer-null ihandle)
      (error "The WINAPI function 'InternetOpen' failed, returning NULL"))
      ;; Remember to later check that url begins with accepted prefixes
    (unwind-protect
	(let ((urlhandle
	       (win32::InternetOpenUrl
		ihandle (ct:create-c-string url) 
		ct:null 0 (logior win32::INTERNET_FLAG_RELOAD) 0)))
	   (when (ct:cpointer-null urlhandle)
	    (error
	     (format 
	      nil "~A '~A'. ~A~%"
	      "Could not open URL" url
	      "The WINAPI function InternetOpenUrl returned NULL.")))
	  (unwind-protect
	      (read-all-from-url-handle urlhandle)
	    (unless (win32::InternetCloseHandle urlhandle)
	      (warn 
	       (format
		nil "~A ~A"
		"Failed to close web connection."
		"The WINAPI function InternetCloseHandle returned FALSE."
		)))))
      (unless (win32::InternetCloseHandle ihandle)
	(warn
	 (format 
	  nil "~A ~A"
	  "Failed to close internet handle returned from InternetOpen."
	  "The WINAPI function Internet CloseHandle returned FALSE"
	  ))))))

(defparameter *wpc-cbuffer-current-size* 0)
(defvar *wpc-cbuffer-requested-size* 4096)
(defvar *wpc-cbuffer* nil)

(defun maybe-allocate-wpc-cbuffer ()
  ;; Allocate the cbuffer if not allocated,
  ;; or reallocate if we want to make it bigger.
  (when (> *wpc-cbuffer-requested-size* *wpc-cbuffer-current-size*)
    (unless (>= *wpc-cbuffer-requested-size* 1024)
      (error "Buffer size must be at least 1024"))
    (setq *wpc-cbuffer* (ct:malloc *wpc-cbuffer-requested-size*))
    (setq *wpc-cbuffer-current-size* *wpc-cbuffer-requested-size*))
  *wpc-cbuffer-current-size*
  )

(defun internet-read-file-error (bytes-requested)
  (error "~A~%~A~%~A~%~A~%~A~%~A~%"
	 "Attempt to read from URL handle failed."
	 "The WINAPI function InternetReadFile returned FALSE."
	 (format 
	  nil
	  "This could mean a line of more than ~D bytes was being"
	  bytes-requested)
	 "downloaded, and would not fit into the buffer.  Try doing"
	 (format 
	  nil
	  "(setq cl-user::*wpc-cubuffer-requested-size ~D)"
	  (* 2 *wpc-cbuffer-current-size*))
	 "and then rerunning your program."
	 ))

(defun read-all-from-url-handle (url-handle)

  (maybe-allocate-wpc-cbuffer)

  (let ((buffer-strings nil)
	(bytes-requested (* 1000 (floor *wpc-cbuffer-current-size* 1000)))
	(bytes-read-cptr (ct:malloc (ct:sizeof '(win32::DWORD *)))))
    ;; Until we hit EOF, which is defined as *bytes-read-cptr == 0.
    ;; (See the Microsoft documentation for InternetReadFile)
    (loop
      (let ((result 
	     (win32::InternetReadFile 
	      url-handle *wpc-cbuffer* bytes-requested bytes-read-cptr)))
	(unless result (internet-read-file-error bytes-requested))
	;; Get how many bytes were actually read.
	(let ((n (ct:cref (win32::DWORD *) bytes-read-cptr 0)))
	  ;; All done.
	  (when (zerop n)
	    (return (apply #'concatenate 'string (reverse buffer-strings))))
	  ;; Null terminate the buffer and create a lisp string from it.
	  (setf (ct:cref (win32::char *) *wpc-cbuffer* n) 0)
	  (push (ct:c-string-to-lisp-string *wpc-cbuffer*) buffer-strings)
	  )))))


(in-package :bio)

(import '(cl-user::web-page-contents cl-user::with-open-web-page))

(defun generic-copy-file (from to)
  (declare (ignore from to))
  (error "Don't know how to copy file in Corman!"))

