;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: weblistener; -*-

(in-package :weblistener)

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
       
;;; DEFINE-URL&ARGS is in the utilities.

(defmacro define-url&pkg&args (name path &rest args)
  `(define-url&args 
    ,name ,path (:pkg "~A" (forward-funcall 'user-session-id)) ,@args))

;;; URL's for the inner workings of the Web Lisp Listener.

(define-url&pkg&args 
 weblistener-oneline-form-response-url
 "/weblistener-oneline-form-response.html" :oneline)
(define-url&pkg&args 
 weblistener-multiline-form-response-url
 "/weblistener-multiline-form-response.html" :multiline)
(define-url&pkg&args
 weblistener-evalstring-form-response-url
 "/weblistener-evalstring-form-response.html" :evalstring :formid)
(define-url&pkg&args
 weblistener-evalstring-url
 "/weblistener-evalstring.html" :evalstring)
(define-url&pkg&args
 weblistener-eval-tutorial-string-url
 "/weblistener-eval-tutorial-string.html" :evalstring)

(define-url&pkg&args redisplay-url "/redisplay.html" :uid)
(define-url&pkg&args clearhack-url "/clearhack.html")
(define-url&pkg&args clear-history-url "/clearhistory.html")
(define-url&pkg&args ml-reformat-url "/ml-reformat.html")

(define-url&pkg&args history-url "/history.html" :which (:history "~D"))
(define-url&pkg&args in-history-url "/inhistory.html" :which (:history "~D"))

(define-url&pkg&args new-history-url 
                     "/new-history.html" (:history "~D") :value)
(define-url&pkg&args out-history-url 
                     "/out-history.html" (:history "~D") :value)

;;; Auxiliary URL's for additional functionality
;;; (upload, frame display, source listings, logs).

(define-url&pkg&args weblistener-frames-url "/frame" :name)
(define-url&pkg&args weblistener-frames-editor-url 
                     "/frame-editor" :name :edit :noshow)
(define-url&pkg&args weblistener-frame-find-url "/frame-find" :nstr :thorough)
(define-url&pkg&args weblistener-toplevel-doc-page-url 
                     "/weblistener-toplevel-doc-page.html")
(define-url&pkg&args application-primitives-url 
                     "/newdocprims" :mode :package :symbol :state)
(define-url&pkg&args doc-alpha-listing-url "/docalpha")
(define-url&pkg&args doc-find-function-source-url 
                     "/find-source" :package :symbol)
(define-url&pkg&args function-source-to-edit 
                     "/function-source-to-edit.html" :package :symbol)
(define-url&pkg&args doc-find-function-file-source-url 
                     "/find-source-file" :package :symbol)
(define-url&pkg&args doc-find-variable-source-url 
                     "/find-variable" :package :symbol)

;;; New-style help pages.
(define-url&pkg&args help-function-url "/help/function" :package :symbol)

;;; New help facility URLs

(define-url&pkg&args previous-sessions-url "/previous-sessions-index")
(define-url&pkg&args previous-system-logs-url "/previous-system-logs-index")
(define-url&pkg&args log-viewer-url "/log-viewer" :file :mode)

(define-url&pkg&args guru-previous-sessions-url 
                     "/guru-previous-sessions-index" :user)

;;; URL's for the various computed pages related to directory listings.

(define-url&pkg&args weblistener-toplevel-directories-url 
                     "/biolingua-toplevel-directories")
(define-url&pkg&args 
 wb-directory-listing-url "/wb-directory-listing" :pathname)
(define-url&pkg&args 
 wb-lisp-file-listing-url "/wb-lisp-file-listing" :pathname)
(define-url&pkg&args 
 wb-up-directory-url "/wb-up-directory" :pathname)
(define-url&pkg&args 
 wb-delete-file-url "/wb-delete-file" :pathname)

;;; URL's for various computed pages related to the editor

(define-url&pkg&args
 weblistener-new-editor-form-response-url
 "/weblistener-new-editor-form-response.html")

(define-url&pkg&args 
 editfunction-url
 "/editfunction.html" :function :package :tempfile)

(define-url&pkg&args 
 editanyfile-url
 "/editanyfile.html" :filepath :create :supersede :initialcontents)

(define-url&pkg&args
 editor-new-file-or-directory-form-url
 "/editnewfileordir.html" :currentdir)

(define-url&pkg&args
 editor-new-file-or-directory-form-response-url
 "/editnewfileordirresponse.html")

(define-url&pkg&args
 edit-previous-url 
 "/editprevious.html")

(define-url&pkg&args
 edit-all-previous-url 
 "/editallprevious.html")


                 
;;; Web widget URL's
 
(define-url&pkg&args web-widget-index-url "/web-widget-index")

;;; Upload facility URL's

(define-url&pkg&args upload-form-url "/weblistener-getfile")
(define-url&pkg&args upload-form-response-url "/weblistener-getfile-response")

;;; Feedback form url

(define-url&pkg&args feedback-form-url "/feedback-form")
(define-url&pkg&args feedback-form-response-url "/feedback-form-response")


;;; URLs for prefs.

(define-url&pkg&args prefs-gui-url "/prefs-gui")


;;; URLs for VPL

(define-url&pkg&args vpl-start-url "ajax/vpl.html")
(define-url&pkg&args 
 vpl-refresh-execution-log "/vpl-refresh-execution-log" :file)


(define-url&pkg&args 
 sseqview-url "/sseqview-url" 
 org contig gene from to rows columns
 font-size gene-names start end next prev goto)
                     
(define-url&pkg&args
 sseqview-orgs-and-contigs-url
 "/sseqview-orgs-and-contigs-url"
 )

(define-url&pkg&args 
 vpl-share-package-url "/vpl-share-package-url"
 share-package
 )

(define-url&pkg&args 
 vpl-restore-session-url "/vpl-restore-session-url"
 session-name 
 )

(define-url&pkg&args
 annotation-modified-request-url
 "/annotation-modified-request-url"
 :seed-id)

(define-url&pkg&args 
 new-fangled-show-sessions-url
 "/new-fangled-show-sessions-url"
 :mode :sessionname 
 )

;; http://edwards.sdsu.edu:7005/annotation-modified-request-url?pkg=MASSAR68364&seed-id=fig%7c347326.2.peg.1