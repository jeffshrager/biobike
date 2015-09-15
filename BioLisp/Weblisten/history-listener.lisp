;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: WEBLISTENER; -*-

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

;;; This gets triggered when the user clicks on one of the history URL's
;;; (the things of the form '<n>>' and '::' preceding the input and
;;; output forms).

;;; It causes the form associated with the prompt the user just
;;; clicked on (i.e., the history URL) to be put
;;; back into one of the form text box
;;; areas so that the user can edit and/or reevaluate the form.


(publish 
 :path *history-url*
 :content-type cl-user::*html-publish-content-type*
 :function
 (lambda (req ent)
   (let* ((input (request-query req))
          (package-name (url-parameter-value :pkg input))
          (package-symbol (keywordize package-name))
          (which (url-parameter-value :which input))
          (history (url-parameter-value :history input)))
     (execute-with-standard-weblistener-environment
      req ent package-symbol
      (lambda () (weblistener-in-history-redisplay which history))
      ))))
     

(defun weblistener-in-history-redisplay (which history)

  ;; find the appropriate form string based on the WHICH and HISTORY

  (let* ((index (1+ (read-from-string history)))
         (formstring 
          (cond
           ((string= which "in") (history-input-string index))
           (t (error "Internal error.  Only handles IN HISTORY!"))
           )))

    ;; Determine which box the form should be displayed in based
    ;; on its size and whether it has newlines in it, then set
    ;; things up so that it will be displayed there.

    (if (and (< (length formstring) 80) (null (find #\Newline formstring)))
        (setq *oneline-form-data* formstring)
      (setq *multiline-form-data* formstring)
      )

    (html 
     (:princ 
      (indirect-to-redisplay (incf *user-display-id*) (user-session-id))))
    ))


(publish 
 :path *new-history-url*
 :content-type cl-user::*html-publish-content-type*
 :function
 (lambda (req ent)
   (let* ((input (request-query req))
          (package-name (url-parameter-value :pkg input))
          (package-symbol (keywordize package-name))
          (which (url-parameter-value :which input))
          (history-index-string (url-parameter-value :history input))
          (value-index-string (url-parameter-value :value input))
          )
     (execute-with-standard-weblistener-environment
      req ent package-symbol
      (lambda () 
        (weblistener-history-redisplay 
         which history-index-string value-index-string
         ))))))

(defun weblistener-history-redisplay 
       (which history-index-string value-index-string)

  ;; If VALUE-INDEX is NIL, then
  ;; find the appropriate form string based on the WHICH and HISTORY
  ;; If it is not NIL, it must be a number and WHICH must be "out", and
  ;; we snarf the string representing the nth value in the output.

  (let* ((hindex (1+ (read-from-string history-index-string)))
         (vindex (read-from-string value-index-string))
         (formstring 
          (cond
           ((plusp vindex)
            (unless (string= which "out") (error "Internal error."))
            (new-history-output-value-string hindex vindex))
           ((string= which "out") (new-history-output-string hindex))
           (t (error "Internal error.  Only handles OUT history."))
           )))

    ;; Determine which box the form should be displayed in based
    ;; on its size and whether it has newlines in it, then set
    ;; things up so that it will be displayed there.

    (if (and (< (length formstring) 80) (null (find #\Newline formstring)))
        (setq *oneline-form-data* formstring)
      (setq *multiline-form-data* formstring))

    (html 
     (:princ 
      (indirect-to-redisplay (incf *user-display-id*) (user-session-id))
      ))))

(publish 
 :path *clear-history-url*
 :content-type cl-user::*html-publish-content-type*
 :function
 (lambda (req ent)
   (let* ((input (request-query req))
          (package-name (url-parameter-value :pkg input))
          (package-symbol (keywordize package-name)))
     (execute-with-standard-weblistener-environment
      req ent package-symbol
      (lambda ()
       (clear-history)
       (html 
        (:princ 
         (indirect-to-redisplay (incf *user-display-id*) (user-session-id))
         )))))))

    
(defun history-url 
       (which history-number &optional (pkgname (string (user-session-id))))
  (make-history-url :pkg pkgname :which which :history history-number))
    
(defun new-history-url
       (which history-number value-number 
              &optional 
              (pkgname (string (user-session-id))))
  (make-new-history-url 
   :pkg pkgname :which which :history history-number :value value-number))


;;; User-callable function CLEAR-HISTORY


(defun clear-history (&optional (how-much :all))
  #.(one-string-nl
     "Delete part or all of the recorded history of the user's input/output"
     "(and hence what gets shown to the user):"
     "-- :all (the default) deletes everything"
     "-- +n deletes the oldest N input/output pairs (those displayed topmost)."
     "-- -n deletes the newest N pairs (those shown nearest the input boxes).")
  (let ((hlen (length *in-history*)))
    (cond
     ((eq how-much :all)
      (setq *in-history* nil)
      (setq *out-history* nil))
     ((eq how-much :logout) 
      (setf *in-history* 
            (if *in-history* 
                (subseq *in-history* 0 1)
              '(("(logout)" (logout)))))
      (setf *out-history* (list (list "" '(":expunged") '(:expunged)))))
     ((not (integerp how-much))
      (error "Invalid CLEAR-HISTORY argument: ~A" how-much))
     ((plusp how-much)
      (if (>= how-much hlen)
          (clear-history :all)
        (progn
          (setf *in-history* (subseq *in-history* 0 (- hlen how-much)))
          (setf *out-history* (subseq *out-history* 0 (- hlen how-much))))))
     ((minusp how-much)
      (let ((how-much (abs how-much)))
        (if (>= how-much hlen)
            (clear-history :all)
          (progn
            (setf *in-history* (nthcdr how-much *in-history*))
            (setf *out-history* (nthcdr how-much *out-history*)))))))
    nil))




(defun clear-all-histories ()
  (unless (wb::weblistener-guru-p)
    (error "You shouldn't be executing this command."))
  (let ((clearings nil))
    (loop for user in *logins* do
          (loop for session in (gethash user *user->sessionids-ht*) do
                (with-protected-globals-bound session
                  (push (list *username* *sessionid*) clearings)
                  (clear-history)
                  )))
    (loop for (user session) in clearings do  
          (cformatt "History cleared for user ~A, session ~A" 
                    user session))
    (clear-history)))



