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

#|

History is stored on a per session basis.  The variables *in-history*
and *out-history* are bound to each user's session's history record.

Each form that the user types in to the weblistener is associated with
a unique history number (unique to the user's session).  This number is
kept track of on the property list of the user's session symbol under the
:history-index property.  This unique history number is also associated with 
the output which results from the evaluation of the user's input.  

Each input history record consists of the unique index, the string the user
typed in, and the form created by reading the input string (or nil if an error
occured while reading).  

Each output history record consists of the unique index, a string containing 
all the printout caused by the read/evaluation of the input string, the forms
that were returned (as multiple values) by evaluation of the input, and
string representations of these forms.  

|#

(defvar *in-history* nil 
  "All forms (as strings) that have been evaluated")
(defvar *out-history* nil 
  "All output (as strings) from evaluation of user's forms")

(defstruct inhist index form string logged?)
(defstruct outhist index printout forms strings logged?)

(defun initialize-history-index ()
  (setf (get wb:*sessionid* :history-index) 0))
(defun next-history-index ()
  (incf (get wb:*sessionid* :history-index)))
(defun current-history-index ()
  (get wb:*sessionid* :history-index))

(defun history-record (string form)
  (input-history-record string form))

(defun input-history-record (string form) 
  (make-inhist :index (current-history-index) :form form :string string))
(defun output-history-record (printout-string value-strings values)
  (make-outhist
   :index (current-history-index) :forms values :strings value-strings
   :printout printout-string))

(defun nth-input-history-record (n)
  (find n *in-history* :key 'inhist-index))
(defun nth-output-history-record (n)
  (find n *out-history* :key 'outhist-index))

(defun history-input-string (n) (inhist-string (nth-input-history-record n)))
(defun history-input-form (n) (inhist-form (nth-input-history-record n)))

(defun new-history-output-string (n) (history-output-string n))

(defun history-output-string (n)
  (let* ((out-record (nth-output-history-record n))
         (printout-string (outhist-printout out-record))
         (value-strings (outhist-strings out-record))
         (mode (out-record-output-mode out-record)))
    (ecase mode
      (:single-value-no-printout (first value-strings))
      (:single-value-printout
       (one-string printout-string nl (first value-strings)))
      (:multiple-values-no-printout (string-join value-strings nl))
      (:multiple-values-printout
       (one-string printout-string nl (string-join value-strings nl)))
      )))

(defun new-history-output-value-string (history-index value-index)
  (history-output-value-string history-index value-index))

(defun history-output-value-string (history-index value-index)
  (nth (1- value-index)
       (outhist-strings (nth-output-history-record history-index))))

(defun new-history-output-datum 
       (n &key (value 1) (as-string? nil) (printout? nil))
  (history-output-datum
   n :value value :as-string? as-string? :printout? printout?))

(defun history-output-datum 
       (n &key (value 1) (as-string? nil) (printout? nil))
  #.(one-string-nl
     "Returns one of the values (or the printout string) from the Nth"
     "output history record.  If AS-STRING? is T the string form of the"
     "value is returned, otherwise the actual Lisp object is returned."
     "If PRINTOUT? is T and VALUE is non-nil the printout-string is"
     "returned as the second value.  If PRINTOUT? is T and VALUE is NIL"
     "the printout-string is returned as the primary value.")
  (let ((out-record (nth-output-history-record n)))
    (unless out-record 
      (error "No output form currently exists indexed by ~D" n))
    (let ((printout-string (outhist-printout out-record))
          (value-strings (outhist-strings out-record))
          (values (outhist-forms out-record)))
      (cond
       (value
        (let* ((i (1- value))
               (v (if as-string? (nth i value-strings) (nth i values))))
          (if printout? (values v printout-string) (values v nil))
          ))
       (printout? printout-string)
       (t (error "Invalid arguments to NEW-HISTORY-OUTPUT-DATUM"))
       ))))



;;; This gets triggered when the user clicks on one of the history URL's
;;; (the things of the form '<n>>' and '::' preceding the input and
;;; output forms).

;;; It causes the form associated with the prompt the user just
;;; clicked on (i.e., the history URL) to be put
;;; back into one of the form text box
;;; areas so that the user can edit and/or reevaluate the form.


(publish 
 :path *in-history-url*
 :content-type cl-user::*html-publish-content-type*
 :function
 (lambda (req ent)
   (let* ((input (request-query req))
          (package-name (url-parameter-value :pkg input))
          (package-symbol (keywordize package-name))
          (history (url-parameter-value :history input)))
     (execute-with-standard-weblistener-environment
      req ent package-symbol
      (lambda () (weblistener-in-history-redisplay history))
      ))))
     

(defun weblistener-in-history-redisplay (history)

  ;; find the appropriate form string based on the WHICH and HISTORY

  (let* ((index (read-from-string history))
         (formstring (history-input-string index)))

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
 :path *out-history-url*
 :content-type cl-user::*html-publish-content-type*
 :function
 (lambda (req ent)
   (let* ((input (request-query req))
          (package-name (url-parameter-value :pkg input))
          (package-symbol (keywordize package-name))
          (history-index-string (url-parameter-value :history input))
          (value-index-string (url-parameter-value :value input))
          )
     (execute-with-standard-weblistener-environment
      req ent package-symbol
      (lambda () 
        (weblistener-out-history-redisplay 
         history-index-string value-index-string
         ))))))

(defun weblistener-out-history-redisplay 
       (history-index-string value-index-string)

  ;; If VALUE-INDEX is NIL, then
  ;; find the appropriate form string based on the WHICH and HISTORY
  ;; If it is not NIL, it must be a number and WHICH must be "out", and
  ;; we snarf the string representing the nth value in the output.

  (let* ((hindex (read-from-string history-index-string))
         (vindex (read-from-string value-index-string))
         (formstring 
          (cond
           ((plusp vindex)
            (new-history-output-value-string hindex vindex))
           (t (new-history-output-string hindex))
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
  (in-history-url which history-number pkgname))

(defun in-history-url 
       (which history-number &optional (pkgname (string (user-session-id))))
  (declare (ignore which))
  (make-in-history-url :pkg pkgname :history history-number))
    
(defun new-history-url
       (which history-number value-number 
              &optional 
              (pkgname (string (user-session-id))))
  (out-history-url which history-number value-number pkgname))

(defun out-history-url
       (which history-number value-number 
              &optional 
              (pkgname (string (user-session-id))))
  (declare (ignore which))
  (make-out-history-url 
   :pkg pkgname :history history-number :value value-number))


;;; User-callable function CLEAR-HISTORY

(defun clear-history (&optional (how-much :all-but-latest) (limit nil))
  #.(one-string-nl
     "Delete part or all of the recorded history of the user's input/output"
     "(and hence what gets shown to the user):"
     " -- :all-but-latest (the default) deletes everything but the last item;"
     "    if there is only one item, it is deleted."
     " -- :all deletes everything"
     " -- +n deletes the oldest N input/output pairs (those displayed topmost)."
     " -- -n deletes the newest N pairs (those shown nearest the input boxes)."
     " -- :from N deletes everything from N inclusive to the most recent item."
     " -- :to N deletes everything from the earliest existing item to N,"
     "inclusive.")
  (setq *** nil) (setq ** nil) (setq * nil)
  (setq +++ nil) (setq ++ nil) (setq + nil)
  (setq /// nil) (setq // nil) (setq / nil)
  (setq *current-repl* nil)
  (let ((hlen (length *in-history*)))
    (cond
     ((eq how-much :all)
      (setq *in-history* nil)
      (setq *out-history* nil))
     ((eq how-much :all-but-latest)
      (when *in-history*
        (if (= (length *in-history*) 1) 
            (setq *in-history* nil *out-history* nil)
          (setq *in-history* (list (first *in-history*))
                *out-history* (list (first *out-history*))))))
     ((eq how-much :logout) 
      (setq *in-history* 
            (if *in-history* 
                (subseq *in-history* 0 1)
              (list (make-inhist :index 0 :form '(logout) :string "(logout)"))))
      (setq *out-history* 
            (list
             (make-outhist 
              :index 0 :printout nil 
              :forms '(:expunged) :strings '(":expunged")))))
     ((eq how-much :from)
      (unless (and (integerp limit) (plusp limit)) 
        (error "Invalid 2nd argument for CLEAR-HISTORY: ~S." limit))
      (setq *in-history* 
            (remove-if (lambda (x) (>= (inhist-index x) limit)) *in-history*))
      (setq *out-history* 
            (remove-if (lambda (x) (>= (inhist-index x) limit)) *out-history*)))
     ((eq how-much :to)
      (unless (and (integerp limit) (plusp limit)) 
        (error "Invalid 2nd argument for CLEAR-HISTORY: ~S." limit))
      (setq *in-history* 
            (remove-if (lambda (x) (<= (outhist-index x) limit)) *in-history*))
      (setq *out-history* 
            (remove-if
             (lambda (x) (<= (outhist-index x) limit)) *out-history*)))
     ((not (integerp how-much))
      (error "Invalid CLEAR-HISTORY argument: ~A" how-much))
     ((plusp how-much)
      (if (>= how-much hlen)
          (clear-history :all)
        (progn
          (setq *in-history* (subseq *in-history* 0 (- hlen how-much)))
          (setq *out-history* (subseq *out-history* 0 (- hlen how-much))))))
     ((minusp how-much)
      (let ((how-much (abs how-much)))
        (if (>= how-much hlen)
            (clear-history :all)
          (progn
            (setq *in-history* (nthcdr how-much *in-history*))
            (setq *out-history* (nthcdr how-much *out-history*)))))))
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

;; So I can find the command via (apropos "HISTORY") !!
(defun history-clear-all () (clear-all-histories))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun memory-object-crude-size (obj &optional (error? t))
  (typecase obj
    (cons (length obj))
    (hash-table (hash-table-count obj))
    (string (length obj))
    (array (array-total-size obj))
    (utils::garray (garray-current-total-size obj))
    (t 
     (memory-object-crude-size-method obj error?)
     )))

(defmethod memory-object-crude-size-method ((obj t) &optional (error? t))
  (if error? (error "Shouldn't get here!") 1))



