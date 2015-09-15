;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: nvpl; -*-

(in-package :nvpl)

;;; +=========================================================================+
;;; | Copyright (c) 2006 JP Massar, John Myers                                |
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

;; Author: JP Massar. 

;;;;;;;;;;;;;;;;;;;;;;;; HOLE-FILLING ROUTINES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Detect when the user types more than one value or garbage after
;; his data, and error out instead of ignoring it.  
(defmethod handle-insert-type ((insert-type t) snippet user-input)
  (let* ((lisp-values
          (handler-case (bbi::read-from-string-many-values user-input)
            (error (c) (oops-unreadable-input user-input c))))
         (single-value 
          (if (null (cdr lisp-values))
              (first lisp-values)
            ;; see if we can get some context as to what kind of holes 
            ;; the user is typing into when he makes this error
            (progn
              (when (snippet-parent snippet)
                (handler-case 
                    (let ((code (snippet-to-code (snippet-parent snippet) t)))
                      (ulog "Multiple input error.  Context: ~S~%" code))
                  (error () nil)
                  ))
              (oops-more-than-one-typed-input-value lisp-values)
              ))))
    (cond
     ((symbolp single-value) (verify-symbol-is-vpl-symbol single-value))
     (t nil))
    (replace-hole-using-value 
     single-value (snippet-id snippet) :vpl-user-error
     )))

(defun verify-symbol-is-vpl-symbol (symbol)
  (let ((chars (symbol-name symbol)))
    (flet ((maybe-oops (ch) 
             (when (find ch chars) 
               (vpl-user-error 
                (formatn
                 (one-string-nl
                  "Invalid character in symbol name: '~A'"
                  "(If you meant your data to be interpreted as a string,"
                  "you need to put a \" at the beginning and end.)")
                 ch
                 )))))
      (cond
       ((and (find #\[ chars) (find #\] chars))
        (vpl-user-error 
         (formatn
          (one-string-nl
           "You cannot use [ ]-notation within an input box!  Sorry!"
           "If this was what you were trying to do you must bring down"
           "the [] operator from the OTHER-COMMANDS palette menu."
           "(If you meant your data to be interpreted as a string,"
           "you need to put a \" at the beginning and end.)"
           ))))
       ((and (find #\{ chars) (find #\} chars))
        (vpl-user-error 
         (formatn
          (one-string-nl
           "You cannot use { }-notation within an input box!  Sorry!"
           "If this was what you were trying to do you must bring down"
           "the {} operator from the OTHER-COMMANDS palette menu."
           "(If you meant your data to be interpreted as a string,"
           "you need to put a \" at the beginning and end.)"
           ))))
       (t 
        (maybe-oops #\{) 
        (maybe-oops #\})
        (maybe-oops #\[)
        (maybe-oops #\])
        )))))
        
        

(defmethod handle-insert-type ((insert-type (eql :string)) snippet user-input)
  (let ((lisp-value 
         (handler-case 
             (cond
              ((zerop (length user-input)) "")
              ((char= (char user-input 0) #\")
               (bbi::bbl-read-from-string user-input))
              (t 
               (setq user-input (s+ "\"" user-input "\""))
               (bbi::bbl-read-from-string user-input)
               ))
           (error (c) (oops-unreadable-input user-input c))
           )))
    (replace-hole-using-value lisp-value (snippet-id snippet) :vpl-user-error)
    ))

(defmethod handle-insert-type ((insert-type (eql :code)) snippet user-input)
  (let ((lisp-value 
         (handler-case 
             (bbi::bbl-read-from-string user-input)
           (error (c) (oops-unreadable-input user-input c)))))
    (insert-constant-into-hole 
     (snippet-id snippet) lisp-value :eval-constant? nil)
    ))

#||

(defmethod handle-insert-type
           ((insert-type (eql :function-call)) snippet user-input)
  (let ((lisp-value 
         (handler-case 
             (bbi::bbl-read-from-string user-input)
           (error (c) (oops-unreadable-input user-input c)))))
    (unless (symbolp lisp-value) 
      (vpl-user-error "Only names of functions are allowed here!"))
    (if (get lisp-value :vpl-template)
        (do-something)
      (

||#  

  

(defmethod handle-insert-type ((insert-type (eql :gene)) snippet user-input)
  (vdbg "Attempting to insert gene...~%")
  (let* ((orgname (get-snippet-info snippet :organism-fname)) 
         (gene (find-gene-from-user-input user-input orgname)))
    (replace-hole-using-value gene (snippet-id snippet) :vpl-user-error)
    ))

(defmethod handle-insert-type ((insert-type (eql :protein)) snippet user-input)
  (vdbg "Attempting to insert protein...~%")
  (let* ((orgname (get-snippet-info snippet :organism-fname)) 
         (protein (find-protein-from-user-input user-input orgname)))
    (replace-hole-using-value protein (snippet-id snippet) :vpl-user-error)
    ))

(defun oops-unreadable-input (user-input c)
  (vpl-user-error
   (formatn
    (one-string-nl
     "The characters ~S are not legal BBL!"
     "They cannot be read/interpreted as either a symbol, a number,"
     "a string, or any other BBL value."
     "Actual reader error: ~A")
    user-input c
    )))

(defun oops-more-than-one-typed-input-value (typed-values)
  (let* ((strings (mapcar (lambda (x) (formatn "~S" x)) typed-values))
         (nstrings (length strings))
         (strings-to-be-displayed 
          (if (> nstrings 5) (subseq strings 0 5) strings))
         (formatted-strings (string-join strings-to-be-displayed #\Newline))
         (every-string-alphabetic? 
          (every (lambda (x) (every 'alpha-char-p x)) strings))
         )

    (vpl-user-error 
     (formatn 
      (one-string-nl
       "You typed ~D items into the input box! (instead of just one)."
       "The ~A~D distinct values you typed in are (shown one per line):"
       ""
       "~A"
       ""
       "You are only allowed to type a single item into an input box."
       (if (and every-string-alphabetic? (= 2 (length strings)))
           (formatn "~%(You may have meant to type ~A or ~A)~%" 
                    (s+ (first strings) "-" (second strings))
                    (s+ (first strings) "_" (second strings))
                    )
         ""
         )
       "(If you meant your data to be interpreted as a string,"
       "you need to put a \" at the beginning and end.)"
       ""
       "(If you meant your data to be interpreted as a list,"
       "you need to enclose it in parentheses (if the values"
       "are all constants) or use the LIST function (if at least"
       "one of the values needs to be evaluated))"
       ""
       "(If you were trying to use { }-notation or [ ]-notation,"
       "you cannot do that within an input box.  Sorry!"
       "You would need to bring down the { } or [ ] operator"
       "from the LISTS-TABLES palette menu.)"
       )
      (length strings)
      (if (> nstrings 5) "first " "")
      (length strings-to-be-displayed)
      formatted-strings
      )))) 

(defvar *newly-inserted-snippet* nil)

;; handles an input-text field being filled in.

(defun handle-multiline-input-text (boxid value &key (tabbed? nil))
  (vdbg "In Handle-multiline-input-text...~%")

  (let ((csb *current-selected-boxid*)
	(iboxid (idstring->id boxid :snippet-id))
	(more-than-one-significant-line? 
          (> 
           (count-if 
            (lambda (s) 
              (and (> (length s) 0) (notevery 'whitespacep s)))
            (split-sequence:split-sequence #\Newline value))
           1)))
    (let ((*newly-inserted-snippet* nil))
      (if (not tabbed?) 
          (handle-input-text boxid value)
	  (handle-input-text-tab boxid value))
      (etypecase *newly-inserted-snippet*
        (symbol-snippet nil)
        (constant-snippet 
         (when more-than-one-significant-line?
           (set-snippet-property *newly-inserted-snippet* :multiline-input t)
           )))
      (when (and csb (/= csb iboxid))
        (setq *current-selected-boxid* nil)
        (unflashing-hilight-box csb))
      *newly-inserted-snippet*)))

(defun handle-multiline-input-text-tab (boxid value)
  (handle-multiline-input-text boxid value :tabbed? t))

(defun handle-input-text (boxid user-input)

  (block exit

    (vdbg "Handle Input Text: boxID: ~A user-input |~A|~%" boxid user-input)
    (vdbg "*csb*: ~D~%" *current-selected-boxid*)

    (setq boxid (idstring->id boxid :snippet-id))

    (handler-case
        
        (let* ((snippet (find-snippet-in-workspace boxid))
               (insert-type (get-snippet-property snippet :insert-type)))
          
          (unless (get-snippet-property snippet :blank-enter-count) 
            (set-snippet-property snippet :blank-enter-count 0))
          (when (zerop (length user-input))
            (when (zerop (get-snippet-property snippet :blank-enter-count))
              (set-snippet-property snippet :hole-open t)
              (set-snippet-property snippet :blank-enter-count 1)
              (return-from exit nil))
            (vpl-user-error 
             #.(one-string-nl
                "You must enter text (or use the clear icon)."
                "Entering 'nothing' has no effect."
                )))

          (handle-insert-type insert-type snippet user-input)
          (vdbg "after insert -- *csb*: ~D~%" *current-selected-boxid*)
          (setq *current-selected-boxid* nil)
          )
      
      (vpl-user-error (c) (error c))
      (vpl-internal-error (c) (error c))
      (error
       (c)
       (vpl-internal-error
        (formatn
         (one-string-nl
          "Insert code failed unexpectedly on user input '~A' !"
          "Actual error: ~A")
         user-input c
         ))))))

(defun handle-input-text-tab (boxid-string user-input &aux boxid)
  (vdbg "In handle-input-text-tab...~%")
  (setq boxid (idstring->id boxid-string :snippet-id))
  (let ((snippet (find-snippet-in-workspace boxid)))
    (cond
     ((toplevel-ws-snippet? snippet)
      (handle-input-text boxid-string user-input))
     (t 
      (let* ((next-hole-snippet (find-next-hole-snippet-for-tab snippet))
             (parent (snippet-parent snippet)))
        (if next-hole-snippet 
            (let ((common-ancestor 
                   (common-ancestor-node-of parent next-hole-snippet)))
              (vdbg "Found next hole snippet...~S~%" next-hole-snippet)
              (handle-input-text boxid-string user-input)
              (set-snippet-property next-hole-snippet :hole-open t)
              ;; Highlight the next box AFTER the redraw 
              (push 
               `(setq *current-selected-boxid* ,(snippet-id next-hole-snippet))
               *more-client-commands*)
              (push 
               `(flashing-hilight-box ,(snippet-id next-hole-snippet))
               *more-client-commands*)
              (setq *modified-snippet* common-ancestor)
              )
          (handle-input-text boxid-string user-input)
          ))))))

(defun find-next-hole-snippet-for-tab (snippet)
  (let ((linearization (linearize-hole-leaves (workspace-root-node))))
    (let ((pos (position snippet linearization)))
      (unless pos 
        (vpl-internal-error "form snippet ~S not in linearization!" snippet))
      (nth (1+ pos) linearization)
      )))

(defun linearize-hole-leaves (snippet)
  (let ((hole-leaves nil))
    (loop for child in (snippet-children snippet) do
          (if (typep child 'form-snippet)
              (setq hole-leaves (append hole-leaves (list child)))
            (setq hole-leaves 
                  (append hole-leaves (linearize-hole-leaves child))
                  )))
    hole-leaves
    ))  

(defun common-ancestor-node-of (s1 s2)
  (cond
   ((eq s1 s2) s1)
   ((eq s1 (workspace-root-node)) s1)
   ((eq s2 (workspace-root-node)) s2)
   ((member s1 (snippet-children s2)) s2)
   ((member s2 (snippet-children s1)) s1)
   (t 
    (let ((root-path-1 (workspace-root-path s1))
          (root-path-2 (workspace-root-path s2)))
      (loop for path1 on root-path-1
            for path2 on root-path-2
            do
            (when (not (eq (second path1) (second path2)))
              (return (car path1)))
            finally 
            (vpl-internal-error 
             "This is impossible!  Two paths are identical!")
            )))))

(defun workspace-root-path (s)
  (if (eq s (workspace-root-node))
      (list s)
    (append (workspace-root-path (snippet-parent s)) (list s))
    ))

(defun replace-hole-using-value (lisp-value sid error-type)
  (vdbg "In replace-hole-using-value...~%")
  (cond
   ((and (symbolp lisp-value) (not (constantp lisp-value)))
    (insert-symbol-into-hole sid lisp-value)
    (vdbg "Inserted ~S into hole snippet as symbol~%" lisp-value)
    t)
   ;; if the user entered a quoted list, use it.  
   ((and (listp lisp-value) (eq 'quote (first lisp-value)))
    (insert-constant-into-hole sid lisp-value)
    (vdbg "Inserted ~S into hole snippet as constant~%" lisp-value)
    t
    )
   ;; some other constant 
   ((and (not (listp lisp-value)) (constantp lisp-value))
    (insert-constant-into-hole sid lisp-value)
    (vdbg "Inserted ~S into hole snippet as constant~%" lisp-value)
    t
    )
   ;; if the user entered a non-quoted list, we assume that it should be
   ;; quoted.
   ((listp lisp-value) 
    (let ((quoted-list (list 'quote lisp-value)))
      (insert-constant-into-hole sid quoted-list)
      (vdbg "Inserted ~S into hole snippet as constant~%" quoted-list)
      t
      ))
   (t 
    (ecase error-type 
      ((nil) nil)
      (:vpl-internal-error 
       (vpl-internal-error 
        (formatn 
         "Not a symbol or constant: can't insert ~S into hole!!~%" 
         lisp-value)))
      (:vpl-user-error 
       (vpl-user-error
        (formatn
         (one-string-nl
          "You cannot use ~A as a value;, it is neither a symbol"
          "nor a legal BBL constant!")
         lisp-value
         ))))
    nil)
   ))

