;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: wb; -*-

(in-package :wb)

;;; +=========================================================================+
;;; | Copyright (c) 2008 JP Massar                                            |
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

(defvar *string-size-min* nil)
(defvar *array-size-min* nil)
(defvar *list-size-min* nil)
(defvar *hash-size-min* nil)
(defvar *usage-user* nil)
(defvar *usage-session* nil)
(defvar *memory-usage-info* nil)
(defvar *memory-history-trace* nil)
(defvar *session-memory-info* nil)
(defvar *user-memory-info* nil)

(defmacro with-memory-history-traced ((trace-object) &body body)
  `(progn
     (push ,trace-object *memory-history-trace*)
     ,@body
     (pop *memory-history-trace*)
     ))

(defun umu (&rest goo)
  (apply 'user-memory-usage goo))

(defun user-memory-usage 
       (&key 
        (string-size-min 10000)
        (array-size-min 10000)
        (list-size-min 10000)
        (hash-size-min 10000)
        (print-report? t)
        (sorted-by-session-space? t)
        (verbose? nil)
        (return-memory-info? nil))
  (let ((*memory-usage-info* nil)
        (*memory-history-trace* nil)
        (*string-size-min* string-size-min)
        (*array-size-min* array-size-min)
        (*list-size-min* list-size-min)
        (*hash-size-min* hash-size-min)
        (users (sort (copy-list *logins*) 'string-lessp :key 'symbol-name)))
    (loop 
     for user in users
     do
     (let ((*usage-user* user)
           (*user-memory-info* (list user)))
       (with-memory-history-traced (user) 
         ;; (print user)
         (let ((*session-memory-info* (list nil)))
           (descend-into-user-symbols user)
           (descend-into-memory (symbol-plist user))
           (when (cdr *session-memory-info*)
             (push (reverse *session-memory-info*) *user-memory-info*)))
         (let ((sessions (gethash user *user->sessionids-ht*)))
           (loop for session in sessions do
                 ;; (print session)
                 (let ((*usage-session* session)
                       (*session-memory-info* (list session)))
                   (with-memory-history-traced (session)
                     (descend-into-user-session session)
                     (when (get session :vpl-session?)
                       (with-memory-history-traced (:vpl-session?)
                         (descend-into-vpl-user-session session)
                         ))
                     (when (cdr *session-memory-info*)
                       (push (reverse *session-memory-info*)
                             *user-memory-info*
                             )))))))
       (when (cdr *user-memory-info*)
         (push (reverse *user-memory-info*) *memory-usage-info*)
         )))
    (when print-report? 
      (if sorted-by-session-space? 
          (sorted-memory-usage-report :verbose? verbose?)
        (memory-usage-report :verbose? verbose?)))
    (when return-memory-info?
      (if sorted-by-session-space?
          (memory-usage-sort)
        *memory-usage-info*
        ))))
          

(defun memory-usage-report (&key (verbose? t))
  (loop for user-record in *memory-usage-info* 
        as user = (first user-record)
        as session-data = (cdr user-record)
        do
        (formatt "~%User: ~S~%" user)
        (loop for session-record in session-data
              as sessionid = (first session-record)
              as memory-traces = (cdr session-record)
              do
              (formatt "  ~S :~%" sessionid)
              (loop for memory-trace in memory-traces do
                    (pretty-print-memory-trace
                     memory-trace :show-trace? verbose?
                     )))))

(defun sorted-memory-usage-report (&key (verbose? t))
  (let ((sorted-data (memory-usage-sort)))
    (loop for (userid sessionid space memory-traces) in sorted-data
          do
          (formatt "~A (~A): Total space: ~A~%" sessionid userid space)
          (loop for memory-trace in memory-traces do
                (pretty-print-memory-trace memory-trace :show-trace? verbose?)
                ))))

;;; Returns list consisting of sublists of the form 
;;; (userid sessionid total-space memory-traces-for-session)
(defun memory-usage-sort ()
  (let ((memory-usage-info nil))
    (loop for user-record in *memory-usage-info* 
          as user = (first user-record)
          as session-data = (cdr user-record)
          do
          (loop for session-record in session-data
                as sessionid = (first session-record)
                as memory-traces = (cdr session-record)
                as memory-objects = (mapcar 'first memory-traces)
                do
                (let* ((trace-spaces 
                        (mapcar 'memory-object-crude-size memory-objects))
                       (session-total-space (reduce '+ trace-spaces)))
                  (push 
                   (list user sessionid session-total-space memory-traces)
                   memory-usage-info
                   ))))
    (setq memory-usage-info (sort memory-usage-info '> :key 'third))
    memory-usage-info
    ))

(defun pretty-print-memory-trace (memory-trace &key (show-trace? t))
  (formatt "    ~A~%" (memory-object-description (first memory-trace)))
  (when show-trace? 
    (formatt "     trace: ")
    (formatt "(")
    (loop for trace-step in (cdr memory-trace) do
          (formatt "~A " (memory-object-description trace-step nil)))
    (formatt ")~%")
    ))

(defun memory-object-description (obj &optional (error? t))
  (typecase obj 
    (cons (formatn "<List of length ~D>" (length obj)))
    (hash-table (formatn "<Hash table of size ~D>" (hash-table-count obj)))
    (string (formatn "<String of length ~D, '~A...'>" 
                     (length obj) (subseq obj 0 25)))
    (array (formatn "<Array of total size ~D, dimensions ~A>"
                    (array-total-size obj) (array-dimensions obj)))
    (utils::garray (formatn "<Garray of total size ~D, ~A >" 
                            (garray-current-total-size obj) obj))
    (vpl::snippet (formatn "<Snippet of type ~A>" (vpl::snippet-type obj)))
    (bbi::labeled-sequence 
     (formatn "<Labeled sequence of size ~D, ~A>"
              (+ (length (bbi::labeled-sequence-label obj))
                 (length (bbi::labeled-sequence-sequence obj)))
              (bbi::labeled-sequence-label obj)
              ))
    (t 
     (if error? 
         (error "Shouldn't get here!")
       (formatn "~S" obj)
       ))))

(defmethod memory-object-crude-size-method 
           ((obj vpl::snippet) &optional (error? t))
  (declare (ignore error?))
  1)

(defun descend-into-user-symbols (user)
  (let ((user-package (find-package user)))
    (do-symbols (s user-package)
      (when (eq (symbol-package s) user-package)
        (with-memory-history-traced (s)
          (when (boundp s) 
            (descend-into-memory (symbol-value s)))
          (when (symbol-plist s)
            (descend-into-memory (symbol-plist s))
            ))))))

(defun descend-into-user-session (session)
  (let* ((session-data (gethash session utils::*saved-variables-hash-table*))
         (variables (first session-data))
         (values (second session-data)))
    ;; (print variables)
    ;; (print values)
    (loop for var in variables 
          for val in values
          do
          (with-memory-history-traced (var)
            (descend-into-memory val)
            ))))

(defun descend-into-vpl-user-session (session)
  (let* ((vpl-session-id (vpl::vpl-sessionid-from-sessionid session))
         (vpl-session-data
          (gethash vpl-session-id utils::*saved-variables-hash-table*))
         (variables (first vpl-session-data))
         (values (second vpl-session-data)))
    (loop for var in variables 
          for val in values
          do
          (with-memory-history-traced (var)
            (descend-into-memory val)
            ))))

(defun already-in-memory-stack? (obj)
  (member obj *memory-history-trace* :test 'eq))

(defun add-memory-trace (obj)
  (push (copy-list (cons obj *memory-history-trace*)) *session-memory-info*))

(defun maybe-add-memory-trace (obj)
  (unless (already-in-memory-stack? obj) (add-memory-trace obj)))

(defmethod descend-into-memory ((obj t)) nil)

(defmethod descend-into-memory ((obj string))
  (when (> (length obj) *string-size-min*) (maybe-add-memory-trace obj)))

(defmethod descend-into-memory ((obj array))
  (let ((already? (already-in-memory-stack? obj)))
    (when (> (array-total-size obj) *array-size-min*) 
      (unless already? (add-memory-trace obj)))
    (unless already? 
      (with-memory-history-traced (obj)
        (loop for j from 0 below (array-total-size obj) do
              (descend-into-memory (row-major-aref obj j))
              )))))

(defmethod descend-into-memory ((obj hash-table))
  (let ((already? (already-in-memory-stack? obj)))
    (when (> (hash-table-count obj) *hash-size-min*)
      (unless already? (add-memory-trace obj)))
    (unless already? 
      (with-memory-history-traced (obj)
        (maphash 
         (lambda (key value) 
           (descend-into-memory key)
           (descend-into-memory value))
         obj
         )))))

(defmethod descend-into-memory ((obj utils::garray))
  (let ((already? (already-in-memory-stack? obj)))
    (when (> (garray-current-total-size obj) *array-size-min*) 
      (unless already? (add-memory-trace obj)))
    (unless already? 
      (with-memory-history-traced (obj)
        (descend-into-memory (utils::garray-data obj))
        ))))
         
(defmethod descend-into-memory ((obj cons))
  (let ((already? (already-in-memory-stack? obj)))
    (multiple-value-bind (length type) 
        (length-circular-or-dotted? obj)
      (case type 
        (:dotted nil)
        (:circular nil)
        (otherwise 
         (when (> length *list-size-min*)
           (unless already? (add-memory-trace obj)))
         (unless already?
           (with-memory-history-traced (:list)
             (loop for elem in obj do (descend-into-memory elem))
             )))))))

(defmethod descend-into-memory ((obj inhist))
  (with-memory-history-traced (:in-history)
    (descend-into-memory (inhist-form obj))
    (descend-into-memory (inhist-string obj))
    ))

(defmethod descend-into-memory ((obj outhist))
  (with-memory-history-traced (:out-history)
    (descend-into-memory (outhist-printout obj))
    (descend-into-memory (outhist-forms obj))
    (descend-into-memory (outhist-strings obj))
    ))

(defmethod descend-into-memory ((obj vpl::uvs))
  (with-memory-history-traced (:vpl-state)
    (with-memory-history-traced (:workspace)
      (descend-into-memory (vpl::uvs-ws obj)))
    (with-memory-history-traced (:workspace-history)
      (descend-into-memory (vpl::uvs-wsh obj)))
    (with-memory-history-traced (:results)
      (descend-into-memory (vpl::uvs-rs obj)))
    (with-memory-history-traced (:vpl-state-hash)
      (descend-into-memory (vpl::uvs-hash obj)))
    ))
    
(defmethod descend-into-memory ((obj vpl::snippet))
  (with-memory-history-traced (obj)
    (with-memory-history-traced (:snippet-children)
      (descend-into-memory (vpl::snippet-children obj)))
    (with-memory-history-traced (:snippet-value)
      (descend-into-memory (vpl::snippet-value obj)))
    (with-memory-history-traced (:snippet-properties)
      (descend-into-memory (vpl::snippet-properties obj)))
    ))
    
(defmethod descend-into-memory ((obj bbi::labeled-sequence))
  (with-memory-history-traced ((bbi::labeled-sequence-label obj))
    (descend-into-memory (bbi::labeled-sequence-sequence obj))
    ))

(defmethod descend-into-memory ((obj frames::%frame))
  (with-memory-history-traced (obj)
    (frames::for-each-frame-slot (slot value) obj
      (when (stringp value) #+oops (frames::isframe? value)
        (with-memory-history-traced (slot)
          (descend-into-memory value)
          )))))
