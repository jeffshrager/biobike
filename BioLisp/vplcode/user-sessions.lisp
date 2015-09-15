;;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: nvpl; -*-

(in-package :nvpl) 

;;; +=========================================================================+
;;; | Copyright (c) 2007 JP Massar, Jeff Elhai, Mark Slupesky, Peter Seibel   |
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

;;; Author: JP Massar. 

(defun save-vpl-public-session 
       (dir &key (comment nil) &aux (dirname (namestring dir)))
  (ulogdbg "saving user session to public directory ~A" dirname)
  (let ((prefs-file (merge-pathnames *prefs-file* dir))
        (owner-file (merge-pathnames *owner-file* dir))
        (current-owner nil))
    (when (probe-file owner-file)
      (with-open-file (p owner-file :direction :input)
        (setq current-owner (read p nil nil nil))
        ))
    (when current-owner 
      (when (not (eq current-owner wb:*username*))
        (vpl-user-error 
         (one-string-nl
          "A public session named '~A' already exists,"
          "and you are not the owner of that public session."
          "(The owner of that session is '~A')"
          "You cannot overwrite a session you did not create!"
          "Please try again using another session name.")
         (lastelem (pathname-directory dir))
         current-owner
         )))
    (save-vpl-user-session dir :public? t :comment comment)
    (delete-file prefs-file)
    (with-open-file (p owner-file :direction :output :if-exists :supersede)
      (format p "~S" wb::*username*)
      )))
  
(defun save-vpl-user-session 
       (dir &key (public? nil) (comment nil) &aux (dirname (namestring dir)))

  (ulogdbg "Saving user session to ~A~%" dirname)
  
  (let ((comments-file (merge-pathnames *comments-file* dir))
        (prefs-file (merge-pathnames *prefs-file* dir))
        (vars-file (merge-pathnames *session-vars-file* dir))
        (dfs-file (merge-pathnames *session-dfs-file* dir))
        (workspace-file (merge-pathnames *session-workspace-file* dir))
        (save-complete-file (merge-pathnames *session-save-complete-file* dir))
        (package-name (string-downcase (package-name *package*)))
        (*print-readably* t)
        (*print-pretty* t)
        (*print-length* nil)
        (*print-level* nil)
        (*print-pprint-dispatch* bbi::*biolisp-print-pprint-dispatch*)
        (defined-var-symbols nil)
        (define-function-symbols nil)
        (save-workspace-error nil)
        (bad-vars nil)
        (circular-vars nil)
        (bad-dfs nil))
    
    (unless (string-equal package-name (symbol-name wb::*username*))
      (vpl-user-error 
       (one-string-nl
        "You cannot save a user session if you are"
        "not in your own package! You are in the"
        "~A package.")
       *package*
       ))
    
    ;; Save the user's comment (if any) describing the session 
    ;; to the comments file

    (when comment
      (with-open-file (p comments-file :direction :output :if-exists :supersede)
        (vdbg "Saving comments to ~S~%" (namestring comments-file))
        (format p "~S" comment)
        ))

    ;; Save the user's preferences in the prefs file

    (with-open-file (p prefs-file :direction :output :if-exists :supersede)
      (vdbg "Saving preferences to ~S~%" (namestring prefs-file))
      (format p "(in-package :~A)~%~%" package-name)
      (loop for prefs-descriptor in wb::*weblistener-prefs-variables*
            as pref-variable = (first prefs-descriptor)
            do 
            (format p "(setq ~S ~S)~%~%" 
                    pref-variable (symbol-value pref-variable)
                    )))

    ;; Save all the user's variables to the vars file...

    (with-open-file (p vars-file :direction :output :if-exists :supersede)
      (vdbg "Saving variables to ~S~%" (namestring vars-file))
      (format p "(in-package :~A)~%~%" package-name)
      ;; save a little space 
      (let ((*print-right-margin* 120))
        (do-symbols (x *package*)
          (when (and (eq *package* (symbol-package x)) 
                     (boundp x)
                     (not 
                      (member (string x) *vpl-user-variables-not-to-save* 
                              :test 'string-equal)))
            (if (toplevel-circular-structure? (symbol-value x))
                (push x circular-vars)
              (handler-case 
                  (progn
                    (pprint 
                     `(defparameter ,x 
                        ,(serialized-snippet-value (symbol-value x))) p)
                    (terpri p)
                    (push x defined-var-symbols)
                    )
                (error (c) (push (list x c (symbol-value x)) bad-vars))
                ))))
        (when defined-var-symbols 
          (terpri p)
          (loop for var in defined-var-symbols do
                (pprint `(nvpl::add-define-symbol-for-user ',var) p)
                ))))

    ;; Save all the user's function definition to the df file...

    (with-open-file (p dfs-file :direction :output :if-exists :supersede)
      (vdbg "Saving DFs to ~S~%" (namestring dfs-file))
      (format p "(in-package :~A)~%~%" package-name)
      (do-symbols (x *package*)
        (let ((pd (get x :procedure-definition)))
          (when (and pd 
                     (eq *package* (symbol-package x)) 
                     (fboundp x)
                     (eq 'bbi::define-function (first pd))
                     )
            (handler-case 
                (progn
                  (pprint pd p)
                  (push x define-function-symbols))
              (error (c) (push (list x c) bad-dfs)))
            (terpri p)
            )))
      (when define-function-symbols 
        (terpri p)
        (loop for var in define-function-symbols do
              (pprint `(nvpl::add-define-function-symbol-for-user ',var) p)
              )))

    ;; Saved workspace and the results area...
    
    (handler-case 
        (n-save-vpl-workspace workspace-file nil)
      (error (c) (setq save-workspace-error c)))
    
    (when (null save-workspace-error)
      (with-open-file 
          (p save-complete-file :direction :output :if-exists :supersede)
        (format p "Success!~%"))
      (vdbg "All user session files saved.~%"))

    ;; Tell the user if any variables or function definitions could not be saved

    (when (or bad-vars circular-vars bad-dfs save-workspace-error)
      (vpl-user-error 
       (s+ 
        (if (not bad-vars)
            ""
          (s+ 
           (one-string-nl
            "The following variables could not be saved because their"
            "values either are or contain an object which the system"
            "does not know how to store:"
            ""
            )
           (let ((bv ""))
             (loop for (b nil value) in bad-vars do 
                   (setq bv (s+ bv (string b) 
                                (format nil " (of type ~A)" (type-of value))
                                #\Newline)))
             bv)
           ""
           (one-string-nl
            "Your other variables were saved.  Please consult"
            "the system administrators for further explanation of"
            "this error."
            ""
            ""
            )))
        (if (not circular-vars)
            ""
          (s+ 
           (one-string-nl
            "The following variables could not be saved because their values"
            "are circular (recursive), ie, one or more of their subcomponents"
            "points back to a higher level sub-object: "
            "")
           (let ((bv ""))
             (loop for b in circular-vars do 
                   (setq bv (s+ bv (string b) #\Newline)))
             bv)
           ""
           (one-string-nl
            "Your other variables were saved.  Please consult"
            "the system administrators for further explanation of"
            "this error."
            ""
            ""
            )))
            
        (if (not bad-dfs)
            ""
          (s+ 
           (one-string-nl
            "The following functions could not be saved"
            "because their definitions contain an object which the system"
            "does not know how to store:"
            ""
            )
           (let ((bf ""))
             (loop for (b nil) in bad-dfs do 
                   (setq bf (s+ bf (string b) #\Newline)))
             bf)
           ""
           (one-string-nl
            "Your other functions were saved.  Please consult"
            "the system administrators for further explanation of"
            "this error."
            ""
            )))
        (if (not save-workspace-error)
            ""
          (one-string-nl
           "Some node in the workspace or results area cannot be saved,"
           "or some other error occured while trying to save your workspace."
           "Here's the exact error:"
           (format nil "~A" save-workspace-error)
           )))))

    (ulogdbg "Session saved to ~A~%" dirname)
    
    (user-info-message 
     (formatn 
      (one-string-nl
       "Your workspace was saved ~Aas ~A" 
       ""
       "To restore this workspace, use Session --> workspaces - list"
       "and click the 'Restore' link next to ~A")
      (if public? "publicly " "")
      (lastelem (pathname-directory dir))
      (lastelem (pathname-directory dir))
      ))))

;;; We need to prevent the VPL reaper from running while user sessions
;;; are being restored, because the restoration process temporarily 
;;; leaves the user's state inconsistent, and the VPL reaper detects that
;;; inconsistent state if running concurrently with the restoration and 
;;; errors out.  

(defun restore-vpl-user-session (dir &key (type :user))
  (unwind-protect 
      (progn
        (with-lock (*restore-vpl-user-session-lock-lock*)
	  (incf *restore-vpl-user-session-lock*))
        (restore-vpl-user-session-aux dir :type type))
    (with-lock (*restore-vpl-user-session-lock-lock*)
      (decf *restore-vpl-user-session-lock*))))

(defun restore-vpl-user-session-aux
       (dir &key (type :user) &aux (dirname (namestring dir)))
  (ulogdbg "Restoring user session from ~A~%" dirname)
  (let ((prefs-file (merge-pathnames *prefs-file* dir))
        (vars-file (merge-pathnames *session-vars-file* dir))
        (dfs-file (merge-pathnames *session-dfs-file* dir))
        (workspace-file (merge-pathnames *session-workspace-file* dir)))
    (flet ((load-defs (file type-string)
             (unless (probe-file file)
               (vpl-user-error 
                (formatn 
                 (one-string-nl
                  "The session ~A file does not exist!"
                  "Your user session cannot be restored!"
                  "Please consult the system administrators for help.")
                 (namestring file)
                 )))
             ;; allegro compiler cannot deal with humongous files 
             (handler-case 
                 (let ((size 0))
                   (with-open-file (p file) (setq size (file-length p)))
                   (cond 
                    ;; see comment below
                    ((> size 100000) (load file) (load file))
                    (t 
                     (unless 
                         ;; compile twice to avoid order of definition problems
                         (ignore-errors (utils::cl file) (utils::cl file t) t)
                       (progn (load file) (load file))
                       ))))
               (error
                (c)
                (vpl-user-error 
                 (formatn
                  (one-string-nl
                   "Could not compile/load the session ~A file,"
                   "~A"
                   "Actual error: ~A"
                   "Your user session cannot be restored!"
                   "Please consult the system administrators for help.")
                  type-string (namestring file) c
                  ))))))
      (load-defs prefs-file "preferences")
      (load-defs vars-file "variables")
      (load-defs dfs-file "function definitions")
      (case type
        (:example 
         (load-vpl-example-workspace workspace-file))
        ((:user :public)
         (n-restore-vpl-workspace workspace-file)
         ))
      (ulogdbg "User session from ~A restored.~%" dirname)
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *circular-detection-hash* nil)

(defun toplevel-circular-structure? (obj)
  (typecase obj
    ((or string number character symbol) nil)
    (otherwise 
     (let ((*circular-detection-hash* (make-hash-table :test 'eq)))
       (prog1 
           (circular-structure? obj)
         (setq *circular-detection-hash* nil)
         )))))

(defmethod circular-structure? ((x t)) nil)

(defmethod circular-structure? ((x string)) nil)

(defmethod circular-structure? ((x number)) nil)

(defmethod circular-structure? ((x character)) nil)

(defmethod circular-structure? ((x symbol)) nil)

(defmethod circular-structure? ((the-list cons)) 
  (block exit 
    (mapl 
     (lambda (c) 
       (when (gethash c *circular-detection-hash*)
         (return-from exit t))
       (setf (gethash c *circular-detection-hash*) t))
     the-list)
    (loop for elem in the-list do 
          (when (circular-structure? elem) (return-from exit t))
          finally (return-from exit nil)
          )))
  
(defmethod circular-structure? ((x array))
  (block exit
    (setf (gethash x *circular-detection-hash*) t)
    (let ((size (array-total-size x)))
      (loop for j from 0 below size do
            (let ((elem (row-major-aref x j)))
              (when (circular-structure? elem)
                (return-from exit t)
                ))
            finally (return-from exit nil)
            ))))

(defmethod circular-structure? ((the-hash hash-table)) 
  (block exit
    (setf (gethash the-hash *circular-detection-hash*) t)
    (maphash 
     (lambda (h) 
       (when (gethash h *circular-detection-hash*)
         (return-from exit t)
         ))
     the-hash)
    (return-from exit nil)
    ))     

(defmethod circular-structure? ((the-garray utils::garray))
  (block exit
    (setf (gethash the-garray *circular-detection-hash*) t)
    (gmap
     (lambda (h) 
       (when (gethash h *circular-detection-hash*)
         (return-from exit t)
         ))
     the-garray
     :missing-value nil)
    (return-from exit nil)
    ))

(defmethod circular-structure? ((x error)) nil)

#||
#+:weblistener-aframes
(defmethod circular-structure? ((x frames::%aframe)) nil)
||#

(defmethod circular-structure? ((x bbi::labeled-sequence)) 
  (setf (gethash x *circular-detection-hash*) t)
  (circular-structure? (bbi::labeled-sequence-sequence x)))

(defmethod circular-structure? ((x wb::url)) nil)
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun restore-vpl-public-session (public-session-directory)
  (with-temp-directory (dir user::*tmp-directory*)
    (let ((customized-dir dir)
          (shared-dir public-session-directory))
      (customize-shared-session shared-dir customized-dir)
      (restore-vpl-user-session customized-dir :type :public)
      )))

(defun output-in-package-form (package-string stream)
  (format stream "(in-package :~A)~%~%" package-string))

(defun in-package-package-of-file (file)
  (with-open-file (p file :direction :input)
    (let ((form (read p nil nil nil)))
      (if (or (null form) 
              (not (listp form))
              (not (eq 'lisp:in-package (first form)))
              (not (= 2 (length form)))
              (and (not (symbolp (second form)))
                   (not (stringp (second form)))))
          (values nil (formatn "Invalid or misplaced in-package form ~A" form))
        (string (second form))
        ))))

(defun customize-shared-session 
       (shared-dir customized-dir &key (username wb::*username*))
  (multiple-value-bind (shared-package-string emessage)
      (in-package-package-of-file 
       (merge-pathnames *session-vars-file* shared-dir))
    (unless shared-package-string (error emessage))
    (ensure-directories-exist customized-dir)
    ;; create a dummy prefs file.  Don't customize with the previous
    ;; user's preferences!  
    (with-open-file 
        (p (merge-pathnames *prefs-file* customized-dir)
           :direction :output :if-exists :supersede)
      (output-in-package-form username p))
    (let ((scanner 
           (ppcre::create-scanner
            (s+ shared-package-string ":") :case-insensitive-mode t)))
      (customize-shared-file 
       shared-dir customized-dir *session-vars-file* t scanner username)
      (customize-shared-file 
       shared-dir customized-dir *session-dfs-file* t scanner username)
      (customize-shared-file 
       shared-dir customized-dir *session-workspace-file* nil scanner username)
      )
    ;; successfully saved user session directories have this file, 
    ;; so we have to create it here as well.  
    (with-open-file 
        (p (merge-pathnames *session-save-complete-file* customized-dir)
           :direction :output :if-exists :supersede)
      (format p "Success!")
      )))


(defun customize-shared-file 
       (shared-dir customized-dir file replace-in-package? scanner username)
  (with-open-file (p (merge-pathnames file shared-dir) :direction :input)
    (with-open-file (q (merge-pathnames file customized-dir) 
                       :direction :output :if-exists :supersede)
      (formatt "Creating ~A~%" (merge-pathnames file customized-dir))
      (loop 
       with replacement-string = (s+ username ":")
       as line = (read-line p nil nil nil)
       for count from 0
       until (null line)
       do
       (if (and replace-in-package? (zerop count))
           (output-in-package-form username q)
         (write-line 
          (replace-package-prefixes line scanner replacement-string) q)
         )))))

(defun replace-package-prefixes (line scanner replacement-string)
  (ppcre::regex-replace-all scanner line replacement-string))

            
   
