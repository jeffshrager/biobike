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

;; Author: JP Massar, John Myers.  

(defvar *palette-menu-variables* nil)

(defmacro maybe-create-menu (menuvar force? &body body)
  `(if (and (null ,force?) ,menuvar)
       ,menuvar 
     (setq ,menuvar (progn ,@body))
     ))

;; REVIEW 2010-01-19 <peter@greyhound> -- the unhygenic use of menu-id
;; seems a bit yucky here. Seems like the call to CREATE-PALATTE-MENU
;; and passing the menu-id parameter could be moved into the macro.
(defmacro define-palette-menu (variable color &body body)
  `(progn
     (defvar ,variable nil)
     (setf (get ',variable :palette-menu-color) ,color)
     (setf (get ',variable :palette-menu-id) (new-unique-id :menu-id))
     (setf (get ',variable :palette-menu-creator) 
           (lambda (&optional (force? nil))
             (let ((menu-id (get ',variable :palette-menu-id)))
               (maybe-create-menu ,variable force? ,@body)
               )))
     (push ',variable *palette-menu-variables*)
     ))

(defun instantiate-palette-menu (variable &optional (force? nil))
  (funcall (get variable :palette-menu-creator) force?)
  (initialize-a-palette-menu 
   (symbol-value variable) (get variable :palette-menu-color)
   ))

(defun initialize-palette-menus ()
  (let ((menu-data-and-color-list nil)
        (force? (or *force-vpl-sysinit* (null *vpl-system-initialized?*))))
    (labels ((instantiate-standard-palette-menu (variable force?)
               (funcall (get variable :palette-menu-creator) force?)
               (push 
                (list 
                 (symbol-value variable)
                 (get variable :palette-menu-color))
                menu-data-and-color-list
                ))
             (init-palette (force?)
               (when force? (init-vpl-data-id-hash))
               ;; bundle up the system palette menus and send them all 
               ;; down to the client at once.  
               (instantiate-standard-palette-menu '*vpl-help-menu* force?)
               (instantiate-standard-palette-menu '*vpl-file-menu* force?)
               (instantiate-standard-palette-menu '*vpl-session-menu* force?)
               (instantiate-standard-palette-menu '*vpl-edit-menu* force?)
      ;        (instantiate-standard-palette-menu '*vpl-exit-menu* force?)

               (initialize-many-palette-menus 
                (reverse menu-data-and-color-list))
               
               ;; send all the function palette menus to the client 
               (vdbg "Functions menus...~%")
               (initialize-function-palette-menus force?)
          
               (vdbg "Data menu...~%")
               (instantiate-palette-menu '*vpl-data-menu* force?)
               #+test
               (instantiate-palette-menu '*vpl-test-menu* force?)
               (ulogdbg "Initialized palette...~%")
               ))

      (start-palette-init-message)

      (if force?
          (with-lock (*vpl-system-initialization-lock*)
	    (ulogdbg "Initializing VPL system menus...~%")
	    (init-palette t)     
            (setq *vpl-system-initialized?* t)
            
            ; JKMyers Mar 7 2013.  This must be called after nvpl::*vpl-module-tree* has been set up properly.
            (FORWARD-FUNCALL 'help::jhelp-load-vpl-functions)
            )
	  (init-palette nil)))

    (initialize-favorites-palette-menu)
    (initialize-my-functions-menu)
    (initialize-my-variables-menu)
    (initialize-instance-menus user::*ai* user::*organisms-descriptor* force?)

    (end-palette-init-message)

    ))

(defmethod initialize-instance-menus 
           ((app t) (orgdesc t) &optional (force? nil))
  (declare (ignore force?))
  nil
  )

(defun initialize-function-palette-menus (&optional (force? nil))
  ;; create all the function menus using the module data 
  (let ((menu-data (generate-vpl-module-tree force?)))
    ;; send them all down in a single blit
    (initialize-many-palette-menus 
     (mapcar 
      (lambda (md) (list md palette-function-menus-color))
      menu-data 
      ))))

(define-palette-menu *vpl-help-menu* "red"
  (create-palette-menu
   menu-id
   "HELP!" 
   NIL
   #|
   (list 
    (create-palette-submenu
     "Example Code" 
     ()
     (create-example-code-menu-items)
     ))
	  |#
   ;; options
   (create-menu-items 
    `(("How to get started" get-started-help-operator)
	  ("Ways to get help" get-help-on-help-operator)
	  ("Interface Tutorial" display-vpl-tutorial-operator)
	  ("Examples" get-examples-operator)
      ("BioBIKE - The Manual" get-manual-operator)
      ("Known bugs/workarounds" troubleshooting-operator)
      ("Report problem/Ask for help" report-problem-operator)
      #+obsolete
      ("What's new?" whats-new-operator)
      ))))

(define-palette-menu *vpl-file-menu* "black" 
  (create-palette-menu
   menu-id
   "FILE"
   ;; no submenus for now
   ()
   ;; options
   (create-menu-items 
    `(
      ("Files - yours" browse-user-files-operator)
      ("Files - shared" browse-shared-files-operator)
      ("Files- upload" upload-file-operator)
      ("User contributed stuff" browse-user-contributed-stuff-operator)
    ; ("Frames" browse-system-frames-operator)
      ("Exit" exit-immediately-operator)
      ("Exit and logout" exit-and-logout-operator)
      ("Exit stage left" exit-stage-left-operator)
      ("Announce (all)" announce-to-all-operator)
      ("Announce (VPL users)" announce-to-vpl-users-operator)

      ))))


(define-palette-menu *vpl-edit-menu* "black"
  (create-palette-menu 
   menu-id
   "EDIT"
   ()
   (create-menu-items
    `(("Paste to toplevel" paste-toplevel-function)
      ("New data box" hole-toplevel-function)
      ("Workspace - clear" clear-workspace-operator)
      ("Workspace - reverse order" reverse-workspace-order-function)
      ("Results - clear" clear-results-operator)
      ("Results - reverse order" reverse-results-order-function)
      ("Redraw all" redraw-all-operator)
      ;; ("New function box" function-hole-toplevel-function)
      ))))

(define-palette-menu *vpl-session-menu* "black"
  (create-palette-menu 
   menu-id
   "SESSION"
   ()
   (create-menu-items
    `(("Execution log - current" show-execution-log-operator)
      ("Execution logs - all" browse-user-logs-operator)
      (,(formatn "~A - save" *session-or-workspace*)
       save-user-session-operator)
      (,(formatn "~A - share" *session-or-workspace*)
       save-session-publicly-operator)
      (,(formatn "~As - list" *session-or-workspace*)
       new-restore-user-session-operator)
      ("New session" new-session-operator)
      ("New session (weblistener)" weblistener-page-operator)
      ("Join another session" share-with-another-user-operator)
      ("Preferences" preferences-page-operator)
      ))))

(define-palette-menu *vpl-report-problem-menu* "black"
  (create-palette-menu
   menu-id
   "PROBLEM"
   ()
   (create-menu-items
    `(("Report problem" report-problem-operator))
    )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun create-example-code-menu-items ()
  (setq *example-code-files* nil)
  (let* ((vpl-example-code-directory 
          (translate-simple-lp
           (case cl-user::*vpl-version*
             (1 (error "Obsolete VPL version!"))
             (2 "biol:vpl;example-code;")
             (otherwise 
              (vpl-internal-error 
               "No case for VPL Version ~D" cl-user::*vpl-version*)
              ))))
         (directory-subdirs 
          (remove-if 
           (lambda (p) (string-equal "CVS" (lastelem (pathname-directory p))))
           (remove-if-not 
            'pathname-names-directory?
            (directory-with-subdirs-in-directory-form 
             vpl-example-code-directory
             )))))
    (vdbg "Example directory subdirs: ~A~%" directory-subdirs)
    (when (> (length directory-subdirs) 12)
      (vpl-internal-error "Too many examples!"))
    (setq *example-code-files* directory-subdirs)
    (loop for j from 1 
          for example-subdir in directory-subdirs 
          as operator = (create-example-code-operator j)
          as menu-option-string = 
          (string-capitalize 
           (substitute 
            #\Space #\- (lastelem (pathname-directory example-subdir))))
          collect
          (menu-item menu-option-string operator)
          )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-palette-menu *vpl-data-menu* "blue" 
  (create-palette-menu 
   menu-id
   "DATA"
   (create-organism-submenus cl-user::*organisms-descriptor*)
   ;; standard data options
   (cons 
    '("New data box" hole-toplevel-function)
    (create-menu-items 
     (mapcar 
      (lambda (x) 
        (menu-item 
         (symbol-name x)
         (get-id-and-add-vpl-data-to-hash x)))
      (bbi::vpl-data-menu-variables 
       cl-user::*ai* cl-user::*organisms-descriptor*)
      )))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+test
(defparameter *vpl-test-menu-options* 
  (loop for j from 1 to 2000
        collect
        (list (formatn "~R" j) 'save-user-session-operator)
        ))

#+test
(define-palette-menu *vpl-test-menu* "black" 
  (create-palette-menu
   menu-id
   "TEST"
   ;; no submenus for now
   ()
   ;; options
   (create-menu-items *vpl-test-menu-options*)
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *vpl-data-id-hash* nil)

(defun init-vpl-data-id-hash ()
  (setq *vpl-data-id-hash* (make-hash-table)))

(defun get-datum-from-id (id)
  (multiple-value-bind (datum found?)
      (gethash id *vpl-data-id-hash*)
    (if found? 
        (cond
         ((and (symbolp datum) (null (symbol-package datum)))
            (let ((f (get datum :menu-function)))
              (if f (values (funcall f datum) t) (values datum t))
              ))
         ((frames::isframe? datum)
          (let ((f (#^menu-function datum)))
            (if f (values (funcall f datum) t) (values datum t))
            ))
         (t (values datum t))
         )
      (values datum nil)
      )))

(defun get-id-and-add-vpl-data-to-hash (datum)
  (let ((id (new-unique-id :data-id)))
    (setf (gethash id *vpl-data-id-hash*) datum)
    id
    ))

(defun get-user-variable-from-id (id)
  (gethash id (get wb::*username* :opcodes-to-variables-hash)))

(defun get-id-and-add-user-data-to-hash (symbol)
  (let ((id (new-unique-id :data-id))
        (user-variables-hash (get wb::*username* :opcodes-to-variables-hash)))
    (unless user-variables-hash 
      (vpl-internal-error "Variables hash for ~S not initialized!" 
                          wb::*username*))
    ;; get rid of any other entries that map to this symbol
    (maphash 
     (lambda (key val)
       (when (eq val symbol) (remhash key user-variables-hash)))
     user-variables-hash
     )
    (setf (gethash id user-variables-hash) symbol)
    id
    ))

