;;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: nvpl; -*-

(in-package :nvpl) 

;;; +=========================================================================+
;;; | Copyright (c) 2005 JP Massar, Jeff Elhai, Mark Slupesky, Peter Seibel   |
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

;;; *** Must change this to :vpl when this becomes the real system 

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *vpl-package* (find-package :nvpl)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import (list '*vpl-package*) (find-package :cl-user)))

(defvar *template-symbol->tid* (make-hash-table :test 'eql))
(defvar *tid->template-symbol* (make-hash-table :test 'eql))

;;; Used by the SEXP stuff for symbols.  Probably the WRONG THING...

(defparameter *default-symbol-package-for-xml* (find-package :cl-user))

#+allegro
(defvar *vpl-system-initialization-lock* 
  (mp:make-process-lock :name "VPL system initialization lock"))

(defvar *vpl-system-initialized?* nil)

(defvar *force-vpl-sysinit* nil)

(defvar *snippet-gc-enabled* t)

(defparameter *multiselect-enable* t)

;; These values need to be in sync with the client code.  
;; They will be used for drag and drop.  
(defparameter *palette-id* 10)
(defparameter *workspace-id* 11)
(defparameter *results-id* 12)

(defparameter *useless-menu-id* 5000)

(defun new-useless-menu-id () (incf *useless-menu-id*))

(defparameter *vpl-gc-count-threshold* 50)

(defparameter *disable-selected-box-on-redraw?* t)

;;; See new-bnf.lisp for structure of templates and snippets

(defconstant +hole+ :|hole|)

(defvar *example-code-files* nil)

(defvar *command-origin* nil)
(defvar *copy-successful?* nil)
(defvar *more-client-commands* nil)

(defparameter *comments-file* "comments.txt")
(defparameter *prefs-file* "prefs.lisp")
(defparameter *owner-file* "owner.lisp")
(defparameter *session-vars-file* "variables.lisp")
(defparameter *session-dfs-file* "define-functions.lisp")
(defparameter *session-workspace-file* "workspace.lisp")
(defparameter *session-save-complete-file* "save-complete.info")
(defparameter *vpl-user-variables-not-to-save* '("*MY-CURRENT-DB*"))

#+allegro
(defvar *restore-vpl-user-session-lock-lock*
  (mp:make-process-lock :name "Restore VPL user session lock lock"))

(defparameter *restore-vpl-user-session-lock* 0)

(defparameter *public-sessions-dir* 
  (append-subdir wb::*home-directory* "public-sessions"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *session-or-workspace* "workspace")

(defvar *vpl-workspace-modified?* nil)
(defvar *vpl-redraw-workspace?* nil)
(defvar *vpl-workspace-history-length* 10)

(defvar *vpl-state* nil)
(defvar *current-selected-boxid* nil)
(defvar *modified-snippet* nil)
(defvar *focus-snippet* nil)
(defvar *function-from-palette-to-workspace-toplevel?* nil)
(defvar *current-clipboard-box* nil) 
(defvar *current-wrap-target* nil)
(defvar *vpl-workspace-history* nil)
(defvar *vpl-workspace-history-index* nil)
(defvar *vpl-execution-history-index* nil)

(defvar *save-history?* nil)
(defvar *save-results?* nil)

(defparameter *vpl-state-variables* 
  '(*vpl-state*
    *current-selected-boxid*
    *current-clipboard-box*
    *current-wrap-target*
    ;; *vpl-console-log*
    *vpl-workspace-history* 
    *vpl-workspace-history-index*
    *vpl-execution-history-index*
    ))

;;; UVS = user VPL state

(defstruct uvs ws wsh rs hash)

(defmacro with-ws-node ((var) &body body)
  `(let ((,var (uvs-ws *vpl-state*))) ,@body))

(defmacro with-rs-node ((var) &body body)
  `(let ((,var (uvs-rs *vpl-state*))) ,@body))

(defun workspace-root-node () (uvs-ws *vpl-state*))

(defun results-root-node () (uvs-rs *vpl-state*))

(defun workspace-root-node? (s) (eq s (workspace-root-node)))

(defun results-root-node? (s) (eq s (results-root-node)))

(defun workspace-histories () (uvs-wsh *vpl-state*))

(defun toplevel-ws-snippet? (s)
  (eq (snippet-parent s) (workspace-root-node)))

(defun toplevel-rs-snippet? (s) 
  (eq (snippet-parent s) (results-root-node)))

(defun toplevel-snippet? (s)
  (or (toplevel-ws-snippet? s) (toplevel-rs-snippet? s)))

(defun root-snippet? (s)
  (or (workspace-root-node? s) (results-root-node? s)))

#||

The above data structure represents a user's VPL state.  

WS represents the user's workspace.  It is an object of type
TOPLEVEL-WS-SNIPPET.  Its children are the toplevel snippets in the
user's current workspace.

WSH is the user's workspace history (a list of TOPLEVEL-WS-SNIPPETs).
It contains the user's prior workspace states, up to a certain 
limit (currently 10).

RS represents the user's results area.  It is an object of type
TOPLEVEL-RS-SNIPPET.  Its children are each TOPLEVEL-OUTPUT-SNIPPETs.

We don't have an RSH. (We don't maintain snapshots of the results, we
simply maintain a list of previous results, each result being one of
the snippets).

HASH is the user's snippet hash-table we talk about above.

||#

(defun initialize-user-vpl-state (&optional (force? t))
  (when (or force? (null *vpl-state*))
    (setq *vpl-state* (make-uvs :ws nil :wsh nil :rs nil))
    (initialize-user-snippet-hash t)
    (setf (uvs-ws *vpl-state*) (make-instance 'toplevel-ws-snippet))
    (setf (uvs-rs *vpl-state*) (make-instance 'toplevel-rs-snippet))    
    ))

(defmacro with-user-vpl-session-info-bound ((&rest stuff) &body body)
  (declare (ignore stuff))
  `(with-saved-variables-values (wb::*vpl-session-info*)
     ,@body
     ))

(defun vpl-sessionid-from-sessionid (sessionid)
  (keywordize (s+ "VPL-" (symbol-name sessionid))))  

(defun create-vpl-session-info () 
  (setq wb::*vpl-session-info* (vpl-sessionid-from-sessionid wb::*sessionid*))
  (initialize-saved-variables wb::*vpl-session-info* *vpl-state-variables*))
 
(defun not-modified! () 
  (setq *vpl-workspace-modified?* nil)
  (setq *vpl-redraw-workspace?* nil))

(defun redraw! () (setq *vpl-redraw-workspace?* t))

(defmacro with-workspace-modified-and-redrawn ((&rest options) &body body)
  (declare (ignore options))
  ;; Deep copy the current workspace
  `(let ((old-workspace (copy-workspace))
         (*vpl-workspace-modified?* t)
         (*vpl-redraw-workspace?* t)
         (*disable-selected-box-on-redraw?* t)
         (*function-from-palette-to-workspace-toplevel?* nil)
         ;; make sure WRAP is disabled if any change occurs to the workspace
         ;; and it was enabled at the start of the command.  
         (wrap-enabled? *current-wrap-target*)
         )
     ;; All modifications take place in the original
     ;; (vlog "Before body...~%")
     (progn ,@body)
     ;; (vlog "after body...~%")
     ;; If the body returns, the modifications, if any, were successful.
     ;; Unless specifically instructed otherwise,
     ;;   -- redraw the workspace,
     ;;   -- store the saved, original workspace in place of the now-modified
     ;; one, and move the modified workspace in the workspace history to
     ;; be at the head of the workspace stack and make it the current workspace.
     (when (or *vpl-redraw-workspace?* *vpl-workspace-modified?*)
       (when wrap-enabled? (setq *current-wrap-target* nil)))
     (when *vpl-redraw-workspace?* (maybe-redraw-everything-in-workspace))
     (when *vpl-workspace-modified?* 
       (add-to-workspace-history old-workspace))
     ;; (treeprint-snippet (workspace-root-node))
     ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *vpl-console-log* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro if-vpl-debug (&body body) `(when *vpl-console-log* ,@body))

(defun vdbg (&rest format-args)
  (if-vpl-debug 
   (let ((msg (apply 'format nil format-args)))
     (format t "~A" msg)
     (when (wb::log-message-needs-newline? msg) (terpri))
     (force-output)
     )))

(defun vdbg! (&rest format-args)
  (let ((*vpl-console-log* t))
    (apply 'vdbg format-args)
    ))

(defun ppvdbg (form) 
  (when *vpl-console-log* (pprint form *standard-output*) (terpri)))

(defun syslog (&rest format-args)
  (apply 'wb::log-system-event format-args))

(defun ulog (&rest format-args)
  (apply 'wb::log-user-event format-args))

(defun ulogdbg (&rest format-args)
  (apply 'wb::log-user-event format-args)
  (apply 'vdbg format-args)
  )

(defun ulogdbg! (&rest format-args)
  (apply 'wb::log-user-event format-args)
  (apply 'vdbg! format-args)
  )

(defun syslogdbg (&rest format-args)
  (apply 'syslog format-args)
  (apply 'vdbg! format-args)
  )

(defun usyslog (&rest format-args)
  (apply 'syslog format-args)
  (when wb::*sessionid* (apply 'ulog format-args))
  )

(defun usyslogdbg (&rest format-args)
  (apply 'usyslog format-args)
  (apply 'vdbg format-args)
  )
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *vpl-opcode-hash* (make-hash-table :test 'eql))

(defmacro defun-opcode (name (argname &rest argspecs) &body body)
  (let ((valid-specifiers '(&rest &key &optional))
        (docstring (first body)))
    (when (and argspecs (not (member (first argspecs) valid-specifiers)))
      (error "Argspecs must begin with one of ~A !" valid-specifiers))
    (if (stringp docstring) 
        (pop body)
      (setq docstring nil)
      )
    `(progn 
       (maybe-add-new-opcode ',name)
       ,@(when docstring 
           `((setf (get ',name :defun-opcode-docstring) ,docstring)))
       #+not-used
       (pushnew ',name *all-vpl-operator-functions*)
       (defun ,name (,argname ,@argspecs)
         ,@body
         ))))

(defun maybe-add-new-opcode (operator-name)
  (unless (operator->opcode operator-name nil)
    (add-new-opcode operator-name)
    ))

(defun add-new-opcode (operator-name)
  (let ((new-opcode (new-unique-id :operator-id)))
    (setf (gethash new-opcode *vpl-opcode-hash*) operator-name)
    (setf (gethash operator-name *vpl-opcode-hash*) new-opcode)
    new-opcode
    ))

#+not-used
(defun redefine-vpl-opcodes ()
  (setq *vpl-opcode-hash* (make-hash-table :test 'eql))
  (loop for operator in *all-vpl-operator-functions* 
        do 
        (add-new-opcode operator)
        ))

(defun opcode->operator (opcode &optional (error-if-not-found? t))
  (let ((operator (gethash opcode *vpl-opcode-hash*)))
    (if (or operator (null error-if-not-found?))
        operator 
      (error "Operator for opcode ~D not found!" opcode)
      )))

(defun operator->opcode (operator &optional (error-if-not-found? t))
  (let ((opcode (gethash operator *vpl-opcode-hash*)))
    (if (or opcode (null error-if-not-found?))
        opcode 
      (error "Opcode for operator ~S not found!" operator)
      )))

(defun show-vpl-opcodes ()
  (formatt "~%~%;; VPL opcodes and associated operators ~%~%")
  (let ((data
         (sort 
          (remove-if  
           (lambda (x) (not (symbolp (first x))))
           (hash-table-contents *vpl-opcode-hash*))
          '<
          :key 'second
          )))
    (loop for (key value) in data do 
          (when (symbolp key) (formatt "  ~5D   ~S~%" value key)))
    (terpri)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(flet ((clv (f) 
         (let ((version 
                (ecase cl-user::*vpl-version*
                  ;; (1 "vpl")
                  (2 "vplcode")
                  ;; (3 "vpl3")
                  )))
           (cl (s+ "biol:" version ";" f) t)
           )))
  (defun cvco () (clv "commands.lisp"))
  (defun cvbx () (clv "boxes.lisp"))
  (defun cvop () (clv "operators.lisp"))
  (defun cvsd () (clv "snippet-defs.lisp"))
  (defun cvsc () (clv "snippet-creation.lisp"))
  (defun cvtm () (clv "templates.lisp"))
  (defun cvtc () (clv "template-code.lisp"))
  (defun cvcd () (clv "clear-and-delete.lisp"))
  (defun cvsn () (clv "snippet-menus.lisp"))
  (defun cvcg () (clv "codegen.lisp"))
  (defun cvsw () (clv "save-workspace.lisp"))
  (defun cvdf () (clv "defs.lisp"))
  (defun cvpm () (clv "palette-menus.lisp"))
  (defun cvmm () (clv "module-menus.lisp"))
  (defun cvrp () (clv "repl.lisp"))
  (defun cvhp () (clv "help.lisp"))
  (defun cvex () (clv "execute.lisp"))
  (defun cvrh () (clv "result-handlers.lisp"))
  (defun cvcp () (clv "cut-and-paste.lisp"))
  (defun cvsr () (clv "surround.lisp"))
  (defun cvuo () (clv "utility-operators.lisp"))
  (defun cvss () (clv "shared-sessions.lisp"))
  (defun cvdd () (clv "drag-and-drop.lisp"))
  (defun cvib () (clv "input-boxes.lisp"))
  (defun cvscm () (clv "send-client-messages.lisp"))
  (defun cvrcm () (clv "receive-client-messages.lisp"))
  (defun cvus () (clv "user-sessions.lisp"))

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           
(defparameter loop-tag-color "#0000ff")
(defparameter loop-conditional-clause-tag-color "#00AA66")
(defparameter curly-bracket-color "#FF2299")
(defparameter ref-bracket-color "#FF2299")
(defparameter optional-tag-color "#0000ff")
(defparameter function-name-color "#000000")
(defparameter define-function-name-color "#000000")
(defparameter macro-name-color "#000000")
(defparameter define-function-tag-color "#FFAA11")
(defparameter default-literal-color "#FF8811")
(defparameter loop-toplevel-constituent-color "#9933AB")
(defparameter define-function-token-arg-color "#8B008B")
(defparameter keyword-name-color "#000000")
(defparameter flag-name-color "#000000")
(defparameter equal-sign-color "#0000FF")

(defparameter palette-help-menus-color "red")
(defparameter palette-function-menus-color "green")
(defparameter palette-data-menus-color "blue")
(defparameter palette-favorites-menu-color "blue")
(defparameter palette-tools-menus-color "black")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro set-wrap-function (symbol wrap-function)
  `(setf (get ',symbol :wrap-function) ',wrap-function))


(set-wrap-function bbl::if-true insert-wrapee-into-if-true-style-snippet)
(set-wrap-function bbl::if-false insert-wrapee-into-if-true-style-snippet)

(set-wrap-function bbl::when insert-wrapee-into-when-style-snippet)
(set-wrap-function bbl::unless insert-wrapee-into-when-style-snippet)


(defmacro set-hole-locator-function (symbol locator-function)
  `(setf (get ',symbol :hole-locator-function-for-wrap) ',locator-function))

(set-hole-locator-function
 bbl::if-true find-wrapee-for-if-true-style-snippet)
(set-hole-locator-function
 bbl::if-false find-wrapee-for-if-true-style-snippet)

(set-hole-locator-function bbl::when find-wrapee-for-when-stype-snippet)
(set-hole-locator-function bbl::unless find-wrapee-for-when-stype-snippet)

(set-hole-locator-function bbl::loop find-wrapee-for-loop-snippet)
(set-hole-locator-function bbl::for-each find-wrapee-for-loop-snippet)

(set-hole-locator-function bbl::assign find-wrapee-for-assign-snippet)        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun user-temp-vpl-dir ()
  (append-subdir (wb::visitor-directory wb:*username*) "vpltmp"))

(defun user-temp-vpl-dir-url (file)
  (wb::publish-path-for-file-in-viewable-subdir file))

(defun user-temp-vpl-dir-lisp-listing-url (file)
  (wb::make-wb-lisp-file-listing-url 
   :pathname (wb::canonicalize-pathname-namestring (namestring file))))

(defmacro with-html-to-stream (stream &body body)
  `(net.html.generator:html-stream ,stream ,@body))

(defmacro with-file-in-user-code-dir 
          ((code-dir-var filepath-var filename) &body body)
  (let ((userdir-var (gensym "USERDIR-")))
    `(let (,userdir-var ,code-dir-var ,filepath-var)
       (declare (ignorable ,code-dir-var))
       (setq ,userdir-var (wb::visitor-directory wb:*username*))
       (setq ,code-dir-var (append-subdir ,userdir-var "vplcode"))
       (handler-case (ensure-directories-exist ,code-dir-var)
         (error 
          (c)
          (vpl-internal-error 
           "Cannot access directory ~S.  Actual error: ~A" 
           (namestring ,code-dir-var) c
           )))
       (setq ,filepath-var (merge-pathnames ,code-dir-var ,filename))
       ,@body
       )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun numeric-opcode-operator
       (generic-symbol operator-name-generator number)
  (let ((opname (funcall operator-name-generator number)))
    (unless (fboundp opname)
      (vdbg "Creating new operator ~A~%" opname)
      (eval `(defun-opcode ,opname (sid) (,generic-symbol sid ,number))))
    opname
    ))

(defun google ()
  (wb::make-url :path "http://www.google.com" :display-string "Google Search"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (and (find-package :nvpl)
               (find-package :vpl)
               (not (eq (find-package :vpl) (find-package :nvpl))))
    (import (list (intern "GOOGLE" *vpl-package*)) :bbl)
    (export (list (intern "GOOGLE" *vpl-package*)) :bbl)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro with-clipboard-saved (&body body)
  (let ((saved-var (gensym "SAVED-CLIPBOARD-")))
    `(let ((,saved-var *current-clipboard-box*))
       (unwind-protect
           (progn ,@body)
         (setq *current-clipboard-box* ,saved-var)
         ))))

(defmacro with-snippet-from-id 
          ((snippet-var location-var id) &body body)
  `(multiple-value-bind (,snippet-var ,location-var)
       (find-snippet-in-workspace-or-output-history ,id nil)
     ,@body
     ))

(defmacro with-snippet-from-id-in-workspace
          ((snippet-var 
            id 
            &optional 
            (results-error-message 
             "Cannot do that operation using results node!"
             ))
           &body workspace-body
           )
  (let ((where-var (gensym "WHERE-"))
        (id-var (gensym "ID-")))
    `(let ((,id-var ,id))
       (with-snippet-from-id 
        (,snippet-var ,where-var ,id-var)
        (ecase ,where-var
          (:workspace ,@workspace-body)
          (:results (vpl-user-error ,results-error-message))
          (:neither 
           (vpl-internal-error
            (one-string-nl
             "The snippet id being operated on, ~A,"
             "either does not exist or is no longer in the workspace or"
             "results areas.")
            ,id-var           
            )))))))

(net.aserve:publish-directory 
 :prefix "/vplfiles"
 :destination (s+ user::*source-root* "vpl/tutorial"))

