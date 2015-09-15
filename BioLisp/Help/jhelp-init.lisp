;;; -*- Package: help; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :help)

;;; +=========================================================================+
;;; | Copyright (c) 2011 John K. Myers           |
;;; |                                                                         |
;;; |                                                                         |
;;; | THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,         |
;;; | EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF      |
;;; | MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  |
;;; | IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY    |
;;; | CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,    |
;;; | TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE       |
;;; | SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                  |
;;; +=========================================================================+

;;; Authors:  John K. Myers
;; Feb 6 '12   J.Myers.  Hacked call to jhelp-load-vpl-symbols into a forward reference.
;; Jul 12 '12  J.Myers.  Current theory is to try to delete/expunge the Function items from the Function hash
;;             that are duplicated inside jhelp-load-vpl-symbols, instead of trying to block them in the first place...
;;             see jhelp-handle-box-wksp-req.lisp...ooo.  Can't do that.  Entering a symbol digests it into the barrels.
;;             So, we have to put special checks in when building Functions and Symbols to avoid duplicated entries.
;;             These are going into jhelp-load-oldhelp.lisp.
;; Jul 13 '12  Loading order conflict on package bbi:: now required to run (jhelp-load-oldhelp).  Circumventing with
;;             (forward-package-funcall :bbi .

(defvar *jhelp-init* T)

(defun jhelp-init ()

	;;Reset the JHelp Engine down to the metal.
  (clear-JHelp-Engine)

	;; Load the 7 or 8 various hashtables of the old help system.
  (jhelp-load-oldhelp)

;;  (jhelp-load-vpl-symbols)    ;;moved forward.   ...This source is now in jhelp-handle-box-wksp-req, 'cause of nvpl:.

	;; Load the complex JPages and nested logical forms of the new system.  Works.
(when *jhelp-use-jpages*
 (jhelp-load-jhelp))		;;Requires jhelp-pages.lisp to be fleshed out, and non-zombie complex boxes.

	;; Delayed run-time loading of magic for handling boxes being instantiated in the workspace.
	;; Also where (jhelp-load-vpl-symbols) is defined, which needs to come up way last.
  ;; Sure hope this is synchronous. 'Cause we need to use it next command.
  (cl-user::load-system* "websrc:Help;" '("jhelp-handle-box-wksp-req")) ;;Ugly but currently needed.

  (setq *jsymbol-count* 0)

	;; Load all of the VPL symbols into special first-level JPage objects.   Works well, start to finish.
;;  So.  This was causing problems the first time it was compiled.
;;  Here's some voodoo to fix it.
; Taken out March 7 2013.  We need to call jhelp-load-vpl-functions now, way forward in ...
;    (forward-package-funcall :help
;  :jhelp-load-vpl-symbols)  ;;Now defined in jhelp-handle-box-wksp-req, because of nvpl.  Call after loading it.

  (when *jhelp-use-salience* 
    (jhelp-weight-by-salience))
)



(defun jhelp-weight-by-salience  ()
  ;;this has been rewritten in runtime so we can switch live.

)



#|  ;;DEPRECATED.  DELETE SOON.
(defun create-help-barrels ()
  (if *documentation*
      (loop for type being the hash-keys in *documentation* using (hash-value table) do
            (terpri)(terpri)(terpri)
            (format t "~a => ~a~%========~%" type table)
            ;(unless (eq type 'symbol-doc)
(when (eq type 'topic)
 
              (loop for name being the hash-keys in table using (hash-value thing) do
                    (format t "~s => ~s~%" name thing)
                    (format T "   name: |~s|  docstring: |~s|~%" (name thing) (docstring thing))
                    (format T "   text: |~s|~%" (text thing))
                    (format T "   keywords: |~s|~%" (keywords thing))
                    (terpri)
              )
            )
       )
    (progn
      nil
      )
    )
)

(defvar *help-barrels* (create-help-barrels))

|#


