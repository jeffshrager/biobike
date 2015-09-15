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

(defparameter *vpl-reaper-process* nil)
(defparameter *crude-size-threshold* 100000)
(defparameter *kill-vpl-reaper-process* nil)
;; In minutes
(defparameter *vpl-reaper-object-duration* 15)
;; In seconds
(defparameter *vpl-reaper-process-sleep-duration* 60)

(defparameter *state-id-enabled* t)

(defun handle-hello ()
  (let ((*state-id-enabled* nil))
    (vdbg "In HELLO")
    (syslog "RESPONDING TO 'HELLO' COMMAND:~%")
    (syslog "*Channel* = ~A~%" *channel*)
    (initialize-vpl-reaper-process)
    ;; initialize session
    (kill-all-session-processes)
    (reset-shared-mode)
    (vpl-initialize-server)
    (setq *vpl-workspace-modified?* nil)
    (clear-weblistener-history)
    (setf (get wb::*sessionid* :vpl-channel) *channel*)
    (setf (get wb::*sessionid* :session-state-id) 0)
    (vdbg "'HELLO' COMMAND FINISHED.~%")
    (syslog ">>> STARTING VPL SESSION FOR USER ~A~%" wb::*username*)))

(defun kill-all-session-processes ()
  ;; kill-execution-process returns nil if no processes on execution queue
  ;; otherwise it kills the process at the beginning of the queue.
  ;; when the process is killed, it takes itself off the queue.
  (loop while (kill-execution-process :show-status? nil)
        do
        (sleep 1)))

(defun initialize-vpl-reaper-process ()
  (unless *vpl-reaper-process*
    (syslog "Starting VPL reaper process.~%")
    (setq 
     *vpl-reaper-process*
     (wb::run-function-as-process 
      "vpl-reaper-process"
      (lambda ()
        (loop 
         for j from 0 
         until *kill-vpl-reaper-process*
         do
         (sleep *vpl-reaper-process-sleep-duration*)
         (if (plusp *restore-vpl-user-session-lock*)
             (syslog "VPL reaper process inhibited because of lock...")
           (progn
             ;; stop it from cluttering up log 
             (when (zerop (mod j 10))
               (syslog "VPL reaper process run notification.~%"))
             (handler-case 
                 (reap-vpl-user-results)
               (error 
                (c)
                (syslogdbg "VPL reaper process died!  Actual error: ~A~%" c)
                (return)
                )))))
        (syslog "VPL reaper process terminated!~%")
        )))))

(defun reset-shared-mode ()
  (vwhen (previous-channel (get wb::*sessionid* :vpl-channel))
    (unshare-channel-from-all-other-users previous-channel))
  (setf (get wb::*sessionid* :session-menu-ids) nil)
  (setf (get wb::*sessionid* :slave-shared-mode) nil)
  (when (get wb::*sessionid* :other-user-channels) 
    (vpl-clear-other-user-channels)
    ))

(defun clear-weblistener-history ()
  ;; Make sure history doesn't hang around across refreshes.
  ;; If you happen to have a weblistener session attached to your VPL session,
  ;; you'll lose the history in your weblistener session as well.  
  (setq *** nil) (setq ** nil) (setq * nil)
  (setq +++ nil) (setq ++ nil) (setq + nil)
  (setq /// nil) (setq // nil) (setq / nil)
  (setq wb::*current-repl* nil)
  (setq wb::*in-history* nil)
  (setq wb::*out-history* nil)
  )

(defun vpl-initialize-server ()

  (syslog "Initializing Server.~%")
  (initialize-vpl-browser-title)
  (send-json "clear-palette")
  (initialize-palette-menus)
  (enable-new-hole-mode)
  (syslog
   "Initializing user ~A, session ~A ~%" wb::*username* wb::*sessionid*)
  (vdbg "Resetting CSB and CCB variables!~%")
  (setq *current-selected-boxid* nil)
  (setq *current-clipboard-box* nil)
  (setf (get wb::*sessionid* :vpl-gc-count) 0)
 
  ;; We want to save the old workspace history when the user does a refresh, 
  ;; just until he takes some action other than UNDO.  If he does do an UNDO,
  ;; as the very first operation, we want to be able to restore the user's
  ;; workspace history from before his refresh.

  ;; So, first we store the state of his history onto his session property 
  ;; list, then we reinitialize his VPL state, blowing away his workspace, 
  ;; history, results and snippet hash.  Then we add back to the snippet hash
  ;; all the snippets in his now-saved history, so they won't get GCed away.
  ;; Note that this only works as long as we don't do a snippet-gc 
  ;; immediately, which there is no purpose in doing, since the snippet hash
  ;; has been cleared.  Once the user has taken some action other than UNDO, 
  ;; when a snippet-gc does take place the saved workspace history on the 
  ;; property list will no longer be valid; it should be obliterated so that
  ;; the lisp GC can reclaim those snippets.

  (let ((previous? (not (null *vpl-workspace-history-index*))))

    (setf (get wb::*sessionid* :workspace-history-index)
          *vpl-workspace-history-index*)
    (let ((wsh (when *vpl-state* (uvs-wsh *vpl-state*)))
          (rsh (when *vpl-state* (uvs-rs *vpl-state*))))
      (setf (get wb::*sessionid* :workspace-history) wsh)
      (setf (get wb::*sessionid* :stored-results) rsh)
      (initialize-user-vpl-state)
      ;; Add back workspace history and results snippets to hash;
      ;; they'll get snippet-GCed once the user starts to do something else.
      (loop for s in wsh do (add-snippet s))
      (when rsh (add-snippet rsh)))
  
    (when previous?
      (show-status "<-- Click left arrow to restore"))
    (setq *vpl-workspace-history-index* nil)
    (setq *vpl-execution-history-index* 0)
  
    (let ((userdir (user-temp-vpl-dir)))
      (usyslog "Creating user VPL directory ~A~%" userdir)
      (ensure-directories-exist userdir)
      (usyslog "Purging user VPL directory...~%")
      (purge-vpl-directory userdir)
      )
  
    ))

(defun purge-vpl-directory (userdir)
  (file-operate 
   (list userdir)
   (lambda (file-pathname) 
     (let* ((name (pathname-name file-pathname))
            (sessionid (keywordize (lastelem (string-split name #\-))))
            (active-sessions 
             (gethash wb:*username* wb::*user->sessionids-ht*)))
       (null (member sessionid active-sessions))
       ))
   (lambda (file-pathname)
     (ignore-errors (delete-file file-pathname))
     (vdbg "Deleted VPL dir file: ~A~%" (namestring file-pathname))
     )))

(defun reap-vpl-user-results 
       (&key (crude-size-threshold *crude-size-threshold*))
  (let ((users (copy-list wb::*logins*))
        (*crude-size-threshold* crude-size-threshold))
    (loop for user in users do
          (let ((sessions (gethash user wb::*user->sessionids-ht*)))
            (loop for session in sessions do
                  (vpl-purge-user-session-results session)
                  )))))

(defun vpl-purge-user-session-results (session)
  (let* ((vpl-session-id (vpl::vpl-sessionid-from-sessionid session))
         (vpl-session-data
          (gethash vpl-session-id utils::*saved-variables-hash-table*)))
    (when vpl-session-data
      (let* ((variables (first vpl-session-data))
             (data (second vpl-session-data))
             (vpl-state-variable-pos (position '*vpl-state* variables))
             (vpl-state-object (nth vpl-state-variable-pos data))
             (toplevel-results-object (uvs-rs vpl-state-object))
             (user-result-boxes (snippet-children toplevel-results-object))
             (current-time (get-universal-time))
             (large-object-duration-in-seconds 
              (* *vpl-reaper-object-duration* 60)))
        (loop for result-box in user-result-boxes 
              as timestamp = (get-snippet-property result-box :timestamp)
              as crude-size = (get-snippet-property result-box :crude-size)
              as first-value-box = (first (snippet-children result-box))
              as message = "RESULT PURGED! (Too big)"
              as message-string = "\"RESULT PURGED! (Too big)\""
              do
              ;; we'll kill the beast if 
              ;;   1) it's too big, and 
              ;;   2) it came into existence at least N seconds ago, 
              ;;      where N = large-object-duration-in-seconds
              (when (and (> crude-size *crude-size-threshold*)
                         (> (- current-time timestamp)
                            large-object-duration-in-seconds
                            ))
                (syslog "Purging sessionid ~A snippet ~A~%" session result-box)
                (set-snippet-property result-box :crude-size 1)
                (set-snippet-property result-box :result-type :success)
                (set-snippet-property result-box :error-condition nil)
                (set-snippet-property result-box :printout nil)
                (set-snippet-property result-box :values (list message))
                (set-snippet-property result-box :nvalues 1)
                (set-snippet-property 
                 result-box 
                 :value-strings (list message-string))
                (setf (snippet-children result-box) (list first-value-box))
                (setf (snippet-value first-value-box) message-string)
                ))))))



#||

(let ((cs (get-snippet-property result-box :crude-size)))
                (print (list 'cs cs))
                (when (null cs) (describe result-box))
                (get-snippet-property result-box :crude-size))

||#                              