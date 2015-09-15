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

;; Author: JP Massar


#||

How to do autosave.

Need to have a timestamp for last activation of session, not just 
last execution.
 -- This seems to work. In receive-client-message.lisp in 
    vpl-json-decode-message-and-execute-it

Need to have status of autosave -- NIL, :invalid, :valid
  NIL means no autosave yet.
  INVALID means activity occurred after autosave.
  VALID means autosave exists and is current.
 -- Implemented. In receive-client-message.lisp in
    vpl-json-decode-message-and-execute-it

Need variable to disable mechanism. Done.

A process started by vpl startup which wakes up every minute (two minutes?)
and finds the session with the maximum time since last activation which
has an invalid or NIL autosave status.

If max is > a certain amount (five minutes?) then
  Critical region where it places lock on property list
  Issue command to do session save similar to below, with a session name
    dynamically bound.
  Maybe need timeout to session save command to insure process
    never gets stuck?
  Process has unwind protect to catch any error and undo lock.
Else nothing.
Then it puts itself to sleep and repeats.

VPL threads must check lock.  If locked, sleep for 1 second, try again.
Repeat a couple times.  If failure report to user and give up. Report
to sysadmin and log.

||#

#||

NOT IMPLENETED YET!


(defparameter *enable-workspace-autosave* t)

(defparameter *autosave-wakeup-every* 180)

(defvar *autosave-name* nil)

(defun autosave-process-function ()
  (loop 
   while t
   do
   (when *enable-workspace-autosave*
     (let ((session-to-save (find-session-to-autosave))
           (saved? nil))
       (unwind-protect 
           (handler-case 
               (progn
                 (do-the-session-save session-to-save)
                 (setq saved? t))
             (error 
              (c)
              (setq saved? nil)
              (syslog 
               (formatn 
                "Error autosaving ~A, Actual error: ~A"
                session-to-save c
                ))))
         (when saved?
           (setf (get session-to-save :last-activity-time) (get-universal-time))
           (setf (get session-to-save :autosave-status) :saved)
           ))))
   (sleep *autosave-wakeup-every*)
   ))

(defun find-session-to-autosave ()
  nil
  )

(defun do-the-session-save (session-to-save)
  (let ((*autosave-name* (formatn "_Autosave-~A" session-to-save)))
  nil
  ))

  
||#