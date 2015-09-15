;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: nvpl; -*-

(in-package :nvpl)

;;; +=========================================================================+
;;; | Copyright (c) 2006 John Myers, JP Massar                                |
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

(defparameter *shared-sessions-enabled* t)

(defun send-json (type &rest rest)
  (let ((session-id 
         (if *state-id-enabled*
             `("sessionID" ,(incf (get wb::*sessionid* :session-state-id))))))
    (let ((*state-id-enabled* nil))
      (send-out `(:vpl-json ,(json `(:type ,type ,@session-id ,@rest)))))))

(defun send-json-without-sessionid (type &rest rest)
  (let ((session-id `("sessionID" 0)))
    (let ((*state-id-enabled* nil))
      (send-out `(:vpl-json ,(json `(:type ,type ,@session-id ,@rest))))
      )))

(defun send-out (message)
  (when *state-id-enabled* 
    (setq message (hack-message-to-add-state-id message)))
  (vdbg "Message out: ~S~%" message)
  (if *shared-sessions-enabled* 
      (let ((channels (channels-to-send-to wb::*sessionid* *channel*)))
        (send-message-to-channels channels message))
    (send *channel* message)
    ))

(defun hack-message-to-add-state-id (message)
  (let ((current-state (incf (get wb::*sessionid* :session-state-id))))
    `(,(first message) ,current-state ,@(rest message))
    ))

(defun send-message-to-channels (channels message)
  (loop for channel in channels do (send channel message)))

(defun channels-to-send-to (sessionid incoming-channel)
  (let ((personal-channels (get sessionid :shared-session-channels))
        (other-user-channels (get sessionid :other-user-channels)))
    (cond 
     ((null personal-channels)
      (cons incoming-channel other-user-channels))
     ;; Standard case -- a single channel
     ((and (null (cdr personal-channels)) (null other-user-channels))
      (unless (eq incoming-channel (first personal-channels))
        (vpl-internal-error "Only channel is not incoming channel!"))
      personal-channels)
     ;; Session shared with other users, but not with self
     ((null (cdr personal-channels))
      (cons (first personal-channels) other-user-channels))
     ;; Session shared with self, but not other users
     ((and (cdr personal-channels) (null other-user-channels))
      (cons incoming-channel (remove incoming-channel personal-channels)))
     ;; General case -- shared with self and other users
     (t
      (append
       (cons 
        incoming-channel
        (remove incoming-channel personal-channels)
        )
       other-user-channels
       )))))


;;; (:list (:session-id "foo123") (:session-counter "24")) ...
(defun create-share-request-message (his-sessionid)
  `(:vpl "sharerequest" 
    (:list 
     (:session-id ,(string his-sessionid))
     ;; dummy session counter
     (:session-counter "1")
     )))

;; (:VPL "palettemenumouseclick" (:SESSION-ID "MASSAR85745") (:NUMBER "1589"))

(defun handle-share-request ()
  (vdbg "In handle-share-request...~%")
  (syslog "~S Receiving shared session request" wb::*sessionid*)
  ;; We want to make the screen attached to OTHER-CHANNEL
  ;; look like the screen (the internal state) of the sessionid
  ;; whose info is currently instantiated.
  ;; The browser at the other end of OTHER-CHANNEL has a 
  ;; VPL client running, and so already has a palette,
  ;; workspace, and results area.  
  ;; We need to make that palette area look like the currently
  ;; instantiated palette area, that workspace look like the currently
  ;; instantiated workspace, and similarly for the results area.  
  (let ((*vpl-system-initialized?* nil))
    (not-modified!)
    (remove-all-palette-menus)
    (initialize-palette-menus)
    (redraw-everything-in-workspace)
    (redraw-everything-in-results)
    (when *current-selected-boxid* 
      (flashing-hilight-box *current-selected-boxid*)
      )
    (pushnew *channel* (get wb::*sessionid* :other-user-channels))
    ))

;; When a user who has been in share mode does a browser refresh,
;; he gets a new channel.  But the old channel is still on the list
;; of channels being shared by the session of the user that he was sharing. 
;; Since we don't know who he was sharing with or that sesssionid,
;; we loop through all user sessions and potentially remove the channel
;; from the :other-user-channels lists for every session.  
(defun unshare-channel-from-all-other-users (channel)
  (maphash 
   (lambda (userid sessionids) 
     (declare (ignore userid))
     (loop for sessionid in sessionids do
           (setf (get sessionid :other-user-channels)
                 (remove channel (get sessionid :other-user-channels))
                 )))
   wb::*user->sessionids-ht*
   ))

#+obsolete
(defun vpl-clear-other-user-channels ()
  (let ((other-channels (get wb::*sessionid* :other-user-channels)))
    (loop for channel in other-channels 
          do
          (send channel `(:vpl (:clear-workspace)))
          (send channel `(:vpl (:clear-results)))
          )))

(defun vpl-clear-other-user-channels ()
  (let ((other-channels (get wb::*sessionid* :other-user-channels)))
    (loop for channel in other-channels 
          do
          (let ((*channel* channel))
            (send-json "clear-workspace")
            (send-json "clear-results")
            ))))
            