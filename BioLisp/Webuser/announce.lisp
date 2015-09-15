;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: wb; -*-

(in-package :wb)

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

;;; Interface functions:  ANNOUNCE, CLEAR-ANNOUNCEMENTS, DELETE-ANNOUNCEMENT


(defvar *weblistener-system-messages* nil)
(defvar *event-time*)

(defvar *message-output-hook-function* nil)


(defstruct sysmsg
  format
  to
  from
  to-window-type
  how-many-times
  how-often
  expires
  id
  color
  ;; A list of the form (user count last-message-printed-time)
  current-users
  ;; Just a list of logins, like TO
  expired-users
  )


(defun canonicalize-announce-message (message)
  (cond
   ((listp message) message)
   ((stringp message) (list :text message))
   (t (error "illegal message format"))
   ))

(defun canonicalize-announce-to-list (to)
  (cond
   ((eq to :all) to)
   ((symbolp to) (canonicalize-announce-to-list (list to)))
   ((listp to) (mapcar 'string-downcase (mapcar 'string to)))
   (t (error "Bad to list."))))

(defun message-printable? (msg user)
  (let ((ct (get-universal-time))
        (ep (sysmsg-expires msg))
        (hmt (sysmsg-how-many-times msg))
        (ho (sysmsg-how-often msg))
        (twt (sysmsg-to-window-type msg))
        )
    ;; When the user is a user to whom this message should go to...
    (when (or (eq (sysmsg-to msg) :all) 
              (member user (sysmsg-to msg) :test 'equal))
      ;; When the message hasn't expired...
      (when (or (null ep) (> ep ct))
        ;; As long as the user hasn't seen the message already enough times...
        (when (or (null hmt) (not (message-limit-exceeded? msg user)))
          ;; and the time interval specified between message printings is
          ;; satisfied
          (when (or (null ho) (how-often-satisfied? msg user))
            ;; and the window type is appropriate 
            (case twt
              (:all t)
              (:vpl wb::*vpl-executing?*)
              (:weblistener (not wb::*vpl-executing?*))
              )))))))

(defun message-deletable? (msg)
 (let ((ct (get-universal-time))
        (ep (sysmsg-expires msg))
        (hmt (sysmsg-how-many-times msg))
        (users (sysmsg-to msg))
        )
   ;; The message has expired or all users on the TO list
   ;; have seen the message enough times to have been put on the EXPIRED list.
   (or (and ep (>= ct ep))
       (and hmt
            (not (eq users :all))
            (every (lambda (user) 
                     (member user (sysmsg-expired-users msg) :test 'string=)
                     )
                   users
                   )))))

(defun maybe-add-user (msg user)
  (when (or (eq (sysmsg-to msg) :all)
            (member user (sysmsg-to msg) :test 'string=))
    (when (and (null (find-sysmsg-user-record msg user))
               (null (find user (sysmsg-expired-users msg) :test 'string=)))
      (push (list user 0 nil) (sysmsg-current-users msg))
      )))
                       

(defun message-limit-exceeded? (msg user)
  (let ((limit (sysmsg-how-many-times msg)) 
        (ur (find-sysmsg-user-record msg user)))
    (and limit (<= limit (second ur)))
    ))

(defun how-often-satisfied? (msg user)
  (let ((ho (sysmsg-how-often msg))
        (ur (find-sysmsg-user-record msg user)))
    (and ho ur 
         (or (null (third ur)) (> (get-universal-time) (+ ho (third ur))))
         )))
    
(defun find-sysmsg-user-record (msg user)
  (find user (sysmsg-current-users msg) :key 'first :test 'string=))

(defun update-sysmsg-user-info (msg user)
  (let ((ur (find-sysmsg-user-record msg user)))
    (when ur
      (incf (second ur))
      (setf (third ur) (get-universal-time))
      )))

(defmethod delete-sysmsg ((tag symbol))
  (setq *weblistener-system-messages* 
        (delete tag *weblistener-system-messages* :key 'sysmsg-id)))

(defmethod delete-sysmsg ((msg sysmsg))
  (setq *weblistener-system-messages* 
        (delete msg *weblistener-system-messages*)))

(defun expire-user (msg user)
  (push user (sysmsg-expired-users msg))
  (setf (sysmsg-current-users msg)
        (delete user (sysmsg-current-users msg) :key 'first :test 'string=)))

(defun maybe-output-sysmsg (msg user)
  (maybe-add-user msg user)
  (when (message-printable? msg user)
    (really-output-sysmsg msg)
    (update-sysmsg-user-info msg user))
  (when (message-deletable? msg) (delete-sysmsg msg))
  )

(defun really-output-sysmsg (msg)
  (if *message-output-hook-function*
      (funcall *message-output-hook-function* msg)
    (let* ((*event-time* (sysmsg-expires msg))
           (format (sysmsg-format msg))
           (output-string 
            (if (eq :text (first format)) 
                (second format) 
              (apply 'format nil format)))
           (color (sysmsg-color msg))
           (from (sysmsg-from msg))
           )
      (declare (special *event-time*))
      (flet ((do-msg ()
               (html 
                (:i
                 (:princ-safe (format nil ";; Message from ~A: " from))
                 (:princ-safe output-string)
                 :br
                 ))))
        (if (null color)
            (do-msg)
          (html ((:font :color color) (do-msg)))
          )))))

(defun announce-reboot (&optional (min 5))
  #.(one-string-nl  
     "Shorthand for the somewhat complex annoucement command telling everyone"
     "that the system is going to be rebooted in 5 (by default) minutes. "
     "The optional argument gives minutes to report (default = 5)."
     )
  (let* ((time-reboot-will-happen (+ (get-universal-time) (* 60 min)))
         (formatted-time 
          (make-timestamp-string 
           :universal-time time-reboot-will-happen
           :mode :hhmmss
           )))
    (announce 
     (list
      (one-string-nl
       "The BioLingua system will go down at ~A (in less than ~D minutes)"
       "for a reboot. (It will probably be up 10 minutes later.)") 
      formatted-time min)
     :expires (minutes-from-now min) :to :all
     :how-many-times nil :how-often 60 :color "red"
     )))

(defun announce 
       (message
        &key
        (to :all)
        (from *username*)
        (to-window-type :all)
        (expires (hours-from-now 24))
        (how-many-times 1)
        (how-often nil)
        (color "blue")
        id 
        )
  #.(one-string-nl
     "Print MESSAGE when a user executes a form.  MESSAGE can either be"
     "a string or a list of a string (interpreted as a format string)"
     "and format arguments (which get evaluated each time MESSAGE is printed)."
     "MESSAGE will be printed to a given user if TO is :ALL or that user is"
     "a member of TO (which must either be :ALL or a list of symbols"
     "interpreted as login names)."
     "MESSAGE will be identified as coming from FROM, which defaults to the"
     "user executing the ANNOUNCE function."
     "MESSAGE will no longer be printed out (and its existence obliterated)"
     "when the current time exceeds EXPIRES, which defaults to 24 hours"
     "from the current time.  EXPIRES must either be NIL (the message never"
     "expires (until the system goes down)) or an integer representing a"
     "universal time in the future.  (See the functions (HOURS-FROM-NOW...), "
     "(MINUTES-FROM-NOW...), and (SECONDS-FROM-NOW...) for easy ways to create"
     "such universal times.)"
     "MESSAGE will be printed HOW-MANY-TIMES to each user (default once)."
     "MESSAGE will be printed at most every HOW-OFTEN seconds to each user."
     "(The default is NIL, meaning to print it each time the user executes"
     "a form until EXPIRES or HOW-MANY-TIMES is satisfied.)"
     "MESSAGE will be printed in color COLOR (default \"blue\")."
     "The legal values are \"red\", \"green\" or \"blue\"."
     "ANNOUNCE returns ID (which defaults to a gentemp'ed symbol), which can"
     "be used to identify MESSAGE in case it needs to be deleted using"
     "(DELETE-ANNOUNCEMENT...)."
     "The function (MINUTES-UNTIL) can be used in MESSAGE as a format"
     "argument.  It will evaluate to the number of minutes until the MESSAGE"
     "expires.")
  (setq message (canonicalize-announce-message message))
  (setq to (canonicalize-announce-to-list to))
  (when (null id) (setq id (gentemp "MSG" :webuser)))
  (when expires
    (unless (integerp expires)
      (error "Invalid EXPIRES: ~A (not an integer)" expires))
    (unless (> expires (get-universal-time))
      (error "Invalid EXPIRES: ~A (represents a time in the past!)" expires)))
  (when how-many-times
    (unless (and (integerp how-many-times) (plusp how-many-times))
      (error "Invalid HOW-MANY-TIMES: ~A (not a +integer)" how-many-times)))
  (when how-often
    (unless (and (integerp how-often) (plusp how-often))
      (error "Invalid HOW-OFTEN: ~A (not a +integer)" how-often)))
  (when how-often
    (unless expires 
      (error "You can't specify HOW-OFTEN and not specify an expiration!")))
  (when color
    (unless (member color '("red" "green" "blue") :test 'string-equal)
      (error "Invalid COLOR: ~A" color))
    (setq color (string-downcase color)))
  (unless (member to-window-type '(:vpl :weblistener :all))
    (error "Invalid to-window-type: ~A" to-window-type))
  (push 
   (make-sysmsg 
    :format message
    :to to
    :from from
    :to-window-type to-window-type
    :how-many-times how-many-times
    :how-often how-often
    :expires expires
    :id id
    :color color 
    :current-users nil
    :expired-users nil
    )
   *weblistener-system-messages*)
  id
  )

(defmethod output-announcements ((application t) &key (msg-handler nil))
  "Output via HTML any messages targeted to the current user"
  (let ((*message-output-hook-function* msg-handler))
    (handler-case 
        (loop for ann in *weblistener-system-messages* do
              (maybe-output-sysmsg ann (string-downcase (string *username*))))
      (error 
       (c) 
       (html 
        ((:font :color "red") 
         (:princ-safe "Internal error in announcements!")
         :br
         (:princ-safe (formatn "Actual error: ~A" c))
         :br
         (:princ-safe "Please inform the system administrators.")
         :br 
         ))
       (setq *weblistener-system-messages* nil)
       ))))
        

(defun delete-announcement (msg-id) 
  #.(one-string-nl
     "Delete system announcement identified by MSG-ID.  MSG-ID must be"
     "a symbol.  If said messsage does not exist nothing is done."
     )
  (delete-sysmsg msg-id))
  
(defun clear-announcements () 
  "Remove all existing announcements"
  (setq *weblistener-system-messages* nil) t)

(defun minutes-until (&optional (time *event-time*))
  "Returns how many minutes until TIME (which is a universal time)"
  (round (- time (get-universal-time)) 60))

(defun seconds-from-now (s) 
  "Returns the universal time being S seconds into the future"
  (+ (get-universal-time) s))
(defun minutes-from-now (m)
  "Returns the universal time being M minutes into the future"
  (+ (get-universal-time) (* 60 m)))
(defun hours-from-now (h) 
  "Returns the universal time being H hours into the future"
  (+ (get-universal-time) (* 3600 h)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


