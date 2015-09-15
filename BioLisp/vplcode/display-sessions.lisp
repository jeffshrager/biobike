;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: nvpl; -*-

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

(net.aserve::publish 
 :path wb::*new-fangled-show-sessions-url*
 :content-type cl-user::*html-publish-content-type*
 :function
 (lambda (req ent)
   (let* ((input (net.aserve::request-query req))
          (sessionid (url-parameter-value :pkg input))
          (sessionid-symbol (keywordize sessionid))
          )
     (wb::execute-with-standard-weblistener-environment
      req ent sessionid-symbol
      (lambda () 
        (html-for-new-fangled-show-sessions-page req ent sessionid) 
        )))))

(defun html-for-new-fangled-show-sessions-page (req ent sessionid)
  (declare (ignorable ent sessionid))
  (let* ((input (net.aserve::request-query req))
         (mode (url-parameter-value :mode input))
         (ndeletes (url-parameter-value :ndeletes input))
         (deletethem (url-parameter-value :deletethem input))
         (cancel (url-parameter-value :cancel input))
         (dels (url-parameter-value :dels input))
         (session-name (url-parameter-value :sessionname input))
         ) 
    (declare (ignorable input))
    (print (list mode wb::*sessionid* wb::*username*))
    (multiple-value-bind (uinfo pinfo)
        (get-user-and-public-session-info)
      (labels ((show-session (title if-none data delete?)
                 (net.aserve::html 
                  (:h3 
                   (when delete? 
                     (net.aserve::html
                      (:princ "&nbsp;")
                      ((:input :type "submit" 
                        :value 
                        (formatn
                         "Delete selected user ~As" *session-or-workspace*
                         )))))
                   :br 
                   (unless (string-equal title "Public sessions") (html :br))
                   (:princ-safe title))
                  (if (null data)
                      (net.aserve::html (:princ-safe if-none))
                    (net.aserve::html 
                     (:table 
                      (loop 
                       for (session-name comment write-date) in data
                       for j from 1 
                       as timestamp = 
                       (if write-date 
                           (make-timestamp-string 
                            :universal-time write-date
                            :mode :mmddyy)
                         "?"
                         )
                       as url = 
                       (wb::make-new-fangled-show-sessions-url 
                        :mode "DRESTORE" :sessionname session-name)
                       do
                       (net.aserve::html
                        (:tr 
                         (when delete? 
                           (net.aserve::html 
                            (:td 
                             ((:input :type "checkbox" 
                               :name (formatn "delete~D" j)
                               )))))
                         (:td ((:a :href url) "Restore")
                          (:princ "&nbsp;&nbsp;"))
                         (:td (:princ-safe (s+ timestamp)) (:princ "&nbsp;"))
                         ((:td :align "left") 
                          (:b
                           ((:font :color "brown")
                            (:princ-safe (string-capitalize session-name))))
                          (:princ "&nbsp;&nbsp;"))
                         ((:td :align "left")
                          (:princ-safe 
                           (if (string-equal comment "")
                               comment
                             (s+ ";; " comment)
                             )))))))))))
               (title ()
                 (net.aserve::html 
                  (:html 
                   (:head (:title "Sessions page"))
                   (:h2 
                    (:center
                     (:princ-safe
                      (formatn 
                       "Available user and public ~As" 
                       *session-or-workspace*))))
                   :newline
                   :newline
                   )))
               (standard-page () 
                 (let ((ndeletes (length uinfo)))
                   (net.aserve::html
                    ((:form
                      :method "get" :action wb::*new-fangled-show-sessions-url*)
                     (de-hidden "PKG" (string wb::*sessionid*))
                     (de-hidden "NDELETES" (formatn "~D" ndeletes))
                     (de-hidden "MODE" "DREQ")
                     (show-session 
                      (formatn "Your ~As" *session-or-workspace*)
                      (formatn "*** No user ~As available ***"
                               *session-or-workspace*)
                      uinfo t
                      ))))
                 :br :br
                 (show-session 
                  (formatn "Public ~As" *session-or-workspace*)
                  (formatn 
                   "*** No public ~As available ***" *session-or-workspace*)
                  pinfo nil
                  ))
               (get-delete-session-names (session-numbers)
                 (mapcar 
                  (lambda (x) (first (nth (1- x) uinfo)))
                  session-numbers
                  ))
               (get-delete-session-info ()
                 (let* ((session-numbers 
                         (which-user-sessions-to-delete? 
                          input (read-from-string ndeletes)))
                        (session-names 
                         (get-delete-session-names session-numbers))
                        )
                   (values session-names session-numbers)
                   )))
        
        (cond
         ((string-equal mode "DREQ") 
          (title)
          (multiple-value-bind (session-names session-numbers)
              (get-delete-session-info)
            (if (null session-names)
                (standard-page)
              (net.aserve::html
               ((:form :method "get"
                 :action wb::*new-fangled-show-sessions-url*)
                (wb::greentext 
                 "You've requested the following sessions be deleted:")
                :br
                (loop for name in session-names 
                      do
                      (net.aserve::html 
                       (:princ "&nbsp;&nbsp;")
                       (:b ((:font :color "brown") (:princ-safe name)))
                       :br
                       ))
                (de-hidden "PKG" (string wb::*sessionid*))
                (de-hidden "NDELETES" (formatn "~D" ndeletes))
                (de-hidden "MODE" "DCONFIRM")
                (let ((session-number-string 
                       (string-join 
                        (mapcar (lambda (x) (formatn "~D" x)) session-numbers)
                        ","
                        )))
                  (de-hidden "DELS" session-number-string)
                  )
                :br
                ((:input :type "submit" :value "Delete them!"
                  :name "deletethem"))
                "&nbsp;"
                ((:input :type "submit" :value "Cancel" :name "cancel"))
                )
               :hr 
               (show-session 
                "Your sessions" "*** No user sessions available ***" uinfo nil
                )
               :br 
               (show-session 
                "Public sessions" "*** No public sessions available ***"
                pinfo nil
                )))))

         ((string-equal mode "DCONFIRM")
          (cond
           (cancel (title) (standard-page))
           (deletethem 
            (let* ((session-numbers 
                    (mapcar 'read-from-string (string-split dels #\,)))
                   (session-names (get-delete-session-names session-numbers)))
              (multiple-value-bind (deleted not-deleted not-found)
                  (new-delete-named-user-sessions session-names)
                (declare (ignore not-deleted not-found))
                (multiple-value-setq (uinfo pinfo)
                    (get-user-and-public-session-info))
                (title)
                (net.aserve::html 
                 (wb::greentext "The following sessions were deleted:")
                 :br
                 (loop for (name nil) in deleted 
                       for j from 1 
                       do
                       (print j)
                       (net.aserve::html 
                        (:princ "&nbsp;&nbsp;")
                        (:b ((:font :color "brown") (:princ-safe name)))
                        :br
                        ))
                 :hr 
                 (standard-page)
                 ))))
           (t (net.aserve::html (:princ-safe "This should never happen!")))
           ))

         ((string-equal mode "DRESTORE")
          (multiple-value-bind (failure? type)
              (handle-restore-session-request session-name)
            (let ((modifier (if (eq type :public) "Public " "")))
            (if (not failure?)
                (progn
                  (title)
                  (html 
                   (:h3 ((:font :color "green") 
                         (:princ-safe
                          (formatn 
                           "~ASession ~A restored." modifier session-name)
                          )))
                   :hr
                   ))
              (progn
                (title)
                (html 
                 (:h3 ((:font :color "green") 
                       (:princ-safe
                        (formatn 
                         "*** ~ASession ~A NOT restored."
                         modifier session-name))
                       :br
                       (:princ-safe
                        (formatn "*** Actual error: '~A'" failure?))
                       :br
                       (:princ-safe
                        (formatn "*** You may wish to contact a sysadmin."))
                       ))
                 :hr 
                 ))))
            (standard-page)
            ))
        
         (t (title) (standard-page))
         
         )))))

(defun-opcode new-restore-user-session-operator (ignore)
  "Continue working on a user session you had previously saved"
  (declare (ignore ignore))
  (vdbg "in restore-user-session-operator")
  (not-modified!)
  (show-vpl-popup-URL-window 
   (wb::make-new-fangled-show-sessions-url)
   :width "800"
   :height "800"
   ))

(defun which-user-sessions-to-delete? (input ndeletes)
  (loop for j from 1 to ndeletes 
        as param = (keywordize (formatn "Delete~D" j))
        when (url-parameter-value param input)
        collect j
        ))

(defun get-user-and-public-session-info ()
  (let* ((user-session-directories (get-vpl-user-session-directories))
         (public-session-directories (get-vpl-public-session-directories))
         (user-comments (session-comments user-session-directories))
         (public-comments (session-comments public-session-directories))
         (user-session-write-dates 
          (mapcar 'workspace-dir-write-date user-session-directories))
         (public-session-write-dates
          (mapcar 'workspace-dir-write-date public-session-directories))
         (unames 
          (mapcar 
           (lambda (x) (lastelem (pathname-directory x)))
           user-session-directories
           ))
         (pnames
          (mapcar 
           (lambda (x) (lastelem (pathname-directory x)))
           public-session-directories
           ))
         (uinfo (mapcar 'list unames user-comments user-session-write-dates))
         (pinfo
          (mapcar 'list pnames public-comments public-session-write-dates)))
    (setq uinfo (sort uinfo 'string-lessp :key 'first))
    (setq pinfo (sort pinfo 'string-lessp :key 'first))
    ;; append public: to any public workspaces that share a name with
    ;; this user's saved workspaces
    (setq 
     pinfo 
     (loop for (dirname comment write-date) in pinfo
           collect
           (if (member dirname uinfo :test 'string-equal :key 'first)
               (list (s+ "public:" dirname) comment write-date)
             (list dirname comment write-date)
             )))
    (values uinfo pinfo)
    ))

(defun new-delete-named-user-sessions (user-session-names)
  (let* ((directories-not-found nil)
         (user-session-directories nil))
    (loop for name in user-session-names 
          with user-sessions-directory = (user-vpl-user-sessions-dir)
          as user-session-directory = 
          (append-subdir user-sessions-directory name)
          do
          (if (probe-file user-session-directory)
              (push (list name user-session-directory) user-session-directories)
            (push (list name user-session-directory) directories-not-found)
            ))
    (setq directories-not-found (reverse directories-not-found))
    (setq user-session-directories (reverse user-session-directories))
    (let ((dirs-deleted nil)
          (dirs-not-deleted nil))
      (loop for (name delete-dir) in user-session-directories 
            do
            (handler-case 
                (progn
                  (remove-directory delete-dir)
                  (push (list name delete-dir) dirs-deleted))
              (error () (push (list name delete-dir) dirs-not-deleted))
              ))
      (values dirs-deleted dirs-not-deleted directories-not-found)
      )))

(defun de-hidden (name value)
  (forward-package-funcall :data-editor :hidden name value))
       
(defun handle-restore-session-request (session-name)
  (let ((*channel* (get wb::*sessionid* :vpl-channel))
        (wb::*vpl-executing?* t)
        (restore-error nil)
        (type nil))
    (handler-case 
        ;; put us into the proper vpl environment 
        (with-user-vpl-session-info-bound (wb::*vpl-session-info*)
          (setq type (restore-named-user-session session-name))
          ;; above function only redraws the results area, 
          ;; leaving the workspace to higher level functionality
          ;; which we don't have here.  So force it.  
          (redraw-everything-in-workspace)
          )
      (error (c) (setq restore-error c)))
    (values restore-error type)
    ))




