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

(defun-opcode get-manual-operator (ignore)
  "Open the BioBIKE Manual"
  (declare (ignore ignore))
  (ulogdbg "asking for Manual")
  (not-modified!)
  (show-vpl-popup-URL-window 
   (help::help-for-bbl-users-url)
   :height "800px"
   ))
   
(defun-opcode get-examples-operator (ignore)
  "Open Examples page"
  (declare (ignore ignore))
  (ulogdbg "asking for examples")
  (not-modified!)
  (show-vpl-popup-URL-window 
   (help::make-help-topic-url :name "BioBIKE Examples")
   :height "800px"
   ))

   (defun-opcode get-started-help-operator (ignore)
  "Advice on how to get started"
  (declare (ignore ignore))
  (ulogdbg "asking for advice on getting started")
  (not-modified!)
  (show-vpl-popup-URL-window 
   (help::make-help-topic-url :name "How to Get Started")
   ; (help::help-for-bbl-users-url)
   :height "800px"
   ))

(defun-opcode get-help-on-help-operator (ignore)
  "Learn more about BBL"
  (declare (ignore ignore))
  (ulogdbg "asking for general help")
  (not-modified!)
  (show-vpl-popup-URL-window 
   (help::make-help-topic-url :name "Ways of Getting Help")
   ; (help::help-for-bbl-users-url)
   :height "800px"
   ))

(defun-opcode troubleshooting-operator (ignore)
  "Bring up troubleshooting/list of known bugs file"
  (declare (ignore ignore))
  (ulogdbg "asking for troubleshooting")
  (not-modified!)
  (show-vpl-popup-url-window
   ;; this file name needs to be consistent with the def-documentation-file
   ;; entry in .../biolisp/Doc/externaldf/standard-docfiles.lisp
   "/weblistenerdocs/externaldf/textfiles/troubleshooting.html"
   :height "800px" :width "800px"
   ))

(defun-opcode display-vpl-tutorial-operator (ignore)
  "The defininitive guide to understanding the VPL"
  (declare (ignore ignore))
  (ulogdbg "asking for vpl tutorial")
  (not-modified!) 
  (show-vpl-popup-URL-window 
   "tutorial/vpl2-tutorial.html"
   :height "800px" :width "800px"
   ))

(defun-opcode browse-user-logs-operator (ignore)
  "Browse all your logs"
  (declare (ignore ignore))
  (ulogdbg "asking for user logs")
  (not-modified!)
  (show-vpl-popup-URL-window 
   (wb::make-previous-sessions-url)
   :height "800px" :menubar "yes" :location "yes" :status "yes"
   ))

(defun-opcode browse-user-files-operator (ignore)
  "Browse the directory containing your files"
  (declare (ignore ignore))
  (ulogdbg "asking for user files")
  (not-modified!)
  (show-vpl-popup-URL-window
   (wb::directory-listing-url (wb::visitor-directory wb::*username*))
   :height "800px" :menubar "yes" :location
   "yes" :directories "yes" :status "yes"
   ))

(defun-opcode browse-shared-files-operator (ignore)
  "Browse the directory containing communal files"
  (declare (ignore ignore))
  (ulogdbg "asking for shared files")
  (not-modified!)
  (let ((shared-directories 
         (wb::application-shared-files-directories cl-user:*ai*)))
    (when (> (length shared-directories) 1)
      (vpl-internal-error "More than one shared directory!"))
    (let ((sd (namestring (first shared-directories))))
      (show-vpl-popup-URL-window
       (wb::directory-listing-url sd)
       :height "800px" :menubar "yes" :location
       "yes" :directories "yes" :status "yes"
       ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun-opcode browse-user-contributed-stuff-operator (ignore)
  "Display variables and functions contributed by users"
  (declare (ignore ignore))
  (ulogdbg "Asking for user-contributed stuff")
  (not-modified!)
  (bbi::maybe-create-share-directory)
  (let ((data (bbi::read-shared-info)))
    (html-for-browse-user-contributed-stuff data)
    ))

(defun rbtr12 (td1-text td2-text)
  (net.aserve::html 
   (:tr
    ((:td :align "right")
     ((:font :color "brown") (:princ-safe td1-text)))
    (:td (:princ-safe td2-text))
    )
   :newline
   ))

(defun html-for-browse-user-contributed-stuff (data)
  #.(one-string-nl
     "Display the names and other information about user-contributed stuff"
     "in an HTML page, with links to cause sharing for each package.")
  (create-and-use-unique-file 
   (user-temp-vpl-dir)
   (lambda (file p)
     (declare (ignore file))
     (let ((net.aserve::*html-stream* p))
       (net.aserve::html 
        (:html 
         (:h2 
          (:center "User contributed packages available for common use"))
         :newline
         (if (null data)
             (net.aserve::html (:princ-safe "*** No packages available ***"))
           (loop 
            for record in data 
            do
            (display-shared-package-info record t)
            ))))))
   (lambda (file)
     (show-vpl-popup-URL-window 
      (user-temp-vpl-dir-url file) 
      :width "800"
      :height "800"
      ))
   :name (s+ "user-contributed-" (string wb::*sessionid*))
   :type "html"
   ))

(net.aserve::publish 
 :path wb::*vpl-share-package-url*
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
        (let* ((input (net.aserve::request-query req))
               (channel (get sessionid-symbol :vpl-channel))
               (share-package-name (url-parameter-value :share-package input)))
          (let ((*channel* channel)
                (wb::*vpl-executing?* t)
                (share-error nil))
            (handler-case 
                (bbi::use-share-function share-package-name)
              (error (c) (setq share-error c)))
            (if share-error 
                (net.aserve::html 
                 (:h2 (:center "Oops!"))
                 (:princ-safe 
                  (formatn 
                   "An error occurred trying to use shared package ~A."
                   share-package-name))
                 :br :br
                 "The actual error is: " 
                 :br :br
                 (:i (:princ-safe (formatn "~A" share-error)))
                 :br :br
                 "Please report this error to the system administrators."
                 )
              (progn
                (net.aserve::html
                 (:center
                  (:h2 
                   (:princ-safe
                    (formatn
                     "You are now using package ~A." share-package-name)))))
                (let ((share-data (bbi::read-shared-info)))
                  (loop 
                   for record in share-data 
                   as name = (first record)
                   do 
                   (when (string-equal name share-package-name)
                     (display-shared-package-info record nil)
                     (return nil)
                     ))))))))))))


(defun display-shared-package-info (record use?)
  (let* ((plist (cdr record))
         (name (first record))
         (user (getf plist :user))
         (functions (getf plist :functions))
         (variables (getf plist :variables))
         (description (getf plist :description)))
    (net.aserve::html
     (:b (:princ-safe name))
     "&nbsp;&nbsp;"
     (when use?
       (net.aserve::html 
        ((:a :href 
          (wb::make-vpl-share-package-url :share-package name))
         "(Use this package)")
        :br
        :newline
        ))
     (:table 
      (rbtr12 "Author: " (string user))
      (rbtr12 "Functions: " 
          (string-join (mapcar 'string functions) ", "))
      (rbtr12 "Variables: " 
          (string-join (mapcar 'string variables) ", "))
      (rbtr12 "Description: " (or description "No description" ))
      )
     :br
     :newline
     )))

(defun do-user-contributed-stuff (name)
  (let ((share-record (bbi::use-share-function name)))
    (show-status 
     (formatn "User contributed package ~A now in use" 
              (string-upcase (first share-record)))
     )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun-opcode browse-system-frames-operator (ignore) 
  "Bring up the frame viewer"
  (declare (ignore ignore))
  (ulogdbg "asking for system frames")
  (not-modified!)
  (show-vpl-popup-URL-window 
   (frames::make-toplevel-frames-url)
   :height "800px"
   ))

(defun-opcode weblistener-page-operator (ignore)
  "Bring up the weblistener, BBL's text interface"
  (declare (ignore ignore))
  (ulogdbg "asking for weblistener page")
  (not-modified!)
  (let ((new-session-id 
         (let ((*standard-output* wb::*system-standard-output*)
               (*error-output* wb::*system-error-output*)
               (*trace-output* wb::*system-trace-output*))
           (wb::connect-to-new-session wb::*username* nil nil)
           )))
    (show-vpl-popup-URL-window 
     (wb::make-weblistener-evalstring-url 
      :pkg new-session-id :evalstring ":from-vpl")
     :height "1000px" :width "800px" :location "yes"
     :directories "yes" :status "yes" :menubar "yes"
     )))

(defun-opcode new-session-operator (ignore)
  "Start a brand new session"
  (declare (ignore ignore))
  (ulogdbg "asking for new session")
  (not-modified!)
  (show-vpl-popup-URL-window
   (wb::login-webpage-url cl-user:*ai*)
   :height "800px" :width "800px" :location "yes" :menubar "yes"
   ))

(defun-opcode upload-file-operator (ignore)
  "Upload a file to your directory"
  (declare (ignore ignore))
  (ulogdbg "asking to upload a file")
  (not-modified!)
  (show-vpl-popup-URL-window 
   (wb::make-upload-form-url)
   :height "800px" :width "800px" :location "yes"
   :directories "yes" :status "yes" :menubar "yes"
   ))

;; Can call kill-execution-process directly once fully implemented
(defun-opcode kill-process-operator (ignore)
  "Stop the current execution if it is taking too long"
  (declare (ignore ignore))
  (ulogdbg! "Asking to kill ~A execution process!" wb::*sessionid*)
  (not-modified!)
  (let ((pid 
         (when (fboundp 'kill-execution-process) 
           (forward-funcall 'kill-execution-process))))
    (when pid (show-status "Execution terminated!"))
    ))


(defun-opcode exit-normally-operator (ignore)
  "Send feedback or report problems as you leave the VPL"
  (declare (ignore ignore))
  (ulogdbg "asking to exit normally")
  (not-modified!)
  (show-vpl-popup-URL-window 
   (wb::make-feedback-form-url)
   :height "600px" :relative-p 0
   ))

(defun-opcode exit-immediately-operator (ignore)
  "Leave the VPL"
  (declare (ignore ignore))
  (ulogdbg "asking to exit immediately")
  (not-modified!)
  (user-info-message 
   #.(one-string-nl
      "Please exit using the browser's exit button"
      "at the top right.")))

(defun-opcode exit-stage-left-operator (ignore)
  "Just what it says"
  (declare (ignore ignore))
  (ulogdbg "asking to exit stage left")
  (not-modified!)
  (show-vpl-popup-URL-window
   (wb::make-doc-directory-url "snagglepuss.jpg")
   :relative-p 0
   :height "220px" :width "222px" :toolbar "no" :resizable "no"
   :scrollbars "no"
   ))

(defun-opcode exit-and-logout-operator (ignore)
  "Leave the VPL and log out"
  (declare (ignore ignore))
  (ulogdbg "asking to exit and logout")
  (not-modified!)
  (show-status "Logged out!")
  (let ((weblistener-window-id
         (show-vpl-popup-URL-window 
          (wb::make-weblistener-evalstring-url :evalstring "(wb::logout)")
          :height "inherit" :width "inherit" :location "yes"
          :directories "yes" :status "yes" :menubar "yes"
          )))
    (vdbg "Created popup...~%")
    (kill-vpl-window weblistener-window-id)
    (user-info-message 
     #.(one-string-nl
        "You have been logged out.  Please exit using the"
        "browser's exit button at the top right."
        "You will not be able to do any further operations"
        "in this VPL window.")))
  ;; Mark the session as logged-out so that the highlevel code that
  ;; receives messages implements a noop if it ever gets a message again
  ;; destined for this sessionid.  (Otherwise we were running into 
  ;; infinite message stream problems.)  
  (setf (get wb::*sessionid* :logged-out) t)
  )
   
(defun-opcode save-user-session-operator (ignore)
  "Save your user session so you can work on it later"
  (declare (ignore ignore))
  (save-session-aux
   'get-vpl-user-session-directories
   'save-named-user-session
   "user"
   ))

(defun-opcode save-session-publicly-operator (ignore)
  "Save your session so that others can use it"
  (declare (ignore ignore)) 
  (save-session-aux 
   'get-vpl-public-session-directories 
   'save-named-public-session
   "public"
   ))

(defun save-session-aux (dir-function operator type)
  (ulogdbg "asking to save ~A session" type)
  (not-modified!)
  (let ((sessions (funcall dir-function)))
    (when sessions
      (user-info-message 
       (formatn
        (s+ 
         "The following saved ~A ~As exist:~%~%"
         (one-directory-name-per-line sessions)
         "~%Enter a unique name below to avoid overwriting~%"
         "an existing ~A ~A"
         )
        type *session-or-workspace* type *session-or-workspace*
        ))))
  (show-dialog 
   operator
   (formatn "Please enter a ~A identifier" *session-or-workspace*)
   (formatn "~A ~A name:" type *session-or-workspace*)
   0
   ))

(defun-opcode help-search-for-operator (ignore)
  "Attempts to find functions associated with your search string"
  (declare (ignore ignore))
  (ulogdbg "asking for help search")
  (not-modified!)
  (show-dialog 
   'help-search-for-function
   ;; "help-search-for-function"
   "help search"
   "Word or function name to search on:" 0
   ))

(defun user-vpl-user-sessions-dir ()
  (append-subdir (wb::visitor-directory wb:*username*) "vpl-user-sessions"))

(defun save-named-user-session (user-session-name)
  (save-named-session 
   user-session-name (user-vpl-user-sessions-dir) 'save-vpl-user-session "user"
   ))

(defun save-named-public-session (public-session-name)
  (save-named-session 
   public-session-name *public-sessions-dir* 'save-vpl-public-session "public"
   ))

(defun save-named-session 
       (session-name directory operator type &aux comment)
  (flet ((stw (x) (string-trim utils::*whitespace* x)))
    (setq session-name (stw session-name))
    (let ((semipos (position #\; session-name)))
      (when semipos
        (setq comment (stw (subseq session-name (1+ semipos))))
        (when (zerop (length comment)) (setq comment nil))
        (setq session-name (stw (subseq session-name 0 semipos)))
        )
      (unless (every (lambda (ch) 
                       (or (digit-char-p ch) 
                           (alpha-char-p ch)
                           (char= ch #\-)
                           (char= ch #\_)
                           (char= ch #\.)
                           (char= ch #\+)
                           ))
                     session-name
                     )
        (vpl-user-error 
         (one-string-nl
          "Invalid ~A session ID name!"
          "Please do not use any characters except letters, numbers,"
          "dashes, underscores, and dots."
          "(You entered '~A' )"
          "Please try again with an acceptable ~A session ID name."
          )
         type session-name type
         ))
      (let* ((session-directory (append-subdir directory session-name)))
        (ensure-directories-exist session-directory)
        (funcall operator session-directory :comment comment)
        (not-modified!)
        ))))

(defun help-in-bbl-context (strings)
  (let ((previously-in-bbl-mode? (wb::bbl-mode?))
        (help-result nil))
    (handler-case 
        (wb::bbl-mode :verbose? nil)
      (error 
       ()
       (nvpl::vpl-internal-error 
        "Internal error: Can't get into BBL mode!  Please report this!"
        )))
    (unwind-protect 
        (if (listp strings) 
            (setq help-result (eval `(help::help ,@strings)))
          (setq help-result (eval `(help::help ,strings))))
      (when (not previously-in-bbl-mode?) 
        (handler-case (wb::biolisp-mode)
          (error 
           ()
           (nvpl::vpl-internal-error 
            "Internal error: Can't get back into Biolisp mode!"
            )))))
    help-result 
    ))

(defun help-search-for-function (help-string)
  (wb::log-vpl-help-request help-string)
  (let* ((hs (string-trim *whitespace* help-string))
         (pos (position #\Space hs))
         (help-result nil))
    (ulog "User search string: '~A'~%" hs)
    ;; if it looks like the user typed in multiple words separated by 
    ;; spaces, invoke help with multiple string arguments
    (if pos 
        (let* ((strings (string-split hs #\Space)))
          ;; make sure we're in bbl context for help search invoked from vpl!
          (setq help-result (help-in-bbl-context strings)))
      ;; otherwise get rid of extraneous whitespace goo and call help
      ;; with a single string argument
      (progn 
        (loop for ws in *whitespace* do 
              (vwhen (pos (position ws hs)) (return pos)))
        (when pos 
          (setq hs (subseq hs 0 pos)))
        (setq help-result (help-in-bbl-context hs))
        ))
    (if (null help-result)
        (progn
          (ulog "Nothing relevant found for ~A" hs)
          (create-and-use-unique-file 
           (user-temp-vpl-dir)
           (lambda (file p)
             (declare (ignore file))
             (with-html-to-stream 
                 p 
               (html 
                (:b 
                 (:princ (s+ "Nothing relevant found for '" help-string "'."))))
               ))
           (lambda (file) 
             (show-vpl-popup-URL-window
              (wb::publish-path-for-file-in-viewable-subdir file)
              :relative-p 0
              :width "400px" :height "400px"
              ))
           :name (s+ "value-" (string wb::*sessionid*))
           :type "html"
           ))
      (process-single-vpl-return-value help-result))
    nil
    ))
        
(defun one-file-per-line (file-list)
  (let ((s ""))
    (loop for file in file-list do
          (setq s (s+ s (formatn "  ~A~%" (file-namestring file)))))
    s))

(defun session-comments (directories)
  (loop for dir in directories 
        as comments-file = (merge-pathnames *comments-file* dir)
        collect
        (if (not (probe-file comments-file))
            ""
          (with-open-file (p comments-file :direction :input)
            (read p)
            ))))
           

;;; After this date user sessions which have been saved
;;; successfully contain a file called save-complete.info.
;;; Only user session directories which have this file should
;;; assumed to be valid (but saved user sessions created before 
;;; this date don't have this file, so we have to check for that).

(defconstant may-13-2008 3419709867)

(defun get-vpl-user-session-directories ()
  (get-session-directories (user-vpl-user-sessions-dir)))

(defun get-vpl-public-session-directories ()
  (get-session-directories *public-sessions-dir*))

(defun get-session-directories (sessiondir)
  (let* ((sessions-directory sessiondir)
         (possible-session-dirs 
          (remove-if-not
           'pathname-names-directory?
           (directory-with-subdirs-in-directory-form sessions-directory)
           )))
    (loop for dir in possible-session-dirs 
          when (dir-contains-success-file dir)
          collect dir
          )))

(defun dir-contains-success-file (dir)
  (handler-case 
      (let ((save-complete-file 
             (merge-pathnames *session-save-complete-file* dir))
            (workspace-file (merge-pathnames *session-workspace-file* dir)))
        (or 
         (probe-file save-complete-file)
         (let ((write-date (file-write-date workspace-file)))
           (< write-date may-13-2008)
           )))
    (error () nil)
    ))

(defun workspace-dir-write-date (dir)
  (ignore-errors
    (let ((save-complete-file (merge-pathnames *session-save-complete-file* dir)))
      (file-write-date save-complete-file)
      )))


(defun one-directory-name-per-line (directory-list)
  (let ((s ""))
    (loop for dir in directory-list do
          (setq 
           s
           (s+ s (formatn "  ~A~%" (lastelem (pathname-directory dir))))))
    s
    ))

(defun two-column-directory-names-per-line 
       (user public &key (first-col-width 30))
  (let ((nuser (length user))
        (npublic (length public)))
    (flet ((pad-or-clip (s) 
             (let ((len (length s)))
               (cond
                ((= len first-col-width) s)
                ((> len first-col-width) 
                 (maybe-clip-string s first-col-width))
                ((< len first-col-width) 
                 (s+ 
                  s 
                  (make-string (- first-col-width len) :initial-element #\Space)
                  ))))))
      (cond
       ((= nuser npublic) nil)
       ((> nuser npublic) 
        (setq 
         public 
         (append public (make-list (- nuser npublic) :initial-element ""))))
       ((< nuser npublic) 
        (setq 
         user 
         (append user (make-list (- npublic nuser) :initial-element "")))))
      (apply 
       's+ 
       (loop for udir in user 
             for pdir in public
             collect
             (s+ (pad-or-clip udir) " " (pad-or-clip pdir) (string #\Newline))
             )))))

(defun restore-named-user-session (user-session-name)
  (when (every 'whitespacep user-session-name)
    (vpl-user-error 
     #.(one-string-nl
        "You didn't enter a session name!  Please close the window that lists"
        "the session names if it is still open and try the"
        "'Restore user session' command again."
        )))
  (setq user-session-name (string-trim utils::*whitespace* user-session-name))
  (let* ((public-prefix "public:")
         (public? (initial-subsequence-of? user-session-name public-prefix))
         (user-sessions-directory (user-vpl-user-sessions-dir))
         (public-sessions-directory *public-sessions-dir*)
         (user-session-directory 
          (append-subdir user-sessions-directory user-session-name))
         (public-session-directory 
          (append-subdir
           public-sessions-directory
           (if public? 
               (subseq user-session-name (length public-prefix))
             user-session-name
             )))
         (names-user-session? (probe-file user-session-directory))
         (names-public-session? (probe-file public-session-directory)))
    (cond
     ((and (null names-user-session?) (null names-public-session?))
      (vpl-user-error 
       #.(one-string-nl
          "The user session '~A' doesn't exist in either your user"
          "sessions directory or the public sessions directory!"
          "Perhaps you mistyped it.  Please close the window that lists"
          "the user session names if it is still open and try the"
          "'Restore user session' command again."
          )
       user-session-name
       ))
     ((and public? (null names-public-session?))
      (vpl-user-error 
       #.(one-string-nl
          "The user session '~A' doesn't exist in the public sessions"
          "directory. Perhaps you mistyped it."
          "Please close the window that lists"
          "the user session names if it is still open and try the"
          "'Restore user session' command again."
          )
       user-session-name
       ))
     (t nil)
     )
    (not-modified!)
    (redraw!)
    (cond
     (names-user-session? 
      (restore-vpl-user-session user-session-directory))
     (names-public-session? 
      (restore-vpl-public-session public-session-directory))
     (t (error "Internal error...this should be impossible!"))
     )
    (redraw-results)
    (if names-public-session? :public :user)
    ))

(defun delete-named-user-sessions (user-session-names)
  (let ((message 
         (one-string-nl
          "Please close the window that lists"
          "the session names if it is still open and try the"
          "'Delete user sessions' command again."
          )))
    (when (every 'whitespacep user-session-names)
      (vpl-user-error 
       (one-string-nl "You didn't enter a session name!" message)
       ))
    (flet ((st (s) (string-trim utils::*whitespace* s))
           (dirname (dirpath) (lastelem (pathname-directory dirpath))))
      ;; break up user's input into comma-separated tokens, removing
      ;; all whitespace around them
      (setq user-session-names 
            (remove-if 
             (lambda (x) (zerop (length x)))
             (mapcar
              #'st
              (string-split (st user-session-names) #\,)
              )))
      (let* ((directories-not-found nil)
             (user-session-directories nil))
        (loop for name in user-session-names 
              with user-sessions-directory = (user-vpl-user-sessions-dir)
              as user-session-directory = 
              (append-subdir user-sessions-directory name)
              do
              (if (probe-file user-session-directory)
                  (push user-session-directory user-session-directories)
                (push user-session-directory directories-not-found)
                ))
        (setq directories-not-found (reverse directories-not-found))
        (setq user-session-directories (reverse user-session-directories))
        (when directories-not-found
          (if (= 1 (length user-session-names))
              (vpl-user-error 
               "The user session you specified, ~A, does not exist: ~%~A"
               (dirname (first directories-not-found))
               message
               )
            (vpl-user-error 
             (one-string-nl
              "Some user sessions you specified do not exist:"
              (string-join 
               (mapcar (lambda (x) (s+ "  " (dirname x))) directories-not-found)
               #\Newline
               )
              message
              ))))
        (let ((dirs-deleted nil))
          (loop for dirs on user-session-directories 
                as delete-dir = (first dirs)
                do
                (handler-case 
                    (progn
                      (remove-directory delete-dir)
                      (push delete-dir dirs-deleted))
                  (error 
                   (c)
                   (cond
                    ((= 1 (length user-session-directories))
                     (vpl-user-error 
                      (one-string-nl
                       "Could not delete session ~A."
                       "Actual error: ~A."
                       "Please inform the system administrators.")
                      (dirname delete-dir) c
                      ))
                    (t 
                     (let ((deleted-sessions 
                            (string-join (mapcar #'dirname dirs-deleted) ", "))
                           (undeleted-sessions 
                            (string-join (mapcar #'dirname dirs) ", ")))
                       (vpl-user-error
                        (one-string-nl
                         "Could not delete session ~A."
                         "Actual error: ~A."
                         "Please inform the system administrators."
                         "Sessions that were deleted: ~A"
                         "Sessions that were not deleted: ~A"
                         )
                        (dirname delete-dir) c 
                        deleted-sessions undeleted-sessions
                        ))))))
                finally
                (if (= 1 (length user-session-directories))
                    (show-status 
                     (formatn "User session ~A deleted." 
                              (dirname (first user-session-directories))))
                  (show-status 
                   (formatn
                    "~D sessions deleted."
                    (length user-session-directories)
                    )))))))))

(defun-opcode preferences-page-operator (ignore)
  "Modify the settings for your session"
  (declare (ignore ignore))
  (ulogdbg "asking for preferences page")
  (not-modified!)
  (show-vpl-popup-URL-window 
   (wb::make-prefs-gui-url)
   :height "1000px" :width "800px"
   ;; :height "800px" :width "600px"
   ))

(defun-opcode 
    report-problem-operator (ignore)
  "Report any problems or general confusion to the system administrators"
  (declare (ignore ignore))
  (ulogdbg "asking to report problem")
  (not-modified!)
  (show-vpl-popup-URL-window 
   (wb::make-feedback-form-url)
   :height "700px" :relative-p 0
   ))

(defun-opcode clear-workspace-operator (ignore)
  #.(one-string
     "Remove all boxes from the workspace"
     )
  (declare (ignore ignore))
  (vdbg "in clear-workspace-operator")
  (handle-clear-workspace))

(defun-opcode clear-results-operator (ignore)
  "Clears all but most recent box from the results area (or clears area if only one box)"
  (declare (ignore ignore))
  (vdbg "in clear-results-operator")
  (not-modified!)
  (handle-clear-execution-history)
  )

(defun-opcode undo-operator (ignore)
  "Go back a step in your work"
  (declare (ignore ignore))
  (vdbg "in undo-operator")
  (not-modified!)
  (handle-workspace-back))

(defun-opcode redo-operator (ignore)
  "Go forward a step after going back"
  (declare (ignore ignore))
  (vdbg "in redo-operator")
  (not-modified!)
  (handle-workspace-forward))

;; This doesn't deal with focus, nor does it deal with
;; any changes to the palette.  (This is assuming we have
;; two browser windows attached to the same session and
;; one of them does a redraw after doing something that causes
;; the palette menu to be modified.)
(defun-opcode redraw-all-operator (ignore)
  "Redraw the workspace and results area"
  (declare (ignore ignore))
  (ulogdbg "asking to redraw everything")
  (not-modified!)
  (redraw-everything-in-workspace)
  (redraw-everything-in-results)
  (when *current-selected-boxid* 
    (flashing-hilight-box *current-selected-boxid*))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *announce-permissions-status* :users-with-accounts)

(defun announce-ok-to-use? () 
  (cond 
   ((wb::weblistener-guru-p) t)
   (t 
    (case *announce-permissions-status*
      (:none nil)
      (:all t)
      (:non-demo-users (not cl-user::*allow-anonymous-logins*))
      (:users-with-accounts 
       (not (null (wb::find-login-record (string wb::*username*))))
       )))))

(defun-opcode announce-to-all-operator (ignore)
  "Send a message to everyone using the system"
  (declare (ignore ignore))
  (vdbg "in announce-to-all-operator")
  (not-modified!)
  (if (announce-ok-to-use?) 
      (show-dialog 
       'announce-to-all-function
       ;; "announce-to-all-function"
       "Enter a message to send to all users (the next time they act)"
       "Message:" 
       1
       )
    (vpl-user-error "Only registered users are allowed to send messages!")
    ))

(defun announce-to-all-function (string)
  (usyslog "Announcement: ~A" string)
  (wb::announce string)
  (show-status "Announcement sent to all users.")
  )

(defun-opcode announce-to-vpl-users-operator (ignore)
  "Send a message to everyone using the VPL"
  (declare (ignore ignore))
  (vdbg "in announce-to-vpl-users-operator")
  (not-modified!)
  (if (announce-ok-to-use?)
      (show-dialog 
       'announce-to-vpl-users-function
       ;; "announce-to-vpl-users-function"
       "Enter a message to send to all VPL users (immediately)"
       "Message:" 
       1
       )
    (vpl-user-error "Only registered users are allowed to send messages!")
    ))

(defun announce-to-vpl-users-function (string)
  (usyslog "VPL Announcement: ~A" string)
  (wb::announce string :to-window-type :vpl)
  (show-status "Announcement sent to VPL users.")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code to create prettified log file.  

(defun-opcode show-execution-log-operator (ignore)
  "Show a log of all the boxes you've exeucted."
  (declare (ignore ignore))
  (ulogdbg "asking to show execution log")
  (not-modified!)
  (create-and-use-unique-file 
   (user-temp-vpl-dir)
   (lambda (file p)
     (declare (ignore file))
     (with-html-to-stream p 
       (create-prettified-vpl-execution-log-html)
       ))
   (lambda (file) 
     (show-vpl-popup-URL-window
      (wb::publish-path-for-file-in-viewable-subdir file)
      :relative-p 0
      :width "800px" :height "800px" :menubar "yes"
      ))
   :name (s+ "execution-log-" (string wb::*sessionid*))
   :type "html"
   ))

(defun log-file-name->creation-date (filepath)
  (handler-case 
      (let* ((pn (pathname filepath))
             (name (file-namestring pn))
             (split (string-split name #\-))
             (ymd (second split))
             (hms (third split))
             (creation-date
              (formatn 
               "~A/~A/~A ~A:~A:~A"
               (subseq ymd 4 6) (subseq ymd 6 8) (subseq ymd 2 4)
               (subseq hms 0 2) (subseq hms 2 4) (subseq hms 4 6)
               )))
        creation-date
        )
    (error () "?")
    ))

(defun create-prettified-vpl-execution-log-html (&key (file wb::*log-file*))
  (let* ((records (create-list-of-form-and-return-value-lines file))
         (creation-date-string (log-file-name->creation-date file))
         (write-date (file-write-date file))
         (write-date-string 
          (if write-date 
              (make-timestamp-string :universal-time (file-write-date file))
            "?"
            )))
    (flet ((update () 
             (html 
              ((:a :href
                (wb::make-vpl-refresh-execution-log :file (url-safe-string file)))
               "Update"
               ))))
      (html
       ((:a :href
         (wb::make-log-viewer-url 
          :file (url-safe-string file)
          :mode "full"))
        "Show full log"
        )
       (:h3 
        (:b ((:font :color "green") 
             (:princ-safe (formatn "Execution Log for ~A." wb::*username*))
             :br
             ;; (:princ "&nbsp;&nbsp;")
             (:princ-safe (wb::connected-to-message))
             :br
             :br 
             (:princ-safe (formatn "Logged in: ~A" creation-date-string))
             :br
             (:princ-safe (formatn "Last activity: ~A" write-date-string))
             :br
             ;; (:princ-safe (formatn " ~A" file))
             ;; :br
             :br :br
             (update)
             :hr
             )))
       :br 
       (loop for (form-line more-form-lines value-lines error-lines timestamp)
             in records 
             do
             (html ((:font :size "2")
                    (:princ-safe (formatn "----- ~A" timestamp)) :br))
             (html (:princ-safe form-line) :br)
             (loop for afl in more-form-lines do 
                   (loop for j from 1 to 10 do (html "&nbsp;"))
                   (html (:princ-safe afl) :br))
             (loop for v in value-lines do (html (:princ-safe v) :br))
             (loop for e in error-lines do (html (:princ-safe e) :br))
             )
       :br
       (:h3 (update))
       ))))

(defun create-list-of-form-and-return-value-lines (file)
  (let ((lines (file-to-string-list file)))
    (block exit 
      (when (null lines) (return-from exit nil))
      (labels 
          ((line-is-form-line? (line)
             (search " : Form: " line :test 'string-equal))
           (line-is-value-line? (line) 
             (or (search " : Value " line :test 'string-equal) 
                 (search " : Value: " line :test 'string-equal)))
           (line-is-repl-times-line? (line)
             (search " : repl times:" line :test 'string-equal))
           (find-next-form ()
             (loop 
              while lines
              do
              (when (line-is-form-line? (first lines))
                (unless (second lines) 
                  (error 
                   "Log file format error: Nothing after last form statement"
                   ))
                (let ((form-line (pop lines)))
                  (return form-line)
                  ))
              (pop lines)
              finally (return nil)
              ))
           (find-value-and-error-lines ()
             (let ((more-form-lines nil) (value-lines nil) (error-lines nil))
               (loop 
                while lines
                as line = (first lines)
                do
                (cond
                 ((line-is-repl-times-line? line)
                  (unless (and (null value-lines) (null error-lines))
                    (error "Repl times after value or error????"))
                  (pop lines))
                 ((line-is-value-line? line)
                  (push line value-lines)
                  (pop lines)
                  )
                 ((search ": Error message: " line :test 'string-equal)
                  (loop 
                   while lines
                   as eline = (first lines)
                   do 
                   (cond
                    ((line-is-form-line? eline)
                     (return (reverse lines)))
                    ((or (zerop (length eline))
                         (every 'whitespacep eline))
                     (pop lines)
                     (return (reverse lines)))
                    (t (push eline error-lines) (pop lines))
                    )
                   finally (return (reverse lines))
                   ))
                 (t
                  (if (and (null value-lines) (null error-lines))
                      (progn
                        (push (pop lines) more-form-lines)
                        )
                    (return)
                    ))))
               (values 
                (reverse more-form-lines) 
                (reverse value-lines)
                (reverse error-lines)
                ))))
        (let ((records nil))
          (loop 
           while t
           do
           (let ((form-line (find-next-form))
                 (timestamp nil)
                 (form-text nil))
             (when (null form-line)
               (return-from exit (reverse records)))
             (multiple-value-setq (form-text timestamp)
                 (remove-timestamp-goo-from-log-line form-line))
             (multiple-value-bind 
                 (additional-form-lines value-lines error-lines)
                 (find-value-and-error-lines)
               (push 
                (list 
                 form-text
                 (mapcar 
                  (lambda (s) (string-trim *whitespace* s))
                  additional-form-lines
                  )
                 (mapcar 'remove-timestamp-goo-from-log-line value-lines)
                 (cond 
                  ((null error-lines) nil)
                  ((search "<<< Evaluation failed." (first error-lines)  
                           :test 'string-equal) 
                   (cdr error-lines))
                  (t 
                   (cons
                    (remove-timestamp-goo-from-log-line (first error-lines))
                    (cdr error-lines)
                    )))
                 timestamp
                 )
                records
                )))))))))

;; 20 is the number of characters in the timestamp plus " : "
(defun remove-timestamp-goo-from-log-line (line) 
  (values (subseq line 20) (subseq line 0 17)))
  

(net.aserve:publish 
 :path wb::*vpl-refresh-execution-log*
 :content-type cl-user::*html-publish-content-type*
 :function
 (lambda (req ent)
   (let* ((input (net.aserve:request-query req))
          (package-name (url-parameter-value :pkg input))
          (package-symbol (keywordize package-name))
          (file (url-parameter-value :file input))
          )
     (wb::execute-with-standard-weblistener-environment
      req ent package-symbol
      (lambda () (html-for-vpl-refresh-execution-log file))
      ))))

(defun html-for-vpl-refresh-execution-log (file)
  (create-prettified-vpl-execution-log-html :file file))
                   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun-opcode share-with-another-user-operator (ignore)
  "Monitor another's session, with permission"
  (declare (ignore ignore))
  (ulogdbg "asking to share with another user")
  (not-modified!)
  (show-dialog 
   'share-with-another-user-command
   ;; "share-with-another-user-command"
   (one-string
    "Please enter a user's session ID.<br>  "
    "To return control to you, refresh your VPL window.")
   "Session ID:" 0
   ))

(defun share-with-another-user-command (userid)
  (vdbg "In share-with-another-user-command, userid = ~S~%" userid)
  (let ((sessionid (keywordize (string-upcase userid))))
    (syslog "User ~A requesting share with ~A" wb::*sessionid* sessionid)
    (let ((share-command-message (create-share-request-message sessionid)))
      (vdbg "Message is ~A~%" share-command-message)
      (let ((return-code (vpl *channel* share-command-message)))
        (when return-code 
          (setf (get wb::*sessionid* :slave-shared-mode) t)
          )))))
    
(defun-opcode whats-new-operator (ignore)
  "Check out the VPL blog"
  (declare (ignore ignore))
  (ulogdbg "asking for whats new page")
  (not-modified!) 
  (show-vpl-popup-URL-window 
   "http://vplblog.wordpress.com/"
   ;; "../weblistenerdocs/whatsnew.html"
   :height "900px" :width "900px"
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun-opcode enable-subsystems-selection-operator (ignore)
  "Put up the (large) subsystem selection menus"
  (declare (ignore ignore))
  (ulogdbg "enabling subsystem menus")
  (if user::*master-list-enabled*
      (forward-funcall 'create-subsystems-menu)
    (vpl-internal-error "Subsystems operator exists in non-seed acache!")
    ))