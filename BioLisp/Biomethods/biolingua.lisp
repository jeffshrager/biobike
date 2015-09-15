;;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: biolisp; -*-

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

;;; Author: JP Massar. 

;;; Methods for the BIOLINGUA flavor of the Weblistener  
;;; Called from blload.lisp at system startup time.
;;; Loads the <app>-instance-init.lisp file if any, loads the knowledge
;;; bases and calls the application instance instance init method.

(defmethod application-initializations :after ((app cl-user:biolingua)) 
  (cformatt "Running Biolingua-specific initializations")
  (let ((instance-directory cl-user:*instance-home-directory*))
    (when (null instance-directory) 
      (cformatt "No *INSTANCE-HOME-DIRECTORY* value provided.")
      (cformatt "Looking in (user-homedir-pathname): ~A"
                (user-homedir-pathname))
      (setq instance-directory (user-homedir-pathname)))
    (let ((instance-load-file (proper-instance-init-file instance-directory)))
      (when instance-load-file
        (handler-case
            (progn
              (cformatt "Compile/loading ~A" (namestring instance-load-file))
              (c/l instance-load-file))
          (error
           (c)
           (cformatt "Problem compile/loading ~A." instance-load-file)
           (cformatt "Actual error: ~A" c)
           (cformatt "Trying LOAD in case of write-permission failure...")
           (load instance-load-file)
           )))))
  (if cl-user:*biowebserver*
      (ecase user:*frame-system-version*
        (:sframes 
         (cformatt "Not currently loading KDB frames..."))
        (:old 
         (cformatt "Frames table to be resized.")
         (resize-frame-table 80000)
         (cformatt "Knowledge bases to be loaded.")
         (forward-funcall 'bio::make-kdb-frames)
         )
        (:new 
         (ecase user:*acache-frame-system-mode* 
           (:acache 
            (cformatt "Acache running; frames assumed to exist in db."))
           (:pseudo-acache
            (cformatt "Pseudo-acache running; knowledge frames being loaded...")
            (forward-funcall 'bio::load-kdb-frames :verbose? t)
            ))))
    (progn
      (cformatt "Frames table not resized.")
      (cformatt "No knowledge data loaded.")
      ))
  (application-instance-initializations cl-user:*ai*)
  (webuser:announce
   (formatn "For questions and help, ~A" *default-support-email-address*)
   :from "system" :expires nil :to-window-type :weblistener)
  (cformatt "Biolingua application initializations complete.")
  )

(defun proper-instance-init-file (local-dir)
  (let ((instance-init-pathname 
         (make-pathname
          :host nil
          :name (string-downcase 
                 (one-string 
                  (string cl-user:*application-instance*) 
                  "-instance-init"))
          :type "lisp")))
    (let* ((liif (merge-pathnames instance-init-pathname local-dir))
           (siif 
            (merge-pathnames 
             instance-init-pathname (translate-simple-lp "biol:Biomethods;")))
           (local? (probe-file liif))
           (system? (probe-file siif)))
      (cond 
       ((and (null local?) (null system?))
        (cformatt
         (one-string
          "FYI: No instance init file ~A found either "
          "locally or in system directory")
         instance-init-pathname)
        nil)
       ((and local? (null system?))
        (cformatt "FYI: No system version of instance-init-file ~A" liif)
        liif)
       ((and (null local?) system?)
        (cformatt "FYI: No local version of instance init file ~A" siif)
        siif)
       (t
        (let ((local-write (file-write-date liif))
              (system-write (file-write-date siif))
              (local-text (file-to-string liif))
              (system-text (file-to-string siif)))
          (cond
           ((string= local-text system-text) 
            (cformatt 
             "FYI: Local and system instance-init files are identical.")
            liif)
           ((>= local-write system-write)
            (warn 
             (one-string-nl
              "System instance init file ~A differs from"
              "local instance init file ~A and the local file,"
              "which will be loaded, is more recent. The system copy should" 
              "be merged with the local version and checked into SourceForge.")
             siif liif)
            liif
            )
           (t
            (error
             (one-string-nl
              "System instance init file ~A"
              "differs from"
              "local instance init file ~A"
              "and the system file is more recent. You need to merge the"
              "local file with the system file or replace the local file"
              "with the system file."
              "If you merge the two make sure the revised source is"
              "checked into SourceForge.")
             siif liif
             )))))))))
     
      
;; if this is a BIOWEBSERVER we preload all organism data and then
;; we load organism data as requested.  We then load any modules
;; present in the application instance home directory.

(defmethod application-instance-initializations ((app cl-user:biolingua))
  (cformatt "Running ~a instance-specific initializations." (type-of app))
  (flet ((doit 
	  ()       
	  (let ((preload-successful? t))
	    (cformatt "Preloading organisms...")
	    (handler-case
	     (forward-package-funcall :bio :preload-organisms)
	     (error
	      (c)
	      (setq preload-successful? nil)
	      (cformatt "Could not preload: ~A" c)
	      (cformatt "Skipping loading of organisms, if any...")))
	    (when preload-successful?
	      (let ((orgs cl-user:*organisms-to-load-list*))
		(when (eq orgs :all)
		  (setq 
		   orgs 
		   (forward-package-funcall :bio :available-organisms)
		   ))
		(cformatt "Organisms to be loaded: ~A" orgs)
		(loop for organism in orgs do
		      (forward-package-funcall :bio :load-organism organism)
		      ))))))
    (if cl-user:*biowebserver*
        (progn
          (ecase user::*frame-system-version*
            (:new
             (ecase user:*acache-frame-system-mode*
               (:acache 
                (cformatt 
                 (one-string
                  "In :new :acache mode: Not loading organisms; "
                  "DB must either have been loaded, or be manually loaded!"
                  )))
               (:pseudo-acache (doit))))
            (:old (doit)))
          (forward-funcall 'bio::determine-cyano-organism-kegg-ids)
          )
      (progn
	(cformatt "No organism data loaded.")
	)))
  (load-instance-modules)
  (cformatt "~a instance-specific initializations complete." (type-of app))
  )

(defmethod application-version ((app cl-user:biolingua) &key (mode :long))
  (let ((current-version "5.2"))
    (case mode
      (:short (one-string "v" current-version))
      (:number current-version)
      (otherwise (one-string "BioBIKE Listener v" current-version))
      )))


(defmethod application-name ((app cl-user:biolingua)) "BioBIKE")


;;; Methods having to do with logging in and the login page.

(defmethod login-webpage-url ((app cl-user:biolingua)) "/biologin")

(defmethod new-login-webpage-url ((app cl-user:biolingua)) "/new-biologin")

(defmethod guru-login-webpage-url ((app cl-user:biolingua)) "/gbiologin")

(defmethod application-packages-to-use ((app cl-user:biolingua))
  ;; All the standard Weblistener packages, plus BIOLISP, BIOUTILS
  ;; and the ORG-ALIASES if enabled.  
  (let ((biopackages 
         (union (list bio::*organism-nickname-package*) '(:biolisp :bioutils))))
    (append biopackages (call-next-method))
    ))

(defmethod application-symbols-to-shadowing-import
           ((application cl-user:biolingua))
  shadowlisp:*symbols-to-shadowing-import*
  )

(defmethod application-readtable ((application cl-user:biolingua))
  (frames:frames-readtable))

(defmethod login-header ((app cl-user:biolingua))
  (html
   (:head 
    ((:font :size 6 :color "brown")
     (:center
      (:princ-safe 
       (s+ 
        (formatn
         "Welcome to ~A (" 
         (or *application-instance-pretty-name* 
             "Welcome to the Original BioBIKE Secure Server"))
        (application-version app :mode :short)
        ")"))
      :br
      (:princ-safe "Cyanobacterial Edition"))))))

(defun html-for-login-and-password 
       (&key (prior? nil) (email? nil) (password? nil))
  (html
   (:table 
    (:tr
     (:td (:big "LOGIN NAME: "))
     (:td ((:input :type "text" :name "loginname")))
     (when prior?
       (html 
        (:td (:i ((:font :color "green") "(prior registration not required)")))
        ))
     (when email? 
       (html
        (:tr
         (:td (:big "EMAIL ADDRESS: "))
         (:td ((:input :type "text" :name "email")))
         (case cl-user::*allow-anonymous-logins* 
           (:with-email (html (:td (:i "(required)"))))
           (otherwise (html (:td (:i "(optional but advised)"))))
           ))))
     (when password? 
       (html 
        (:tr
         (:td (:big "PASSWORD: "))
         (:td ((:input :type "password" :name "password")))
         )))))))

(defun insert-biobike-picture ()
  (html 
   ((:img 
     :style "float: right;"
     :src "/weblistenerdocs/highwheeler.gif"
     :width "400" :height "300"
     ))))

(defun insert-programming-mode-radio-buttons (which-button)
  (macrolet ((lisp-button (checked?)
               `((:input :type "radio" :name "configuration" 
                  :value "lisp" 
                  ,@(when checked? `(:checked "yes")))
                 "Lisp"))
             (bbl-button (checked?)
               `((:input :type "radio" :name "configuration" 
                  :value "BBL" 
                  ,@(when checked? `(:checked "yes")))
                 "BBL (text language)"))
             (vpl-button (checked?)
               `((:input :type "radio" :name "configuration" 
                  :value "VPL"
                  ,@(when checked? `(:checked "yes")))
                 "VPL (graphical language)"))
             (sp () "&nbsp;&nbsp;&nbsp;"))
    (ecase which-button
      ((:default nil)
       (case cl-user::*code-creation-mode*
         (:weblistener 
          (case cl-user::*repl-mode* 
            (:biolisp 
             (html 
              (lisp-button t) (sp) (bbl-button nil) (sp) (vpl-button nil)))
            (:bbl
             (html 
              (lisp-button nil) (sp) (bbl-button t) (sp) (vpl-button nil)))
            (otherwise nil)
            ))
         (:vpl
          (case cl-user::*repl-mode* 
            (:biolisp
             (error "Cannot do this configuration yet!"))
            (:bbl 
             (html 
              (lisp-button nil) (sp) (bbl-button nil) (sp) (vpl-button t))
             )))))
      (:lisp 
       (html (lisp-button t) (sp) (bbl-button nil) (sp) (vpl-button nil)))
      (:bbl
       (html (lisp-button nil) (sp) (bbl-button t) (sp) (vpl-button nil)))
      (:vpl 
       (html (lisp-button nil) (sp) (bbl-button nil) (sp) (vpl-button t)))
      )))

(defun insert-previous-and-new-login-buttons ()
  (html
   (:table 
    (:tr
     (:td ((:input :type "submit" :value "Previous Session" 
            :name "prevsession")))
     (:td "&nbsp;&nbsp;")
     (:td ((:input :type "submit" :value "New Login" :name "newlogin")))
     ))))

;; *** This function is basically obsolete, replaced by new-weblogin-function
;; *** below.  However it is still called by the guru-login-webpage-url code
;; *** in .../weblisten/login-html.lisp  

(defmethod weblogin-function ((app cl-user::biolingua) req ent)
  (with-http-response-and-body (req ent)
    (html 
     (:title (:princ-safe (useful-weblistener-title)))
     :br
     (:body 

      (login-header cl-user:*ai*)
      (insert-biobike-picture)
      
      ((:form :method "get" :action *weblogin-form-response-url*)
       ((:input :type "hidden" :name "guru" :value *guru-login-mode*))
       :br
       
       ;; login/password/email boxes
       (if cl-user:*allow-anonymous-logins*
           (if *guru-login-mode* 
               (html-for-login-and-password :password? t)
             (html-for-login-and-password :prior? t :email? t))
         (html-for-login-and-password :password? t))
       
       ;; programming mode radio buttons
       (insert-programming-mode-radio-buttons :default)
       
       ;; previous/new login buttons
       (insert-previous-and-new-login-buttons)
       )
      
      ;; this is in a table, otherwise text extends to the right margin
      ;; underneath the bike picture if the screen is not wide enough
      (:table 
       (:tr
        (:td (login-addenda cl-user:*ai*))))
      
      (login-credits cl-user:*ai*)
      (login-disclaimer cl-user:*ai*)
      
      ))))

(defmethod login-addenda ((app cl-user:biolingua))
  (html
   (:h3 (:princ-safe "What is BioBIKE?") :br)
   (:ul
    (:li
     (:b "B") "iological " (:b "I") "ntegrated " (:b "K") "nowledge "
     (:b "E") "nvironment")
    (:li (:b "A knowledge resource") :br
     (:princ-safe
      (one-string
       "BioBIKE brings together available genomic, metabolic, and "
       "experimental data pertinent to a given research community"))
     :br)
    (:li (:b "A programming environment") :br
     (:princ-safe
      (one-string
       "BioBIKE provides a programming language accessible to biologists "
       "without programming experience."))
     :br 
     (:princ-safe
      (one-string
       "Built into it are concepts familiar to molecular biologists and "
       "powerful tools to manipulate and analyze biological data.")
      ))
    (:li 
     (:b (:princ-safe "For a description of BioBIKE, see:"))
     :br
     (:princ-safe
      (one-string 
       "Elhai J, Taton A, Massar JP, Myers JK, Travers M, Casey J, "
       "Slupesky M, Shrager J (2009).  "
       "BioBIKE: A web-based, programmable, integrated "
       "biological knowledge base. "
       ))
     ((:a :href "http://nar.oxfordjournals.org/cgi/content/full/gkp354"
       :target "_blank")
      "Nucl Acids Res 37:W28-W32"
      )))
   ((:a :href 
     "new-help/help-topic-url?PKG=DOCUSER0&NAME=HELP%20for%20BBL%20users")
    "General Help for VPL users")
   :br
   ((:a :href (forward-funcall 'make-doc-directory-url))
    "Learn more about the underlying software")
   :br :br 
   (unless cl-user::*allow-anonymous-logins* 
     (html 
      ((:a :href bio::*biobike-login-request-url*)
       (:big "Request a login name/password")
       )))
   :hr
   ))

(defmethod login-credits ((app cl-user:biolingua))
  (html
   :p
   ((:font :color "olivedrab")
    "The biologists and engineers who have contributed to "
    "BioBIKE so far include: "
    "Joe Anderson, "
    "Devaki Bhaya, "
    "Johnny Casey "
    "Victor Clarke, "
    ((:a :target "_blank" :href *elhai-home-page-url*) "Jeff Elhai")
    (:princ-safe " (NSF grant PI, guidance on visual interface), ")
    "Dexter Gulick, "
    "Bob Haxo, "
    "Monica Jain, "
    "Michiko Kato, "
    "Ashvin Kumar, "
    "JP Massar (core engineering lead), "
    "John Myers (Javascript expert), "
    "Craig Noe, "
    "Tara Nulton, "
    "Marc Santoro, "
    "Karl Schweighofer, "
    ((:a :target "_blank" :href *seibel-home-page-url*) "Peter Seibel")
    (:princ-safe " (Ajax expert, 'Practical Common Lisp' author), ")
    ((:a :target "_blank" :href *shrager-home-page-url*) "Jeff Shrager")
    ", CTO of "
    ((:a :target "_blank" :href "http://www.collabrx.com/") "CollabRx") ", "
    "Mark Slupesky, "
    "Colin Smith, "
    ((:a :target "_blank" :href *mt-home-page-url*) "Mike Travers")
    (:princ-safe " (knowledge framework and interface engineering lead), ")
    "Arnaud Taton, "
    "Sumudu Watugala, "
    "and Robel Wolde."
    :p
    "The Frame system and Frame browser were conceived of and originally "
    "written by Mike Travers, with rewrites by JP Massar. "
    "The idea for a Web Server is Jeff Shrager's and "
    "a prototype implementation was done by Dan Siroker. JP Massar designed "
    "and implemented the existing Web Server, as well as much of the "
    "Biolingua-specific code, with ideas and coding assistance from "
    "Jeff Shrager, Mike Travers, Mark Slupesky, and Jeff Elhai."
    :p
    "The BBL language was designed by Jeff Elhai, JP Massar, and Peter Seibel "
    "with help from Mike Travers, Jeff Shrager and others."
    :p
    "The visual interface was designed by Jeff Elhai, JP Massar, Peter Seibel, Mike "
    "Travers, John Myers and others, and was implemented by John Myers, "
    "Johnny Casey, JP Massar, and others."
    :p
    "Thanks to "
    ((:a :target "_blank" :href *franz-url*) "Franz Inc.")
    " and "
    ((:a :target "_blank" :href *lispworks-url*) "LispWorks")
    " for contributions supporting both BioLisp and the BioBIKE server, "
    " which was developed using Lispworks and Franz's Allegro "
    "Common Lisp, and which runs on Franz's ACL8.1 and uses the "
    ((:a :target "_blank" :href *allegroserve-source-url*)
     "AllegroServe engine.")
    :p
    "Thanks also to Edi Weitz for "
    ((:a :target "_blank" :href *ppcre-source-url*) "Cl-ppcre,")
    " a Perl-compatible regular expression pattern matcher."
    :p
    "This work is currently supported by NSF grant DBI0850146."
    "This work was originally supported by NASA's program in "
    "Astrobiology and Fundamental Biology."
    :br :br
    )))

(defmethod login-disclaimer ((app cl-user:biolingua))
  (html
   :hr
   ((:font :color "red")
    :br
    "USE THIS TOOL AT YOUR OWN RISK.  Constantly being updated, "
    "it may become unavailable at any time for any reason. "
    "Morever, things are likely to change without warning and/or "
    "suddenly stop working altogether.  We offer no guarantee nor "
    "promise of reliable service or support at this time. "
    "(But we give it our best effort)."
    :p
    "All input and output are logged for debugging."
    )))


(defmethod a-login-form-response-function ((app cl-user::biolingua) req ent)
  (with-http-response-and-body (req ent)
    (let* ((input (request-query req))
           (login (space-trim (url-parameter-value :loginname input)))
           (password (or (url-parameter-value :password input) ""))
           (email (url-parameter-value :email input))
           (newlogin? (url-parameter-value :newlogin input))
           (prevsession? (url-parameter-value :prevsession input))
           (configuration (url-parameter-value :configuration input))
           (*user-provided-email* email)
           (*guru-login-mode* (url-parameter-value :guru input))
           )
      (setq *guru-login-mode* 
            (and *guru-login-mode* (string-equal "T" *guru-login-mode*)))
      #+debug
      (setq *foo* 
            (list :login login :password password 
                  :newlogin? newlogin? :prevsession? prevsession?
                  :configuration configuration :guru *guru-login-mode*))
      (when configuration 
        (setq configuration (keywordize configuration)))
      (flet ((oops (msg &optional (password-message? nil))
               #+debug
               (print password-message?)
               (html
                :br (:big (:princ "USER LOGIN ERROR:")) :br
                (let ((lines (string-split msg #\Newline)))
                  (loop for line in lines do (html (:princ-safe line) :br)))
                ;; (:princ-safe msg)
                :br :br
                ((:a :href (login-webpage-url cl-user:*ai*)) "Try again")
                (when password-message? 
                  (html 
                   :br :br 
                   ((:a :href (s+ *forgotten-password-response-url* "?"
                                  (formatn "loginname=~A" login)))
                    "Send me my password")))
                )))
        (handler-case
            (if (or (null login) (equal login "")) 
                (oops "No login name provided!")
              (multiple-value-bind (visitor-package-symbol existing?)
                  (progn
                    (login-and-create-package login password email))
                (set-up-biobike-language-configuration
                 configuration visitor-package-symbol)
                (flet ((new-session ()
                         (let ((new-sessionid
                                (progn 
                                  (connect-to-new-session
                                   visitor-package-symbol existing? nil))))
                           ;; This should be methodized for BioLingua and
                           ;; common code from below pulled out
                           (go-to-biobike-user-input-page 
                            configuration new-sessionid)
                           )))
                  (cond
                   ((null visitor-package-symbol) 
                    (oops "Ierror! LOGIN-AND-CREATE-PACKAGE returned NIL!"))
                   (newlogin? (new-session))
                   (prevsession? 
                    (let ((sessions (user-session-info visitor-package-symbol)))
                      (case configuration
                        (:vpl
                         (if (and sessions (some-vpl-session? sessions))
                             (previous-vpl-session-page
                              visitor-package-symbol existing?)
                           (new-session)
                           ))
                        ((:lisp :bbl)
                         (if (and sessions (some-weblistener-session? sessions))
                             (previous-session-page
                              visitor-package-symbol existing?)
                           (new-session)
                           )))))
                   (t 
                    (oops "Internal error: Impossible to reach clause in cond"))
                   ))))
          (login-failed
           (c)
           (let ((reason (login-failed-reason c)))
             (cond
              ((eq reason :invalid-password) (oops "Invalid password" t))
              (t (oops (login-failed-reason c)))
              )))
          (error (c) (oops (formatn "Unknown failure: ~A" c)))
          )))))


;;; Weblistener methods

(defmethod weblistener-redisplay-page-title ((app cl-user:biolingua))
  (application-version app :mode :long))


(defmethod weblistener-verify-evalstring ((app cl-user:biolingua) string) 
  (declare (ignorable string))
 t)

;;; Auxiliary page methods

(defmethod webpage-head-html ((application cl-user:biolingua) title)
  (html (:title "BioLingua: " (:princ-safe title))))

(defmethod webpage-toplinks ((application cl-user:biolingua) &key (listener? t))
  (html
   :newline
   (when listener?
     (html
      ((:a :href (return-hack-url)) "Listener") " "
      :newline))
   ((:a :href (forward-funcall 'make-doc-directory-url)) "BioDocs") " "
   :newline
   ((:a :href (forward-funcall 'make-weblistener-frame-find-url)) 
    "FindFrames") " "
   :newline
   ((:a :href 
     (forward-funcall 
      'make-weblistener-toplevel-directories-url
      :pkg (user-session-id)))
    "BioFiles") " "
   :newline
   ((:a :href *hyperspec-url*) "LispDocs") " "
   :newline
   ))


;;; Documentation and directory listing methods

(defmethod viewable-application-toplevel-directories 
           ((app cl-user:biolingua))
  (list "Biol:" "Bioetc:" "home:"))


(defmethod application-apropos-packages ((application cl-user:biolingua))
  (let ((package-list (list :biolisp :bioutils))
        (pkg (user-session-package))
        (user-mode (wb::user-mode)))
    (when pkg
      (cond
       ((eq user-mode :bbl) (setq package-list (list :bbl)))
       (t nil)))
    (append package-list (call-next-method))
    ))

(defmethod application-from-email-address ((app cl-user:biolingua))
  cl-user:*biolingua-email-address*
  )

(defmethod application-user-init-file ((app cl-user:biolingua)) 
  "biolisp.ini")

(defmethod application-toplevel-frames ((app cl-user:biolingua)) 
  (append
   (ecase user::*frame-system-version* 
     (:old (forward-package-funcall :bio :create-kdb-toplevel-frames))
     (:sframes nil))
   (list (list :organisms (forward-package-funcall :bio :loaded-organisms)))
   ;; Any Weblistener toplevel frames.
   (call-next-method)
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod html-for-weblistener-title ((app cl-user:biolingua))
  (flet ((pad () (html (:td "&nbsp;&nbsp;&nbsp;"))))
    (html 
     (:table 
      (:tr
       ((:td  :valign "top") 
        (:big (:i (:b (:princ-safe (application-version app :mode :long))))))
       (pad)
       ((:td  :valign "bottom") 
        ((:form :name "nav2")
         ((:select 
           :name "url" 
           :onChange 
           "eval(document.nav2.url.options[document.nav2.url.selectedIndex].value)")
          ((:option :selected "selected") "Help...")
          :newline
          ((:option) "------------")
          :newline
          ((:option :value (eval.in.listener "(help help)")) 
           "Using (help)")
          :newline
          ((:option :value 
            (window.open 
             (forward-funcall 'make-weblistener-toplevel-doc-page-url)))
           "BioBIKE Overview")
          :newline
          ((:option :value 
            (window.open 
             (s+ 
              (forward-funcall 'make-weblistener-toplevel-doc-page-url)
              "#BIOLISP")))
           "BioLisp Documentation")
          :newline
          ((:option :value 
            (window.open 
             (symbol-value-in-package 
              :*bbl-documentation-toplevel-url* 
              (find-package :bbli))))
           "BBL Documentation")
          :newline
          ((:option :value 
            (window.open (forward-funcall 'make-application-primitives-url))) 
           "BioLisp primitives")
          :newline
          ((:option :value 
            (window.open (forward-funcall 'help::make-help-modules-url))
            )
           "Available functions")
          :newline
          ((:option :value 
            (window.open (forward-funcall 'help::make-help-glossary-url)))
           "BioBIKE glossary")
          :newline
          ((:option :value (window.open *hyperspec-url*)) "Lisp Hyperspec")
          :newline
          #+still-doesnt-work
          ((:option :value
            (window.open 
             (s+ 
              (forward-funcall 'make-weblistener-toplevel-doc-page-url)
              "#COMMONLISP")))
           "Help about Lisp")
          :newline
          ((:option :value 
            (page.open
             (forward-funcall 'make-feedback-form-url))) "Send Feedback")
          :newline
          ((:option :value 
            (eval.in.listener 
             "(help::list-live-tutorials)"
             ))
           "Live Tutorials")
          )))
       (pad)
       ((:td  :valign "bottom") 
        ((:form :name "nav3")
         ((:select 
           :name "url" 
           :onChange 
           "eval(document.nav3.url.options[document.nav3.url.selectedIndex].value)")
          ((:option :selected "selected") "Tools...")
          ((:option) "------------")
          ((:option :value (eval.in.listener "(my-stuff)")) "My Stuff")
          ((:option 
            :value (window.open (forward-funcall 'make-prefs-gui-url)))
           "Prefs...")
          ((:option 
            :value (window.open (forward-funcall 'make-upload-form-url))) 
           "Upload File")
          ((:option 
            :value (window.open (forward-funcall 'make-web-widget-index-url)))
           "Web Widgets")
          ((:option 
            :value 
            (page.open (forward-funcall 'login-webpage-url cl-user:*ai*)))
           "Switch Sessions")
          )))
       )))))

(defun window.open (string)
  (formatn "window.open('~a')" string))

(defun eval.in.listener (string)
  (formatn "document.location.href=~s" 
           (make-weblistener-evalstring-url :evalstring string)))
(defun page.open (string) 
  (formatn "document.location.href=~s" string))

(defmethod html-for-weblistener-links ((application cl-user:biolingua))
  (flet ((pad () (html :newline (:td) (:td) (:td) (:td) (:td) (:td))))
    (html
     (:table
      (:tr 
       :newline
       (:td ((:a :href 
              (forward-funcall 'make-weblistener-toplevel-directories-url))
             (:big "Browse Files")))
       (pad)
       :newline
       (:td ((:a :href (forward-funcall 'make-previous-sessions-url)
              :target "_blank")
             (:big "Session Logs")))
       (pad)
       :newline
       (:td
        ((:a :href (forward-funcall 'make-edit-all-previous-url)) 
         (:big "Files:"))
        " "
        ((:a :href (forward-funcall 'make-edit-previous-url)) 
         (:big "Prev"))
        " "
        ((:a :href (forward-funcall 'make-edit-all-previous-url))
         (:big "All")))
       (pad)
       :newline
       (:td ((:a :href (frames::make-toplevel-frames-url))
             (:big "Browse Frames")))
       (pad)
       :newline
       (:td 
        ((:a :href (forward-package-funcall :help :make-new-help-options-url)
          :target "_blank")
         (case (forward-funcall 'user-mode) 
           (:bbl (html (:b (:big ((:font :color "red") "Help")))))
           (:biolisp (html (:big "Help")))
           )))
       )))))

(defmethod initial-user-repl-mode ((app cl-user:biolingua))
  (ecase (get wb::*username* :login-language) 
    ((nil) 
     (ecase cl-user:*repl-mode*
       (:bbl (bbl-mode))
       (:biolisp (biolisp-mode))
       ))
    (:lisp (biolisp-mode))
    (:bbl (bbl-mode))
    ))

(defmethod weblistener-repl-mode ((application cl-user:biolingua))
  (case (forward-package-funcall :webuser :user-mode application)
    (:bbl
     (forward-funcall
      'record-initfile-message 
      (one-string-nl
       ""
       ";; You are in BBL mode."
       ";; To learn about BBL,"
       ";; use the Help menu below and choose 'BBL Documentation'."
       ";; To switch to BioLisp mode, type: (biolisp-mode)"
       ";; To learn about BioLisp choose the 'Documentation' option.")))
    (:biolisp
     (forward-funcall
      'record-initfile-message
      (one-string-nl
       ""
       ";; You are in BioLisp mode."
       ";; To learn about BioLisp,"
       ";; use the Help menu below and choose 'Documentation'."
       ";; To switch to BBL mode, type: (bbl-mode)"
       ";; To learn about BBL,"
       ";; use the Help menu below and choose 'BBL Documentation.'")))
    (otherwise 
     (forward-funcall 
      'record-initfile-message
      (formatn 
       (one-string-nl
        ""
        ";; Unknown user mode, ~S, (not BioLisp or BBL)."
        ";; This is almost certainly a fatal error!")
       (forward-package-funcall :webuser :user-mode application)
       )))))
          
(defmethod user-mode-internal ((app cl-user:biolingua))
  (cond ((forward-funcall 'bbl-mode?) :bbl)
	((forward-funcall 'biolisp-mode?) :biolisp)
	(t
         (error
          (one-string-nl
           "USER-MODE was unable to figure out what mode you are in."
           "This shouldn't happen. Please contact BioBIKE support.")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(defvar *bll-login-or-email* nil)
(defvar *bll-state* nil)
(defvar *bll-password* nil)
(defvar *bll-new-or-previous* nil)
(defvar *bll-configuration* nil)
(defvar *bll-new-account-login* nil)
(defvar *bll-new-account-email* nil)
(defvar *bll-new-account-fullname* nil)
(defvar *bll-new-account-affiliation* nil)


(defmacro with-biobike-login-page-headers (&body body)
  `(html 
    (:title (:princ-safe (useful-weblistener-title)))
    :br
    (:body 
     (login-header cl-user:*ai*)
     (insert-biobike-picture)
     ((:form :method "post" :action (new-login-webpage-url cl-user:*ai*))
      :br
      ,@body
      ))))

;; xxxxxxxxxx
(defmethod new-weblogin-function ((app cl-user::biolingua) req ent)
  
  (with-http-response-and-body (req ent)
      
    (handler-case 
          
        (let* ((input (request-query req))
               (login-or-email (url-parameter-value :loginname input))
               (state (url-parameter-value :state input))
               (password (or (url-parameter-value :password input) ""))
               (new-session? (url-parameter-value :newlogin input))
               (prev-session? (url-parameter-value :prevsession input))
               (previous-new-or-prev-state 
                (url-parameter-value :prev-np-state input))
               (configuration (url-parameter-value :configuration input))
               (new-account-login (url-parameter-value :loginname input))
               (new-account-email (url-parameter-value :email input))
               (new-account-fullname (url-parameter-value :fullname input))
               (new-account-affiliation 
                (url-parameter-value :affiliation input))
               (make-the-account? (url-parameter-value :newaccount input))
               (pseudo-account? (url-parameter-value :nothanks input))
               (confirmed? (url-parameter-value :confirmed input))
               (goback? (url-parameter-value :goback input))
               (emailconfirm (url-parameter-value :emailconfirm input))
               (suggested-login (url-parameter-value :suggestedlogin input))
               (password-help? (url-parameter-value :helpwithpassword input))
               (*bll-login-or-email*
                (and login-or-email (space-trim login-or-email)))
               (*bll-state* (if state (keywordize state) :none))
               (*bll-password* (and password (space-trim password)))
               (*bll-new-or-previous* 
                (cond
                 ((and (null new-session?) (null prev-session?))
                  (if previous-new-or-prev-state 
                      (keywordize previous-new-or-prev-state)
                    :none
                    ))
                 ((and (null new-session?) prev-session?) :prev)
                 ((and new-session? (null prev-session?)) :new)
                 ((and new-session? prev-session?) 
                  (error "This should be impossible!"))
                 ))
               (*bll-configuration*
                (if configuration (keywordize configuration) :default))
               (*bll-new-account-login* 
                (and new-account-login (space-trim new-account-login)))
               (*bll-new-account-email*
                (and new-account-email (space-trim new-account-email)))
               (*bll-new-account-fullname*
                (nil-or-space-trim new-account-fullname))
               (*bll-new-account-affiliation*
                (nil-or-space-trim new-account-affiliation)))

          (if (null cl-user:*allow-anonymous-logins*)
                
              (ecase *bll-state* 
                (:none 
                 (with-biobike-login-page-headers 
                   (page-for-password-login)
                   ))
                (:password 
                 (if password-help?
                     (with-biobike-login-page-headers
                       (page-for-password-help)
                       )
                   (initial-password-login password)
                   ))
                (:passwordhelp 
                 (let ((login *bll-login-or-email*))
                   (if (or (null login) (nullstring? login))
                       (with-biobike-login-page-headers
                         (page-for-password-help)
                         )
                     (password-help login)
                     ))))
     
            (ecase *bll-state* 
             
              (:none 
               (with-biobike-login-page-headers
                 (page-for-initial-login)
                 ))

              (:initial (initial-weblogin))
              
              (:new-account 
               (cond
                (pseudo-account?
                 (let ((entered-login *bll-login-or-email*))
                   (if (string-equal suggested-login entered-login)
                       ;; *** work in progress ***
                       (let ((email *bll-new-account-email*))
                         (let ((account-record
                                (add-new-pseudo-account
                                 entered-login 
                                 :email
                                 (when (and email (not (nullstring? email)))
                                   email
                                   ))))
                           (biobike-do-the-login account-record)
                           ))
                     (if (or (nullstring? entered-login)
                             (null entered-login))
                         (with-biobike-login-page-headers 
                           (page-for-invalid-login))
                       (let ((account-record 
                              (account-exists? entered-login :by :login)))
                         (cond
                          ((null account-record) 
                           (with-biobike-login-page-headers
                             (page-for-verifying-new-pseudo-account 
                              entered-login
                              )))
                          ((pseudo-account? account-record) 
                           (with-biobike-login-page-headers
                             (page-for-initial-login :taken entered-login)
                             ))
                          (t 
                           (with-biobike-login-page-headers
                             (page-for-initial-login :taken entered-login)
                             ))))))))
                (make-the-account? 
                 (with-biobike-login-page-headers 
                   (verify-new-account-information)
                   ))
                (goback? 
                 (with-biobike-login-page-headers
                   (page-for-initial-login)
                   ))
                (t (login-failed "Internal error in :new-account state"))
                ))

              (:verifyaccount
               (cond
                (goback? 
                 (with-biobike-login-page-headers
                   (page-for-creating-new-account 
                    :login *bll-new-account-login*
                    :email *bll-new-account-email* 
                    :fullname *bll-new-account-fullname*
                    :affiliation *bll-new-account-affiliation*
                    )))
                (confirmed?
                 (let ((account-record
                        (add-new-account 
                         :aliases nil
                         :login
                         (keywordize (string-upcase *bll-new-account-login*))
                         :password nil
                         :fullname *bll-new-account-fullname*
                         :email *bll-new-account-email*
                         :expiration nil
                         :status :standard
                         :other 
                         (when *bll-new-account-affiliation*
                           (list :affiliation *bll-new-account-affiliation*)
                           ))))
                   (unless account-record
                     (login-failed 
                      (one-string-nl
                      "Account already exists!!"
                      "This is odd, because it didn't exist"
                      "when you began the account creation process..."
                      "Please report this to the system administrators."
                      )))
                   (biobike-do-the-login account-record)
                   ))
                (t (login-failed "Internal error in :verifyaccount state"))
                ))
              
              (:verifypseudo 
               (cond
                (goback?
                 (with-biobike-login-page-headers (page-for-initial-login)))
                (confirmed? 
                 (let ((account-record 
                        (add-new-pseudo-account
                         (keywordize (string-upcase *bll-new-account-login*))
                         )))
                   (biobike-do-the-login account-record)
                   ))
                (t (login-failed "Internal error in :verifypseudo state"))
                ))
              
              (:emailconfirm 
               (let ((account-record
                      (account-exists? *bll-login-or-email* :by :login)))
                 (if (string-equal (user-email account-record) emailconfirm)
                     (biobike-do-the-login account-record)
                   (with-biobike-login-page-headers
                     (page-for-email-confirm account-record t)
                     ))))
              
              (:password 
               (let ((account-record
                      (account-exists? *bll-login-or-email* :by :login)))
                 (if (password-ok? password (user-drowssap account-record))
                     (biobike-do-the-login account-record)
                   (with-biobike-login-page-headers
                     (page-for-initial-login :bad-password t)
                     ))))
                 
              )))
        
      (login-failed 
       (c)
       (login-error-page (login-failed-reason c))
       )

      (error 
       (c)
       (html 
        (:princ-safe (formatn "Oops!  ~A~%" c))
        :br 
        (:princ-safe "Untrapped internal login code error!")
        :br
        (:princ-safe "Please contact the system administrators!")
        ))
      )))



(defun ready-to-log-in ()
  (html (:princ-safe "Ready to log in!")))

(defun initial-weblogin ()
  (if (new-login-email-address? *bll-login-or-email*)
      ;; is the entered email address syntactically plausible? 
      (if (not (valid-email? *bll-login-or-email*))
          (with-biobike-login-page-headers (page-for-invalid-email))
        ;; is the entered email address in the password file?
        (vif (account-record (account-exists? *bll-login-or-email* :by :email))
             (let ((login (user-login account-record))
                   (guru? (eq (user-status account-record) :guru)))
               ;; is the user a guru? If so, he must provide a password
               (if guru? 
                   (with-biobike-login-page-headers (page-for-guru-login login))
                 (biobike-do-the-login account-record)
                 ))
             ;; email address not found in password file.  
             (multiple-value-bind (login modified? original)
                 (derive-suggested-login-from-email *bll-login-or-email*)
               (with-biobike-login-page-headers
                 (page-for-creating-new-account
                  :login login :email *bll-login-or-email*
                  :message-type (when modified? :revised-login)
                  :original original
                  )))))
    (if (not (valid-username? *bll-login-or-email*))
        (with-biobike-login-page-headers (page-for-invalid-login))
      (vif (account-record (account-exists? *bll-login-or-email* :by :login))
           ;; account record exists, but account is only a pseudo account
           (if (pseudo-account? account-record)
               (let ((login (string-upcase *bll-login-or-email*)))
                 (if (impermissible-login login)
                     (with-biobike-login-page-headers 
                       (page-for-initial-login :package-conflict login))
                   (biobike-do-the-login account-record)
                   ))
             (cond
              ;; trapdoor for me!
              ((eq (keywordize (user-login account-record)) :massar)
               (biobike-do-the-login account-record))
              ;; real account, account has guru status 
              ((eq (user-status account-record) :guru) 
               (with-biobike-login-page-headers
                 (page-for-guru-login (user-login account-record))))
              (t
               ;; normal real account.  ask for email address. 
               (with-biobike-login-page-headers 
                 (page-for-email-confirm account-record nil)))
              ))
           (let ((login (string-upcase *bll-login-or-email*)))
             (if (impermissible-login-package login)
                 (with-biobike-login-page-headers
                   (page-for-initial-login :package-conflict login))
               ;; no account with the login that the user provided exists.  
               ;; ask the user if he wants to make an account
               (with-biobike-login-page-headers 
                 (page-for-creating-new-account)
                 )))))

    ))

(defun initial-password-login (password)
  ;; Does the login name seem to be a email address?
  (if (new-login-email-address? *bll-login-or-email*)
      ;; is the entered email address syntactically plausible? 
      (if (not (valid-email? *bll-login-or-email*))
          ;; Nope.
          (with-biobike-login-page-headers 
            (page-for-password-login :bad-email t))
        ;; Yup. Is the entered email address in the password file?
        (vif (account-record (account-exists? *bll-login-or-email* :by :email))
             ;; Yup. Is the stored password the same as the entered password?
             (if (password-ok? password (user-drowssap account-record))
                 (biobike-do-the-login account-record)
               ;; Nope.
               (with-biobike-login-page-headers
                 (page-for-password-login :oops t)
                 ))
             ;; Can't find email address.
             (with-biobike-login-page-headers
               (page-for-password-login :oops t)
               )))
    ;; It looks like a login name. Does that login exist in the password file?
    (vif (account-record (account-exists? *bll-login-or-email* :by :login))
         ;; Yes.  Is the stored password the same as the entered password?
         (if (password-ok? password (user-drowssap account-record))
             (if (impermissible-login *bll-login-or-email*)
                 (with-biobike-login-page-headers 
                   (page-for-password-login :oops t))
               (biobike-do-the-login account-record))
           ;; Nope.
           (with-biobike-login-page-headers
             (page-for-password-login :oops t)
             ))
         ;; Can't find login name.
         (with-biobike-login-page-headers
           (page-for-password-login :oops t)
           ))))

(defun password-help (login)
  (with-biobike-login-page-headers
    (let ((account (or (account-exists? login :by :login)
                       (account-exists? login :by :email))))
      (cond
       ((user-email account) 
        (let ((email (user-email account))
              (password (user-drowssap account)))
          (handler-case 
              (email-me (list "Your password is" password)
                        :to email :subject "Your Biobike info")
            (error 
             (c)
             (page-for-password-help :email-error (list email c))
             ))
          (page-for-password-help :password-sent t)
          ))
       (t (page-for-password-help :oops t))
       ))))
      
                     

(defun page-for-initial-login 
       (&key (bad-password nil) (package-conflict nil) (taken nil))
  (let ((oops (or bad-password package-conflict taken)))
    (hidden-input "state" "initial")
    (when oops
      (redtext
       (cond
        (bad-password 
         (one-string
          "Invalid login attempt. You may have typed your password wrong.  "
          "Please try again."
          ))
        (package-conflict
         (formatn
          (one-string
           "Sorry. You cannot use the username '~A' because it conflicts "
           "with a name essential to the internals of the Biobike system.  "
           "Please use a different login name.")
          package-conflict
          ))
        (taken 
         (formatn
          (one-string
           "Sorry.  You cannot use the username '~A' because it is already "
           "in use.  Please try a different username."
           )
          taken
          ))))
      (html :br))
    (new-html-for-login-box :prior? t)
    ;; programming mode radio buttons
    (insert-programming-mode-radio-buttons :default)
    ;; previous/new login buttons
    (new-insert-previous-and-new-login-buttons)
    (new-insert-login-addenda)
    (unless oops
      (login-credits cl-user:*ai*)
      (login-disclaimer cl-user:*ai*)
      )))


(defun page-for-invalid-login ()
  (page-for-invalid-login-aux 
   (one-string
    "Your login name may contain only English letters and numbers; "
    "the first character must be a letter; and it "
    "must be at least two characters long.  Please try again."
    )))

(defun page-for-invalid-email ()
  (page-for-invalid-login-aux
   (one-string
    "Your email address must contain "
    "a single '@' along with letters, numbers, periods, hypens, "
    "and underscores."
    "  No other characters are allowed.  Please try again."
    )))

(defun page-for-invalid-login-aux (message)
  (html 
   (hidden-input "state" "initial")
   (hidden-input "prev-np-state" (string *bll-new-or-previous*))
   (redtext message)
   (new-html-for-login-box :login *bll-login-or-email*)
   (insert-programming-mode-radio-buttons *bll-configuration*)
   (new-insert-previous-and-new-login-buttons)
   (new-insert-login-addenda)
   ))

(defun page-for-email-confirm (account-record bad-email?)
  (html 
   (hidden-input "state" "emailconfirm")
   (hidden-input "loginname" (string *bll-login-or-email*))
   (hidden-input "prev-np-state" (string *bll-new-or-previous*))
   (html-for-email-confirm account-record bad-email?)
   (insert-programming-mode-radio-buttons *bll-configuration*)
   (new-insert-previous-and-new-login-buttons)
   (new-insert-login-addenda)
   ))

(defun page-for-guru-login (login)
  (html
   (hidden-input "state" "password")
   (hidden-input "prev-np-state" (string *bll-new-or-previous*))
   (new-html-for-login-box 
    :login-locked? t :password? t :login login)
   (insert-programming-mode-radio-buttons *bll-configuration*)
   (new-insert-previous-and-new-login-buttons)
   (new-insert-login-addenda)
   ))

(defun page-for-password-login (&key (oops nil) (bad-email nil))
  (html
   (hidden-input "state" "password")
   (cond
    (oops 
     (redtext
      (one-string
       "Either your registration was not recognized or "
       "your password was typed incorrectly.  Please try again."
       )))
    (bad-email
     (redtext
      "You entered a badly-formed email address.  Please try again."
      )))
   (new-html-for-login-box :password? t)
   (when oops
     (html
      "&nbsp;&nbsp;"
      (:td ((:input :type "submit" :value ""
             :name "dummy" :style "width: 0; height: 0; border: none;")))
      ((:input :type "submit" :value "Forgot my password - Help" 
        :name "helpwithpassword"))
      :br
      ))
   (insert-programming-mode-radio-buttons *bll-configuration*)
   (new-insert-previous-and-new-login-buttons)
   (new-insert-login-addenda)
   (unless oops
     (login-credits cl-user:*ai*)
     (login-disclaimer cl-user:*ai*)
     )))

(defun page-for-password-help 
       (&key (oops nil) (password-sent nil) (email-error nil))
  (html
   (hidden-input "state" "passwordhelp")
   (cond
    (oops
     (redtext 
      (formatn "Sorry, no password found for '~A'." *bll-login-or-email*)))
    (password-sent (greentext "Your password has been emailed to you!"))
    (email-error
     (redtext 
      (formatn "Could not send email to '~A'.  Actual error: ~A"
               (first email-error) (second email-error)
               )))
    (t
     (greentext 
      (one-string
       "Enter your biobike login or email and your "
       "password will be mailed to you."
       ))))
   :br :br
   (unless password-sent
     (html
      (:table
       (:tr 
        (:td "LOGIN or EMAIL:")
        (:td ((:input :type "text" :name "loginname")))
        (:td ((:input :type "submit" :value "Send me my password")))
        ))))
   (new-insert-login-addenda)
   ))
     
(defun page-for-creating-new-account 
       (&key
        error-message login email fullname affiliation message-type original)
  (let ((suggested-login (or login *bll-login-or-email*)))
    (html 
     (hidden-input "state" "new-account")
     (hidden-input "suggestedlogin" suggested-login)
     (greentext "&nbsp;&nbsp;You are not a registered user yet! " :safe? nil)
     :br
     (:ul
      (:li
       (greentext
        (one-string
         "If you would like to register, please enter the "
         "requested information below. Your email address, name, "
         "and affiliation will be used in communications you may initiate "
         "with either the Help Desk or with other users.  "
         "If you annotate via the Biobike system, this information "
         "will be used "
         "to identify you as the person who made the annotations.  "
         "Otherwise, this information will not be shared with anyone."
         )))
      (:li
       (greentext
        (formatn 
         (one-string
          "If you do not wish to register, "
          "click the 'No registration' button.  "
          "You will still be able to use the system; you will be logged "
          "in as the user '~A'~A, but without registering someone "
          "else could come along and use the same name.")
         suggested-login
         (case message-type 
           (:revised-login 
            (formatn " (the login name '~A' is already in use)" original))
           (otherwise "")
           ))))
      (:li
       (greentext 
        "Or you can return to the initial Biobike login page using 'Exit'."
        )))
     (greentext "Please provide the following information:")
     :br :br
     (when error-message 
       (html (redtext error-message) :br :br))
     (new-biobike-account-table login email fullname affiliation)
     (insert-new-account-buttons :login suggested-login)
     (insert-programming-mode-radio-buttons *bll-configuration*)
     (html :br)
     (new-insert-login-addenda)
     )))

(defun new-biobike-account-table (login email fullname affiliation)
  (html
   (:table 
    (:tr
     (:td (:big "Nickname:"))
     (:td ((:input :type "text" :name "loginname"
            :value 
            (string (or login *bll-login-or-email*))
            )))
     (:td "(Letters and numbers only, no spaces)"))
    (:tr 
     (:td (:big "Email:"))
     (:td ((:input :type "text" :name "email" :value (or email "")))))
    (:tr 
     (:td (:big "Name:"))
     (:td ((:input :type "text" :name "fullname" :value (or fullname ""))))
     (:td "(First and last)"))
    (:tr 
     (:td (:big "Affiliation:"))
     (:td ((:input :type "text" :name "affiliation"
            :value (or affiliation ""))))
     (:td "(School, institution, etc, or 'None')")
     ))))


(defun page-for-verifying-new-account ()
  (flet ((maybe-not-provided (text) 
           (if (or (null text) (nullstring? text)) "(not provided)" text)))
    (html 
     (hidden-input "state" "verifyaccount")
     (hidden-input "configuration" (string *bll-configuration*))
     (hidden-input "loginname" *bll-new-account-login*)
     (hidden-input "email" *bll-new-account-email*)
     (hidden-input "fullname" *bll-new-account-fullname*)
     (hidden-input "affiliation" *bll-new-account-affiliation*)
     ((:font :color "green")
      (:big 
       (:b
        (:princ-safe 
         "Please verify the following information and click Confirm if correct."
         ))))
     :br :br
     (:table 
      (:tr
       (:td (:big "Nickname:"))
       (:td (browntext *bll-new-account-login*)))
      (:tr 
       (:td (:big "Email:"))
       (:td (browntext *bll-new-account-email*)))
      (:tr 
       (:td (:big "Full name:"))
       (:td (browntext (maybe-not-provided *bll-new-account-fullname*))))
      (:tr 
       (:td (:big "Affiliation:"))
       (:td (browntext (maybe-not-provided *bll-new-account-affiliation*))))
      )
     ((:input :type "submit" :value "Confirm" :name "confirmed"))
     (:princ "&nbsp;")
     ((:input :type "submit" :value "Go back" :name "goback"))
     :br 
     (new-insert-login-addenda)
     )))

(defun page-for-verifying-new-pseudo-account (login)
  (html
   (hidden-input "state" "verifypseudo")
   (hidden-input "loginname" login)
   (hidden-input "configuration" (string *bll-configuration*))
   (greentext 
    (formatn "You have asked to log in as the guest user '~A'.  " login))
   :br 
   (greentext "Please confirm.")
   :br :br
   ((:input :type "submit" :value "Yes, log me in" :name "confirmed"))
   "&nbsp;&nbsp;"
   ((:input :type "submit" :value "No, take me back" :name "goback"))
   :br
   (new-insert-login-addenda)
   ))

(defun new-account-page-with-error-message (message)
  (page-for-creating-new-account 
   :error-message message
   :login *bll-new-account-login* 
   :email *bll-new-account-email*
   :fullname *bll-new-account-fullname* 
   :affiliation *bll-new-account-affiliation*
   ))

(defun verify-new-account-information ()
  (let ((login-valid? (valid-username? *bll-new-account-login*))
        (email-valid? (valid-email? *bll-new-account-email*))
        (login-provided? (not (nullstring? *bll-new-account-login*)))
        (email-provided? (not (nullstring? *bll-new-account-email*)))
        (fullname-provided? 
         (and *bll-new-account-fullname* 
              (not (nullstring? *bll-new-account-fullname*))))
        (affiliation-provided? 
         (and *bll-new-account-affiliation*
              (not (nullstring? *bll-new-account-affiliation*)))))
    (cond
     ((and (not login-provided?) (not email-provided?))
      (new-account-page-with-error-message 
        "You must provide a valid login name and an email address!"
        ))
     ((not login-provided?)
      (new-account-page-with-error-message
       "You must provide a valid login along with your email address."))
     ((not email-provided?)
      (new-account-page-with-error-message
       "You must provide an email address along with your login name."))
     ((and (not login-valid?) (not email-valid?))
      (new-account-page-with-error-message 
       (one-string
        "Your login name may contain only letters and numbers.  "
        "No other characters are allowed.  "
        "Your email address must contain "
        "a single '@' along with letters, numbers, periods, hypens, "
        "and underscores."
        "  No other characters are allowed.  Please try again."
        )))
     ((not login-valid?) 
      (new-account-page-with-error-message
       (one-string
        "Your login name may contain only letters and numbers.  "
        "No other characters are allowed. Please try again."
        )))
     ((not email-valid?) 
      (new-account-page-with-error-message
       (one-string
        "Your email address must contain "
        "a single '@' along with letters, numbers, periods, hypens, "
        "and underscores."
        "  No other characters are allowed.  Please try again."
        )))
     ((not fullname-provided?)
      (new-account-page-with-error-message
       (one-string
        "You must provide your full name in order to create an account.  "
        "Please enter your full name in the 'Name:' box.")))
     ((not affiliation-provided?)
      (new-account-page-with-error-message
       (one-string
        "You must provide an affiliation (your company name, university, or "
        "'self') in order to create an account.  "
        "Please enter your affililation in the 'Affiliation:' box.")))
     (t 
      (let ((login (string-upcase *bll-new-account-login*)))
        (let ((existing-login? (account-exists? login :by :login))
              (existing-email?
               (account-exists? *bll-new-account-email* :by :email)))
          (cond
           ((and existing-login? existing-email?) 
            (new-account-page-with-error-message
             (one-string
              "This email address is already attached to another user name.  "
              "Contact the system administrators if you think you have "
              "forgotten your username.  If you want to use the system now "
              "you can log in with a login name and no email by going "
              "back to the original login page by clicking back on "
              "your browswer a few times."
              )))
           (existing-login? 
            (new-account-page-with-error-message
             (one-string
              "That login name is already in use by another user.  "
              "Please select a different login name."
              )))
           (existing-email? 
            (new-account-page-with-error-message
             (one-string
              "This email address is already attached to another user name.  "
              "Contact the system administrators if you think you have "
              "forgotten your username."
              )))
           (t 
            (if (or (impermissible-login login) 
                    (impermissible-login-package login))
                (new-account-page-with-error-message
                 (formatn
                  (one-string
                   "Sorry.  You cannot use the username '~A' because it "
                   "conflicts with a name essential to the internals of "
                   "the Biobike system. Please use a different login name.")
                  login
                  ))
              (page-for-verifying-new-account)
              )))))))))

(defun impermissible-login (login)
  (member (keywordize login) '(:docuser :evaluser)))

(defun impermissible-login-package (login)
  (member (find-package login) cl-user::*all-non-user-packages*))

(defun pseudo-account? (account-record) 
  (let ((fullname (user-fullname account-record)))
    (or (null fullname) (string-equal "" fullname))
    ))

(defun hidden-input (name value)
  (html ((:input :type "hidden" :name name :value value))))

(defun new-login-email-address? (s) (not (null (position #\@ s))))

(defun new-html-for-login-box 
       (&key (prior? nil) (password? nil) (login nil) (login-locked? nil))
  (when prior? 
    (html
     (greentext "Please log in with either: ")
     (:ul 
      (:li (greentext "Your email address"))
      (:li (greentext "Your Biobike nickname (if you've already set one up)"))
      (:li 
       (greentext
        "A nickname you would like to use (if this is your first time here)"))
      )))
  (html
   (:table 
    (:tr
     (:td (:big "LOGIN or EMAIL: "))
     (if login 
         (if login-locked? 
             (html 
              (:td 
               ((:input :type "hidden" :name "loginname" :value login))
               (:b (:princ-safe (string-capitalize login)))
               ))
           (html (:td ((:input :type "text" :name "loginname"
                        :value (string login))))))
       (html (:td ((:input :type "text" :name "loginname")))))
     (when prior?
       (html 
        (:td (:i ((:font :color "green") "(prior registration not required)")))
        ))
     (when password? 
       (html 
        (:tr
         (:td (:big "PASSWORD: "))
         (:td ((:input :type "password" :name "password")))
         )))))))

(defun html-for-email-confirm (account-record bad-email?)
  (let ((login (user-login account-record))
        (fullname (user-fullname account-record)))
    (html
     (:table 
      (:tr
       (:td (:big "Login: "))
       (:td (:b (:princ-safe (string login)))))
      (:tr 
       (:td (:big "Name: ")) 
       (:td (:b (:princ-safe (if fullname (string fullname) "Not provided"))))))
     ((:font :color "green")
      (:princ-safe "If this is not you, ")
      ((:a :href (new-login-webpage-url cl-user:*ai*)) "please click here")
      (:princ-safe 
       " to return to the login page and log in under a different user name.")
      :br
      (:princ-safe 
       "  Otherwise enter your email address to confirm your login.")
      )
     (when bad-email? 
       (html
        :br
        (redtext
          (one-string
           "The email address you entered does not correspond to the "
           "login name you provided.  Please try again."
           ))))
     (:table 
      (:tr 
       (:td (:big "Email: "))
       (if bad-email? 
           (html 
            (:td 
             ((:input :type "text" :name "emailconfirm" :value bad-email?))))
         (html (:td ((:input :type "text" :name "emailconfirm"))))
         ))))))

(defun new-insert-previous-and-new-login-buttons ()
  (let ((nlstyle "") (psstyle ""))
    (ecase *bll-new-or-previous* 
      ((:none nil))
      (:new 
       (setq nlstyle "font-weight: bold;"))
      (:prev 
       (setq psstyle "font-weight: bold;"))
      )
    (html
     (:table 
      (:tr
       ;; Need this dummy submit button because otherwise there is no way
       ;; to distinguish between the user hitting ENTER in the input box
       ;; vs clicking on the NEW LOGIN button.  
       (:td ((:input :type "submit" :value ""
              :name "dummy" :style "width: 0; height: 0; border: none;")))
       (:td ((:input :type "submit" :value "New Login"
              :name "newlogin" :style nlstyle)))
       (:td "&nbsp;&nbsp;")
       (:td ((:input :type "submit" :value "Previous Session"
              :name "prevsession" :style psstyle
              ))))))))


(defun insert-new-account-buttons (&key (login nil))
  (html
   (:table
    (:tr
     (:td 
      ((:input :type "submit" :value "Register" :name "newaccount"))))
    (:tr
     (:td ((:input :type "submit" :value "No Registration" :name "nothanks")))
     (when login
       (html 
        (:td (greentext (formatn "(But log me in anyway as '~A')" login)))
        )))
    (:tr
     (:td ((:input :type "submit" :value "Exit" :name "goback")))
     (:td (greentext "(Return to initial login page)")))
    )))
      

;; this is in a table, otherwise text extends to the right margin
;; underneath the bike picture if the screen is not wide enough
(defun new-insert-login-addenda ()
  (html
   :br
   (:table 
    (:tr
     (:td (login-addenda cl-user:*ai*))))))

(defun derive-suggested-login-from-email (email)
  (let* ((before (first (string-split email #\@)))
         (modified? nil)
         (original before))
    (when (not (valid-username? before))
      (setq before (remove-if-not 'alphanumericp before))
      (setq original before))
    (loop while t
          for j from 1 
          do
          (if (account-exists? before :by :login)
              (progn
                (setq before (formatn "~A~D" before j))
                (setq modified? t))
            (return (values before modified? original))
            ))))
            
(defun colored-text (color text &key (safe? t))
  (flet ((doit (colorstring)
           (if safe?
               (html ((:font :color colorstring) (:princ-safe text)))
             (html ((:font :color colorstring) (:princ text)))
             )))
    (if (keywordp color) 
        (doit (string-downcase (string color)))
      (doit color)
      )))

(defun browntext (text &key (safe? t)) (colored-text :brown text :safe? safe?))
(defun greentext (text &key (safe? t)) (colored-text :green text :safe? safe?))
(defun redtext (text &key (safe? t))(colored-text :red text :safe? safe?))
        
(defun biobike-login-and-create-package (account-record)

  (let* ((login (user-login account-record))
         (email (user-email account-record))
         (user-string (package-string-of-a-user login))
         (lowername (string-downcase user-string))
         (visitor-package-symbol (package-symbol-of-a-user login))
         (visitor-homedir (visitor-directory lowername))
         (existing? nil)
         )

    (verify-a-user-directory visitor-package-symbol visitor-homedir)

    ;; Create a package for the visitor.  Its name is the login
    ;; name of the visitor.  It might already exist, because the
    ;; user might have logged in previously using the same or
    ;; a different session.

    (if (find-package visitor-package-symbol)
        (progn
          (when (null (package-use-list (find-package visitor-package-symbol)))
            (login-failed 
             (formatn 
              (one-string-nl
               "Your package has been corrupted!!"
               "Your package does not now use any packages!!"
               "You cannot log in until this is fixed."
               "Either the system will have to be rebooted or you"
               "can contact a system administrator, or you can try"
               "to fix this yourself by logging in as someone else and"
               "doing package surgery (but only if you know exactly what"
               "you're doing!)"))))
          (setq existing? t))
      (handler-case
          (set-up-user-package visitor-package-symbol)
        (error 
         (c)
         (login-failed
          (formatn
           (one-string-nl
            "System failure:  Could not create user package ~A. "
            "Actual error is ~A "
            "Please notify system administrators. "
            "You cannot log in until this is rectified.")
           visitor-package-symbol c
           )))))

      (setf (get visitor-package-symbol :password-file-email) email)
      (setf (get visitor-package-symbol :login-email) email)
      (setf (get visitor-package-symbol :email) email)
      (setf (get visitor-package-symbol :full-name) 
            (or (user-fullname account-record) 
                (package-string-of-a-user (string visitor-package-symbol))))
      (setf (get visitor-package-symbol :initial-login-time)
            (get-universal-time))
      (when (pseudo-account? account-record)
        (setf (get visitor-package-symbol :pseudouser) t))

    ;; Initialize completion system for user
    (when (not existing?)
      (multiple-value-bind (okay? error-condition)
          (initialize-cih-for-user visitor-package-symbol)
        (unless okay?
          (login-failed
           (formatn
            (one-string-nl
             "System failure: Could not initialize your completion directory."
             "Please notify the system administrators."
             "You cannot log in until this is rectified."
             "Actual error: ~A")
            error-condition)))))
    
    (execute-action-items '*after-new-login-actions*)

    (values visitor-package-symbol existing?)

    ))

(defun set-up-biobike-language-configuration (config visitor-package-symbol)
  (case config
    (:vpl 
     (setf (get visitor-package-symbol :login-language) :bbl)
     (setf (get visitor-package-symbol :login-code-creation-mode)
           :vpl))
    (:lisp 
     (setf (get visitor-package-symbol :login-language) :lisp)
     (setf (get visitor-package-symbol :login-code-creation-mode)
           :weblistener))
    (:bbl
     (setf (get visitor-package-symbol :login-language) :bbl)
     (setf (get visitor-package-symbol :login-code-creation-mode)
           :weblistener
           ))))

(defun go-to-biobike-user-input-page (config new-sessionid)
  (case config
    (:vpl 
     (setf (get new-sessionid :session-language) :bbl)
     (forward-funcall 'wb::init-vpl)
     (html (:princ (indirect-to-vpl new-sessionid))))
    (:lisp
     (setf (get new-sessionid :session-language) :lisp)
     (html 
      (:princ (indirect-to-redisplay 0 new-sessionid))
      ))
    (:bbl
     (setf (get new-sessionid :session-language) :bbl)
     (html 
      (:princ (indirect-to-redisplay 0 new-sessionid)))
     )))

(defun biobike-do-the-login (account-record)
  (let* ((email (user-email account-record))
         (*user-provided-email* email)
         ;; this should not be used anymore but we bind it
         ;; because the code that gets called references it
         (*guru-login-mode* nil))
    (multiple-value-bind (visitor-package-symbol existing?)
        (biobike-login-and-create-package account-record)
      (set-up-biobike-language-configuration
       *bll-configuration* visitor-package-symbol)
      (flet ((new-session ()
               (let ((new-sessionid
                      (progn 
                        (connect-to-new-session
                         visitor-package-symbol existing? nil))))
                 ;; This should be methodized for BioLingua and
                 ;; common code from below pulled out
                 (go-to-biobike-user-input-page 
                  *bll-configuration* new-sessionid)
                 )))
        (when (null visitor-package-symbol) 
          (login-failed
           "Internal error! LOGIN-AND-CREATE-PACKAGE returned NIL!"))
        (case *bll-new-or-previous*
          (:new (new-session))
          ((:prev :none)
           (let ((sessions (user-session-info visitor-package-symbol)))
             (case *bll-configuration*
               (:vpl
                (if (and sessions (some-vpl-session? sessions))
                    (previous-vpl-session-page
                     visitor-package-symbol existing?)
                  (new-session)
                  ))
               ((:lisp :bbl)
                (if (and sessions (some-weblistener-session? sessions))
                    (previous-session-page
                     visitor-package-symbol existing?)
                  (new-session)
                  )))))
          (otherwise
           (login-failed 
            "Internal error: Impossible to reach clause in :new/:prev case"))
          )))))

