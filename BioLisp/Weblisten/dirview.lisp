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

;;; Author:  JP Massar

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Utility routines.

(defun canonicalize-pathname-namestring (x)
  "Make sure all slashes are forward slashes"
  (case (os?)
    ((:unix :windows) (substitute #\/ #\\ x))
    (otherwise (error "Need to implement this for os ~A" (os?)))
    ))

(defun canonicalize-pathname-to-namestring (p)
  (canonicalize-pathname-namestring 
   (namestring (cl-user:translate-simple-lp p))
   ))

(defun appname () (application-name cl-user:*ai*))

(defun viewable-toplevel-directories ()
  (mapcar
   'canonicalize-pathname-to-namestring
   (viewable-application-toplevel-directories cl-user:*ai*)
   ))

(defun user-viewable-directory-namestrings ()
  (append
   (if (wb::weblistener-guru-p)
       (list (canonicalize-pathname-namestring *instance-home-directory*))
     nil)
   (mapcar 
    'canonicalize-pathname-to-namestring
    (cons 
     (visitor-directory *username*)
     (application-shared-files-directories cl-user:*ai*)
     ))
   (viewable-toplevel-directories)
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      
(defun generic-directory-command-url (url-creator pathname)
  (funcall 
   url-creator
   :pathname 
   (url-safe-string 
    (canonicalize-pathname-namestring (namestring pathname)))))

(defun directory-listing-url (directory-pathname)
  (generic-directory-command-url 
   'make-wb-directory-listing-url directory-pathname))
(defun lisp-file-listing-url (file-pathname)
  (generic-directory-command-url 
   'make-wb-lisp-file-listing-url file-pathname))
(defun up-directory-url (directory-pathname)
  (generic-directory-command-url 
   'make-wb-up-directory-url directory-pathname))

(defun safe-editfile-url (filename)
  (forward-funcall 'make-editfile-url :file (url-safe-string filename)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; This gets triggered from the 'Browse BioFiles' link.  This is
;;; the standard entry point into the directory/file browser system.

(publish 
 :path *weblistener-toplevel-directories-url*
 :content-type cl-user::*html-publish-content-type*
 :function
 (lambda (req ent)
   (let* ((input (request-query req))
          (package-name (url-parameter-value :pkg input))
          (package-symbol (keywordize package-name))
          )
     (execute-with-standard-weblistener-environment
      req ent package-symbol
      (lambda () (html-for-weblistener-viewable-directories))
      ))))

;;; Just list the toplevel directories the user can view.

(defun html-for-weblistener-viewable-directories ()
  (let ((title (formatn "~A Viewable File Directories" (appname))))
    (with-standard-weblistener-page-header (title)
      (html :hr)
      (html-for-simple-list-of-directories 
       (user-viewable-directory-namestrings)
       ))))

(defun html-for-simple-list-of-directories (dirs)
  (if (null dirs)
      (html 
       (:h3 (:princ-safe "<<< No directories available to be viewed >>>")))
    (dolist (path dirs)
      (html 
       (:h3 ((:a :href (directory-listing-url path)) (:princ-safe path)))
       ))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Code implementing directory listings.


(publish 
 :path *wb-directory-listing-url*
 :content-type cl-user::*html-publish-content-type*
 :function
 (lambda (req ent)
   (let* ((input (request-query req))
          (package-name (url-parameter-value :pkg input))
          (package-symbol (keywordize package-name))
          (pathname (url-parameter-value :pathname input)))
     (execute-with-standard-weblistener-environment
      req ent package-symbol
      (lambda () (html-for-directory-listing pathname))
      ))))

;;; The code that computes the HTML for a directory listing.

(defun html-for-directory-listing 
    
       (path 
        &key
        (names-not-to-show '("drowssap"))
        (types-not-to-show '("fasl" "fsl"))
        (types-with-special-urls 
         '((lisp-file-listing-url 
            ("lisp" "lisp~" "lsp" "lsp~" "cl" "cl~"))
           ))
        (title (formatn "~A Directory Listing" (appname))))
  
  (with-standard-weblistener-page-header (title)

    ;; Separate the files from the subdirectories.
    ;; List the subdirectories first, with a link to the directory-lister url
    ;; Then list the files, each with a link dependent on its type, a DEL link
    ;; if the file is deleteable, and an EDIT link if the file is editable.
  
    (let* ((listing (directory-with-subdirs-in-directory-form path))
           (subdirs 
            (sort-directory-paths
             (remove-if-not #'pathname-names-directory? listing)))
           (files 
            (sort-filename-paths
             (remove-if #'pathname-names-directory? listing)))
           (directory 
            (canonicalize-pathname-namestring 
             (namestring (pathname-in-directory-form path))))
           (modifyable? (directory-is-modifyable-by-current-user? directory))
           )
      (html 
       (:h3 :br 
        " Listing for " (:princ-safe directory))
       :p
       (when modifyable?
         (html
          ((:a :href 
            (make-editor-new-file-or-directory-form-url :currentdir directory))
           (:small "New file or subdirectory"))
          "&nbsp;&nbsp"
          ((:a :href 
            (make-wb-delete-files-in-directory-url :currentdir directory))
           (:small "Delete directory files")
           )))
       :hr 
       (:table
        (:tr 
         (:td (:small "up"))
         (:td (:small ((:a :href (up-directory-url path)) (:princ-safe ".."))))
         (:td (:small " ")))
        (dirview-table-rows-for-directories subdirs modifyable?)
        :p
        (dirview-table-rows-for-files 
         files 
         types-with-special-urls names-not-to-show types-not-to-show
         modifyable?
         ))))))

(defun dirview-table-rows-for-directories (dirs modifyable?)
  (declare (ignore modifyable?))
  (let ((ndirs (length dirs)))
    (cond
     ((zerop ndirs) nil)
     ((< ndirs 6)
      (loop for dir in dirs do 
            (let ((url (directory-listing-url dir))
                  (tag (lastelem (pathname-directory dir))))
              (html 
               (:tr 
                (:td (:small "dir"))
                (:td (:small ((:a :href url) (:princ-safe tag))))
                )))))
     (t
      (with-two-html-table-columns (dirs dir count) ()
        (let ((url (directory-listing-url dir))
              (tag (lastelem (pathname-directory dir))))
          (html
           (:td (:small "dir"))
           (:td (:small ((:a :href url) (:princ-safe tag))))
           (:td (:small " "))
           (:td (:small "&nbsp;&nbsp;"))
           count
           )))))))

(defun dirview-table-rows-for-files

       (files
        types-with-special-urls names-not-to-show types-not-to-show
        modifyable?
        &aux 
        files-to-be-displayed files-and-urls
        )

  ;; Remove any files with filenames or filetypes we don't want displayed.

  (setq files-to-be-displayed
        (remove-if
         (lambda (file)
           (or
            (member 
             (pathname-name file)
             names-not-to-show :test 'string-equal)
            (member 
             (pathname-type file)
             types-not-to-show :test 'string-equal)))
         files
         ))
            
  ;; Publish URLs for each file to be displayed (either standard URLs
  ;; or special ones based on the file's type)

  (setq
   files-and-urls
   (loop for file in files-to-be-displayed
         as url = nil
         as type = (pathname-type file)
         collect
         (progn
           (unless
               (loop for (url-generator types) in types-with-special-urls do
                     (when (member type types :test 'string-equal) 
                       (setq url (funcall url-generator file))
                       ;; why is this next line here?  In case you want to
                       ;; download the file using the 'Download source' link.
                       (publish-path-for-file-in-viewable-subdir file)
                       (return t))
                     finally (return nil))
             (setq url (publish-path-for-file-in-viewable-subdir file)))
           (list file url)
           )))

  (with-two-html-table-columns (files-and-urls file-and-url count) ()
    (let* ((file (first file-and-url))
           (url (second file-and-url))
           (tag (file-namestring file))
           (date (file-write-date file))
           (datestring 
            (if date 
                (make-timestamp-string 
                 :universal-time date :mode :mmddyyhhmm)
              "")))
      count
      ;; If you're a guru, or the file is in your home directory,
      ;; or it is in one of the shared directories, then you can
      ;; edit it and delete it.
      (if modifyable?
          ;; An editable file.
          (html 
           (:td (:small 
                 ((:a :href (safe-make-editanyfile-url file nil nil nil))
                  ((:font :color :green) (:princ "edit")))
                 "&nbsp;"
                 ))
           (:td (:small ((:a :href url) (:princ-safe tag))))
           (:td (:small (:princ-safe datestring)))
           (:td (:small "&nbsp;&nbsp;"))
           )
        ;; A file you aren't allowed to edit.
        (html
         (:td (:small " "))
         (:td (:small ((:a :href url) (:princ-safe tag))))
         (:td (:small (:princ-safe datestring)))
         (:td (:small "&nbsp;&nbsp;"))
         )))))

(defun sort-directory-paths (paths &optional (predicate 'string-lessp))
  (flet ((last-directory-component (p) (lastelem (pathname-directory p))))
    (dolist (p paths)
      (unless (stringp (last-directory-component p))
        (ierror "Last component of subdirectory not a string!! ~A" p)))
    (sort paths predicate :key #'last-directory-component)
    ))

(defun sort-filename-paths (paths &optional (predicate 'string-lessp))
  (sort paths predicate :key 'file-namestring))


(defun viewable-subdir-publish-name (filepath)
  #.(one-string-nl
     "Create the 'alias' pathname via which FILEPATH can be reached"
     "and return it along with the real namestring of FILEPATH."
     "The alias is used in PUBLISH-FILE and then to generate a URL"
     "to access the raw file.")
  (block exit
    (let ((namestring 
           (canonicalize-pathname-namestring (namestring filepath))))
      (loop for view in (viewable-toplevel-directories)
            for n from 1 do
            (when (or (weblistener-guru-p)
                      (search view namestring :test 'string-equal))
              (return-from exit
                (values
                 (one-string
                  (formatn "/Topdir~D" n)
                  "/"
                  (canonicalize-pathname-namestring
                   (enough-namestring filepath (pathname view))
                   ))
                 namestring
                 ))))
      (values nil namestring)
      )))


(defun publish-path-for-file-in-viewable-subdir (filepath)
  "Publish an 'alias' pathname to FILEPATH and return that alias"
  (multiple-value-bind (publish-name namestring)
      (viewable-subdir-publish-name filepath)
    (when publish-name (publish-file :path publish-name :file namestring))
    publish-name
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code to create hyperlinked Lisp code displays from a .lisp file.

(publish 
 :path *wb-lisp-file-listing-url*
 :content-type cl-user::*html-publish-content-type*
 :function
 (lambda (req ent)
   (let* ((input (request-query req))
          (package-name (url-parameter-value :pkg input))
          (package-symbol (keywordize package-name))
          (pathname (url-parameter-value :pathname input))
          )
     (execute-with-standard-weblistener-environment
      req ent package-symbol
      (lambda () (lisp-file-listing-function pathname))
      ))))

(defun lisp-file-listing-function (pathname)
  (let ((title (formatn "~A Lisp File Listing" (appname))))
    (with-standard-weblistener-page-header (title) 
      (multiple-value-bind (publish-name namestring)
          (viewable-subdir-publish-name pathname)
        (declare (ignore namestring))
        (html-for-hyperlinked-lisp-file pathname :publish-name publish-name)
        ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           
;;; Code to deal with the 'up' link in a directory listing

(publish 
 :path *wb-up-directory-url*
 :content-type cl-user::*html-publish-content-type*
 :function
 (lambda (req ent)
   (let* ((input (request-query req))
          (package-name (url-parameter-value :pkg input))
          (package-symbol (keywordize package-name))
          (pathname-string (url-parameter-value :pathname input)))
     (execute-with-standard-weblistener-environment
      req ent package-symbol
      (lambda () (wb-up-directory-function pathname-string))
      ))))

(defun wb-up-directory-function (pathname-string)
  (let ((title (formatn "~A Directory Listing" (appname)))
        (directory-pathname (pathname pathname-string)))
    (cond
     ;; At permitted toplevel.  Go to list of viewable toplevel dirs.
     ((member pathname-string (viewable-toplevel-directories) 
              :test 'string-equal)
      (with-standard-weblistener-page-header (title)
        (html-for-simple-list-of-directories 
         (user-viewable-directory-namestrings)
         )))
     ;; Within one of the toplevel dirs.  Go up one directory level.
     ((some (lambda (viewable-path)
              (let ((pos (search viewable-path pathname-string 
                                 :test 'string-equal)))
                (and pos (zerop pos))
                ))
            (viewable-toplevel-directories))
      (html-for-directory-listing
       (merge-pathnames
        (make-pathname 
         :directory (butlast (pathname-directory directory-pathname))
         :defaults directory-pathname
         ))))
     (t
      (html 
       (:h3 "UNRECOGNIZED DIRECTORY: " 
        (:princ-safe pathname-string) (:princ-safe " !!!"))))
     )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; URL's and code dealing with deletion of files.
;;; (Can't delete directories yet)


;;; This URL triggers a menu page showing the files is a directory
;;; and a checkbox next to each one to delete it, with a button to
;;; trigger actual deletion once a number of checkboxes have been selected.

(define-url&pkg&args
 wb-delete-files-in-directory-url
 "/delfilesindirmenu.html" :currentdir)

;;; This URL handles the act of hitting the 'DELETE THEM' button from
;;; the above described page.

(define-url&pkg&args
 wb-delete-files-in-directory-form-response-url
 "/delfilesindirresponse.html" :currentdir)


(publish 
 :path *wb-delete-files-in-directory-url*
 :content-type cl-user::*html-publish-content-type*
 :function
 (lambda (req ent)
   (let* ((input (request-query req))
          (package-name (url-parameter-value :pkg input))
          (package-symbol (keywordize package-name))
          (currentdir (url-parameter-value :currentdir input))
          )
     ;; This binds *username*, *req* and *ent*
     (execute-with-standard-weblistener-environment
      req ent package-symbol
      (lambda () (wb-delete-files-in-directory-function currentdir))
      ))))

(defun wb-delete-files-in-directory-function (currentdir)

  ;; Find all the files in the directory CURRENTDIR 
  ;; (can't delete subdirectories yet...), and gather up other
  ;; useful information

  (let* ((listing (directory-with-subdirs-in-directory-form currentdir))
         (files
          (sort-filename-paths
           (remove-if #'pathname-names-directory? listing)))
         (nfiles (length files))
         (directory-name
          (canonicalize-pathname-namestring
           (namestring (pathname-in-directory-form currentdir))))
         (title (formatn "File Deletion Menu for ~A" directory-name))
         (editable-directory?
          (directory-is-modifyable-by-current-user? currentdir))
         )

    (block exit

      ;; Don't even show the deletion menu if this isn't a deletable
      ;; directory relative to who the user is.

      (when (not editable-directory?)
        (editor-oops "Directory ~A cannot be modified." directory-name)
        (return-from exit))

      ;; Create a form with 'hidden' information providing the current
      ;; directory, the number of files listed, etc, and a two-column
      ;; listing of the files in the directory along with a checkbox
      ;; for each file.  The checkboxes have names 'File0', 'File1' ...
      ;; so that we can retrieve the information about each checkbox
      ;; in the form handler once we know how many files were listed.

      (with-standard-weblistener-page-header (title)
        :hr
        (when (zerop nfiles)
          (html (:b "There are no files in the directory."))
          (return-from exit))
        (html
         ((:form :name "deletefiles" :method "POST"
           :action *wb-delete-files-in-directory-form-response-url*)
          :newline
          ((:input :type "HIDDEN" :name "currentdir" 
            :value currentdir))
          :newline
          ((:input :type "HIDDEN" :name "nfiles" 
            :value (formatn "~D" nfiles)))
          :newline
          ((:input :type "HIDDEN" :name "timestamp" 
            :value (formatn "~D" (get-universal-time))))
          ((:input :type "HIDDEN" :name "PKG" 
            :value (string (wb::user-session-id))))
          :newline
          ((:input :type "SUBMIT" :name "deletetop" :value "Delete Them"))
          :newline
          :p
          (with-two-html-table-columns (files file-entry count) ()
            (let ((identifier (formatn "File~D" count))
                  (namestring (file-namestring file-entry)))
              (html
               (:td
                ((:input :type "checkbox" :name identifier
                  :value 
                  ;; Using a name with an '&' in it apparently causes
                  ;; PortableAserve to die horribly but Allegro's aserve
                  ;; works just fine and passes the string through properly
                  ;; escaped.  So other than in Allegro you can't delete
                  ;; any file with a '&' in it, but at least the code
                  ;; doesn't blow up...
                  #+:allegro
                  namestring
                  #-:allegro
                  (url-safe-string namestring)
                  )
                 (:small (:princ-safe namestring))
                 )))))
          :newline
          :p
          ((:input :type "SUBMIT" :name "deletebottom" :value "Delete Them"))
          ))))))


;; Handle the form with all the checkboxes.
;; Extract the usual information from the form response, along with
;; a timestamp and which checkboxes were checked.  The value of each
;; checked checkbox is the name of the file to be deleted.

(publish
 :path *wb-delete-files-in-directory-form-response-url*
 :content-type cl-user::*html-publish-content-type*
 :function
 (lambda (req ent)
   (let* ((body (get-request-body req))
          (url-assoc-list (form-urlencoded-to-query body))
          (package-name (url-parameter-value :pkg url-assoc-list))
          (package-symbol (keywordize package-name))
          (currentdir (url-parameter-value :currentdir url-assoc-list))
          (nfiles (parse-integer (url-parameter-value :nfiles url-assoc-list)))
          (timestamp 
           (parse-integer (url-parameter-value :timestamp url-assoc-list)))
          (filedata
           (remove-if
            'null
            (loop for filenum from 0 below nfiles 
                  as name = (keywordize (formatn "File~D" filenum))
                  collect
                  (url-parameter-value name url-assoc-list)
                  ))))
     ;; This binds *username*, *req* and *ent*
     (execute-with-standard-weblistener-environment
      req ent package-symbol
      (lambda () 
        (wb-really-delete-files-in-directory
         currentdir timestamp filedata
         ))))))

                 
(defun wb-really-delete-files-in-directory (currentdir timestamp filedata)

  (let* ((title (formatn "File Deletion Summary: ~A" currentdir))
         (now (get-universal-time)))

    (with-standard-weblistener-page-header (title)

      (html :hr)

      ;; Just in case, impose a timelimit on the duration between 
      ;; generating a delete-file menu and clicking the 'DELETE THEM' button

      (if (> (- now timestamp) 300)

          (html 
           (:big 
            (:b 
             (:princ-safe "Delete operation timed out.")
             :br
             (:princ-safe 
              "(Regenerate the deletion checkbox page "
              "from a directory listing page and try again.)"
              ))))

        (html

         ;; Try to delete each file, keeping track of those that
         ;; cannot be deleted.

         (let ((oops-list nil) (deleted-list nil))
           (loop for file in filedata do
                 (handler-case 
                     (let ((deleted? 
                            (delete-file (merge-pathnames file currentdir))))
                       (unless deleted?
                         #+:lispworks 
                         (error "File does not exist.")
                         #-:lispworks
                         (error "DELETE-FILE returning NIL!!!"))
                       (push file deleted-list))
                   (error 
                    (c)
                    (push (list file c) oops-list)
                    )))

           ;; Report those files that were deleted successfully.

           (let* ((len (length deleted-list))
                  (verb (if (= 1 len) "was" "were"))
                  (plural (if (= 1 len) "" "s"))
                  (punc (if (zerop len) "." ":")))
             (html
              (:b 
               (:princ-safe 
                (formatn "~D file~A ~A deleted~A" len plural verb punc)))
              :p
              (with-two-html-table-columns (deleted-list name count) ()
                (:td (:small (:princ-safe name)))
                count
                )))

           ;; When any files failed to be deleted, report those files
           ;; along with the reason they could not be deleted.

           (when oops-list
             (let ((len (length oops-list)))
               (html
                :p
                (:b (:princ-safe 
                     (formatn "~D file~A could not be deleted:"
                              len (if (> len 1) "s" "")
                              )))
                :p
                (:table
                 (loop for (file condition) in oops-list do
                       (html
                        (:tr
                         (:td (:small (:princ-safe file)))
                         (:td "&nbsp;&nbsp;"
                          (:small (:princ-safe (formatn "~A" condition))))
                         )))))))

           ))))))
             
  