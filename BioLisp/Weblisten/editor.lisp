;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: WEBLISTENER; -*-

(in-package :weblistener)

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

(defparameter *file-size-too-big-to-edit* 1000000)


(defun safe-make-editanyfile-url (path create? supersede? text)
  (make-editanyfile-url
   :filepath 
   (url-safe-string (canonicalize-pathname-namestring (namestring path)))
   :create create?
   :supersede supersede?
   :initialcontents (and text (url-safe-string text))
   ))


(defun directory-is-modifyable-by-current-user? 
       (currentdir 
        &key
        (user-homedir (visitor-directory *username*))
        (shared-dirs (application-shared-files-directories cl-user:*ai*))
        (guru-ok? t)
        )
  #.(one-string-nl
     "A directory is considered modifyable by a given user if:"
     "  -- user is a guru, and GURU-OK? is non-NIL (the default), or"
     "  -- the directory is the user's weblistener home directory, or"
     "  -- the directory is one of the application's shared directories.")
  (let ((guru? (and guru-ok? (weblistener-guru-p *username*))))
    (or guru?
        (some
         (lambda (d) (pathname-is-in-subdirectory-of? currentdir d))
         (cons user-homedir shared-dirs)
         ))))

(defun editor-oops (format-string &rest format-args)
  (html 
   (:html
    :p
    (:h3 (:center (:b "Editor operation error.")))
    :p
    (:big 
     (:princ-safe (apply 'format nil format-string format-args))
     ))))

(defmacro with-two-html-table-columns 
          ((list item-symbol count-symbol) 
           (&rest table-options) &body td-body)
  (let ((listsym (gensym "LIST-")))
    `(flet ((generate-a-td-from-item (,item-symbol ,count-symbol)
              (html ,@td-body :newline)))
       (let* ((,listsym ,list)
              (len (length ,listsym))
              (halflen (ceiling len 2))
              (unique (cons nil nil))
              )
         (unless (zerop len)
           (html
            ((:table ,@table-options)
             :newline
             (let ((col1-list (subseq ,listsym 0 halflen))
                   (col2-list 
                    (cond
                     ((= len 1) (list unique))
                     ((evenp len) (subseq ,listsym halflen))
                     ((oddp len) 
                      (append (subseq ,listsym halflen) (list unique))
                      ))))
               (loop for item1 in col1-list
                     for item2 in col2-list
                     for count1 from 0 by 2
                     for count2 from 1 by 2
                     do
                     (html
                      (:tr
                       :newline
                       (generate-a-td-from-item item1 count1)
                       (when (not (eq item2 unique))
                         (generate-a-td-from-item item2 count2)
                         ))))))))))))


;;; Create an editor window with the definition of a function in it.

(publish
 :path *editfunction-url*
 :content-type cl-user::*html-publish-content-type*
 :function
 (lambda (req ent)
   (let* ((input (request-query req))
          (package-name (url-parameter-value :pkg input))
          (package-symbol (keywordize package-name))
          (function-name (url-parameter-value :function input))
          (function-package (url-parameter-value :package input))
          (temp-filename (url-parameter-value :tempfile input))
          )
     (execute-with-standard-weblistener-environment
      req ent package-symbol
      (lambda ()
        (edit-function-function function-name function-package temp-filename)
        )))))

(defun edit-function-function (function-name function-package temp-filename)
  (block exit
    ;; The temp file is created in the user's toplevel directory
    (let* ((user-homedir (visitor-directory *username*))
           (filename 
            (or temp-filename 
                (string-downcase (one-string function-name ".src"))))
           (fullpath (merge-pathnames user-homedir filename))
           (package (find-package (keywordize function-package)))
           (symbol (find-symbol function-name package))
           )
      (unless symbol 
        (return-from exit
          (editor-oops "No symbol '~A' in package '~A' seems to exist."
                       function-name function-package)))
      (unless (fboundp symbol)
        (return-from exit
          (editor-oops 
           "The symbol '~A' in package '~A' has no function definition."
           function-name function-package
           )))
      (edit-any-existing-text-as-file
       fullpath
       (one-string
        (formatn 
         "(in-package :~A)~%~%" (string-downcase (package-name package)))
        (or
         ;; We've stored the procedure definition away using 
         ;; our own DEFUN macro.
         (vwhen (form (get symbol :procedure-definition))
           (forward-funcall 'pretty-print-to-string form :downcase))
         ;; There's a source file.  Use definition from source file.
         (and (system-specific-source symbol :function)
              (function-definition-source-text symbol :function))
         "<<< No definition available >>>"
         ))))))

;;; Create an editor window whose contents, when saved, will be
;;; written to FILEPATH.

;;; If FILEPATH exists, the contents of the editor window are:
;;;   If SUPERSEDE is NIL, the contents of the file,
;;;   IF SUPERSEDE is non-NIL, INITIALCONTENTS

;;; If FILEPATH does not exist, 
;;;   If CREATE is NIL, an error message is generated.
;;;   If CREATE is non-NIL
;;;      an empty file named by FILEPATH is created, and
;;;      the contents of the editor window are INITIALCONTENTS.

(publish
 :path *editanyfile-url*
 :content-type cl-user::*html-publish-content-type*
 :function
 (lambda (req ent)
   (let* ((input (request-query req))
          (package-name (url-parameter-value :pkg input))
          (package-symbol (keywordize package-name))
          (filepath (url-parameter-value :filepath input))
          (create? (url-parameter-value :create input))
          (supersede? (url-parameter-value :supersede input))
          (initialcontents (url-parameter-value :initialcontents input))
          )
     (execute-with-standard-weblistener-environment
      req ent package-symbol
      (lambda () 
        (edit-any-file filepath create? supersede? initialcontents)
        )))))


(defun edit-any-file (filepath create? supersede? initialcontents)

  (block exit

    (let* ((fullpath (pathname filepath))
           (fullname (namestring filepath))
           (file-exists? (probe-file filepath))
           )

      (flet ((oops (format-string &rest format-args)
               (apply 'editor-oops format-string format-args)
               (return-from exit nil)
               ))
                       
        ;; The file must reside under the user's home directory,
        ;; or in the shared directory unless the user is a guru.

        (unless (directory-is-modifyable-by-current-user?
                 (dirpath-of-filepath fullpath))
          (if file-exists?
              (oops "File '~A' cannot be modified." fullname)
            (oops "File '~A' cannot be created in directory '~A'."
                  (file-namestring fullpath)
                  (namestring (dirpath-of-filepath fullpath))
                  )))

        ;; Verify that the file is editable.

        (cond
         ;; If file doesn't exist and we are told to create it, try to do so.
         ((and (not file-exists?) create?)
          (handler-case
              (with-open-file (p fullpath :direction :output) 
                #+:lispworks
                (declare (ignore p))
                nil)
            (error 
             (c) 
             (oops "Could not create ~A.~%  Signaled error: ~A" fullname c)
             )))
         ;; If file does not exist and we aren't creating it, give up
         ((not file-exists?)
          (oops "File ~A does not exist or cannot be accessed." fullname)
          ))

        ;; Okay, file exists.  Make sure it isn't too big to deal with.

        (when (null supersede?)
          (handler-case
              (with-open-file (p fullpath :direction :input)
                (let ((filesize (file-length p)))
                  ;; If file exists but is too big, give up
                  (when (<= *file-size-too-big-to-edit* filesize)
                    (oops 
                     "File ~A is too big (it has ~D characters, limit is ~D)"
                     fullname filesize *file-size-too-big-to-edit*)
                    (return-from exit nil)
                    )))
            (error
             (c)
             (oops 
              "File ~A is accessible but cannot be opened. Signaled error: ~A"
              fullname c
              ))))

        ;; Make sure we can write to the file

        (handler-case
            (with-open-file 
                (p fullpath :direction :output :if-exists :append)
              #+:lispworks
              (declare (ignore p))
              nil)
          (error
           (c)
           (oops 
            "File ~A cannot be written to. Signaled error: ~A"
            fullname c
            )))

        ;; Okay, the file is as editable as we can check for.  Go for it.

        (if (or (not file-exists?) (and file-exists? supersede?))
            (edit-any-existing-text-as-file 
             fullpath (or initialcontents ""))
          (edit-any-existing-file fullpath)
          )

        ))))


(defun add-file-to-users-edit-list (fullpath)
  (let ((filename (namestring fullpath)))
    (setf (get (wb::user-session-id) :edit-list)
          (delete filename (get (wb::user-session-id) :edit-list) 
                  :test 'string=)) 
    (push filename (get (wb::user-session-id) :edit-list))))


(defun edit-any-existing-file (fullpath)
  (add-file-to-users-edit-list fullpath)
  (let ((filechars (file-to-string fullpath)))
    (edit-any-existing-text-as-file fullpath filechars)
    ))

(defun edit-any-existing-text-as-file (fullpath text)
  (let ((dirpath 
         (canonicalize-pathname-namestring
          (namestring (dirpath-of-filepath fullpath))))
        (filename (file-namestring fullpath)))
    (html
     (:html
      (:title (:princ-safe (namestring fullpath)))
      (:head
       ;;(:princ *mini-editor-javascript*)
       (:princ +javascript-minieditor-include-string+)
       (:princ (user-specific-javascript-arglist-include-string wb:*username*))
       (:body
        (:h2 
         ((:a :href (make-wb-directory-listing-url :pathname dirpath))
          (:princ-safe dirpath))
         "&nbsp;&nbsp;"
         (:princ-safe filename)
         "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"
         ((:a :href (wb::return-hack-url)) 
          ((:font :color :green :size 3) "[Return to Listener]")))
        ;; All the links will go on top
        (html-for-weblistener-links cl-user:*ai*)
        :newline
        ;; And the edit box and its controlling buttons/hyperlinks below.
        (new-editor-input-forms text fullpath)
        :newline
        ))))))

(defun new-editor-input-forms (filechars fullpath)
  (let* ((userpackage (string (wb::user-session-package-name))))
    (html
     ((:form :name "editbox" :method "POST"
       :action *weblistener-new-editor-form-response-url*)
      :newline
      :br
      ;; The commands the user has available.
      ((:input :type "SUBMIT" :name "editorsave" :value "Save"))
      :newline
      ((:input :type "SUBMIT" 
        :name "editorsavecompileload" :value "Save-Compile-Load"))
      ((:input :type "SUBMIT" :name "editorreformat" :value "Reindent"))
      :newline
      ((:input :type "SUBMIT" :name "newfileordir" :value "New"))
      :newline
      "&nbsp;&nbsp;" "Info: " 
      :newline
      ((:input :type "text" :name "closer" :size 44 :value ""))
      :newline
      :br :br
      ((:textarea :name "editorbuffer" :rows 50 :cols 80
        :onkeyup "minieditorkeyup(event, this, window.document.editbox)"
        :onmouseup "minieditorkeyup(event, this, window.document.editbox)")
       (:princ-safe filechars))
      :newline
      ((:input :type "HIDDEN" :name "PKG" :value (string (user-session-id))))
      :newline
      ((:input :type "HIDDEN" :name "USERPACKAGE" :value userpackage)) 
      :newline
      ((:input :type "HIDDEN" :name "FILENAME" :value (namestring fullpath)))
      ))))


;;; This gets executed when a user clicks on any of the buttons
;;; on a page that is being edited -- e.g., the SAVE or REFORMAT buttons.

(publish
 :path *weblistener-new-editor-form-response-url*
 :content-type cl-user::*html-publish-content-type*
 :timeout 3600
 :function
 (lambda (req ent)
   (let* ((body (get-request-body req))
          (url-assoc-list (form-urlencoded-to-query body))
          (text-input (url-parameter-value :editorbuffer url-assoc-list nil))
          (package-name (url-parameter-value :pkg url-assoc-list))
          (package-symbol (keywordize package-name))
          (filename (url-parameter-value :filename url-assoc-list))
          (save? (url-parameter-value :editorsave url-assoc-list))
          (save-compile-and-load? 
           (url-parameter-value :editorsavecompileload url-assoc-list))
          (reformat? (url-parameter-value :editorreformat url-assoc-list))
          (new? (url-parameter-value :newfileordir url-assoc-list))
          (commands (list save? save-compile-and-load? reformat? new?))
          )
     (handler-case
         (progn
           (unless (= 1 (count-if 'identity commands))
             (error "More than one command is activated!!!"))
           (cond
            (save? 
             (execute-with-standard-weblistener-environment
              req ent package-symbol
              (lambda ()
                (new-editor-save-command filename text-input)
                (edit-any-existing-file filename)
                )))
            (save-compile-and-load?
             (new-editor-save-compile-load-command 
              req ent package-symbol filename text-input)
             )
            (reformat?
             (execute-with-standard-weblistener-environment
              req ent package-symbol
              (lambda ()
                (new-editor-reformat-command filename text-input)
                )))
            (new?
             (execute-with-standard-weblistener-environment
              req ent package-symbol
              (lambda () (editor-new-file-or-directory-form filename))
              ))))
       (error
        (c)
        (execute-with-standard-weblistener-environment
         req ent package-symbol (lambda () (error c))
         ))))))

        
(defun new-editor-save-command (fullpath text-to-save)
  (handler-case
      (with-open-file (p fullpath :direction :output :if-exists :supersede)
        (ecase (os?)
          (:unix (write-sequence text-to-save p))
          ;; hack to prevent ^M from showing up in Emacs.  Don't know why.
          (:windows
           (loop for line in (string-split text-to-save #\Newline) do
                 (write-line line p)
                 ))))
    (error
     (c)
     (editor-oops
      "Could not save revised text to ~A! Actual error ~A" 
      (namestring fullpath) c
      ))))

(defun new-editor-save-compile-load-command 
       (req ent package-symbol fullpath text-to-save)
  (declare (ignore filename))
  (new-editor-save-command fullpath text-to-save)
  (let* ((pathname (pathname fullpath))
         (filetype (pathname-type pathname))
         (compile-load-command
          (one-string 
           (cond
            ((or (string-equal "BBL" filetype) (string-equal "BIKE" filetype))
             "(bbi::bbcl ")            
            (t "(load (lisp:compile-file "))
           (formatn "~S" (namestring fullpath))
           "))"
           )))
    ;; Put the user back in the Weblistener after the COMPILE/LOAD
    ;; happens.
    (weblistener-standard-form-response
     req ent nil (string package-symbol) compile-load-command :wb-oneline
     )))

(defun reformat-file (file)
  (let* ((constituents (parse-file-into-comments-whitespace-and-code file))
         (new-file-string 
          (reformat-comments-whitespace-and-code constituents)))
    (with-open-file (p file :direction :output :if-exists :supersede)
      (loop for ch across new-file-string do
            (write-char ch p)
            ))))

(defun parse-file-into-comments-whitespace-and-code (file)
  (let ((chars (file-to-string file)))
    (parse-string-into-comments-whitespace-and-code chars)
    ))
    
;; IN-PACKAGE can take keyword arguments, so length >= 2
(defun in-package-form? (form)
  (and (listp form) (>= (length form) 2) (eq 'in-package (first form))))



;;; Returns a list of sublists.
;;; The first element of a sublist is one of:
;;;   -- :WHITESPACE
;;;   -- :SEMI-COMMENT
;;;   -- :VB-COMMENT
;;;   -- :LISP-FORM
;;;   -- :PACKAGE-ERROR
;;;   -- :ERROR
;;;   -- :UNTERMINATED-VB-COMMENT
;;;   -- :UNTERMINATED-LISP-FORM

;;; Only the last sublist can be of type :UNTERMINATED-VB-COMMENT, 
;;; :UNTERMINATED-LISP-FORM, or :ERROR.

;;; The 2nd element of the list is a substring of the input string
;;; composed of the characters of the input string representing
;;; the component.

;;; If the sublist is of type :ERROR, there is a third element to
;;; the sublist, being the error condition signaled.

;;; :PACKAGE-ERROR occurs when a :LISP-FORM is the form (in-package <xxx>)
;;; and XXX is not a designator for an existing package.

(defun parse-string-into-comments-whitespace-and-code (s)
  (let* ((len (length s)) 
         (lastpos (1- len))
         (pos 0)
         (unique (cons nil nil))
         (stuff nil)
         (inner-start 0)
         (*package* *package*))
    (block outer-loop
      (loop
       (block inner-loop
         (setq inner-start pos)
         (loop for j fixnum from pos below len
               as chj = (aref s j)
               do
               (incf pos)
               (cond
                ;; Ignore whitespace.
                ((whitespacep chj) nil)
                ;; Found a semicolon.  Grab everything between it and
                ;; the next Newline (or EOF) inclusive.
                ((eql chj #\;)
                 (unless (= j inner-start)
                   (push (list :whitespace (subseq s inner-start j)) stuff))
                 (loop for k fixnum from pos below len
                       as chk = (aref s k) 
                       do
                       (when (eql chk #\Newline) 
                         (push (list :semi-comment (subseq s j (1+ k))) stuff)
                         (setq pos (1+ k))
                         (return-from inner-loop)
                         ))
                 ;; Must have gotten to end of string w/o finding a Newline
                 (push (list :semi-comment (subseq s j)) stuff)
                 (return-from outer-loop)
                 )
                ;; Found "#|".  Grab everything between it and the next
                ;; "|#" inclusive.  If we hit EOF that's an error in the
                ;; structure of the file.
                ((and (eql chj #\#)
                      (not (= j lastpos))
                      (eql (aref s (1+ j)) #\|)
                      )
                 (unless (= j inner-start)
                   (push (list :whitespace (subseq s inner-start j)) stuff))
                 (loop for k fixnum from pos below len
                       as chk = (aref s k)
                       do
                       (if (and (eql chk #\|)
                                (not (= k lastpos))
                                (eql (aref s (1+ k)) #\#))
                           (progn
                             (push 
                              (list :vb-comment (subseq s j (+ k 2)))
                              stuff)
                             (setq pos (+ k 2))
                             (return-from inner-loop)
                             )))
                 ;; Must have gotten to end of string w/o finding matching
                 ;; close comment construct.
                 (push (list :unterminated-vb-comment (subseq s j)) stuff)
                 (return-from outer-loop)
                 )
                ;; Something that better start a Lisp form.
                (t
                 (unless (= j inner-start)
                   (push (list :whitespace (subseq s inner-start j)) stuff))
                 (handler-case
                     (multiple-value-bind (value position)
                         (read-from-string 
                          s nil unique :start j :preserve-whitespace t)
                       (cond
                        ;; EOF
                        ((eq value unique) (return-from outer-loop))
                        ;; end of form finishes string.
                        ((>= position len)
                         (push (list :lisp-form (subseq s j) value) stuff))
                        (t 
                         (push 
                          (list :lisp-form (subseq s j position) value)
                          stuff)
                         (setq pos position)
                         ))
                       ;; Deal with IN-PACKAGE form to set package
                       ;; properly so reader can read package prefixes
                       ;; and intern symbols in proper packages!  Ugh.
                       (when (in-package-form? value)
                         (let* ((package-designator (second value))
                                (package (find-package package-designator)))
                           (if package
                               (setq *package* package)
                             (push 
                              (list 
                               :package-error "" package-designator package)
                              stuff
                              ))))
                       (if (>= position len)
                           (return-from outer-loop)
                         (return-from inner-loop)
                         ))
                   (end-of-file 
                    ()
                    (push (list :unterminated-lisp-form (subseq s j)) stuff)
                    (return-from outer-loop)
                    )
                   (error 
                    (c)
                    (push (list :error (subseq s j) c) stuff)
                    (return-from outer-loop)
                    )))))
         ;; If we get to here the inner loop must have read whitespace
         ;; through to the end of the string.
         (unless (>= inner-start len)
           (push (list :whitespace (subseq s inner-start)) stuff))
         (return-from outer-loop)
         )))
    (nreverse stuff)
    ))

(defun reformat-comments-whitespace-and-code (constituent-list)
  (let ((*package* *package*))
    (string-join
     (mapcar
      (lambda (constituent)
        (ecase (first constituent)
          ((:whitespace :semi-comment :vb-comment) (second constituent))
          ((:unterminated-vb-comment)
           (one-string-nl
            (second constituent)
            ";; WARNING -- unterminated '#|' comment No '|#' found. !!!!"
            ";; WARNING -- unterminated '#|' comment No '|#' found. !!!!"
            ))
          (:package-error
           (one-string-nl
            " "
            " "
            ";; WARNING.  SERIOUS PROBLEM ABOVE!!"
            (formatn 
             ";; The package ~S does not seem to exist!" 
             (third constituent))
            ";; The reformatter has read and rewritten the forms after the"
            (formatn 
             ";; above form in different package, ~A."
             (fourth constituent))
            ";; This is almost certainly not what you want!"
            ";; Use the BACK button on your browser to restore your file,"
            ";; and use the Weblistener to create your package before you"
            ";; try to edit a file using it again."
            ";; WARNING.  SERIOUS PROBLEM ABOVE!!"
            " "
            ))
          (:error
           (one-string-nl
            " "
            ";; *****>> DANGER, DANGER WILL ROBINSON, DANGER!!!  <<*****"
            ""
            ";; READER ERROR TRYING TO READ NEXT FORM!"
            (delete 
             #\Newline
             (formatn ";; ACTUAL ERROR: ~A" (third constituent)))
            ""
            ";; *****>> DANGER, DANGER WILL ROBINSON, DANGER!!!  <<*****"
            " "
            (second constituent)
            ))
          ((:lisp-form)
           (let ((form (third constituent)))
             (prog1
                 ;; Get rid of leading newline
                 (subseq 
                  (forward-funcall 'pretty-print-to-string form :downcase) 1)
               (when (in-package-form? form)
                 (setq *package* (find-package (second form)))
                 ))))))
      constituent-list)
     ""
     )))


(defun new-editor-reformat-command 
       (fullpath buffer-text &aux reformatted-text)
  (setq reformatted-text 
        (pretty-indent-lisp-form-string buffer-text))
  (edit-any-existing-text-as-file fullpath reformatted-text)
  )

(publish
 :path *editor-new-file-or-directory-form-url*
 :content-type cl-user::*html-publish-content-type*
 :function
 (lambda (req ent)
   (let* ((input (request-query req))
          (package-name (url-parameter-value :pkg input))
          (package-symbol (keywordize package-name))
          (currentdir (url-parameter-value :currentdir input))
          )
     (execute-with-standard-weblistener-environment
      req ent package-symbol
      (lambda () (editor-new-file-or-directory-form currentdir))
      ))))


(defun editor-new-file-or-directory-form (fullpath)
  (let* ((userpackage (string (wb::user-session-package-name)))
         (currentdir
          (canonicalize-pathname-namestring
           (namestring (dirpath-of-filepath fullpath))
           )))
    (html
     (:h2 (:center (:b "Create New File or Directory")))
     (:h3 "Current Directory: " (:princ-safe currentdir))
     ((:form :name "newfileordir" :method "POST"
       :action *editor-new-file-or-directory-form-response-url*)
      :newline
      :br
      ((:input :type "radio" :name "fileordir" :value "file" :checked "yes")
       "New File")
      :newline
      ((:input :type "radio" :name "fileordir" :value "dir") 
       "New Subdirectory")
      :newline
      "&nbsp;&nbsp&nbsp;Name: "
      ((:input :type "text" :name "fileordirname" :value "" :size 40))
      :newline
      ((:input :type "HIDDEN" :name "PKG" :value (string (user-session-id))))
      :newline
      ((:input :type "HIDDEN" :name "USERPACKAGE" :value userpackage))
      :newline
      ((:input :type "HIDDEN" :name "CURRENTDIR" :value currentdir))
      ))))


(publish
 :path *editor-new-file-or-directory-form-response-url*
 :content-type cl-user::*html-publish-content-type*
 :timeout 3600
 :function
 (lambda (req ent)
   (let* ((body (get-request-body req))
          (url-assoc-list (form-urlencoded-to-query body))
          (package-name (url-parameter-value :pkg url-assoc-list))
          (package-symbol (keywordize package-name))
          (currentdir (url-parameter-value :currentdir url-assoc-list))
          (newname (url-parameter-value :fileordirname url-assoc-list))
          (newwhat (url-parameter-value :fileordir url-assoc-list))
          (newfile? (string-equal newwhat "file"))
          (newdir? (string-equal newwhat "dir"))
          )
     (execute-with-standard-weblistener-environment
      req ent package-symbol
      (lambda ()
        (editor-create-new-file-or-directory-command
         currentdir newname newfile? newdir?
         ))))))


(defun editor-create-new-file-or-directory-command
       (fullpath newname newfile? newdir?)
  (cond 
   (newfile? (editor-create-new-file fullpath newname))
   (newdir? (editor-create-new-subdirectory fullpath newname))
   (t
    (editor-oops 
     "Internal error.  Neither 'New File' nor 'New Subdirectory' specified!"
     ))))

(defun editor-create-new-file (fullpath newname)

  (block exit

    (let* ((new-file-path
            (handler-case
                (merge-pathnames newname fullpath)
              (error
               (c)
               (editor-oops
                (formatn
                 (one-string
                  "Problem attempting to use '~A' as the name of a new file. "
                  "Could not merge it successfully with '~A'. "
                  "Actual system error message: ~A")
                 newname fullpath c
                 ))
               (return-from exit)
               )))
           (new-name (namestring new-file-path))
           )

      ;; Verify the created path has a name and/or type component...

      (unless (or (pathname-name new-file-path) (pathname-type new-file-path))
        (editor-oops
         (formatn
          (one-string
           "The new file name you provided, '~A', does not name an actual "
           "file when merged with the current directory, '~A'. (Why this "
           "is so is not obvious. Perhaps you ended the name with a '/' ?)")
          newname fullpath
          ))
        (return-from exit)
        )

      ;; Verify that user didn't provide a full pathname to some file
      ;; somewhere else.

      (unless (pathname-is-in-subdirectory-of? new-file-path fullpath)
        (editor-oops
         (formatn
          (one-string
           "You are trying to create file '~A', but that is not in "
           "the current directory, '~A' (or in one of its subdirectories). "
           "At present, you are only allowed to create a new file in a "
           "subdirectory of the current directory.")
          new-name fullpath
          ))
        (return-from exit)
        )

      ;; Verify that file doesn't already exist.

      (when (probe-file new-file-path)
        (editor-oops (formatn "File '~A' already exists." new-name))
        (return-from exit)
        )

      ;; Try to create the file

      (handler-case
          (with-open-file (p new-file-path :direction :output) 
            #+:lispworks
            (declare (ignore p))
            nil)
        (error
         (c)
         (editor-oops
          (formatn
           (one-string
            "Could not create new file '~A'. Perhaps permissions are invalid? "
            "Actual system error: ~A")
           new-name c
           ))
         (return-from exit)
         ))

      ;; Okay, we created it.  Now edit it.

      (edit-any-existing-file new-file-path)
    
      )))


(defun editor-create-new-subdirectory (fullpath newname)

  (block exit

    (let* ((new-dir-path
            (handler-case
                (append-subdir fullpath newname)
              (error
               (c)
               (editor-oops
                (formatn
                 (one-string
                  "Problem using '~A' as the name of a new subdirectory. "
                  "Could not combine it successfully with '~A'. "
                  "Actual system error message: ~A")
                 newname fullpath c
                 ))
               (return-from exit)
               )))
           (new-name (namestring new-dir-path))
           )

      ;; Verify the created path has no name and/or type component...

      (when (or (pathname-name new-dir-path) (pathname-type new-dir-path))
        (editor-oops
         (formatn
          (one-string
           "The new subdiretory name you provided, '~A', seems to designate "
           "a file, not a subdirectory when combined with the current "
           "directory, '~A'.")
          newname fullpath
          ))
        (return-from exit)
        )

      ;; Verify that user didn't provide a full pathname to some directory
      ;; somewhere else.

      (unless (pathname-is-in-subdirectory-of? new-dir-path fullpath)
        (editor-oops
         (formatn
          (one-string
           "You are trying to create directory '~A', but that is not within "
           "the current directory, '~A'. "
           "At present, you are only allowed to create a subdirectory within "
           "the current directory.")
          new-name fullpath
          ))
        (return-from exit)
        )

      ;; Verify that file doesn't already exist.

      (when (probe-file new-dir-path)
        (editor-oops (formatn "Subdirectory '~A' already exists." new-name))
        (return-from exit)
        )

      ;; Try to create the file

      (handler-case
          (ensure-directories-exist new-dir-path :verbose nil)
        (error
         (c)
         (editor-oops
          (formatn
           (one-string
            "Could not create new subdirectory '~A'. "
            "Perhaps permissions are invalid? "
            "Actual system error: ~A")
           new-name c
           ))
         (return-from exit)
         ))

      ;; Okay, we created it.  Display the containing directory.

      (html
       (:h2 (:princ-safe (formatn "New directory '~A' created" new-dir-path)))
       :br
       ((:a :href (make-wb-directory-listing-url :pathname fullpath))
        "List containing directory.")
       :p
       ((:a :href (make-weblistener-evalstring-url :evalstring "T"))
        "Back to Weblistener")
       )

      )))

(publish
 :path *edit-previous-url*
 :content-type cl-user::*html-publish-content-type*
 :function
 (lambda (req ent)
   (let* ((input (request-query req))
          (package-name (url-parameter-value :pkg input))
          (package-symbol (keywordize package-name))
          )
     (execute-with-standard-weblistener-environment
      req ent package-symbol
      (lambda () (editor-edit-previous-url))
      ))))


(defun editor-edit-previous-url ()
  (let ((edit-files (get (wb::user-session-id) :edit-list)))
    (if edit-files 
        (edit-any-file (first edit-files) t nil nil)
      (with-standard-weblistener-page-header ("")
        (html (:big (:princ "No previously edited file")))
        ))))


(publish
 :path *edit-all-previous-url*
 :content-type cl-user::*html-publish-content-type*
 :function
 (lambda (req ent)
   (let* ((input (request-query req))
          (package-name (url-parameter-value :pkg input))
          (package-symbol (keywordize package-name))
          )
     (execute-with-standard-weblistener-environment
      req ent package-symbol
      (lambda () (editor-edit-all-previous-url))
      ))))

(defun editor-edit-all-previous-url () 
  (with-standard-weblistener-page-header ("Previously edited files")
    (let ((edit-files (get (wb::user-session-id) :edit-list)))
      (if (null edit-files) 
          (html (:big (:princ "No previously edited files")))
        (progn 
          (html :hr :p)
          (dolist (file edit-files) 
            (html 
             ((:a :href (make-editanyfile-url 
                         :filepath (canonicalize-pathname-namestring file)
                         :create nil :supersede nil))
              (:princ-safe file))
             :br
             )))))))
            
  
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Code to do indentation in the multiline box and in the editor

;;; PRETTY-INDENT-LISP-FORM-STRING (string) returns the indented string.


(defstruct pstack lppos lpindentpos linenum line)

(defvar *state-stack* nil)
(defvar *indented-lines* nil)

(defparameter *special-indent-symbols* 
  '((if (4 4)) 
    (loop (6 2))
    (cond (6 2))
    ))

(defun indent-info-for-symbol (symbol)
  (second (find symbol *special-indent-symbols* :key 'first)))

(defun pretty-indent-lisp-form-string (s)
  #.(one-string-nl 
     "S is assumed to be a string with one or more newlines in it."
     "The lines should represent a Lisp form.  The function attempts"
     "to reindent each line (that is, add or remove spaces at the"
     "beginning of the line) in a quasi-standard lisp indenting style.")
  (handler-case
      (flet ((slt (x) (string-trim *whitespace* x)))
        (let* ((lines 
                (remove-if 
                 'pretty-indent-inserted-message?
                 (mapcar #'slt (string-split (slt s) #\Newline))))
               (nlines (length lines)))
          (block exit
            (when (= nlines 1) (return-from exit (first lines)))
            (when (= nlines 0) (return-from exit ""))
            (let ((*state-stack*
                   (list 
                    (make-pstack :lppos 0 :lpindentpos 0 :linenum 0 :line "")))
                  (*indented-lines* nil))
              (loop for line in lines do
                    (adjust-state-for-next-line line))
              (setq *indented-lines* 
                    (modify-indentation-for-special-symbols
                     (reverse 
                      (if (cdr *state-stack*) 
                          (cons 
                           (create-pretty-indent-message
                            "Too few right parens") 
                           *indented-lines*)
                        *indented-lines*))))
              (string-join *indented-lines* #\Newline)
              ))))
    (error 
     (c)
     (one-string-nl
      (create-pretty-indent-message "Error attempting to indent form.")
      (create-pretty-indent-message (formatn "Actual error: ~A" c))
      s
      ))))

           

;;; Analyze the next line wrt the previously done analysis (aka the stack)

(defun adjust-state-for-next-line (line)
  (let* ((prev-state (first *state-stack*))
         (indentpos (pstack-lpindentpos prev-state))
         (linenum (pstack-linenum prev-state))
         (indented-line 
          (one-string 
           (make-string indentpos :initial-element #\Space) 
           line)))
    (push indented-line *indented-lines*)
    (multiple-value-bind (lparen-list excess-rparens)
        (excess-parentheses-analysis indented-line)
      ;; The two loops are mutually exclusive
      (loop for pos in lparen-list
            as ipos = (indent-position-for-lparen pos indented-line)
            do
            (push
             (make-pstack
              :lppos pos :lpindentpos ipos 
              :linenum (1+ linenum) :line indented-line)
             *state-stack*
             ))
      (loop with message-added? = nil
            for j from 0 below excess-rparens 
            do
            (if (null (cdr *state-stack*))
                (unless message-added?
                  (setq message-added? t)
                  (push 
                   (create-pretty-indent-message
                    "Too many right parens on previous line!")
                   *indented-lines*
                   ))
              (pop *state-stack*)
              )))))


(defun indent-position-for-lparen (ppos line)
  (let ((indent-type (indentation-type-for-ppos ppos line)))
    (case indent-type 
      (:paren (1+ ppos))
      (:function (+ ppos 2))
      (:blank (+ ppos 2))
      (:other (+ ppos 2))
      (otherwise 
       (if (integerp indent-type) 
           (+ ppos indent-type)
         (error "Internal error. Should not get here.")))
      )))

(defun indentation-type-for-ppos (ppos line)
  (let ((after-lparen-string
         (if (= ppos (1- (length line))) 
             ""
           (string-right-trim *whitespace* (subseq line (1+ ppos))))))
    (cond 
     ((zerop (length after-lparen-string)) :blank)
     ((eql (char after-lparen-string 0) #\() :paren)
     (t 
      (handler-case 
          (multiple-value-bind (token pos)
              (read-from-string after-lparen-string)
            (cond
             ((symbolp token) 
              (let ((indent-data (indent-info-for-symbol token)))
                (if indent-data
                    (if (= pos (length after-lparen-string))
                        (second indent-data)
                      (first indent-data))
                  :function)))
             (t :other)
             ))
        (error () :other)
        )))))

(defun excess-parentheses-analysis (line)
  (let ((positions nil))
    (loop with excess-rparens = 0
          for ch across line
          for pos from 0
          do
          (cond
           ((eql ch #\() (push pos positions) (decf excess-rparens))
           ((eql ch #\)) (pop positions) (incf excess-rparens))
           (t nil))
          finally
          (return (values (nreverse positions) excess-rparens))
          )))

(defvar *pretty-indent-inserted-prefix* ";;!Reindent! ")


(defun pretty-indent-inserted-message? (line)
  (initial-subsequence-of? line *pretty-indent-inserted-prefix*))

(defun create-pretty-indent-message (message)
  (one-string *pretty-indent-inserted-prefix* message))

(defun line-begins-with-special-indent-symbol? (pattern line)
  (block exit
    (vwhen (pos (search pattern line :test 'char-equal))
      (loop for j from 0 below pos do
            (unless (whitespacep (char line j))
              (return-from exit nil)))
      (+ pos (length pattern) -1)
      )))
            
(defun number-of-forms-beyond-position (line pos)
  (block exit 
    (let ((subline (subseq line (1+ pos)))
          (forms nil)
          (unique (cons 1 1)))
      (with-input-from-string 
          (s subline)
        (handler-case
            (loop for form = (read s nil unique)
                  until (eq form unique)
                  do
                  (push form forms))
          (error () (return-from exit -1))))
      (length forms)
      )))
         

(defun modify-indentation-for-special-symbols (lines)
  (let ((results nil)
        (unprocessed-lines lines))
    (loop until (null (cdr unprocessed-lines))
          as curline = (first unprocessed-lines)
          as nextline = (second unprocessed-lines)
          do 
          (vif (next-line-indent (special-indent? curline nextline))
               (progn 
                 (pop unprocessed-lines)
                 (pop unprocessed-lines)
                 (push curline results)
                 (push 
                  (one-string
                   (make-string next-line-indent :initial-element #\Space)
                   nextline)
                  results))
               (progn 
                 (pop unprocessed-lines)
                 (push curline results)
                 ))
          finally (push (first unprocessed-lines) results)) 
    (reverse results)))

(defparameter *special-after-indent-symbols* 
  '(
    ("(multiple-value-bind" 1 2)
    ("(handler-case" 0 2)
    ))

(defun special-indent? (line nextline)
  (loop for (pattern nforms nindent) in *special-after-indent-symbols* do
        (vwhen (pos (line-begins-with-special-indent-symbol? pattern line))
          (when (= nforms (number-of-forms-beyond-position line pos))
            (when (= 1 (number-of-forms-beyond-position nextline -1))
              (return nindent)
              )))
        finally (return nil)))

                       
       
#+test
(progn

(defparameter t1
  (one-string-nl
   "(defun foo (x) "
   "(1+ x)"
   ")"))


(defparameter t2 
  (one-string-nl 
   "(defun foo (x)"
   "(loop for j from 0 below x"
   "do"
   "(if (< j 5)"
   "(print 'foo)"
   "(print 'bar)"
   ")))"
   ))

(defparameter t3
  (one-string-nl
   "(let ((x 5) (y 10))"
   "(let ((z 3)"
   "(q 4))"
   "(loop for j below x"
   "for k below y collect"
   "(+ j k)"
   ")))"))

(defparameter t4
  (one-string-nl
   "(defun foo (x y) (+ x y)"
   "  (let ((x 5)"
   "    (+ x 3)"
   "))"))


(defparameter t5
  (one-string-nl
   "(defun foo (x) "
   "(1+ x)"
   "))"
   "(foo x))"
   ))


(defparameter t6
  (one-string-nl 
   "(defun foo (x)"
   "\"This is a documentation string\""
   "   (declare (fixnum x))"
   ""
   "      (loop for j from 0 below x"
   "do"
   ""
   "(if (< j 5)"
   "(print 'foo)"
   "              (print 'bar))"
   ""
   "))"
   ))

(defparameter t7
  (one-string-nl
   "(defun foo (x y"
   " z)"
   "(cond ((< x 5) 4)"
   "((< y 5) 3))"
   "(cond"
   "((< z 2) 12)"
   "  ((> z 2) 15)"
   "))"
   ))

(defparameter t8
  (one-string-nl 
   "(defun foo (x y)"
   "(multiple-value-bind (z q) "
   "   (bar 3) "
   "(print 2)"
   "(handler-case "
   " (xyzzy)"
   "(error () 'foo)"
   ")))"
   ))

)

