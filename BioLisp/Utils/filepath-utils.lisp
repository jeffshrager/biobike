;;; -*- Package: utils; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :utils)

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

;;; Author: JP Massar

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defparameter *utility-filepath-bootstrap-symbols*
    '(cl-user::translate-simple-lp 
      cl-user::add-simple-lp-host
      cl-user::delete-simple-lp-host
      ))

  (import *utility-filepath-bootstrap-symbols* :utils)
  
  (defparameter *utility-filepath-user-symbols* 
    '(
      c/l 
      cl
      make-new-temp-file-path
      with-temp-file-in
      with-temp-directory
      create-and-use-unique-file
      file-to-string
      file-to-string-list
      list-file-contents
      file-operate
      copy-text-file
      remove-directory
      ))

  (defparameter *utility-filepath-api-symbols*
    (append 
     *utility-filepath-bootstrap-symbols*
     *utility-filepath-user-symbols*
     '(
       ld
       cf
       file-size-in-lisp-chars
       file-to-limited-string
       strip-file-of-returns-preceding-newlines
       convert-linefeeds-to-crlfs
       dirpath-of-filepath
       pathname-in-directory-form
       directory-wildcard
       append-subdir
       pathname-names-directory?
       directory-with-subdirs-in-directory-form
       pathname-is-in-subdirectory-of?
       purge-files-before
       pathname-of-new-type
       pathname-of-new-name
       *delete-temp-files*
       )))

  (export *utility-filepath-api-symbols* (find-package :utils)))


(defun webpath->lisp-physical-pathname (path)
  "Converts any legitimate path to a physical pathname."
  (handler-case
      (cl-user:translate-simple-lp path)
    (error () (translate-logical-pathname path))))
           

(defun c/l (file &optional (recompile? nil recompile-provided?))
  #.(one-string-nl
     "Compile FILE, a file which should have a .lisp file type,"
     "if FILE is newer than its binary version, or the binary version"
     "does not exist.  Then load the binary (.fasl)."
     "FILE can be a simple logical pathname string, a lisp logical pathname"
     "string, a pathname, or a string denoting a file.")
  (setq file (webpath->lisp-physical-pathname file))
  (if recompile-provided?
      (cl-user::compile/load file recompile?)
    (cl-user::compile/load file)
    ))

(defun cl (&rest args) (apply 'c/l args))

(defun ld (file &rest load-args)
  #.(one-string-nl
     "Calls LISP:LOAD on FILE and LOAD-ARGS, first translating FILE."
     "FILE can be a simple logical pathname string, a lisp logical pathname"
     "string, a pathname, or a string denoting a file.")
  (setq file (webpath->lisp-physical-pathname file))
  (apply 'load file load-args))

(defun cf (file &rest compile-args)
  #.(one-string-nl
     "Calls COMPILE-FILE on FILE and COMPILE-ARGS, first translating FILE."
     "FILE can be a simple logical pathname string, a lisp logical pathname"
     "string, a pathname, or a string denoting a file.")
  (setq file (webpath->lisp-physical-pathname file))
  (apply 'compile-file file compile-args)
  )

(let ((unique-counter 10))

(defun make-new-temp-file-path 

       (directory 
        &rest args
        &key 
        (prefix "T")
        (name "temp")
        (type "temp")
        (if-exists :again)
        )

  #.(one-string-nl
     "Returns a full pathname to a file in DIRECTORY"
     "whose FILE-NAMESTRING is of the form <prefix><unique-id>-<name>.<type>."
     "If PREFIX is NIL then no <prefix>, <unique-id> or '-' is used."
     "If PREFIX is a symbol its symbol-name is used as <prefix>."
     "If PREFIX is a string it itself is used as <prefix>."
     "if NAME is NIL then no <name> or '-' is used."
     "If NAME is a symbol its symbol-name is used, else it must be a string."
     "If TYPE is NIL then no <type> or '.' is used."
     "If TYPE is a symbol its symbol-name is used, else it must be a string."
     "<unique-id> is unique with respect to the current process,"
     "therefore it is possible that a file of the name generated might exist,"
     "having been created by some other program or a differenct instance of"
     "whatever program is using this function."
     "In this case IF-EXISTS is examined -- it can be one of: "
     "  -- NIL (return the path anyway silently)"
     "  -- :WARN (return the path anyway after calling WARN to note duplicate)"
     "  --,:ERROR (signal an error, do not return)"
     "  -- :AGAIN (the default; call this function again with the same"
     "     arguments , thereby generating a new <unique-id>, which eventually"
     "     must result in a non-existing filename being generated."
     "The second value returned is whether the file designated by the"
     "returned pathname currently exists.")
  (flet ((gensymed-prefix (s) (formatn "~A~D" s (incf unique-counter))))
    (let* ((string-prefix 
            (cond
             ((null prefix) nil)
             ((symbolp prefix) (gensymed-prefix (string prefix)))
             ((stringp prefix) (gensymed-prefix prefix))
             (t (error "Invalid PREFIX: ~A" prefix))
             ))
           (string-name
            (cond
             ((null name) nil)
             ((symbolp name) (string name))
             ((stringp name) name)
             (t (error "Invalid NAME: ~A" name))
             ))
           (string-type
            (cond
             ((null type) nil)
             ((symbolp type) (string type))
             ((stringp type) type)
             (t (error "Invalid TYPE: ~A" name))
             ))
           (full-name
            (cond
             ((and string-prefix string-name)
              (one-string string-prefix "-" string-name))
             (string-prefix string-prefix)
             (string-name string-name)
             (t (error "Must specify either a non-nil PREFIX or NAME!"))
             ))
           (filepath
            (make-pathname
             :name full-name
             :type string-type
             :defaults (webpath->lisp-physical-pathname directory)
             )))
      (if (probe-file filepath)
          (progn
            (ecase if-exists
              ((nil) nil)
              (:warn (warn "temp file ~A already exists..." filepath))
              (:error (error "temp file ~A already exists..." filepath))
              (:again 
               (if (null string-prefix)
                   (error 
                    (formatn
                     (one-string
                      "File ~A already exists, and no prefix causing a "
                      "unique id to be generated was specified.")
                     filepath
                     ))
                 (setq filepath 
                       (apply 'make-new-temp-file-path directory args)))))
            (values filepath t))
        (values filepath nil)
        )))))

(defvar *delete-temp-files* t) ; Useful for debugging

(defmacro with-temp-file-in 
          ((var directory 
                &key 
                (prefix nil prefix-s?)
                (name nil name-s?)
                (type nil type-s?)
                (if-exists nil if-exists-s?)
		(delete? t)
                (on-delete-error :warn))
           &body body)
  #.(one-string-nl
     "Creates a pathname representing a temporary file in DIRECTORY "
     "using (MAKE-NEW-TEMP-FILE-PATH ...), binds that pathname to VAR,"
     "and insures that if the execution of BODY has in fact"
     "created a file named by said pathname that DELETE-FILE"
     "is called on the pathname.  (Which could fail, but if so"
     "that failure is reported as a warning by default or by calling ERROR"
     "if ON-DELETE-ERROR is :error.)")
  `(let ((,var (make-new-temp-file-path
                ,directory
                ,@(when prefix-s? `(:prefix ,prefix))
                ,@(when name-s? `(:name ,name))
                ,@(when type-s? `(:type ,type))
                ,@(when if-exists-s? `(:if-exists ,if-exists))
                )))
     (unwind-protect
         (progn ,@body)
       (when (and ,delete? *delete-temp-files*)
	 (handler-case 
	  (when (probe-file ,var) (delete-file ,var))
	  (error 
	   (c)
	   (let ((message
		  (formatn
		   (one-string-nl
		    "Problem deleting file created by WITH-TEMP-FILE-IN."
		    "Temporary file path: ~A"
		    "Actual error condition: ~A")
		   ,var c
		   )))
	     (case ,on-delete-error
	       (:warn (warn message))
	       (otherwise (error message))
	       ))))))))

(defmacro with-temp-directory
          ((var parent-directory 
                &key 
                (prefix nil prefix-s?)
                (name nil name-s?)
                (if-exists nil if-exists-s?)
		(delete? t)
                (on-delete-error :warn))
           &body body)
  #.(one-string-nl
     "Creates a directory in which to create temporary files."
     "You give the parent directory (where this one will be created)"
     "and the new directory is bound to VAR as a pathname, which can be"
     "passed on to, for example, WITH-TEMP-FILE-IN, to create temp files."
     "Most key args are as in WITH-TEMP-FILE-IN.")
  `(let ((,var (pathname-in-directory-form
                (make-new-temp-file-path
                 ,parent-directory
                 ,@(when prefix-s? `(:prefix ,prefix))
                 ,@(when name-s? `(:name ,name))
                 ,@(when if-exists-s? `(:if-exists ,if-exists))
                 :type nil
                 ))))
     (ensure-directories-exist ,var)
     (unwind-protect
         (progn ,@body)
       (when ,delete?
	 (handler-case 
             (ecase (os?) 
               (:unix 
                (forward-package-funcall
                 :excl :run-shell-command
                 (format nil "rm -Rf ~a" (namestring ,var))))
               (:windows 
                (remove-directory ,var :if-not-empty? :delete-files)
                ))
	  (error 
	   (c)
	   (let ((message
		  (formatn
		   (one-string-nl
		    "Error deleting directory created by WITH-TEMP-DIRECTORY."
		    "Temporary dir path: ~A"
		    "Actual error condition: ~A")
		   (namestring ,var) c
		   )))
	     (case ,on-delete-error
	       (:warn (warn message))
	       (otherwise (error message))
	       ))))))))

(defun create-and-use-unique-file
       (directory write-function operator-function 
                  &key 
                  (name "temp")
                  (type "tmp")
                  (create-directory? t)
                  )
  #.(one-string-nl
     "Creates a unique file in directory DIRECTORY whose file type is"
     "TYPE and whose name contains NAME (and other uniquifying characters)."
     "Once the file has been created, it is opened for writing and"
     "WRITE-FUNCTION is called, being passed two arguments:"
     "  the file and the output stream."
     "Then the stream is closed and OPERATOR-FUNCTION is called"
     "on the file (passed as a pathname)."
     "If CREATE-DIRECTORY? is non-nil then ENSURE-DIRECTORIES-EXIST"
     "is called on DIRECTORY before any other operations."
     ""
     "*** Note: The file created is NOT deleted. ***")
  (when create-directory?
    (ensure-directories-exist directory))     
  (with-temp-file-in 
      (file directory 
            :name name :type type :delete? nil :if-exists :again :prefix "")
    (with-open-file (stream file :if-exists :error :direction :output)
      (funcall write-function file stream))
    (funcall operator-function file)
    ))
      
  

(defun file-size-in-lisp-chars (file)
  #.(one-string-nl
     "The number of lisp characters needed to store the characters in FILE."
     "(Might not the same as the file size of FILE because CR/NL gets"
     "treated as one character by Lisp.  Grossly inefficient...")
  (with-open-file (p file :direction :input)
    (let ((size 0))
      (declare (fixnum size))
      (do ((ch (read-char p nil nil) (read-char p nil nil)))
          ((null ch))
        (setf size (the fixnum (1+ size))))
      size
      )))


(defun file-to-string (file &key (max 1000000))
  #.(one-string-nl
     "Returns a string containing all the characters in FILE with line"
     "terminators converted to Newlines.  If the string would exceed MAX"
     "characters (default a million) a warning is issued and NIL is returned.")
  (block exit
    (let ((buffer-size (file-size-in-lisp-chars file)))
      (when (> buffer-size max)
        (warn 
         "File ~A is too big! Allowed: ~A, actual: ~A"
         file max buffer-size)
        (return-from exit nil))
      (let ((buffer (make-string buffer-size)))
        (with-open-file (p file :direction :input)
          (unless (= buffer-size (read-sequence buffer p))
            (ierror "READ-SEQUENCE failed to read entire file!")))
        buffer
        ))))

(defun file-to-limited-string (file &key (max 10000))
  #.(one-string-nl
     "Returns a string containing all the characters in FILE with line"
     "terminators converted to Newlines.  If the string would exceed MAX"
     "characters (default ten thousand) then only the first MAX characters"
     "are returned.  The 2nd value returned is T if all the file's characters"
     "were returned, and NIL if the file was too big.")
  (let* ((file-size (file-size-in-lisp-chars file))
         (buffer-size (if (> file-size max) max file-size)))
    (let ((buffer (make-string buffer-size)))
      (with-open-file (p file :direction :input)
        (unless (= buffer-size (read-sequence buffer p))
          (ierror "READ-SEQUENCE failed to read entire file!")))
      (values buffer (<= file-size max))
      )))


(defun file-to-string-list (file)
  #.(one-string-nl
     "Returns a list of strings, each string representing a line from FILE."
     "(Newline characters and EOF in the file determine where each line ends.")
  (with-open-file (i file :direction :input)
    (loop for line = (read-line i nil nil) until (null line) collect line)))


(defun file-has-return-newline-sequence (filepath)
  (block exit
    (with-open-file (p filepath :direction :input :if-does-not-exist :error)
      (do ((ch (read-char p nil nil) (read-char p nil nil)))
          ((null ch))
        (cond
         ((eql ch #\Newline) (return-from exit nil))
         ((eql ch #\Return)
          (return-from exit (eq (read-char p nil nil) #\Newline)))))
      nil
      )))
      
(defun strip-file-of-returns-preceding-newlines (filepath &key (verbose t))
  #.(one-string-nl
     "Destructively modify the contents of FILEPATH, getting rid of any"
     "#\Return characters immediately preceding #\Newline characters."
     "(Used to deal with Windows files moved to Unix)."
     "Returns T if the file was modified and if so, as a 2nd value, the"
     "number of #\Return #\Newline sequences modified.")
  (when (file-has-return-newline-sequence filepath)
    (let ((temppath (merge-pathnames "temp.temp" filepath))
          (count 0))
      (when verbose
        (cformatt "Stripping Returns preceding Newlines from ~A" 
                (namestring filepath))
        (cformatt "Temporary file: ~A~%" (namestring temppath)))
      (with-open-file 
          (in filepath :direction :input :if-does-not-exist :error)
        (with-open-file 
            (out temppath :direction :output :if-exists :supersede)
          (do ((ch (read-char in nil nil) (read-char in nil nil)))
              ((null ch))
            (cond
             ((null ch) (return))
             ((eql ch #\Return)
              (let ((next-char (read-char in nil nil)))
                (cond
                 ((null next-char) (write-char ch out) (return))
                 ((eql next-char #\Newline) 
                  (incf count)
                  (write-char next-char out))
                 (t
                  (write-char ch out)
                  (write-char next-char out)
                  ))))
             (t (write-char ch out))
             ))))
      (when verbose 
        (cformatt "Strip completed.  ~D Returns removed" count)
        (cformatt "Deleting original, renaming temp as original"))
      (delete-file filepath)
      (rename-file temppath filepath)
      (values t count)
      )))

(defun convert-linefeeds-to-crlfs (filepath &key (verbose t))
  #.(one-string-nl
     "Destructively modify the contents of FILEPATH, changing all linefeed"
     "occurences to carriage return/linefeeds."
     "(Used to deal with Unix files moved to Windows)."
     "Returns T if the file was modified and if so, as a 2nd value, the"
     "number of linefeeds modified.")
  (let ((temppath (merge-pathnames "temp.temp" filepath))
        (count 0))
    (when verbose
      (cformatt "Converting all linefeeds to CRLFs from ~A" 
                (namestring filepath))
      (cformatt "Temporary file: ~A~%" (namestring temppath)))
    (with-open-file 
        (in filepath :direction :input :if-does-not-exist :error
            :element-type '(unsigned-byte 8))              
      (with-open-file 
          (out temppath :direction :output :if-exists :supersede
               :element-type '(unsigned-byte 8))
        (do ((ch (read-byte in nil nil) (read-byte in nil nil)))
            ((null ch))
          (cond
           ((null ch) (return))
           ((eql ch (char-code #\linefeed))
            (incf count)
            (write-byte (char-code #\Return) out)
            (write-byte (char-code #\linefeed) out))
           (t (write-byte ch out))
           ))))
    (when verbose 
      (cformatt "Strip completed.  ~D linefeeds changed" count)
      (cformatt "Deleting original, renaming temp as original"))
    (delete-file filepath)
    (rename-file temppath filepath)
    (values (plusp count) count)
    ))


;;;;;;;;; Utilities having to do with pathnames

;;; Stuff derived from Peter Seibel's unpublished book on Lisp
;;; from the chapter on files and pathnames.

(defun component-present-p (value)
  ;; :unspecific is a special value that can be used instead of
  ;; NIL in certain circumstances but is equivalent to it for our
  ;; purposes.
  (and value (not (eql value :unspecific))))

#+:SBCL
(defun naked-relative-subdirectory-path-p (path)
  (let ((pdir (pathname-directory path)))
    (or
      (and (listp pdir) (eq :relative (first pdir)))
      (not (pathname-directory path)))))

#-:SBCL
(defun naked-relative-subdirectory-path-p (path)
  (and (notany 
        'component-present-p
        (list
         (pathname-host path) (pathname-device path) (pathname-name path)
         (pathname-type path) (pathname-version path)))
       (and (listp (pathname-directory path))
            (eq :relative (first (pathname-directory path)))
            )))

(defun pathname-in-directory-form (name)
  #.(one-string-nl
     "Return a pathname representing the given pathname in `directory"
     "form', i.e. with all the name elements in the directory component and"
     "NIL in the name and type components. Returns its argument if it is"
     "already in directory form.")
  (let ((pathname (pathname name)))
    (cond 
     ((or (component-present-p (pathname-name pathname))
          (component-present-p (pathname-type pathname)))
      (make-pathname 
       :directory 
       (append (pathname-directory pathname) (list (file-namestring pathname)))
       :name nil
       :type nil
       :defaults pathname))
     (t pathname))))

(defun directory-wildcard (name)
  #.(one-string-nl
     "Given a pathname or pathname string that represents a directory,"
     "returns a pathname suitable for using with the DIRECTORY function"
     "to get a listing of that directory.  I.e., it creates a WILD pathname"
     "'denoting' all the files in the directory.")
  (make-pathname 
   :name :wild :type :wild :defaults (pathname-in-directory-form name)))


(defun dirpath-of-filepath (filepath)
  #.(one-string-nl
     "Returns a pathname consisting of the HOST, DEVICE and DIRECTORY"
     "components of (PATHNAME FILEPATH).  I.e., it strips off the name and"
     "type, if any, from FILEPATH, returning a pathname representing the"
     "directory the file named by FILEPATH lives in.")
  (let ((p (pathname filepath)))
    (make-pathname
     :host (pathname-host p)
     :device (pathname-device p)
     :directory (pathname-directory p)
     :name nil :type nil :version nil
     )))

(defun append-subdir 
       (existing-path 
        relative-subdir 
        &optional (translate-logical-existing-path? nil))
  #.(one-string-nl
     "Returns a pathname with the directory portion of EXISTING-PATH"
     "'extended' by RELATIVE-SUBDIR."
     "If RELATIVE-SUBDIR is a string it can either be of the form \"foo\""
     "or \"foo/\". If it is a pathname, it must be a pathname with only"
     "a directory component and that component must be relative."
     "If the optional argument TRANSLATE-LOGICAL-EXISTING-PATH? is T "
     "(default NIL), TRANSLATE-LOGICAL-PATHNAME is first called on "
     "EXISTING-PATH (which is a noop if EXISTING-PATH is not a logical"
     "pathname.  Examples:"
     "(append-subdir \"C:/foo/bar/\" \"baz\") -> #P\"C:/foo/bar/baz/\""
     "(append-subdir \"foo/xyz.lisp\"\"bar\") -> #P\"foo/bar/xyz.lisp\""
     "(append-subdir \"Bioetc:Data;\" \"Go\" t) -> "
     "  #P\"/usr/local/bioetc/data/go/\"")
  (let ((canonical-path
         (if translate-logical-existing-path?
             (translate-logical-pathname existing-path)
           (pathname existing-path)
           )))
    (cond
     ((pathnamep relative-subdir)
      (unless (naked-relative-subdirectory-path-p relative-subdir)
        (error 
         (formatn
          "Must use a relative subdirectory path with no other components: ~A"
          relative-subdir
          )))
      (merge-pathnames relative-subdir canonical-path))
     ((stringp relative-subdir)
      (cond 
       ((zerop (length relative-subdir)) canonical-path)
       ((char= #\/ (lastelem relative-subdir))
        (append-subdir 
         canonical-path 
         (subseq relative-subdir 0 (1- (length relative-subdir)))
         ))
       (t
        (merge-pathnames
         (make-pathname 
          :host nil :device nil :directory `(:relative ,relative-subdir)
          :name nil :type nil :version nil)
         canonical-path
         ))))
     (t (error "Must provide string or a pathname: ~A" relative-subdir))
     )))

(defun pathname-names-directory? (pathname)
  "True if PATHNAME has a directory component and no NAME or TYPE components"
  (let* ((path (pathname pathname)) 
         (dir (pathname-directory path))
         (name (pathname-name path))
         (type (pathname-type path)))
    (values
     (and dir 
          (or (null name) (eq :unspecific name))
          (or (null type) (eq :unspecific type)))
     (first (pathname-directory path))
     )))


(defun directory-with-subdirs-in-directory-form (path)
  #.(one-string-nl
     "Like Common Lisp DIRECTORY function, but insures that subdirectories"
     "are returned in 'directory' format -- with no name or type component."
     "Some lisps (like Allegro) by default return subdirectories in 'file'"
     "format -- with the name of the subdirectory as the name component of"
     "the path instead of the last subcomponent of the directory component.")
  (let ((wildcard (directory-wildcard path)))
    (block nil
      #+(or :lispworks :sbcl)
      ;; SBCL, CMUCL, and Lispworks already do what we want.
      (return (directory wildcard))
      #+(or ccl :ccl :mcl)
      ;; OpenMCL and Macinstosh Common Lisp can be told to return directories.
      (return (directory wildcard :directories t))
      #+:allegro
      ;; Allegro normally returns directories in file form but can be
      ;; told to use directory form
      (return (directory wildcard :directories-are-files nil))
      ;; CLISP will not return subdirectories except when the directory
      ;; component ends with :wild and the name and type components are
      ;; NIL. And as of version 2.32 there's a bug that prevents it from
      ;; returning certain files if :type is NIL.
      #+ccl
      (return
       (nconc 
        (directory (make-pathname :type nil :defaults wildcard))
        (directory (make-pathname 
                    :directory 
                    (append (pathname-directory wildcard) (list :wild))
                    :name nil
                    :type nil
                    :defaults wildcard
                    ))))
      (error "Need implementation!!")
      )))
      
(defun pathname-is-in-subdirectory-of? (pathname directory-pathname)
  #.(one-string-nl
     "Returns T if PATHNAME names a file or directory in DIRECTORY-PATHNAME"
     "or its subdirectories."
     "Neither PATHNAME nor DIRECTORY-PATHNAME need actually exist, the"
     "test is simply on the form of the pathname objects."
     "Both PATHNAME and DIRECTORY-PATHNAME are first converted to physical"
     "pathname objects using PATHNAME and TRANSLATE-LOGICAL-PATHNAME.")
  (let ((p (translate-logical-pathname (pathname pathname)))
        (dp (translate-logical-pathname
             (pathname-in-directory-form (pathname directory-pathname))
             )))
    (flet ((pequal (c1 c2)
             #.(ecase (os?) (:windows '(equalp c1 c2)) (:unix '(equal c1 c2)))
             ))
      (and (pequal (pathname-host p) (pathname-host dp))
           (pequal (pathname-device p) (pathname-device dp))
           (loop for cp in (pathname-directory p)
                 for cdp in (pathname-directory dp)
                 do
                 (when (not (pequal cp cdp)) (return nil))
                 finally (return t)
                 )))))


(defun list-file-contents (filename &key (line-limit 100))
  #.(one-string-nl
     "Print out all the lines of a file.  The key :line-limit (default 100) "
     "stops you from dumping a huge file accidentally.  You can set this "
     "to NIL to see the whole file.")
  (unless (or (null line-limit) (and (integerp line-limit) (plusp line-limit)))
    (error "Invalid LINE-LIMIT value: ~A~%" line-limit))
  (format t "----- ~a -----~%" filename)
  (with-open-file (i filename)
    (loop for line = (read-line i nil nil)
	  as n from 1 by 1
	  until (null line)
          do 
	  (format t "~a~%" line)
	  (cond ((null line) (return nil))
		((and line-limit (>= n line-limit))
		 (format t "***** Listing truncated at ~a lines; use :line-limit new-limit or nil to see the whole file *****~%" line-limit)
                 (format t "----- ~a -----~%" filename)
		 (return-from list-file-contents nil)
		 ))))
  (format t "----- ~a (end) -----~%" filename)
  nil
  )

(defun purge-files-before
       (directory 
        &rest key-args
        &key 
        (universal-time nil)
        (n-weeks-ago nil)
        (n-days-ago nil)
        (n-hours-ago nil)
        (n-minutes-ago nil)
        (n-seconds-ago nil)
        (recursive? nil)
        (files-not-to-delete nil))
  #.(one-string-nl
     "Deletes files in directory DIRECTORY whose last modification date"
     "is previous to the time specification given by one of the keywords"
     "N-WEEKS-AGO, N-DAYS-AGO, N-HOURS-AGO, N-MINUTES-AGO, and N-SECONDS-AGO"
     "which have the obvious semantics.  If UNIVERSAL-TIME is specified"
     "that time (in units of number of seconds after Jan 1 1900)"
     "exactly is used."
     ""
     "Any files whose name and type match the name and type of any files"
     "provided in FILES-NOT-TO-DELETE will not be deleted from the directory."
     )
  (unless (exactly-one-of? 
           universal-time n-weeks-ago n-days-ago
           n-hours-ago n-minutes-ago n-seconds-ago)
    (error "Exactly one time specification keyword must be provided"))
  (let* ((current-time (get-universal-time))
         (threshold 
          (cond 
           (universal-time universal-time)
           (n-weeks-ago (- current-time (* n-weeks-ago #.(* 7 24 60 60))))
           (n-days-ago (- current-time (* n-days-ago #.(* 24 60 60))))
           (n-hours-ago (- current-time (* n-hours-ago #.(* 60 60))))
           (n-minutes-ago (- current-time (* n-minutes-ago 60)))
           (n-seconds-ago (- current-time n-seconds-ago))))
         (do-not-delete (mapcar 'pathname files-not-to-delete)))
    (loop with count = 0
          for file in (directory-with-subdirs-in-directory-form directory)
          do 
          (if (pathname-names-directory? file)
              (when recursive? (apply 'purge-files-before file key-args))
            (unless (member 
                     file do-not-delete :test 
                     (lambda (x y) 
                       (and 
                        (equalp (pathname-name x) (pathname-name y))
                        (equalp (pathname-type x) (pathname-type y))
                        )))
              (let ((last-write-date (file-write-date file)))
                (when last-write-date 
                  (when (< last-write-date threshold)
                    (handler-case 
                        (progn (delete-file file) (incf count))
                      (error (c) (cformatt "Could not delete ~A : ~A" file c))
                      ))))))
          finally 
          (when (plusp count) 
            (cformatt "~D files deleted from ~A" count directory)))
    ))
          
          
(defun pathname-of-new-type (pathname new-type)
  #.(one-string-nl
     "A new pathname identical to (PATHNAME PATHNAME) except that the pathname"
     "type is NEW-TYPE.")
  (let ((p (pathname pathname)))
    (make-pathname 
     :host (pathname-host p) 
     :device (pathname-device p)
     :directory (pathname-directory p)
     :name (pathname-name p)
     :type new-type
     :version (pathname-version p)
     )))

(defun pathname-of-new-name (pathname new-name)
  #.(one-string-nl
     "A new pathname identical to (PATHNAME PATHNAME) except that the pathname"
     "name is NEW-NAME")
  (let ((p (pathname pathname)))
    (make-pathname 
     :host (pathname-host p) 
     :device (pathname-device p)
     :directory (pathname-directory p)
     :name new-name
     :type (pathname-type p)
     :version (pathname-version p)
     )))


(defun file-operate 
       (directories predicate operation &key (root nil) (recursive? nil))
  #.(one-string-nl
     "Find all the file pathnames in DIRECTORIES that satisfy PREDICATE and"
     "call OPERATION on that file pathname."
     "If ROOT is non-nil it is assumed to be a full pathname specifying a"
     "directory, and DIRECTORIES are assumed to be relative pathnames;"
     "ROOT is then merged with each of DIRECTORIES using MERGE-PATHNAMES"
     "to create a full directory pathname."
     "If RECURSIVE? is non-NIL any subdirectories found in DIRECTORIES"
     "are searched recursively for files satisfying PREDICATE, etc."
     "FILE-OPERATE returns a list of two element lists: the first element of"
     "the list being a file satisfying PREDICATE (and therefore operated on),"
     "and the second being the result of OPERATION on the file. The files"
     "are returned in the order they are found (the search being depth-first,"
     "if it is specified recursive, with files in a directory operated on"
     "before the subdirectories are search.")
  (let ((files-operated-on-and-results nil))
    (loop for dir in directories 
          as full-directory-pathname = 
          (if root (merge-pathnames root dir) dir)
          as directory-files = 
          (directory-with-subdirs-in-directory-form full-directory-pathname)
          as subdirectories =
          (remove-if-not 'pathname-names-directory? directory-files)
          as other-files =
          (remove-if 'pathname-names-directory? directory-files)
          do
          (loop for file in other-files 
                when (funcall predicate file)
                do
                (push 
                 (list file (funcall operation file))
                 files-operated-on-and-results
                 ))
          (when recursive?
            (loop for subdir in subdirectories 
                  as results = 
                  (file-operate (list subdir) predicate operation :recursive? t)
                  do
                  (setq files-operated-on-and-results
                        (nconc (reverse results) files-operated-on-and-results)
                        ))))
    (reverse files-operated-on-and-results)
    ))

(defun copy-text-file (from to &key (prepend nil) (postpend nil))
  #.(one-string-nl
     "Creates a copy of file FROM to the pathname TO."
     "If PREPEND is provided, it must be a string, and the characters"
     "are prepended to the created file."
     "If POSTPEND is provided, it must be a string, and the characters"
     "are postpended to the created file."
     "Note: If you want newlines after the prepend or before the postpend,"
     "you must providem them yourself in the strings."
     )
  (with-open-file (p from :direction :input :if-does-not-exist :error)
    (with-open-file (q to :direction :output :if-exists :supersede)
      (block exit
        (when prepend 
          (unless (stringp prepend)
            (error "PREPEND argument to COPY-TEXT-FILE must be a string!"))
          (format q "~A" prepend)
          )
        (loop while t
              do
              (let ((ch (read-char p nil nil nil)))
                (when (null ch) (return-from exit t))
                (write-char ch q)
                )))
      (when postpend
        (unless (stringp postpend)
          (error "POSTPEND argument to COPY-TEXT-FILE must be a string!"))
        (format q "~A" postpend)
        ))))
        
(defun remove-directory 
       (directory-path 
        &key (if-not-empty? :delete-files) (if-does-not-exist? :error))
  #.(one-string-nl
     "Deletes the directory specified by DIRECTORY-PATH if the directory"
     "is empty."
     "If the directory is not empty, the behavior depends on the IF-NOT-EMPTY?"
     "keyword.  Possible values are:"
     "  :ERROR -- Signal an error."
     "  :DELETE-FILES (the default) -- Attempt to recursively delete all files"
     "   in the directory and its subdirectories; then if successful attempt"
     "   to delete the directory again."
     "  Anything else -- The directory is not deleted, the value is returned."
     "Returns T if the directory deletion was successful or NIL"
     "if the value of IF-NOT-EMPTY? is nil and the directory is not empty."
     "If the directory does not exist an error is signaled by default."
     "If the keyword IF-DOES-NOT-EXIST? is not :error (the default),"
     "then its value is returned.")
  #+:allegro 
  (unless (excl:probe-directory directory-path)
    (case if-does-not-exist? 
      (:error (error "Directory ~A does not exist!" directory-path))
      (otherwise (return-from remove-directory if-does-not-exist?))
      ))
  #+:lispworks 
  ;; dummy
  (progn if-does-not-exist?)
  (handler-case 
      #+:allegro
    (excl:delete-directory directory-path)
    #+:lispworks
    (when (io::io-remove-directory directory-path) 
      (signal 'file-error))
    #-(or :allegro :lispworks)
    (error "This needs to be implemented!")
    (file-error 
     ()
     (case if-not-empty?
       (:error 
        (error "Cannot remove directory ~A!  It is not empty!" directory-path))
       (:delete-files
        #+:allegro
        (excl:delete-directory-and-files directory-path)
        #+:lispworks
        (file-operate 
         (list directory-path)
         'identity 
         (lambda (file) 
           (handler-case 
               (delete-file file)
             (error 
              (c)
              (error 
               (one-string-nl
                "REMOVE-DIRECTORY failed.  Could not delete ~A."
                "Actual error: ~A")
               (namestring file) c
               )))))
        #-(or :allegro :lispworks)
        (error "This needs to be implemented!")
        (handler-case 
            #+:allegro
          nil
          #+:lispworks
          (when (io::io-remove-directory directory-path) 
            (signal 'error))
          #-(or :allegro :lispworks)
          (error "This needs to be implemented!")
          (error 
           (c)
           (error 
            (one-string-nl
             "Could not remove directory ~A even after all its contents"
             "had been removed.  Actual error: ~A")
            directory-path c
            ))))
       (otherwise (return-from remove-directory if-not-empty?))
       ))))

#+test
(file-operate
 '("C:/Lispcode/BioLisp/")
 (lambda (x) (equalp (pathname-type x) "txt"))
 (lambda (x) (file-size-in-lisp-chars x))
 :recursive? t
 )

