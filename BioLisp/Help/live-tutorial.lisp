;;; -*- Package: help; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :help)

;;; +=========================================================================+
;;; | Copyright (c) 2002-2006 JP Massar, Jeff Shrager, Mark Slupesky          |
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

#|

********************************************************************
JS20060613: THIS DOCUMENTATION IS COMPLETELY OUT OF DATE (SINCE JP
CHANGED THE WAY THAT THESE ARE HANDLED, APPROX: JP20060515)
********************************************************************

I. LISTING ALL THE AVAILABLE TUTORIALS

From the weblistener, to see a list of all the live tutorials and
be given a startup link for each one, type: 

(list-live-tutorials)

********************************************************************
JS20060613: THIS DOCUMENTATION IS COMPLETELY OUT OF DATE (SINCE JP
CHANGED THE WAY THAT THESE ARE HANDLED, APPROX: JP20060515)
********************************************************************

II. CREATING A LIVE TUTORIAL USING STANDARD HTML

If you want to define a live tutorial using a standard html file:

 -- Add an entry to the list of tutorials where the :file-type has
the value :html, the :filename has the value a string which is the
name of your html file (not including the .html suffix), and the
:description has the value a string which is a short description of
your tutorial.  You can ignore any other properties that you might see
in the list below.

 -- :user-mode indicates who can see this tutorial. If you want everyone
to be able to see it, leave out user-mode, or, if you'd rather not get
an error at load time, use :user-mode :all   If you want to tutorial
to never be seen, use some user mode that does exist, like :never or :invisible
If you want the tutorials to be seen in a couple of different modes, you
can make the argument to :user-mode a list of modes.

 -- Create a file in the directory specified to the
CREATE-NEW-LIVE-TUTORIALS call above.  (This would translate to
/usr/local/biolisp/Doc/livetutorials/ on Nostoc or
/home/biovcu/biolisp/Doc/livetutorials/ on Press.)  The file must be
named the same name as your specified by the :filename property, and
must have extension .html.

 -- Use the loop-tutorial.html file in the 'livetutorials'
subdirectory of the directory the file you are reading now is in as a
template to make your own live tutorial.  The idea of these files is
that wherever you put a consecutive set of lines beginning with '>>' a
DO IT button will show up when the user views this live tutorial
through the weblistener which will transfer the code after the '>>'
markers to the Weblistener to be executed.

Example: To create a live tutorial named "fred" you might use the
following entry:

("fred" :file-type :html :filename "fred-tutorial" 
  :description "the flintstones")

********************************************************************
JS20060613: THIS DOCUMENTATION IS COMPLETELY OUT OF DATE (SINCE JP
CHANGED THE WAY THAT THESE ARE HANDLED, APPROX: JP20060515)
********************************************************************

III. CREATING A LIVE TUTORIAL USING AN LHTML FILE

 -- Add an entry to the list of tutorials below where the :file-type has
the value :lhtml, the :filename is as above (not including the .lhtml
suffix), and the :description is as above.  Also you must specify,
using the :lhtml-function property, the name of a function which will
generate the html for your live tutorial via allegroserve (this
function will be defined in the file you named with the :filename
property).  Optionally you can specify a start function (which
defaults to BEGIN-LIVE-<name>-TUTORIAL)

 -- Create a file as above.  The file must be named the same name
as you specified by the :filename property and must have extension .lhtml

 -- Use the lloop.html file in the same directory as a template to make
your own live tutorial code in the file.  You must rename the functions in your
file so that they do not conflict with other live tutorial function names and
so that the primary function (at the top of the file) corresponds to the 
name you gave it using the :lhtml-function property.  

Example: To create a live tutorial named "wilma" you might use the 
following entry:

("wilma" :file-type :lhtml 
         :filename "wilma-tutorial" 
         :description "fred's wife" 
         :lhtml-function wb::html-for-live-wilma-tutorial
         :start-function :start-wilma-going)

The wilma-tutorial.lhtml file must then contain a definition for 
the function HTML-FOR-LIVE-WILMA-TUTORIAL.  

In addition, a new function available to all weblistener users called
START-WILMA-GOING is created.  If executed, it will display
a link which, when clicked, will start up your tutorial.  

Label entries can be included to divide up the table. The labels are
identified by inclusion of a :color field, which gives the color of
the label, as:

    ("Test Lable"
     :filename "label"
     :sort-order 101
     :user-mode :biolisp
     :color "yellow"
     :description "this is a test label")

Note that the label MUST have a unique filename, like all other
entries because this is used as the internal key for the entry
(although in this case it doesn't have to point to a file), and a
sort-order if you want it to show up in a particular place. Labels are
not linked, they just appear in the right place.

********************************************************************
JS20060613: THIS DOCUMENTATION IS COMPLETELY OUT OF DATE (SINCE JP
CHANGED THE WAY THAT THESE ARE HANDLED, APPROX: JP20060515)
********************************************************************

IV. CREATING YOUR OWN SET OF LIVE TUTORIALS

 -- Using the call to CREATE-NEW-LIVE-TUTORIALS above as a template,
create a .lisp file whereever you like which contains a modified call
to this function.  Instead of "websrc:Doc;livetutorials;" put a path
to a directory you choose that will contain your own live tutorial
files.

 -- Remove the existing live tutorial definitions from the call
and add your own new ones.  Create the actual live tutorial .html
or .lhtml files you've referenced in the tutorial definitions.

  -- load the .lisp file you've created using LOAD into the Weblistener.
This will add your tutorials to the set of system tutorials.

Warning: Don't name your live-tutorials the same as any existing tutorials,
because yours will overwrite and destroy the existing ones.!!

  -- Use (list-live-tutorials) to verify that the ones you created
are available and that the links work, etc.

|#

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(create-new-live-tutorials list-live-tutorials) :webuser)
  (export '(create-new-live-tutorials list-live-tutorials) :webuser))

;; :filename -- a string, must be full pathname 
;; :file-type -- either :html or :lhtml 
;; :user-mode -- a keyword or a list of keywords
;; :sort-order -- an integer 
;; :description -- a string, this is really the summary 
;; :section-header -- two strings, a title, and a color 
;; :lhtml-function -- used only wth file type lhtml, must be a symbol
;; :start-function -- used only with file type :lhtml, must be a symbol

(define-doc-definer 
 tutorial
 def-live-tutorial
 create-a-live-tutorial
 (
  (:text :non-nil-list ddd-identity identity help:text)
  (:keywords :list ddd-all-symbols-or-strings identity help:keywords)
  (:see-also :list verify-see-also-entries parse-see-also-entries help:see-also)
  (:author :list ddd-all-strings identity help:author)
  (:filename :exactly-one ddd-string identity help:filename)
  (:file-type :exactly-one verify-tutorial-file-type identity help:file-type)
  (:user-mode 
   :non-nil-list ddd-all-keywords identity help:user-mode)
  (:sort-order
   :exactly-one ddd-integer parse-tutorial-sort-order help:sort-order)
  (:description :one-or-none ddd-string-or-nil identity help:description)
  (:section-header
   :list verify-tutorial-section-header identity help:section-header)
  (:lhtml-function :exactly-one ddd-symbol identity help:lhtml-function)
  (:start-function :exactly-one ddd-symbol identity help:start-function)
  )
 :after-code (tutorial-verification-code obj)
 )

(defparameter *valid-tutorial-file-types* '(:html :lhtml)) 


(defun verify-tutorial-file-type (type)
  (if (member type *valid-tutorial-file-types*)
      (values t type)
    (values nil "Must be one of ~S !" *valid-tutorial-file-types*)))

(defun verify-tutorial-section-header (sh)
  (if (and (every 'stringp sh) (= (length sh) 2))
      (values t sh)
    (values nil "Must be exactly two strings, a title and a color!")))

(defun parse-tutorial-sort-order (sort-order)
  (format nil "~3,'0d" sort-order))
  
(defun tutorial-verification-code (obj)
  (let ((name (help:name obj))
        (section-header? (help:section-header obj)))
    (unless (or (help:filename obj) section-header?)
      (error "~S definition has no filename!" name))
    (unless (help:user-mode obj) 
      (warn 
       #.(one-string-nl
          "~s has no :user-mode parameter. It will be visible"
          "  in all user modes. (You can get rid of this warning"
          "  by adding :user-mode :all instead of leaving it out.)")
       name)
      (setf (help:user-mode obj) (list :all)))
    (unless (or (help:file-type obj) section-header?) 
      (warn 
       #.(one-string-nl
          "The live tutorial ~S has no file-type. HTML is assumed."
          "(You can get rid of this warning by adding (:file-type :html))")
       name)
      (setf (help:file-type obj) :html))
    (unless (or (not (eq :lhtml (help:file-type obj)))
                (help:lhtml-function obj))
      (error "~S has no lhtml function name for the :lhtml file-type!" name))
    (unless (help:sort-order obj) 
      (warn 
       #.(one-string-nl
          "The live tutorial ~S has no sort order."
          "It will sort at the end alphabetically by name."
          "(You can get rid of this warning by adding (:sort-order n))")
       name)
      (setf (help:sort-order obj) (formatn "999~a" name)))
    (unless section-header?
      (let ((file-string 
             (s+ (help:filename obj) "."
                 (string-downcase (help:file-type obj)))))
        (handler-case
            (let ((path (pathname file-string)))
              (unless (probe-file path) 
                (warn 
                 (one-string-nl
                  "While defining ~S,"
                  "Cannot access live tutorial file ~s."
                  "(It may not exist or the system may not have permission"
                  "to access it.)")
                 name (namestring path)))
              (setf (help:filename obj) (namestring path))
              )
          (error 
           ()
           (error "file string ~S cannot be interpreted as a file!" 
                  file-string)))))
    (vwhen (old-tutorial (help:find-documentation name 'help:tutorial))
      (let ((new (help:filename obj))
            (old (help:filename old-tutorial)))
        (unless (equal new old)
          (warn 
           (one-string-nl
            "The live tutorial ~S was previously defined"
            "  using the source file ~s," 
            "  but now the source file is ~s.")
           name old new
           ))))))
