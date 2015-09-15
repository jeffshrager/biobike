;;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: bbi; -*-

(in-package :bbi) 

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

;;; Author: JP Massar, Jeff Elhai, Arnaud Taton. 

(DEFUN tree-geometry (tgftree-file new-tgftree-file twidth tlength bootstrap)
(let* ((tgftree (BBI::FILE-TO-STRING tgftree-file ))
       (splitted-tgftree
         (MATCH-OF-PATTERN-aux NIL "(.*)(\%.*tree geometry.*)(\%.*brackets.*)" 
             tgftree '(NIL NIL NIL T) :CROSS-LINES T))
       (tree-geometry 
                (IF bootstrap    
                (FORMAT NIL "~%~a~%~a~%~a~a~a~%~a~a~a~%~a~%~a~%~a~%~a~%"
                        "% tree geometry"
                        "%"
                        "    \\width{" twidth "}  % width in mm"
                        "     \\height{" tlength "}  % height in mm"
                        "     \\autolength  % ignore len{} in nodes"
                        "     \\paper{a0}"
                        "%     \\variable"
"%
% layout
%
    \\roundness{0.2}  % between 0.0 and 1.0
    \\thickness{0.3}  % in mm
    \\margin{5}{0}{5}{10} % left top right bottom, in mm
    \\style{default}{plain}{9.6}  
    \\style{r}{plain}{12}  

%     \\style{u1}{plain}{9.6}
%     \\style{d1}{italic}{9.6}
%     \\style{u2}{bold}{8.4}
%     \\style{br}{plain}{14.4}
%     \\rulepos{5}{20}
 % position of rule
%     \\separator{:}
%     \\proof

%
")

                (FORMAT NIL "~%~a~%~a~%~a~a~a~%~a~a~a~%~a~%~a~%~a~%~a~%"
                        "% tree geometry"
                        "%"
                        "    \\width{" twidth "}  % width in mm"
                        "    \\height{" tlength "}  % height in mm"
                        "%     \\autolength  % ignore len{} in nodes"
                        "     \\paper{a0}"
                        "%     \\variable"
"%
% layout
%
    \\roundness{0.2}  % between 0.0 and 1.0
    \\thickness{0.3}  % in mm
    \\margin{5}{0}{5}{10} % left top right bottom, in mm
    \\style{default}{plain}{9.6}  
    \\style{r}{plain}{12}  

%     \\style{u1}{plain}{9.6}
%     \\style{d1}{italic}{9.6}
%     \\style{u2}{bold}{8.4}
%     \\style{br}{plain}{14.4}
%     \\rulepos{5}{20}
 % position of rule
%     \\separator{:}
%     \\proof

%
")))
       (joined-tgftree
         (join 
          (ref splitted-tgftree 1)
          tree-geometry
          (ref splitted-tgftree 3))))
  (bbl::WRITE joined-tgftree TO new-tgftree-file TEXT)))

(DEFUN RETURN-TREE (tree-dir retree-out bootstrap width pdf png jpg eps newick)
  (LET* ((tree1 
          (IF bootstrap
              (STRING-OF 
                (BBL::READ FROM (FORMAT NIL "~a" retree-out) JOIN-LINES))
            (STRING-OF 
              (BBL::READ FROM (FORMAT NIL "~a" retree-out) JOIN-LINES))))
         (tree2 (IF bootstrap                            
                    (JOIN (SPLIT tree1 EVERY "):") BY ")")
                  tree1))
         (nexus-tree-file 
          (FORMAT NIL "~a~a"  tree-dir "nexus-tree"))

         (tree-newick  (STRING-OF 
                         (BBL::READ FROM (FORMAT NIL "~a~a" tree-dir "outtree_ll") JOIN-LINES)))
         (label-file-path (FORMAT NIL "~a~a" tree-dir "labels-sed"))
         (tlength (* (LENGTH-OF (BBL:READ label-file-path TABBED)) 8))
         (twidth (IF width width 150)))
    (WITH-OPEN-FILE (nexus-tree nexus-tree-file :direction :output)
      (FORMAT nexus-tree "~a~%~a~%~a~a~%~a"
              "#NEXUS" "begin trees;" "tree 'test' =" tree2 "end;"))
    (LET ((treegraph-commands1 
           (FORMAT NIL "~a~a~a"
                   CL-USER::*BIOTOOLS-DIRECTORY* "tgf10rc4/tgf -t " nexus-tree-file)))
      (bio::PROTECTED-SHELL-COMMAND treegraph-commands1 :directory tree-dir)
      (TREE-GEOMETRY (JOIN nexus-tree-file ".tgf") (JOIN nexus-tree-file ".tgf") twidth tlength bootstrap))

; *******
    (bio::WITH-TEMP-FILE-IN (displayed-tree "" :name "tree" :type NIL :delete? UTILS:*DELETE-TEMP-FILES*) 

; **********

      (LET* ((sed-command (FORMAT NIL "~a~a~a~a~a~a"
                                  "sed -f " label-file-path " " "nexus-tree.tgf"
                                  " > " "nexus-tree1.tgf"))
             (sed-command1 (FORMAT NIL "~a~a~a~a"
                                   "sed -e 's/?/_/g' " "nexus-tree1.tgf"
                                   " > " "nexus-tree2.tgf"))
             (treegraph-commands2 
              (FORMAT NIL "~a~a~a~a"
                      CL-USER::*BIOTOOLS-DIRECTORY* "tgf10rc4/tgf -p " "nexus-tree2" ".tgf"))
             (eps2pdf-command
              (FORMAT NIL "~a~a~a" CL-USER::*BIOTOOLS-DIRECTORY* "others/eps2pdf " "nexus-tree2.eps"))
             (eps2png-command 
              (FORMAT NIL "~a~a~a" CL-USER::*BIOTOOLS-DIRECTORY* "others/eps2png " "nexus-tree2.eps")) 
             (eps2jpg-command 
              (FORMAT NIL "~a~a~a" CL-USER::*BIOTOOLS-DIRECTORY* "others/eps2jpg " "nexus-tree2.eps"))
             (tree-in-biobikewww-path 
              (COND
               (pdf (bio::MERGE-PATHNAMES cl-user::*webtmp-directory* (FORMAT NIL "~A.pdf" displayed-tree)))
               (png (bio::MERGE-PATHNAMES cl-user::*webtmp-directory* (FORMAT NIL "~A.png" displayed-tree)))
               (jpg (bio::MERGE-PATHNAMES cl-user::*webtmp-directory* (FORMAT NIL "~A.jpg" displayed-tree)))
               (eps (bio::MERGE-PATHNAMES cl-user::*webtmp-directory* (FORMAT NIL "~A.eps" displayed-tree)))
               (T   (bio::MERGE-PATHNAMES cl-user::*webtmp-directory* (FORMAT NIL "~A.png" displayed-tree)))))
             (tree-in-tree-dir-path
              (COND
               (pdf (FORMAT NIL "~A~A.pdf" tree-dir  "nexus-tree2"))
               (png (FORMAT NIL "~A~A.png" tree-dir  "nexus-tree2"))
               (jpg (FORMAT NIL "~A~A.jpg" tree-dir  "nexus-tree2"))
               (eps (FORMAT NIL "~A~A.eps" tree-dir  "nexus-tree2"))
               (T   (FORMAT NIL "~A~A.png" tree-dir  "nexus-tree2"))))
             (cp-tree-to-biobikewww 
              (FORMAT NIL "~a~a~a~a" "cp " tree-in-tree-dir-path " " tree-in-biobikewww-path))
             )
        (bio::PROTECTED-SHELL-COMMAND sed-command :directory tree-dir)
        (bio::PROTECTED-SHELL-COMMAND sed-command1 :directory tree-dir)
        (bio::PROTECTED-SHELL-COMMAND treegraph-commands2 :directory tree-dir)
        (COND (pdf (bio::PROTECTED-SHELL-COMMAND eps2pdf-command :directory tree-dir))
              (png (bio::PROTECTED-SHELL-COMMAND eps2png-command :directory tree-dir))
              (jpg (bio::PROTECTED-SHELL-COMMAND eps2jpg-command :directory tree-dir))
              (eps NIL)
              (T (bio::PROTECTED-SHELL-COMMAND eps2png-command :directory tree-dir)))

        (bio::PROTECTED-SHELL-COMMAND cp-tree-to-biobikewww :directory tree-dir)

        (COND 
         (pdf (WB::MAKE-URL :PATH  (wb::webtmp-url (format nil "~a.pdf" displayed-tree)) :DISPLAY-STRING "View tree" :TARGET "_blank"))
         (png (WB::MAKE-URL :PATH  (wb::webtmp-url (format nil "~a.png" displayed-tree)) :DISPLAY-STRING "View tree" :TARGET "_blank"))
         (jpg (WB::MAKE-URL :PATH  (wb::webtmp-url (format nil "~a.jpg" displayed-tree)) :DISPLAY-STRING "View tree" :TARGET "_blank"))
         (eps (WB::MAKE-URL :PATH  (wb::webtmp-url (format nil "~a.eps" displayed-tree)) :DISPLAY-STRING "View tree" :TARGET "_blank"))
         (newick tree-newick)
         (T   (WB::MAKE-URL :PATH  (wb::webtmp-url (format nil "~a.png" displayed-tree)) :DISPLAY-STRING "View tree" :TARGET "_blank")))))))

(DEFUN NEIGHBOR 
       (tree-project
        &KEY neighbor upgma outgroup subreplicate
        randomize-input-order multiple-data-sets)
  
  (LET*  ((tree-dir (bio::PATHNAME-IN-DIRECTORY-FORM 
                     (FORMAT NIL "~a~a~a" (wb::visitor-directory *username*) "tree-project-" tree-project)))
          (phyin (COND 
                  (multiple-data-sets (FORMAT NIL "~a~a~%" tree-dir "outdist"))
                  (T (FORMAT NIL "~a~a~%" tree-dir "outfile"))))
          (number-seed (+ (* 4 (RANDOM-INTEGER)) 1))
          (tree-method (COND 
                        (neighbor (FORMAT NIL "~a" ""))
                        (upgma (FORMAT NIL "~a~%" "N"))
                        (T (FORMAT NIL "~a" ""))))

          (outgroup-O (COND 
                       (outgroup (FORMAT NIL "~a~%~a~%" "O" outgroup))
                       (T (FORMAT NIL "~a" ""))))
          (subreplicate-S (COND 
                           (subreplicate (FORMAT NIL "~a~%~a~%" "S" number-seed))
                           (T (FORMAT NIL "~a" ""))))
          (randomize-J (COND 
                        (randomize-input-order (FORMAT NIL "~a~%~a~%" "J" number-seed))
                        (T (FORMAT NIL "~a" ""))))
          (m-data-set-M (COND 
                         (multiple-data-sets (FORMAT NIL "~a~%~a~%~a~%" "M" multiple-data-sets number-seed))
                         (T (FORMAT NIL "~a" ""))))
          )
    (bio::WITH-TEMP-FILE-IN (cmdneifile tree-dir :name "neicmd" :delete? UTILS:*DELETE-TEMP-FILES*)
      (WITH-OPEN-FILE (neicmd cmdneifile :direction :output)
        (FORMAT neicmd "~a~a~%~a~%~a~a~a~a~a~a~%~a" phyin "F" "out-nei" tree-method outgroup-O subreplicate-S 
                randomize-J m-data-set-M 2 "Y"))
      (LET ((phylip-command (FORMAT NIL "~a/~a < ~a > ~a" 
                                    cl-user::*phylip-executable-dir*
                                    "neighbor" cmdneifile "neighbor.log")))
        (bio::PROTECTED-SHELL-COMMAND phylip-command :directory tree-dir)
        tree-project)
      )))

(DEFUN strip-all-special-characters (string)
  (JOIN 
   (SPLIT  
     (JOIN 
      (SPLIT  
        (LOOP FOR i IN (LIST  #\` #\@ #\# #\$ #\% #\& #\* #\( #\) #\= #\+ #\{ #\} #\[ #\] #\| #\\ #\: #\; #\, #\. #\? #\/ #\' #\" #\SPACE)
              DO (SETQ string (bio::SUBSTITUTE #\_ i string))
              FINALLY (RETURN string))
        EVERY "__") BY "_")
     EVERY "__") BY "_")
  )