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

(defun resample-file-error (name) 
  (ERROR "You must provide the ~A as a file called ~A in your tree-project directory" name name))

(defun resample-data-type-if-file (flag dir name type)
  (cond 
   (flag 
    (IF-TRUE (PROBE-FILE (FORMAT NIL "~a~a" dir name))
             THEN (FORMAT NIL "~a~%" type)
             ELSE (resample-file-error name)
             ))
   (t (FORMAT NIL "~a" ""))
   ))

(defun tree-directory (tree-project)
  (bio::pathname-in-directory-form 
   (format nil "~a~a~a" 
           (wb::visitor-directory *username*) "tree-project-" tree-project)))

(defun reroot-and-return-tree-aux 
       (tree-project
        outgroup unrooted midpoint-root bootstrap pdf png jpg eps newick width)
  (LET*  ((tree-dir (tree-directory tree-project))
          (label-file-path (FORMAT NIL "~a~a" tree-dir "labels-sed"))
          (tree-file (COND (bootstrap "consensus-tree")
                           (T "outtree"))))
    (bio::WITH-TEMP-FILE-IN
        (retree-out tree-dir :name "retree_out" 
                    :delete? UTILS:*DELETE-TEMP-FILES*)
      (bio::WITH-TEMP-FILE-IN 
          (outtree-ll-tmp tree-dir :name "outtree_ll_tmp"
                          :delete? UTILS:*DELETE-TEMP-FILES*)
        (bio::WITH-TEMP-FILE-IN 
            (cmdretreefile tree-dir :name "retreecmd"
                           :delete? UTILS:*DELETE-TEMP-FILES*)
          (WITH-OPEN-FILE (retreecmd cmdretreefile :direction :output)
            (COND        
             (unrooted 
              (FORMAT retreecmd "~a~%~a~%~a~a~%~a~%~a~%~a~%~a~%~a~%" 
                      "0" "Y" tree-dir tree-file  "Q" "Y" "F" retree-out "U"))
             (midpoint-root 
           
              (FORMAT retreecmd "~a~%~a~%~a~a~%~a~%~a~%~a~%~a~%~a~%~a~%" 
                      "0" "Y" tree-dir tree-file
                      "M" "Q" "Y" "F" retree-out "R"))
             (outgroup 
              (bbi::PROCESS-OUTGROUP tree-dir tree-file label-file-path
                                     outgroup retreecmd retree-out))
             (T (FORMAT retreecmd "~a~%~a~%~a~a~%~a~%~a~%~a~%~a~%~a~%" 
                        "0" "Y" tree-dir tree-file  
                        "Q" "Y" "F" retree-out "U"))))
          (LET ((retree-command (FORMAT NIL "~a/~a < ~a > ~a"
                                        cl-user::*phylip-executable-dir*
                                        "retree" cmdretreefile "retree.log")))
            (CASE (bio::PROTECTED-SHELL-COMMAND retree-command
                                                :directory tree-dir)
              (otherwise nil)))
          (LET ((sed-command (FORMAT NIL "~a~a~a~a~a~a"
                                     "sed -f " label-file-path " " retree-out
                                     " > " outtree-ll-tmp))
                (sed-command1 (FORMAT NIL "~a~a~a~a"
                                      "sed -e 's/?/_/g' " outtree-ll-tmp
                                      " > " "outtree_ll_tmp1")))
            (bio::PROTECTED-SHELL-COMMAND sed-command :directory tree-dir)
            (bio::PROTECTED-SHELL-COMMAND sed-command1 :directory tree-dir))
          (bio::PROTECTED-SHELL-COMMAND "mv outtree_ll_tmp1 outtree_ll"
                                        :directory tree-dir)
          (IF bootstrap
              (COND 
               (pdf (RETURN-TREE
                     tree-dir retree-out bootstrap width pdf nil nil nil nil))
               (png (RETURN-TREE
                     tree-dir retree-out bootstrap width nil png nil nil nil))
               (jpg (RETURN-TREE
                     tree-dir retree-out bootstrap width nil nil jpg nil nil))
               (eps (RETURN-TREE
                     tree-dir retree-out bootstrap width nil nil nil eps nil))
               (newick 
                (RETURN-TREE
                 tree-dir retree-out bootstrap width nil nil nil nil newick))
               (T   (RETURN-TREE
                     tree-dir retree-out bootstrap width nil png nil nil nil)))
            (COND 
             (pdf
              (RETURN-TREE tree-dir retree-out nil width pdf nil nil nil nil))
             (png
              (RETURN-TREE tree-dir retree-out nil width nil png nil nil nil))
             (jpg
              (RETURN-TREE tree-dir retree-out nil width nil nil jpg nil nil))
             (eps
              (RETURN-TREE tree-dir retree-out nil width nil nil nil eps nil))
             (newick 
              (RETURN-TREE
               tree-dir retree-out nil width nil nil nil nil newick))
             (T (RETURN-TREE
                 tree-dir retree-out nil width nil png nil nil nil))))
          ))) ; close all temp files
    ))

(defun consensus-tree-aux
       (tree-project 
        extended-majority-rule majority-rule strict user-majority-rule)
  (LET*  ((tree-dir (tree-directory tree-project))
          (phyin (FORMAT NIL "~a~a" tree-dir "outtree"))
          (consensus-type
           (COND 
            (extended-majority-rule (FORMAT NIL "~a" ""))
            (majority-rule (FORMAT NIL "~a~%~a~%~a~%" "C" "C" "C"))
            (strict (FORMAT NIL "~a~%~a~%" "C" "C"))
            (user-majority-rule (FORMAT NIL "~a~%~a~%~a~%~a~%" "C" "C" "C" "C"))
            (T (FORMAT NIL "~a" ""))))
          (branch-fraction 
           (COND 
            (user-majority-rule (FORMAT NIL "~a~%" user-majority-rule))
            (user-majority-rule (FORMAT NIL "~a" "")))))

    (bio::WITH-TEMP-FILE-IN 
        (phy-cmd tree-dir :name "phy_cmd" :delete? UTILS:*DELETE-TEMP-FILES*)
      (WITH-OPEN-FILE (phy_cmd phy-cmd :direction :output)
        (FORMAT phy_cmd "~a~%~a~%~a~%Y~%~a~%~a~%~a~a~%Y~a"
                phyin "F" "out-cons" "F" "consensus-tree" 
                consensus-type 2 branch-fraction))

      (LET ((phylip-command 
             (bio::FORMATN
              "~a/~a < ~a > ~a" 
              cl-user::*phylip-executable-dir*
              "consense" phy-cmd (FORMAT NIL "~a" "consense.log"))))
        (bio::PROTECTED-SHELL-COMMAND phylip-command :directory tree-dir)
        tree-project)
      )))

(defun build-maximum-likelihood-tree-dna-aux
       (tree-project
        user-tree user-tree-lengths 
        transition-transversion-ratio base-frequencies
        number-of-site-categories category-rates
        regional-gamma-distribution-rate 
        regional-gamma-distribution-rate+invariant-sites
        regional-user-distribution-rate number-of-regional-rate-categories
        coefficient-of-variation-of-substitution-rate-among-positions
        fraction-of-invariant-sites regional-category-rates 
        probability-foreach-regional-category-rate
        correlation-of-rate-distribution-of-adjacent-sites
        read-weights-for-positions speedier-analysis global-rearrangements
        jumble-sequence-order multiple-data-sets multiple-data-weights)

  (LET*  ((tree-dir (tree-directory tree-project))
          (phyin
           (COND 
            (multiple-data-sets (FORMAT NIL "~a~a" tree-dir "outfile"))
            (T (FORMAT NIL "~a~a" tree-dir "alignment-phy"))))
          (utree
           (COND
            (user-tree 
             (IF-TRUE (PROBE-FILE (FORMAT NIL "~a~a" tree-dir "intree"))
                      THEN (FORMAT NIL "~a~%" "U")
                      ELSE (ERROR "You must provide the user tree as a file called intree in your tree-project directory")))
            (T (FORMAT NIL "~a" ""))))
          (utree-l
           (COND 
            (user-tree-lengths
             (IF-TRUE user-tree
                      THEN (FORMAT NIL "~a~%" "L")
                      ELSE (ERROR "This option is only available with the option user-tree")))
            (T (FORMAT NIL "~a" ""))))
          (tt-ratio
           (COND 
            (transition-transversion-ratio
             (FORMAT NIL "~a~%~a~%" "T" transition-transversion-ratio))
            (T (FORMAT NIL "~a" ""))))
          (b-freq
           (COND 
            (base-frequencies (FORMAT NIL "~a~%~a~%" "F" base-frequencies))
            (T (FORMAT NIL "~a" ""))))
; categories for sites defined by the user
          (src-nb 
           (COND 
            (number-of-site-categories
             (FORMAT NIL "~a~%~a~%" "C" number-of-site-categories))
            (T (FORMAT NIL "~a" ""))))
          (sc-rates
           (COND 
            (category-rates (FORMAT NIL "~a~%" category-rates))
            (T (FORMAT NIL "~a" ""))))
          (sr-parameters 
           (COND 
            ((ALL-TRUE (list (SAME src-nb "") (SAME sc-rates ""))) 
             (FORMAT NIL "~a" ""))
            ((ANY-TRUE (list (SAME src-nb "") (SAME sc-rates ""))) 
             (ERROR ".."))
            ((ALL-FALSE (list (SAME src-nb "") (SAME sc-rates ""))) 
             (COND 
              ((= (LENGTH-OF (SPLIT category-rates EVERY " "))
                  number-of-site-categories)
               (IF-TRUE (PROBE-FILE (FORMAT NIL "~a~a" tree-dir "categories"))  
                        THEN (FORMAT NIL "~a~a" src-nb sc-rates)
                        ELSE (ERROR "You must provide a file called categories in your tree-project directory")))
              ((NOT (= (LENGTH-OF (SPLIT category-rates EVERY " "))
                       number-of-site-categories))
               (ERROR "You must provide as many rates than the number of categories defined"))))
            ))
; reginal categories (Hidden Markov Model) rates folowing a gamma distribution or + invariant sites or defined by the user 
          (rate-distribution 
           (COND 
            (regional-gamma-distribution-rate (FORMAT NIL "~a~%" "R"))
            (regional-gamma-distribution-rate+invariant-sites 
             (FORMAT NIL "~a~%~a~%" "R" "R"))
            (regional-user-distribution-rate
             (FORMAT NIL "~a~%~a~%~a~%" "R" "R" "R"))
            (T (FORMAT NIL "~a" ""))))
          (number-rate-categories-in-hmm
           (COND 
            (number-of-regional-rate-categories 
             (IF-TRUE 
              (AND
               (OR regional-gamma-distribution-rate 
                   regional-gamma-distribution-rate+invariant-sites)  
               coefficient-of-variation-of-substitution-rate-among-positions) 
              ;; won't work => ""
              THEN (FORMAT NIL "~a~%" number-of-regional-rate-categories)
              ELSE (ERROR "This parameter can only be adjusted if you chosen a gamma distribution")))
            (T (FORMAT NIL "~a" ""))))
; if gamma distribution rates & gamma + isites distribution rates
          (coef-var-subst-posits
           (COND 
            (coefficient-of-variation-of-substitution-rate-among-positions 
             (IF-TRUE
              (OR regional-gamma-distribution-rate
                  regional-gamma-distribution-rate+invariant-sites ) 
              ;; won't work => ""
              THEN (FORMAT NIL "~a~%" coefficient-of-variation-of-substitution-rate-among-positions)
              ELSE (ERROR "This parameter can only be adjusted if you had chosen a gamma distribution")))
            (T (FORMAT NIL "~a" ""))))
; if gamma + isites distribution rates
          (invariant-sites
           (COND 
            (fraction-of-invariant-sites 
             (IF-TRUE 
              (AND regional-gamma-distribution-rate+invariant-sites 
                   coefficient-of-variation-of-substitution-rate-among-positions) 
              ;; won't work => ""
              then (format nil "~a~%" fraction-of-invariant-sites)
              else (error "this parameter can only be adjusted if you had chosen a gamma distribution with ivariant sites")))
            (t (format nil "~a" ""))))
; if user distribution rates
          (hmm-c-rates
           (cond 
            (regional-category-rates  
             (if-true 
              (and regional-user-distribution-rate 
                   number-of-regional-rate-categories) ;
              ;; won't work => ""
              then (format nil "~a~%" regional-category-rates)
              else  (error "this parameter can only be adjusted if you had chosen a user distribution")))
            (t (format nil "~a" ""))))
          (hmm-c-rates-prob 
           (cond 
            (probability-foreach-regional-category-rate 
             (format nil "~a~%" probability-foreach-regional-category-rate))
            (t (format nil "~a" ""))))
; distribution rates
          (rate-distribution-adj-s
           (cond
            (correlation-of-rate-distribution-of-adjacent-sites
             (format nil "~a~%" "a"))
            (t (format nil "~a" ""))))
; ------
          (r-weights 
           (cond 
            (read-weights-for-positions 
             (if-true (probe-file (format nil "~a~a" tree-dir "weights"))
                      then (format nil "~a~%" "w")
                      else (error "you must provide the weights as a file called weights in your tree-project directory")))
            (t (format nil "~a" ""))))
          (analysis-speed 
           (cond
            (speedier-analysis 
             (if-true user-tree
                      then (error "option not available with a user tree")
                      else (format nil "~a~%" "s")))
            (t (format nil "~a" ""))))
          (rearrangements 
           (COND
            (global-rearrangements 
             (IF-TRUE user-tree
                      THEN (ERROR "Option not available with a user tree")
                      ELSE (FORMAT NIL "~a~%" "G")))
            (T (FORMAT NIL "~a" ""))))
          (number-seed (+ (* 4 (RANDOM-INTEGER)) 1))
          (input-seq-o 
           (COND
            (jumble-sequence-order
             (IF-TRUE
              (AND user-tree 
                   (OR (NOT multiple-data-sets) 
                       (NOT  multiple-data-weights)))
              THEN (ERROR "Option not available with a user tree and one data set")
              ELSE (FORMAT NIL "~a~%~a~%~a~%" "J" number-seed jumble-sequence-order)))
            (T (FORMAT NIL "~a" ""))))
          (m-data-sets 
           (COND 
            (multiple-data-sets 
             (IF-TRUE jumble-sequence-order
                      THEN (FORMAT NIL "~a~%~a~%~a~%~a~%~a~%" "F" 
                                   "outml" "M" "D" multiple-data-sets)
                      ELSE (FORMAT NIL "~a~%~a~%~a~%~a~%~a~%~a~%~a~%~a~%"
                                   "F" "outml" "M" "D"
                                   multiple-data-sets number-seed 1 "J")))
            (T (FORMAT NIL "~a" ""))))
          (m-data-weights 
           (COND 
            (multiple-data-weights 
             (IF-TRUE jumble-sequence-order
                      THEN (FORMAT NIL "~a~%~a~%~a~%" "M" "W"
                                   multiple-data-weights)
                      ELSE (FORMAT NIL "~a~%~a~%~a~%~a~%~a~%~a~%" "M" "W"
                                   multiple-data-weights number-seed 1 "J")))
            (T (FORMAT NIL "~a" ""))))
          (r-m-data-weights 
           (COND 
            (multiple-data-weights 
             (IF-TRUE (PROBE-FILE (FORMAT NIL "~a~a" tree-dir "outweights"))
                      THEN (FORMAT NIL "~a~%" "outweights")
                      ELSE (ERROR "You must provide the multiple data set of weights as a file called outweights in your tree-project directory")))
            (T (FORMAT NIL "~a" ""))))
          )
    (bio::WITH-TEMP-FILE-IN 
        (phy-cmd tree-dir :name "phy_cmd" :delete? UTILS:*DELETE-TEMP-FILES*)
      (WITH-OPEN-FILE (phy_cmd phy-cmd :direction :output)
        (FORMAT phy_cmd "~a~%~a~a~a~a~a~a~a~a~a~a~a~a~a~a~%~a~%~a~a~a~a~a~a" 
                phyin utree utree-l tt-ratio b-freq sr-parameters
                rate-distribution rate-distribution-adj-s 
                r-weights analysis-speed rearrangements input-seq-o 
                m-data-sets m-data-weights 2 "Y" 
                number-rate-categories-in-hmm coef-var-subst-posits
                invariant-sites hmm-c-rates hmm-c-rates-prob r-m-data-weights))
      (LET ((phylip-command 
             (bio::FORMATN "~a/~a < ~a > ~a" 
                           cl-user::*phylip-executable-dir*
                           "dnaml" phy-cmd (format nil "~a" "dnaml.log"))))
        (bio::PROTECTED-SHELL-COMMAND phylip-command :directory tree-dir)
        tree-project)
      )))

(defun build-parsimony-tree-dna-aux
       (tree-project 
        user-tree thorough-search-best-tree 
        thorough-search-one-best-tree less-thorough-search-best-tree
        jumble-sequence-order threshold-parsimony transversion-parsimony
        read-weights-for-positions multiple-data-sets multiple-data-weights
        )
  (LET*  ((tree-dir (tree-directory tree-project))
          (phyin 
           (COND 
            (multiple-data-sets (FORMAT NIL "~a~a" tree-dir "outfile"))
            (T (FORMAT NIL "~a~a" tree-dir "alignment-phy"))))
          (search-opt 
           (COND
            (user-tree 
             (IF-TRUE (PROBE-FILE (FORMAT NIL "~a~a" tree-dir "intree"))
                      THEN (FORMAT NIL "~a~%" "U")
                      ELSE (ERROR "You must provide the user tree as a file called intree in your tree-project directory")))
            (thorough-search-best-tree (FORMAT NIL "~a" ""))
            (thorough-search-one-best-tree (FORMAT NIL "~a~%~a~%" "S" "Y"))
            (less-thorough-search-best-tree (FORMAT NIL "~a~%~a~%" "S" "N"))
            (T (FORMAT NIL "~a" ""))))
          (number-seed (+ (* 4 (RANDOM-INTEGER)) 1))
          (input-seq-o 
           (COND
            (jumble-sequence-order
             (FORMAT NIL "~a~%~a~%~a~%" "J" number-seed jumble-sequence-order))
            (T (FORMAT NIL "~a" ""))))
          (tparsimony
           (COND
            (threshold-parsimony
             (FORMAT NIL "~a~%~a~%" "T" threshold-parsimony))
            (T (FORMAT NIL "~a" ""))))
          (tvparsimony
           (COND
            (transversion-parsimony (FORMAT NIL "~a~%" "T"))
            (T (FORMAT NIL "~a" ""))))
          (r-weights
           (COND 
            (read-weights-for-positions 
             (IF-TRUE (PROBE-FILE (FORMAT NIL "~a~a" tree-dir "weights"))
                      THEN (FORMAT NIL "~a~%" "W")
                      ELSE (ERROR "You must provide the weights as a file called weights in your tree-project directory")))
            (T (FORMAT NIL "~a" ""))))
          (m-data-sets 
           (COND 
            (multiple-data-sets 
             (IF-TRUE jumble-sequence-order
                      THEN
                      (FORMAT NIL "~a~%~a~%~a~%~a~%~a~%" "F"
                              "outpars" "M" "D" multiple-data-sets)
                      ELSE 
                      (FORMAT NIL "~a~%~a~%~a~%~a~%~a~%~a~%~a~%~a~%"
                              "F" "outpars" "M" "D"
                              multiple-data-sets number-seed 1 "J")))
            (T (FORMAT NIL "~a" ""))))
          (m-data-weights 
           (COND 
            (multiple-data-weights 
             (IF-TRUE jumble-sequence-order
                      THEN (FORMAT NIL "~a~%~a~%~a~%" "M" "W" multiple-data-weights)
                      ELSE (FORMAT NIL "~a~%~a~%~a~%~a~%~a~%~a~%" "M" "W" multiple-data-weights number-seed 1 "J")))
            (T (FORMAT NIL "~a" ""))))
          (r-m-data-weights
           (COND 
            (multiple-data-weights 
             (IF-TRUE (PROBE-FILE (FORMAT NIL "~a~a" tree-dir "outweights"))
                      THEN (FORMAT NIL "~%~a" "outweights")
                      ELSE (ERROR "You must provide the multiple data set of weights as a file called outweights in your tree-project directory")))
            (T (FORMAT NIL "~a" ""))))
          )
    (bio::WITH-TEMP-FILE-IN 
        (phy-cmd tree-dir :name "phy_cmd" :delete? UTILS:*DELETE-TEMP-FILES*)
      (WITH-OPEN-FILE (phy_cmd phy-cmd :direction :output)
        (FORMAT phy_cmd "~a~%~a~a~a~a~a~a~a~a~%~a~a" 
                phyin search-opt input-seq-o tparsimony
                tvparsimony r-weights m-data-sets m-data-weights
                2 "Y" r-m-data-weights))
      (LET ((phylip-command 
             (bio::FORMATN "~a/~a < ~a > ~a" 
                           cl-user::*phylip-executable-dir*
                           "dnapars" phy-cmd (format nil "~a" "dnapars.log"))))
        (bio::PROTECTED-SHELL-COMMAND phylip-command :directory tree-dir)
        tree-project)
      )))

(defun build-maximum-likelihood-tree-protein-aux
       (tree-project
        user-tree user-tree-lengths JTT PMB PAM
        number-of-site-categories category-rates
        regional-gamma-distribution-rate
        regional-gamma-distribution-rate+invariant-sites
        regional-user-distribution-rate number-of-regional-rate-categories
        coefficient-of-variation-of-substitution-rate-among-positions
        fraction-of-invariant-sites regional-category-rates 
        probability-foreach-regional-category-rate
        correlation-of-rate-distribution-of-adjacent-sites
        read-weights-for-positions speedier-analysis global-rearrangements
        jumble-sequence-order multiple-data-sets multiple-data-weights 
        )

  (LET*  ((tree-dir (tree-directory tree-project))
          (phyin 
           (COND 
            (multiple-data-sets (FORMAT NIL "~a~a" tree-dir "outfile"))
            (T (FORMAT NIL "~a~a" tree-dir "alignment-phy"))))
          (utree 
           (COND
            (user-tree
             (IF-TRUE (PROBE-FILE (FORMAT NIL "~a~a" tree-dir "intree"))
                      THEN (FORMAT NIL "~a~%" "U")
                      ELSE (ERROR "You must provide the user tree as a file called intree in your tree-project directory")))
            (T (FORMAT NIL "~a" ""))))
          (utree-l 
           (COND 
            (user-tree-lengths
             (IF-TRUE user-tree
                      THEN (FORMAT NIL "~a~%" "L")
                      ELSE (ERROR "This option is only available with the option user-tree")))
            (T (FORMAT NIL "~a" ""))))
          (prob-model
           (COND 
            (JTT (FORMAT NIL "~a" ""))
            (PMB (FORMAT NIL "~a~%" "P"))
            (PAM (FORMAT NIL "~a~%~a~%" "P" "P"))
            (T (FORMAT NIL "~a" ""))))
; categories for sites defined by the user
          (src-nb 
           (COND 
            (number-of-site-categories
             (FORMAT NIL "~a~%~a~%" "C" number-of-site-categories))
            (T (FORMAT NIL "~a" ""))))
          (sc-rates 
           (COND 
            (category-rates (FORMAT NIL "~a~%" category-rates))
            (T (FORMAT NIL "~a" ""))))
          (sr-parameters 
           (COND 
            ((ALL-TRUE (list (SAME src-nb "") (SAME sc-rates "")))
             (FORMAT NIL "~a" ""))
            ((ANY-TRUE (list (SAME src-nb "") (SAME sc-rates "")))
             (ERROR ".."))
            ((ALL-FALSE (list (SAME src-nb "") (SAME sc-rates ""))) 
             (COND 
              ((= (LENGTH-OF (SPLIT category-rates EVERY " ")) 
                  number-of-site-categories)
               (IF-TRUE 
                (PROBE-FILE (FORMAT NIL "~a~a" tree-dir "categories"))  
                THEN (FORMAT NIL "~a~a" src-nb sc-rates)
                ELSE (ERROR "You must provide a file called categories in your tree-project directory")))
              ((NOT (= (LENGTH-OF (SPLIT category-rates EVERY " "))
                       number-of-site-categories))
               (ERROR "You must provide as many rates than the number of categories defined"))))
            ))
          ;; reginal categories (Hidden Markov Model) rates folowing
          ;; a gamma distribution or + invariant sites or defined by the user 
          (rate-distribution
           (COND 
            (regional-gamma-distribution-rate (FORMAT NIL "~a~%" "R"))
            (regional-gamma-distribution-rate+invariant-sites
             (FORMAT NIL "~a~%~a~%" "R" "R"))
            (regional-user-distribution-rate
             (FORMAT NIL "~a~%~a~%~a~%" "R" "R" "R"))
            (T (FORMAT NIL "~a" ""))))
          (number-rate-categories-in-hmm
           (COND 
            (number-of-regional-rate-categories
             (IF-TRUE 
              (AND 
               (OR regional-gamma-distribution-rate 
                   regional-gamma-distribution-rate+invariant-sites)  
               coefficient-of-variation-of-substitution-rate-among-positions)
              ;; won't work => ""
              THEN (FORMAT NIL "~a~%" number-of-regional-rate-categories)
              ELSE (ERROR "This parameter can only be adjusted if you chosen a gamma distribution")))
            (T (FORMAT NIL "~a" ""))))
          ;; if gamma distribution rates & gamma + isites distribution rates
          (coef-var-subst-posits
           (COND 
            (coefficient-of-variation-of-substitution-rate-among-positions
             (IF-TRUE
              (OR regional-gamma-distribution-rate
                  regional-gamma-distribution-rate+invariant-sites ) 
              ;; won't work => ""
              THEN 
              (FORMAT 
               NIL "~a~%"
               coefficient-of-variation-of-substitution-rate-among-positions)
              ELSE
              (ERROR "This parameter can only be adjusted if you had chosen a gamma distribution")))
            (T (FORMAT NIL "~a" ""))))
          ;; if gamma + isites distribution rates
          (invariant-sites
           (COND 
            (fraction-of-invariant-sites
             (IF-TRUE
              (AND regional-gamma-distribution-rate+invariant-sites
                   coefficient-of-variation-of-substitution-rate-among-positions) 
              ;; won't work => ""
              THEN (FORMAT NIL "~a~%" fraction-of-invariant-sites)
              ELSE (ERROR "This parameter can only be adjusted if you had chosen a gamma distribution with ivariant sites")))
            (T (FORMAT NIL "~a" ""))))
          ;; if user distribution rates
          (hmm-c-rates 
           (COND 
            (regional-category-rates
             (IF-TRUE
              (AND regional-user-distribution-rate
                   number-of-regional-rate-categories) 
              ;; won't work => ""
              THEN (FORMAT NIL "~a~%" regional-category-rates)
              ELSE  (ERROR "This parameter can only be adjusted if you had chosen a user distribution")))
            (T (FORMAT NIL "~a" ""))))
          (hmm-c-rates-prob 
           (COND 
            (probability-foreach-regional-category-rate
             (FORMAT NIL "~a~%" probability-foreach-regional-category-rate))
            (T (FORMAT NIL "~a" ""))))
          ;; distribution rates
          (rate-distribution-adj-s 
           (COND
            (correlation-of-rate-distribution-of-adjacent-sites
             (FORMAT NIL "~a~%" "A"))
            (T (FORMAT NIL "~a" ""))))
          ;; ------
          (r-weights 
           (COND 
            (read-weights-for-positions
             (IF-TRUE (PROBE-FILE (FORMAT NIL "~a~a" tree-dir "weights"))
                      THEN (FORMAT NIL "~a~%" "W")
                      ELSE (ERROR "You must provide the weights as a file called weights in your tree-project directory")))
            (T (FORMAT NIL "~a" ""))))
          (analysis-speed 
           (COND
            (speedier-analysis (FORMAT NIL "~a~%" "S"))
            (T (FORMAT NIL "~a" ""))))
          (rearrangements 
           (COND
            (global-rearrangements (FORMAT NIL "~a~%" "G"))
            (T (FORMAT NIL "~a" ""))))
          (number-seed (+ (* 4 (RANDOM-INTEGER)) 1))
          (input-seq-o 
           (COND
            (jumble-sequence-order
             (FORMAT NIL "~a~%~a~%~a~%" "J"
                     number-seed jumble-sequence-order))
            (T (FORMAT NIL "~a" ""))))
          (m-data-sets 
           (COND 
            (multiple-data-sets 
             (IF-TRUE jumble-sequence-order
                      THEN
                      (FORMAT NIL "~a~%~a~%~a~%~a~%~a~%"
                              "F" "outpars" "M" "D" multiple-data-sets)
                      ELSE 
                      (FORMAT NIL "~a~%~a~%~a~%~a~%~a~%~a~%~a~%~a~%" 
                              "F" "outpars" "M" "D"
                              multiple-data-sets number-seed 1 "J")))
            (T (FORMAT NIL "~a" ""))))
          (m-data-weights 
           (COND 
            (multiple-data-weights 
             (IF-TRUE jumble-sequence-order
                      THEN 
                      (FORMAT NIL "~a~%~a~%~a~%" "M" "W"
                              multiple-data-weights)
                      ELSE 
                      (FORMAT NIL "~a~%~a~%~a~%~a~%~a~%~a~%" "M" "W"
                              multiple-data-weights number-seed 1 "J")))
            (T (FORMAT NIL "~a" ""))))
          (r-m-data-weights 
           (COND 
            (multiple-data-weights
             (IF-TRUE (PROBE-FILE (FORMAT NIL "~a~a" tree-dir "outweights"))
                      THEN (FORMAT NIL "~a~%" "outweights")
                      ELSE (ERROR "You must provide the multiple data set of weights as a file called outweights in your tree-project directory")))
            (T (FORMAT NIL "~a" ""))))
          )

    (bio::WITH-TEMP-FILE-IN 
        (phy-cmd tree-dir :name "phy_cmd" :delete? UTILS:*DELETE-TEMP-FILES*)
      (WITH-OPEN-FILE (phy_cmd phy-cmd :direction :output)
        (FORMAT phy_cmd "~a~%~a~a~a~a~a~a~a~a~a~a~a~a~a~%~a~%~a~a~a~a~a~a" 
                phyin utree utree-l prob-model sr-parameters
                rate-distribution rate-distribution-adj-s r-weights
                analysis-speed rearrangements input-seq-o m-data-sets
                m-data-weights 2 "Y" number-rate-categories-in-hmm 
                coef-var-subst-posits invariant-sites hmm-c-rates
                hmm-c-rates-prob r-m-data-weights))
      (LET ((phylip-command 
             (bio::FORMATN "~a/~a < ~a > ~a" 
                           cl-user::*phylip-executable-dir*
                           "proml" phy-cmd (format nil "~a" "proml.log"))))
        (bio::PROTECTED-SHELL-COMMAND phylip-command :directory tree-dir)
        tree-project)
      )))




(defun build-parsimony-tree-protein-aux 
       (tree-project 
        user-tree jumble-sequence-order threshold-parsimony
        genetic-code read-weights-for-positions multiple-data-sets
        multiple-data-weights
        )
  (LET*  ((tree-dir (tree-directory tree-project))
          (phyin 
           (COND 
            (multiple-data-sets (FORMAT NIL "~a~a" tree-dir "outfile"))
            (T (FORMAT NIL "~a~a" tree-dir "alignment-phy"))))
          
          (utree 
           (COND
            (user-tree 
             (IF-TRUE (PROBE-FILE (FORMAT NIL "~a~a" tree-dir "intree"))
                      THEN (FORMAT NIL "~a~%" "U")
                      ELSE (ERROR "You must provide the user tree as a file called intree in your tree-project directory")))
            (T (FORMAT NIL "~a" ""))))
          (number-seed (+ (* 4 (RANDOM-INTEGER)) 1))
          (input-seq-o 
           (COND
            (jumble-sequence-order 
             (FORMAT NIL "~a~%~a~%~a~%" "J" number-seed jumble-sequence-order))
            (T (FORMAT NIL "~a" ""))))
          (tparsimony 
           (COND
            (threshold-parsimony
             (FORMAT NIL "~a~%~a~%" "T" threshold-parsimony))
            (T (FORMAT NIL "~a" ""))))
          (g-code
           (COND 
            (genetic-code 
             (FOR-EACH item IN '("U" "M" "V" "F" "Y")
                       INITIALIZE codes = NIL 
                       DO (PUSH (EQUAL item genetic-code) codes)
                       FINALLY 
                       (IF-TRUE (ALL-FALSE codes) 
                                THEN (ERROR "You can use as valid genetic codes: \"U\" for Universal; \"M\" for Mitochondrial; \"V\" for Vertebrate mitochondrial; \"F\" for Fly mitochondrial; \"Y\" for Yeast mitochondrial") 
                                ELSE (RETURN (FORMAT NIL "~a~%~a~%" "U" genetic-code)))))
            (T (FORMAT NIL "~a" ""))))
          (r-weights 
           (COND 
            (read-weights-for-positions 
             (IF-TRUE (PROBE-FILE (FORMAT NIL "~a~a" tree-dir "weights"))
                      THEN (FORMAT NIL "~a~%" "W")
                      ELSE (ERROR "You must provide the weights as a file called weights in your tree-project directory")))
            (T (FORMAT NIL "~a" ""))))
          (m-data-sets 
           (COND 
            (multiple-data-sets 
             (IF-TRUE jumble-sequence-order
                      THEN 
                      (FORMAT NIL "~a~%~a~%~a~%~a~%~a~%" "F" "outpars" "M" "D"
                              multiple-data-sets)
                      ELSE (FORMAT NIL "~a~%~a~%~a~%~a~%~a~%~a~%~a~%~a~%" 
                                   "F" "outpars" "M" "D"
                                   multiple-data-sets number-seed 1 "J")))
            (T (FORMAT NIL "~a" ""))))
          (m-data-weights 
           (COND 
            (multiple-data-weights 
             (IF-TRUE jumble-sequence-order
                      THEN 
                      (FORMAT NIL "~a~%~a~%~a~%" "M" "W" multiple-data-weights)
                      ELSE 
                      (FORMAT NIL "~a~%~a~%~a~%~a~%~a~%~a~%" "M" "W"
                              multiple-data-weights number-seed 1 "J")))
            (T (FORMAT NIL "~a" ""))))
          (r-m-data-weights 
           (COND 
            (multiple-data-weights 
             (IF-TRUE (PROBE-FILE (FORMAT NIL "~a~a" tree-dir "outweights"))
                      THEN (FORMAT NIL "~%~a" "outweights")
                      ELSE (ERROR "You must provide the multiple data set of weights as a file called outweights in your tree-project directory")))
            (T (FORMAT NIL "~a" ""))))
          )
    (bio::WITH-TEMP-FILE-IN 
        (phy-cmd tree-dir :name "phy_cmd" :delete? UTILS:*DELETE-TEMP-FILES*)
      (WITH-OPEN-FILE (phy_cmd phy-cmd :direction :output)
        (FORMAT phy_cmd "~a~%~a~a~a~a~a~a~a~a~%~a~a" 
                phyin utree input-seq-o tparsimony g-code r-weights
                m-data-sets m-data-weights 2 "Y" r-m-data-weights))
      (LET ((phylip-command 
             (bio::FORMATN "~a/~a < ~a > ~a" 
                           cl-user::*phylip-executable-dir*
                           "protpars" phy-cmd (format nil "~a" "protpars.log")
                           )))
        (bio::PROTECTED-SHELL-COMMAND phylip-command :directory tree-dir)
        tree-project)
      )))

(defun build-distance-tree-dna-aux
       (tree-project
        F84 Kimura Jukes-Cantor LogDet Similarity gamma
        transition-transversion-ratio substitution-rate-category-number 
        category-rates weights-for-sites base-frequencies multiple-data-sets
        multiple-data-weights matrix neighbor upgma subreplicate
        randomize-input-order)

  (CHECK-FLAGS-AND-KEYWORDS 
    '(F84 Kimura Jukes-Cantor LogDet Similarity)  
    (list F84 Kimura Jukes-Cantor LogDet Similarity)
    '(gamma transition-transversion-ratio substitution-rate-category-number 
            category-rates weights-for-sites base-frequencies 
            multiple-data-sets multiple-data-weights)
    (list gamma transition-transversion-ratio 
          substitution-rate-category-number
          category-rates weights-for-sites base-frequencies
          multiple-data-sets multiple-data-weights)
    '((T T T T T T T T) 
      (T T T T T NIL T T)
      (T NIL T T T NIL T T) 
      (NIL NIL NIL NIL T NIL T T) 
      (NIL NIL NIL NIL T NIL T T)))

  (LET*  ((tree-dir (tree-directory tree-project))
          (phyin 
           (COND 
            (multiple-data-sets (FORMAT NIL "~a~a" tree-dir "outfile"))
            (T (FORMAT NIL "~a~a" tree-dir "alignment-phy"))))
          (dist-model 
           (COND 
            (F84 (FORMAT NIL "~a" ""))
            (Kimura (FORMAT NIL "~a~%" "D"))
            (Jukes-Cantor (FORMAT NIL "~a~%~a~%" "D" "D"))
            (LogDet (FORMAT NIL "~a~%~a~%~a~%" "D" "D" "D"))
            (Similarity (FORMAT NIL "~a~%~a~%~a~%~a~%" "D" "D" "D" "D"))
            (T (FORMAT NIL "~a" ""))))
          (gamma-model 
           (COND 
            (gamma (FORMAT NIL "~a~%~a~%" "G" gamma))
            (T (FORMAT NIL "~a" ""))))
          (tt-ratio 
           (COND 
            (transition-transversion-ratio
             (FORMAT NIL "~a~%~a~%" "T" transition-transversion-ratio))
            (T (FORMAT NIL "~a" ""))))
          (src-nb 
           (COND 
            (substitution-rate-category-number
             (FORMAT NIL "~a~%~a~%" "C" substitution-rate-category-number))
            (T (FORMAT NIL "~a" ""))))
          (sc-rates 
           (COND 
            (category-rates (FORMAT NIL "~a~%" category-rates))
            (T (FORMAT NIL "~a" ""))))
          (sr-parameters 
           (COND 
            ((ALL-TRUE (list (SAME src-nb "") (SAME sc-rates "")))
             (FORMAT NIL "~a" ""))
            ((ANY-TRUE (list (SAME src-nb "") (SAME sc-rates "")))
             (ERROR ".."))
            ((ALL-FALSE (list (SAME src-nb "") (SAME sc-rates "")))
             (COND 
              ((= (LENGTH-OF (SPLIT category-rates EVERY " ")) 
                  substitution-rate-category-number)
               (IF-TRUE 
                (PROBE-FILE (FORMAT NIL "~a~a" tree-dir "categories"))  
                THEN (FORMAT NIL "~a~a" src-nb sc-rates)
                ELSE (ERROR "You must provide a file called categories in your tree-project directory")))
              ((NOT (= (LENGTH-OF (SPLIT category-rates EVERY " "))
                       substitution-rate-category-number))
               (ERROR "You must provide as many rates than the number of categories defined"))))
            ))
          (weights 
           (COND 
            (weights-for-sites 
             (IF-TRUE (PROBE-FILE (FORMAT NIL "~a~a" tree-dir "weights"))
                      THEN (FORMAT NIL "~a~%" "W")
                      ELSE (ERROR "You must provide the weights as a file called weights in your tree-project directory")))
            (T (FORMAT NIL "~a" ""))))
          (base-freq 
           (COND 
            (base-frequencies (FORMAT NIL "~a~%~a~%" "F" base-frequencies))
            (T (FORMAT NIL "~a" ""))))
          (m-data-sets 
           (COND 
            (multiple-data-sets
             (FORMAT NIL "~a~%~a~%~a~%~a~%~a~%" "F" "outdist" "M" "D"
                     multiple-data-sets))
            (T (FORMAT NIL "~a" ""))))
          (m-data-weights 
           (COND 
            (multiple-data-weights 
             (FORMAT NIL "~a~%~a~%~a~%" "M" "W" multiple-data-weights))
            (T (FORMAT NIL "~a" ""))))
          (r-m-data-weights
           (COND 
            (multiple-data-weights 
             (IF-TRUE (PROBE-FILE (FORMAT NIL "~a~a" tree-dir "outweights"))
                      THEN (FORMAT NIL "~%~a" "outweights")
                      ELSE (ERROR "You must provide the multiple data set of weights as a file called outweights in your tree-project directory")))
            (T (FORMAT NIL "~a" ""))))
          )
      
    (bio::WITH-TEMP-FILE-IN 
        (cmddnadistfile tree-dir :name "dnadistcmd"
                        :delete? UTILS:*DELETE-TEMP-FILES*)
      (WITH-OPEN-FILE (dnadistcmd cmddnadistfile :direction :output)
        (FORMAT dnadistcmd "~a~%~a~a~a~a~a~a~a~a~a~%~a~a" 
                phyin dist-model gamma-model tt-ratio sr-parameters
                weights base-freq m-data-sets m-data-weights
                2 "Y" r-m-data-weights)
        )
      (LET ((phylip-command 
             (FORMAT NIL "~a/~a < ~a > ~a" 
                     cl-user::*phylip-executable-dir*
                     "dnadist" cmddnadistfile "dnadist.log")))
        (bio::PROTECTED-SHELL-COMMAND phylip-command :directory tree-dir)
        (IF-TRUE
         matrix 
         THEN tree-project
         ELSE
         (IF multiple-data-weights 
             (SETF multiple-data-sets multiple-data-weights))
         (NEIGHBOR tree-project :neighbor neighbor
                   :upgma upgma :subreplicate subreplicate 
                   :randomize-input-order randomize-input-order
                   :multiple-data-sets multiple-data-sets))
        tree-project)
      )))

(defun build-distance-tree-protein-aux 
       (tree-project 
        JTT PMB PAM Kimura Similarity Categories gamma
        coefficient-of-variation-of-substitution-rate-among-positions 
        substitution-rate-category-number category-rates 
        read-weights-for-positions
        genetic-code aa-categorization probality-to-change-of-category
        transition-transversion-ratio base-frequencies multiple-data-sets
        multiple-data-weights matrix neighbor upgma subreplicate
        randomize-input-order)
  
  (CHECK-FLAGS-AND-KEYWORDS 
    '(JTT PMB PAM Kimura Similarity Categories) 
    (list JTT PMB PAM Kimura Similarity Categories)
    '(gamma coefficient-of-variation-of-substitution-rate-among-positions 
            substitution-rate-category-number category-rates
            read-weights-for-positions 
            genetic-code aa-categorization probality-to-change-of-category 
            transition-transversion-ratio base-frequencies
            multiple-data-sets multiple-data-weights)
    (list gamma coefficient-of-variation-of-substitution-rate-among-positions 
          substitution-rate-category-number category-rates 
          read-weights-for-positions 
          genetic-code aa-categorization probality-to-change-of-category 
          transition-transversion-ratio base-frequencies multiple-data-sets
          multiple-data-weights)
    '((T T T T T NIL NIL NIL NIL NIL T T)
      (T T T T T NIL NIL NIL NIL NIL T T)
      (T T T T T NIL NIL NIL NIL NIL T T)
      (NIL NIL T T T NIL NIL NIL NIL NIL T T)
      (NIL NIL T T T NIL NIL NIL NIL NIL T T)
      (T T T T T T T T T T NIL NIL)))

  (let* ((tree-dir (tree-directory tree-project))
         (phyin 
          (COND 
           (multiple-data-sets (FORMAT NIL "~a~a" tree-dir "outfile"))
           (T (FORMAT NIL "~a~a" tree-dir "alignment-phy"))))
         (dist-model 
          (COND 
           (JTT (FORMAT NIL "~a" ""))
           (PMB (FORMAT NIL "~a~%" "P"))
           (PAM (FORMAT NIL "~a~%~a~%" "P" "P"))
           (Kimura (FORMAT NIL "~a~%~a~%~a~%" "P" "P" "P"))
           (Similarity (FORMAT NIL "~a~%~a~%~a~%~a~%" "P" "P" "P" "P"))
           (Categories 
            (FORMAT NIL "~a~%~a~%~a~%~a~%~a~%" "P" "P" "P" "P" "P"))
           (T (FORMAT NIL "~a" ""))))
         (gamma-model 
          (COND 
           (gamma
            (IF-TRUE 
             coefficient-of-variation-of-substitution-rate-among-positions 
             THEN  (FORMAT NIL "~a~%" "G")
             ELSE  (ERROR "If you have chosen a gamma distribution, you must provide the coefficient of variation of substitution rate among positions that corresponds to  1/(square root of alpha).")))
           (T (FORMAT NIL "~a" ""))))
         (coef-var-subst-posits
          (COND 
           (coefficient-of-variation-of-substitution-rate-among-positions 
            (IF-TRUE gamma 
                     THEN (FORMAT NIL "~%~a" coefficient-of-variation-of-substitution-rate-among-positions)
                     ELSE (ERROR "This parameter can only be adjusted if you chosen a gamma distribution")))
           (T (FORMAT NIL "~a" ""))))
         (src-nb 
          (COND 
           (substitution-rate-category-number
            (FORMAT NIL "~a~%~a~%" "C" substitution-rate-category-number))
           (T (FORMAT NIL "~a" ""))))
         (sc-rates 
          (COND 
           (category-rates (FORMAT NIL "~a~%" category-rates))
           (T (FORMAT NIL "~a" ""))))
         (sr-parameters 
          (COND 
           ((ALL-TRUE (list (SAME src-nb "") (SAME sc-rates "")))
            (FORMAT NIL "~a" ""))
           ((ANY-TRUE (list (SAME src-nb "") (SAME sc-rates "")))
            (ERROR ".."))
           ((ALL-FALSE (list (SAME src-nb "") (SAME sc-rates ""))) 
            (COND 
             ((= (LENGTH-OF (SPLIT category-rates EVERY " ")) 
                 substitution-rate-category-number)
              (IF-TRUE (PROBE-FILE (FORMAT NIL "~a~a" tree-dir "categories")) 
                       THEN (FORMAT NIL "~a~a" src-nb sc-rates)
                       ELSE (ERROR "You must provide a file called categories in your tree-project directory")))
             ((NOT (= (LENGTH-OF (SPLIT category-rates EVERY " "))
                      substitution-rate-category-number))
              (ERROR "You must provide as many rates than the number of categories defined"))))))
         (r-weights 
          (COND 
           (read-weights-for-positions 
            (IF-TRUE (PROBE-FILE (FORMAT NIL "~a~a" tree-dir "weights"))
                     THEN (FORMAT NIL "~a~%" "W")
                     ELSE (ERROR "You must provide the weights as a file called weights in your tree-project directory")))
           (T (FORMAT NIL "~a" ""))))
         (m-data-sets 
          (COND 
           (multiple-data-sets
            (FORMAT NIL "~a~%~a~%~a~%~a~%~a~%" "F" "outdist" "M" "D"
                    multiple-data-sets))
           (T (FORMAT NIL "~a" ""))))
         (m-data-weights 
          (COND 
           (multiple-data-weights 
            (FORMAT NIL "~a~%~a~%~a~%" "M" "W" multiple-data-weights))
           (T (FORMAT NIL "~a" ""))))
         (r-m-data-weights
          (COND 
           (multiple-data-weights 
            (IF-TRUE (PROBE-FILE (FORMAT NIL "~a~a" tree-dir "outweights"))
                     THEN (FORMAT NIL "~%~a" "outweights")
                     ELSE (ERROR "You must provide the multiple data set of weights as a file called outweights in your tree-project directory")))
           (T (FORMAT NIL "~a" ""))))
         (g-code 
          (COND 
           (genetic-code 
            (FOR-EACH 
             item IN '("U" "M" "V" "F" "Y")
             INITIALIZE codes = NIL 
             DO (PUSH (EQUAL item genetic-code) codes)
             FINALLY 
             (IF-TRUE 
              (ALL-FALSE codes) 
              THEN (ERROR "You can use as valid genetic codes: \"U\" for Universal; \"M\" for Mitochondrial; \"V\" for Vertebrate mitochondrial; \"F\" for Fly mitochondrial; \"Y\" for Yeast mitochondrial") 
              ELSE (RETURN (FORMAT NIL "~a~%~a~%" "U" genetic-code)))))
           (T (FORMAT NIL "~a" ""))))
         (aa-cat 
          (COND 
           (aa-categorization 
            (FOR-EACH 
             item IN '("C" "H" "G")
             INITIALIZE categorizations = NIL 
             DO (PUSH (EQUAL item aa-categorization) categorizations)
             FINALLY 
             (IF-TRUE 
              (ALL-FALSE categorizations) 
              THEN (ERROR "You can use as valid amino-acid categorization: \"C\" for Chemical; \"H\" for Hall; \"G\" for George/Hunt/Barker") 
              ELSE (RETURN (FORMAT NIL "~a~%~a~%" "A" aa-categorization)))))
           (T (FORMAT NIL "~a" ""))))
         (pchange-cat 
          (COND 
           (probality-to-change-of-category 
            (IF-TRUE 
             (AND (>= Probality-to-change-of-category 0)
                  (<= probality-to-change-of-category 1))
             THEN (FORMAT NIL "~a~%~a~%" "E" probality-to-change-of-category)
             ELSE (ERROR "The probability value must be between 0 and 1")))
           (T (FORMAT NIL "~a" ""))))
         (tt-ratio 
          (COND 
           (transition-transversion-ratio
            (FORMAT NIL "~a~%~a~%" "T" transition-transversion-ratio))
           (T (FORMAT NIL "~a" ""))))
         (base-freq 
          (COND 
           (base-frequencies (FORMAT NIL "~a~%~a~%" "F" base-frequencies))
           (T (FORMAT NIL "~a" ""))))
         )

    (bio::WITH-TEMP-FILE-IN 
        (cmdprotdistfile tree-dir :name "protdistcmd" 
                         :delete? UTILS:*DELETE-TEMP-FILES*)
      (WITH-OPEN-FILE (protdistcmd cmdprotdistfile :direction :output)
        (FORMAT protdistcmd "~a~%~a~a~a~a~a~a~a~a~a~a~a~a~%~a~a~a" 
                phyin m-data-sets m-data-weights dist-model gamma-model
                sr-parameters r-weights g-code aa-cat pchange-cat
                tt-ratio base-freq 2 "Y" coef-var-subst-posits
                r-m-data-weights)
        )
      (LET ((phylip-command 
             (FORMAT NIL "~a/~a < ~a > ~a" 
                     cl-user::*phylip-executable-dir*
                     "protdist" cmdprotdistfile "protdist.log")))
        (bio::PROTECTED-SHELL-COMMAND phylip-command :directory tree-dir)
        (IF-TRUE matrix 
                 THEN tree-project
                 ELSE
                 (IF multiple-data-weights 
                     (SETF multiple-data-sets multiple-data-weights))
                 (NEIGHBOR tree-project :neighbor neighbor :upgma upgma
                           :subreplicate subreplicate 
                           :randomize-input-order randomize-input-order
                           :multiple-data-sets multiple-data-sets))
        tree-project)
      )))

(defun resample-alignment-aux
       (tree-project
        molecular-sequences discrete-morphology restriction-sites 
        gene-frequencies bootstrap jackknife permute-species-for-each-character
        permute-character-order permute-within-species rewrite-data
        altered-sampling-fraction block-size number-of-replicates 
        read-weights read-categories-for-sites write-weights 
        read-factors-information read-mixture-file read-ancestors-file
        enzyme-number-written-in-input-file alleles-present-at-each-locus)

  (CHECK-FLAGS-AND-KEYWORDS
    '(molecular-sequences discrete-morphology restriction-sites
                          gene-frequencies)
    (list molecular-sequences discrete-morphology
          restriction-sites gene-frequencies)
    '(altered-sampling-fraction 
      block-size number-of-replicates 
      read-weights read-categories-for-sites write-weights 
      read-factors-information read-mixture-file read-ancestors-file 
      enzyme-number-written-in-input-file alleles-present-at-each-locus)
    (list altered-sampling-fraction block-size number-of-replicates 
          read-weights read-categories-for-sites write-weights 
          read-factors-information read-mixture-file read-ancestors-file 
          enzyme-number-written-in-input-file alleles-present-at-each-locus)
    '((T T T T T T NIL NIL NIL NIL NIL)
      (T T T T NIL T T T T NIL NIL)
      (T T T T NIL T NIL NIL NIL T NIL)
      (T T T T NIL T NIL NIL NIL NIL T)))

  (CHECK-FLAGS-AND-KEYWORDS
    '(bootstrap jackknife permute-species-for-each-character 
                permute-character-order permute-within-species rewrite-data)
    (list bootstrap jackknife permute-species-for-each-character 
          permute-character-order permute-within-species rewrite-data)
    '(altered-sampling-fraction 
      block-size number-of-replicates 
      read-weights read-categories-for-sites write-weights 
      read-factors-information read-mixture-file read-ancestors-file 
      enzyme-number-written-in-input-file alleles-present-at-each-locus)
    (list altered-sampling-fraction block-size number-of-replicates 
          read-weights read-categories-for-sites write-weights 
          read-factors-information read-mixture-file read-ancestors-file 
          enzyme-number-written-in-input-file alleles-present-at-each-locus)
    '((T T T T T T T T T T T)
      (T NIL T T T T T T T T T)
      (NIL NIL T T T NIL T T T T T)
      (NIL NIL T NIL NIL NIL T NIL NIL T T)
      (NIL NIL T NIL NIL NIL T NIL NIL T T)
      (NIL NIL NIL NIL NIL NIL NIL NIL NIL T T)))
  
  (let*  ((tree-dir (tree-directory tree-project))
          (phyin (FORMAT NIL "~a~a" tree-dir "alignment-phy"))
          (data-type 
           (COND 
            (molecular-sequences (FORMAT NIL "~a" ""))
            (discrete-morphology (FORMAT NIL "~a~%" "D"))
            (restriction-sites (FORMAT NIL "~a~%~a~%" "D" "D"))
            (gene-frequencies (FORMAT NIL "~a~%~a~%~a~%" "D" "D" "D"))
            (T (FORMAT NIL "~a" ""))))
          (sampling-method 
           (COND 
            (bootstrap (FORMAT NIL "~a" ""))
            (jackknife (FORMAT NIL "~a~%" "J"))
            (permute-species-for-each-character 
             (FORMAT NIL "~a~%~a~%" "J" "J"))
            (permute-character-order (FORMAT NIL "~a~%~a~%~a~%" "J" "J" "J"))
            (permute-within-species 
             (FORMAT NIL "~a~%~a~%~a~%~a~%" "J" "J" "J" "J"))
            (rewrite-data 
             (FORMAT NIL "~a~%~a~%~a~%~a~%~a~%" "J" "J" "J" "J" "J"))
            (T (FORMAT NIL "~a" ""))))
          (altered-fraction 
           (COND 
            (altered-sampling-fraction 
             (FORMAT NIL "~a~%~a~%" "%" altered-sampling-fraction))
            (T (FORMAT NIL "~a" ""))))
          (bl-size 
           (COND 
            (block-size (FORMAT NIL "~a~%~a~%" "B" block-size))
            (T (FORMAT NIL "~a" ""))))
          (nb-replicates 
           (COND 
            (number-of-replicates 
             (FORMAT NIL "~a~%~a~%" "R" number-of-replicates))
            (T (FORMAT NIL "~a" ""))))
          (r-weights 
           (resample-data-type-if-file read-weights tree-dir "weights" "W"))
          (r-categories 
           (resample-data-type-if-file 
            read-categories-for-sites tree-dir "categories" "C"))
          (w-weights 
           (COND 
            (write-weights (FORMAT NIL "~a~%" "S"))
            (T (FORMAT NIL "~a" ""))))
          (r-factors-information 
           (resample-data-type-if-file 
            read-factors-information tree-dir "factors" "F"))
          (r-mixture 
           (resample-data-type-if-file 
            read-mixture-file tree-dir "mixture" "X"))
          (r-ancestors 
           (resample-data-type-if-file 
            read-ancestors-file tree-dir "ancestors" "N"))
          (enz-nb 
           (COND 
            (enzyme-number-written-in-input-file (FORMAT NIL "~a~%" "E"))
            (T (FORMAT NIL "~a" ""))))
          (alleles 
           (COND 
            (alleles-present-at-each-locus (FORMAT NIL "~a~%" "A"))
            (T (FORMAT NIL "~a" ""))))
          (number-seed (+ (* 4 (RANDOM-INTEGER)) 1))
          )
    (bio::WITH-TEMP-FILE-IN 
        (phy-cmd tree-dir :name "phy_cmd" :delete? UTILS:*DELETE-TEMP-FILES*)
      (WITH-OPEN-FILE (phy_cmd phy-cmd :direction :output)
        (format
         phy_cmd "~a~%~a~a~a~a~a~a~a~a~a~%~a~%~a~%~a~a~a~a~a"
         phyin data-type sampling-method altered-fraction
         bl-size nb-replicates w-weights enz-nb alleles 2 "Y" number-seed 
         r-weights r-categories r-factors-information 
         r-mixture r-ancestors)) 
      (LET ((phylip-command 
             (bio::FORMATN "~a/~a < ~a > ~a" 
                           cl-user::*phylip-executable-dir*
                           "seqboot" phy-cmd "seqboot.log"))) 
        (bio::PROTECTED-SHELL-COMMAND phylip-command :directory tree-dir)
        tree-project)
      )))

(defun tree-of-aux 
       (given-alignment tree-project distance maximum-likelihood parsimony 
                  DNA AA midpoint-root unrooted overwrite pdf png jpg eps
                  newick bootstrap outgroup WIDTH seq-sample from to)
  (LET ((alignment
          (LOOP FOR (label seq) IN given-alignment
                WITH start = (UNLESS-PROVIDED from 1)
                WITH end = (PROVIDED to)
                AS new-seq 
                  = (IF to
                        (SUBSEQ seq start end)
                        (SUBSEQ seq start))
                COLLECT (LIST label new-seq))))
  (COND 
   (overwrite (CREATE-A-TREE-PROJECT-FROM  alignment tree-project overwrite))
   (T (CREATE-A-TREE-PROJECT-FROM  alignment tree-project)))

  (IF bootstrap 
      (RESAMPLE-ALIGNMENT tree-project number-of-replicates bootstrap))
  (COND 
   ((OR (IS-DNA-SEQUENCE? seq-sample) DNA)
    (COND 
     (distance
      (BUILD-DISTANCE-TREE-DNA tree-project MULTIPLE-DATA-SETS bootstrap))
     (maximum-likelihood
      (BUILD-maximum-likelihood-TREE-DNA tree-project MULTIPLE-DATA-SETS NIL))
     (parsimony 
      (BUILD-parsimony-TREE-DNA tree-project MULTIPLE-DATA-SETS bootstrap))
     (T 
      (BUILD-DISTANCE-TREE-DNA tree-project MULTIPLE-DATA-SETS bootstrap)
      )))
   ((OR (IS-PROTEIN-SEQUENCE? seq-sample) AA)
    (COND 
     (distance 
      (BUILD-DISTANCE-TREE-PROTEIN tree-project MULTIPLE-DATA-SETS bootstrap))
     (maximum-likelihood 
      (BUILD-maximum-likelihood-TREE-PROTEIN 
        tree-project MULTIPLE-DATA-SETS NIL))
     (parsimony
      (BUILD-parsimony-TREE-PROTEIN tree-project MULTIPLE-DATA-SETS bootstrap))
     (T 
      (BUILD-DISTANCE-TREE-PROTEIN tree-project MULTIPLE-DATA-SETS bootstrap)
      )))
   (T 
    (ERROR "BioBIKE cannot recognize if you provided an alignment of AA or DNA sequence, please provide this information using the dedicated FLAG")))
  (IF bootstrap (CONSENSUS-TREE tree-project))
  (COND 
   (midpoint-root 
    (IF bootstrap
        (macrolet ((reroot (type) 
                     `(reroot-and-return-tree tree-project midpoint-root 
                        bootstrap width width ,type)))
          (cond
           (pdf (reroot pdf))
           (png (reroot png))
           (jpg (reroot jpg))
           (eps (reroot eps))
           (newick (reroot newick))
           (t (reroot png))
           ))
      (macrolet ((reroot (type)
                   `(reroot-and-return-tree tree-project midpoint-root
                      width width ,type)))
        (cond
         (pdf (reroot pdf))
         (png (reroot png))
         (jpg (reroot jpg))
         (eps (reroot eps))
         (newick (reroot newick))
         (t (reroot png))
         ))))
   (outgroup
    (IF bootstrap
        (macrolet ((reroot (type) 
                     `(reroot-and-return-tree tree-project outgroup outgroup
                        bootstrap width width ,type)))
          (cond
           (pdf (reroot pdf))
           (png (reroot png))
           (jpg (reroot jpg))
           (eps (reroot eps))
           (newick (reroot newick))
           (t (reroot png))
           ))
      (macrolet ((reroot (type) 
                   `(reroot-and-return-tree tree-project outgroup outgroup
                      width width ,type)))
        (cond
         (pdf (reroot pdf))
         (png (reroot png))
         (jpg (reroot jpg))
         (eps (reroot eps))
         (newick (reroot newick))
         (t (reroot png))
         ))))
   (unrooted
    (IF bootstrap
        (macrolet ((reroot (type) 
                     `(reroot-and-return-tree tree-project unrooted 
                        bootstrap width width ,type)))
          (cond
           (pdf (reroot pdf))
           (png (reroot png))
           (jpg (reroot jpg))
           (eps (reroot eps))
           (newick (reroot newick))
           (t (reroot png))
           ))
      (macrolet ((reroot (type) 
                   `(reroot-and-return-tree tree-project unrooted 
                      width width ,type)))
        (cond
         (pdf (reroot pdf))
         (png (reroot png))
         (jpg (reroot jpg))
         (eps (reroot eps))
         (newick (reroot newick))
         (t (reroot png))
         ))))
   (T 
    (IF bootstrap
        (macrolet ((reroot (type) 
                     `(reroot-and-return-tree tree-project midpoint-root  
                        bootstrap width width ,type)))
          (cond
           (pdf (reroot pdf))
           (png (reroot png))
           (jpg (reroot jpg))
           (eps (reroot eps))
           (newick (reroot newick))
           (t (reroot png))
           ))
      (macrolet ((reroot (type) 
                   `(reroot-and-return-tree tree-project midpoint-root  
                      width width ,type)))
        (cond
         (pdf (reroot pdf))
         (png (reroot png))
         (jpg (reroot jpg))
         (eps (reroot eps))
         (newick (reroot newick))
         (t (reroot png))
         )))))))