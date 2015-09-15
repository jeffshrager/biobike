;;; -*- Package: help; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :help)

;;; +=========================================================================+
;;; | Copyright (c) 2002-2006 JP Massar, Jeff Shrager, Peter Seibel           |
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

;;; Authors:  JP Massar, Peter Seibel.


(defvar *stop-table* 
  (create-hash-table 
   '("the"
     "of" "and" "in" "to" "a" "that" "is" "with" "by" "for" "are" 
     "was" "as" "we" "from" "this" "were" 
     "these" "an" "on" "be" "have" "which" "at" "not" 
     "or" "been" "has" "also" "but" 
     "their" "no" "it" "than" "can"
     "may" "when" "there" "they" "its" "i" "you" "he" "she" "had" 
     "into" "here")
   :test #'equalp
   :mode :singleton))

(defstruct single-word-help-results 
  exact-matches
  near-matches
  keyword-matches
  text-matches
  jdocs    ;;JKM Added Jdocs.
  search-words   ;;JKM Added search-words Jun 25 '12.  Needed because we add in extras: stemmed words, hyphen-ated ones.
)
;;Other fields kept in for backwards compatibility;  old version still used for alternate.  Single words good too.


(defmethod wb::out-record-to-html 
           ((form single-word-help-results) (string string) &rest ignore)
(formatt "Out-record-to-html in help-single.")
  (if (use-jhelp?)
      (display-single-word-jhelp-results form)
    (display-single-word-help-results form)))
  

(defmethod print-object ((hr single-word-help-results) stream)
  (format stream "#<biobike help object>"))
  

(defun single-word-help (word &rest keys &key &allow-other-keys)
 ;(print "Single-word-help ")(prin1 word)(princ " ")(prin1 keys)(terpri)
  (flet ((single (word) (apply 'single-word-help-aux (string word) keys))
         (general (words) (apply 'help-function words keys)))
    (cond
     ;; (help one-string)
     ((symbolp word) (single (string word)))
     ;; (help "operating system")
     ;; (help "file-to-string")
     ;; (help "ortholog")
     ((stringp word) 
      (let ((tword (string-trim *whitespace* word)))
        (cond
         ;; Multiple words in a single string. Split them up and
         ;; reinvoke HELP.
         ((elements-in *whitespace* tword)
          (general (split-at-whitespace tword)))
         ;; A single word without a colon
         ((not (position #\: tword)) (single tword))
         ;; A single word with a colon. The user may be asking for
         ;; help about a particular symbol, e.g. (help "utils:one-string")
         ;; Read the string and see if we come up with a valid symbol.
         (t
          (flet ((bad-colon (old new)
                   (cformatt "Invalid help string: ~S" old)
                   (cformatt "('Help' words should not contain ':'s).")
                   (cformatt "Trying HELP with ~S instead..." new)
                   ))
            (handler-case 
                (let ((symbol (read-from-string tword)))
                  ;; Okay, it's valid. 
                  (cond
                   ;; ":xyzzy"
                   ((and (keywordp symbol) (plusp (length (string symbol))))
                    (let ((s (subseq (string symbol) 1)))
                      (bad-colon symbol s)
                      (single s)
                      ))
                   ;; Degenerate case: "utils::"
                   ((keywordp symbol) (help-oops symbol))
                   ;; Is it a function or a variable?
                   ((or (fboundp symbol) (boundp symbol))
                    (help-about-specific-symbol symbol))
                   (t (single (string symbol)))
                   ))
              (error 
               ()
               (let ((s (substitute #\Space #\: tword)))
                 (bad-colon tword s)
                 (general (split-at-whitespace s))
                 ))))))))
     (t (error "Internal error!"))
     )))

;;;; SINGLE WORD MATCHING

;;; Exact matches are defined as with STRING-EQUAL.
;;; Near matches are defined in terms of edit distance.
;;; Partial matches are defined as when the string being 
;;; looked for is a substring of the string being examined.

;;; Find all the exact, near and partial matches for STRING amongst all the
;;; documentation objects in our documentation set.

;;; We compare STRING against the name and keywords of each 
;;; documentation object, and against the name of each symbol.

;;; Remove any duplicated hits. 

;;; Sort the near and partial matches by how close they match.

;;; Depending on the number of exact, near and partial matches, display some
;;; or all of the matches, and if some cannot  be displayed, add a link to a
;;; page displaying all the hits.

;; Debugging goo for single-word-help-aux

(defvar *edm* nil)
(defvar *ndm* nil)
(defvar *ekm* nil)
(defvar *nkm* nil)
(defvar *pdm* nil)
(defvar *pkm* nil)
(defvar *fdm* nil)
(defvar *edm2* nil)
(defvar *ndm2* nil)
(defvar *ekm2* nil)
(defvar *nkm2* nil)
(defvar *pdm2* nil)
(defvar *pkm2* nil)
(defvar *fdm2* nil)

(defun single-word-help-aux
       (string 
        &rest keys
        &key (scope :user-external) &allow-other-keys
        &aux 
        exact-doc-matches exact-keyword-matches 
        near-doc-matches near-keyword-matches 
        partial-doc-matches partial-keyword-matches
        text-matches
        exact-matches near-matches keyword-matches
        )

  (let* ((slen (length string))
         (ustring (string-upcase string))
         (lstring (string-downcase string))
         (*current-search-string* string))
    (declare (special *current-search-string*))

    ;; find all documented items that match exactly by name

    (setq 
     exact-doc-matches
     (find-doc-items-if
      (lambda (doc-obj) 
        (score->fraction (and (string-equal string (string (name doc-obj))) 0)))
      :type :exact-doc
      ))

    ;; find all documented items that match closely enough via EDIT-DISTANCE.
    ;; 'closely enough' is a function of the length of the string we are
    ;; searching for.  We need to do both uppercase and lowercase EDIT-DISTANCE
    ;; matches because the EDIT-DISTANCE function is case-sensitive.  

    (setq
     near-doc-matches
     (find-doc-items-if 
      (lambda (doc-obj) 
        (let ((name (string (name doc-obj))))
          (score->fraction
           (and (not (string-equal string name))
                (or (near-documentation-name-match ustring doc-obj)
                    (near-documentation-name-match lstring doc-obj)
                    )))))
      :type :near-doc
      ))

    ;; find all documented items which have a keyword which matches 
    ;; our search string exactly.  

    (setq
     exact-keyword-matches
     (find-doc-items-if
      (lambda (doc-obj) 
        (score->fraction
         (loop for k in (keywords doc-obj) do
               (when (string-equal string (string k)) (return 0)))))
      :type :exact-keyword
      ))

    ;; find all documented items which have a keyword which matches
    ;; our search string closely enough via EDIT-DISTANCE.  

    (setq
     near-keyword-matches
     (find-doc-items-if
      (lambda (doc-obj) 
        (score->fraction
         (let ((min-distance most-positive-fixnum))
           (loop for k in (keywords doc-obj) 
                 as kname = (string k)
                 when (not (string-equal string kname))
                 do
                 (let ((uscore (near-name-match ustring k))
                       (lscore (near-name-match lstring k)))
                   (when uscore (setq min-distance (min min-distance uscore)))
                   (when lscore (setq min-distance (min min-distance lscore)))
                   ))
           (unless (= min-distance most-positive-fixnum) min-distance)
           )))
      :type :near-keyword
      ))

    ;; find all documented items whose name contains the search string
    ;; as a substring.  e.g., ONE-STRING contains STRING 

    (setq
     partial-doc-matches
     (find-doc-items-if
      (lambda (doc-obj) 
        (score->fraction
         (let ((doc-name (string (name doc-obj))))
           (and (< slen (length doc-name)) 
                (search string doc-name :test 'char-equal)
                (- 1 (/ slen (length doc-name)))
                ))))
      :type :partial-doc
      ))

    ;; find all documented items which contain a keyword whose
    ;; name contains the search string as a substring.  

    (setq 
     partial-keyword-matches
     (find-doc-items-if
      (lambda (doc-obj) 
        (score->fraction
         (let ((fraction-different 1))
           (loop for k in (keywords doc-obj) 
                 as key-name = (string k)
                 as klen = (length key-name)
                 do
                 (when (and (< slen klen)
                            (search string key-name :test 'char-equal))
                   (setq fraction-different
                         (min fraction-different (- 1 (/ slen klen)))
                         )))
           (unless (= fraction-different 1) fraction-different)
           )))
      :type :partial-keyword
      ))

    (setq 
     text-matches
     (find-doc-items-if 
      (lambda (doc-obj) 
        (documentation-file-text-match doc-obj string)
        )
      :type :text-match
      ))
    
    (when *help-debug* 
      (setq *edm* exact-doc-matches)
      (cformatt "exact-doc-matches: ~A" exact-doc-matches)
      (setq *ndm* near-doc-matches)
      (cformatt "near-doc-matches: ~A" near-doc-matches)
      (setq *ekm* exact-keyword-matches)
      (cformatt "exact-keyword-matches: ~A" exact-keyword-matches)
      (setq *nkm* near-keyword-matches)
      (cformatt "near-keyword-matches: ~A" near-keyword-matches)
      (setq *pdm* partial-doc-matches)
      (cformatt "partial-doc-matches: ~A" partial-doc-matches)
      (setq *pkm* partial-keyword-matches)
      (cformatt "partial-keyword-matches: ~A" partial-keyword-matches)
      (setq *fdm* text-matches)
      (cformatt "text-matches: ~A" text-matches)
      )


    ;; exact-doc-matches exact-keyword-matches exact-symbol-matches
    ;; near-doc-matches near-keyword-matches near-symbol-matches
    ;; partial-doc-matches partial-keyword-matches partial-symbol-matches
    ;; --> exact-matches near-matches partial-matches

    ;; The exact matches and the other matches should be disjoint.
    ;; Keyword matches are treated as matches to the doc item that
    ;; contains the keyword.
      
    ;; Exact keyword matches should not intersect the exact doc matches,
    ;; since that would imply a doc item has a name which is identical
    ;; to one of its keywords.
    ;; Exact keyword matches should not intersect exact symbol matches,
    ;; since symbol docs don't have keywords.
    ;; Exact symbol matches may intersect the exact doc matches in the
    ;; sense that they refer to the same symbol/use-of-symbol (via equality
    ;; of the doc-vs-symbol-record-key key function )
    ;; these are resolved in favor of the doc matches.

    ;; Near doc matches, partial doc matches, exact keyword matches, 
    ;; near keyword matches and
    ;; partial keyword matches can all intersect in the sense that they
    ;; can point to the same (EQ) doc object. These are resolved in the above
    ;; order of priority.

    (multiple-value-setq
        (near-doc-matches 
         partial-doc-matches 
         exact-keyword-matches
         text-matches
         near-keyword-matches
         partial-keyword-matches)
        (separate-into-lists 
         (purge-duplicates 
          (append 
           near-doc-matches partial-doc-matches 
           exact-keyword-matches text-matches
           near-keyword-matches partial-keyword-matches)
          :key 'help-match-ref 
          :test 'eql
          :hash-threshold 20)
         (lambda (x) (eq (help-match-type x) :near-doc))
         (lambda (x) (eq (help-match-type x) :partial-doc))
         (lambda (x) (eq (help-match-type x) :exact-keyword))
         (lambda (x) (eq (help-match-type x) :text-match))
         (lambda (x) (eq (help-match-type x) :near-keyword))
         ))
    
    (when *help-debug* 
      (terpri)
      (terpri)
      (terpri)
      (setq *edm2* exact-doc-matches)
      (cformatt "exact-doc-matches: ~A" exact-doc-matches)
      (setq *ndm2* near-doc-matches)
      (cformatt "near-doc-matches: ~A" near-doc-matches)
      (setq *ekm2* exact-keyword-matches)
      (cformatt "exact-keyword-matches: ~A" exact-keyword-matches)
      (setq *nkm2* near-keyword-matches)
      (cformatt "near-keyword-matches: ~A" near-keyword-matches)
      (setq *pdm2* partial-doc-matches)
      (cformatt "partial-doc-matches: ~A" partial-doc-matches)
      (setq *pkm2* partial-keyword-matches)
      (cformatt "partial-keyword-matches: ~A" partial-keyword-matches)
      (setq *fdm2* text-matches)
      (cformatt "text-matches: ~A" text-matches)
      )
    
    ;; Finally, we create three lists: Exact doc matches,
    ;; near and partial doc matches, and exact, near, and partial
    ;; keyword matches.  Then we display the results.

    (setq exact-matches (sort exact-doc-matches '< :key 'help-match-score))
          
    (setq near-matches 
          (sort 
           (append near-doc-matches partial-doc-matches)
           '< 
           :key 'help-match-score 
           ))

    (setq keyword-matches
          (sort 
           (append 
            exact-keyword-matches near-keyword-matches partial-keyword-matches)
           '< 
           :key 'help-match-score
           ))

    (when *help-debug*

      (when exact-matches
        (terpri) (terpri)
        (cformatt "Exact matches:")
        (loop for match in exact-matches do
              (with-match-data (match)
                (formatt "  ~A ~A ~A~%" 
                         name doc-type type (limited-string (docstring ref) 40)
                         ))))
      (when near-matches
        (terpri)
        (cformatt "Near matches:")
        (loop for match in near-matches do
              (with-match-data (match)
                (formatt "~A ~A ~A ~A~%" 
                         name doc-type type score 
                         (limited-string (docstring ref) 40)
                         ))))
      (when keyword-matches
        (terpri)
        (cformatt "Keyword matches:")
        (loop for match in keyword-matches do
              (with-match-data (match)
                (formatt "~A ~A ~A ~A ~A~%" 
                         name doc-type type score 
                         (keywords ref)
                         (limited-string (docstring ref) 40)
                         ))))
      (when text-matches 
        (terpri)
        (cformatt "Text matches:")
        (loop for match in text-matches
              do
              (with-match-data (match)
                (formatt "~A ~A ~A~%" 
                         name (source-file ref) (associated-text-file ref)
                         ))))
      )
    
    ;; restrict the matches to those visible to the user unless 
    ;; :SCOPE :ALL is specified 
    (ecase scope
      (:all nil)
      (:user-external 
       (let ((pkg (find-package wb::*username*)))
         (setq exact-matches (user-external-filter exact-matches pkg))
         (setq near-matches (user-external-filter near-matches pkg))
         (setq keyword-matches (user-external-filter keyword-matches pkg))
         )))
         
    (if (and (null exact-matches)
             (null near-matches)
             (null keyword-matches)
             (null text-matches))
        (let ((partials (string-split string #\-)))
          (if (= 1 (length partials))
              (progn
                (cformatt "No matches found for '~A'" string)
                nil)
            (progn 
              (cformatt 
               #.(one-string-nl
                  "No matches found for '~A' using single word matching."
                  "Attempting to use multiple word match algorithm on the"
                  "partial words ~S...")
               string partials)
              (apply 'multiple-word-help partials keys)
              )))
      (progn
        ;; Jeff doesn't want TYPE docobjs to appear.  
        (when (forward-funcall 'wb::bbl-mode?) 
          (flet ((remove-types (list) 
                   (remove-if 
                    (lambda (x) 
                      (let ((d (help-match-ref x)))
                        (and (typep d 'symbol-doc) (eq (dtype d) :type))
                        ))
                    list
                    )))
            (setq exact-matches (remove-types exact-matches))
            (setq near-matches (remove-types near-matches))
            (setq keyword-matches (remove-types keyword-matches))
            (setq text-matches (remove-types text-matches))
            ))
        (let ((results 
               (make-single-word-help-results
                :exact-matches exact-matches
                :near-matches near-matches
                :keyword-matches keyword-matches 
                :text-matches text-matches
                )))
          ;; Need to save state to be able to display all the matches
          ;; on a separate page using a link printed out to the weblistener
          (setf (get wb::*sessionid* :help-matches) results)
          (setf (get wb::*sessionid* :current-search-string) 
                *current-search-string*)
          results 
          )))

    ))

(defun documentation-file-text-match (docobj string)
  (block exit
    (when (or (< (length string) 4) (gethash string *stop-table*))
      (return-from exit nil))
    (score->fraction 
     (when (typep docobj 'documentation-file)
       (let ((txt (text docobj))
             (sfile (source-file docobj))
             (atfile (associated-text-file docobj)))
         (cond
          (txt 
           (cond
            ((stringp txt) 
             (when (search string txt :test 'string-equal) 
               (add-text-file-match-to-docobj string txt docobj)
               0 
               ))
            ((listp txt) 
             (loop for line in txt 
                   for count from 1 
                   do 
                   (when (and (stringp line)
                              (search string line :test 'string-equal))
                     (add-text-file-match-to-docobj string line docobj)
                     (return 0)
                     )))
            (t (error "Text field contains neither a string nor a list!"))
            ))
          (atfile 
           (let* ((path (txt-file-for-filedoc docobj))
                  (lines (file-to-string-list path)))
             (loop for line in lines
                   for count from 1
                   do
                   (when (search string line :test 'string-equal) 
                     (add-text-file-match-to-docobj string line docobj)
                     (return 0)
                     ))))
          (sfile 
           (let* ((path (source-file-for-filedoc docobj))
                  (lines (file-to-string-list path)))
             (loop for line in lines
                   for count from 1
                   do
                   (when (search string line :test 'string-equal) 
                     (add-text-file-match-to-docobj string line docobj)
                     (return 0)
                     ))))
          (t nil)
          ))))))

(defun add-text-file-match-to-docobj (string line docobj)
  (let ((existing-matches (matches docobj)))
    (when existing-matches 
      (setq existing-matches 
            (delete wb::*sessionid* existing-matches :key 'first)))
    (push (list wb::*sessionid* string line) (matches docobj))
    ))

(defun user-external-filter (match-list package)
  ;; remove symbols that are not accessible to the user.
  (remove-if 
   (lambda (m) 
     (let ((docobj (help-match-ref m)))
       (and (member (type-of docobj)
                    '(help:symbol-doc help:function-documentation))
            (let* ((symbol (help:name docobj))
                   (name (symbol-name symbol))
                   (accessible-symbol
                    (or (find-symbol name package) 
                        (find-symbol name *package*)
                        )))
              (or (null accessible-symbol)
                  (not (eq symbol accessible-symbol))
                  )))))
   match-list 
   ))
                   

(defun near-documentation-name-match
       (string 
        doc-item
        &key
        (frames? nil)
        (threshold
         (forward-package-funcall :wb :edit-distance-threshold (length string)))
        )
  (near-name-match 
   string (help:name doc-item) :frames? frames? :threshold threshold))

(defun near-name-match
       (string 
        name
        &key
        (frames? nil)
        (threshold 
         (forward-package-funcall :wb :edit-distance-threshold (length string)))
        &aux 
        (len (length string)))
  #.(optimization-declaration)
  (declare (ignorable frames?))
  (declare (fixnum threshold))
  (let* ((max-length (the fixnum (+ len threshold)))
         (min-length (the fixnum (- len threshold)))
         (sname (string name))
         (name-len (length sname)))
    (declare (fixnum max-length min-length))           
    ;; Only permit match where the first letter is exact.
    (when (char-equal (elt string 0) (elt sname 0))
      ;; Only calculate the edit-distance of the name of the documentation item
      ;; if it could possibly be transformed into STRING in no more than 
      ;; THRESHOLD edit-distance steps
      (when (and (>= name-len min-length) (<= name-len max-length))
	(let ((ed (compute-edit-distance sname string)))
	  (declare (fixnum ed))
	  (when (<= ed threshold) ed)
	  )))))

;;; Map an arbitrary positive integer score to a value between
;;; 0 and 1, such that the larger the positive integer, the closer
;;; to 1 the result is (and hence the worse the rating)

(defun score->fraction (score)
  (cond
   ((or (null score) (< score 1)) score)
   (t (/ score (+ score 2)))
   ))

(defun match-examples (match)
  (let ((ref (help-match-ref match)))
    (and (typep ref 'help:function-documentation)
         (help:examples ref))))