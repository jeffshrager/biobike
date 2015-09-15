;;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: bbi; -*-

(in-package :bbi) 

;;; +=========================================================================+
;;; | Copyright (c) 2008 JP Massar, Jeff Elhai, Mark Slupesky, Peter Seibel   |
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

;;; Author: JP Massar, Mark Slupesky

#||


   (REPLACE (INTO | INTO-EACH | INTO-COPY-OF | INTO-COPY-OF-EACH) target
      plus required choice of one of the following:
           [  (REPLACING-EVERY) string-or-item
            | (REPLACING-FIRST) string-or-item
            | (REPLACING-EACH) string-or-item
            | (FROM) n (TO) n
            | (FROM) n
            | (TO) n
            | (AT) n]
      plus required choice of one of the following:
          (WITH | WITH-EACH) string-or-item
      plus other options:
           [STRICT RELAXED])  ;; relaxed is default

The examples now become:

  (REPLACE INTO "xyzzy" REPLACING-EVERY "z" WITH "x") --> "xyxxy"
  (REPLACE INTO "xyzzy" REPLACING-FIRST "z" WITH "x") --> "xyxzy"
  (REPLACE INTO '((1 2) 3 4) REPLACING-EVERY '(1 2) WITH '(3 4))
                --> ((3 4) 3 4)

  (REPLACE INTO "xyzzy" FROM 1 TO 3 WITH "y") --> "yyyzy"
  (REPLACE INTO "xyzzy" FROM 1 TO 3 WITH "yaq") --> "yaqzy"
  (REPLACE INTO "xyzzy" FROM 3 WITH "y") --> "xyyyy"
  (REPLACE INTO "xyzzy" TO 3 WITH "y") --> "yyyzy" (implicit from 1)
  (REPLACE INTO '(a b c d e) FROM 1 TO 3 WITH 1) --> (1 1 1 D E)

And others:

  (REPLACE INTO-EACH '("abcd" "1234") AT 3 WITH "x") --> ("abxd" "12x4")
  (REPLACE INTO "abcd" AT 3 WITH-EACH '(1 2)) --> ("ab1d" "ab2d")
  (REPLACE INTO-EACH '("abcd" "1234") AT 3 WITH-EACH '("x" "y"))
           --> (("abxd" "abyd") ("12x4" "12y4"))
  (REPLACE INTO '(1 2 3 4) REPLACING-EACH '(1 2) WITH-EACH '(3 4))
           --> (3 4 3 4)
  (REPLACE INTO "abcd" REPLACING-EACH '("a" "b") WITH-EACH '("c" "d"))
           -->  "cdcd"

||#

(defparameter *possible-replace-modes*
  '(:into :into-each :into-copy-of :into-copy-of-each))
(defparameter *possible-replace-replacing-modes*
  '(:replacing-every :replacing-first :replacing-each))
(defparameter *possible-replace-with-modes*
  '(:with :with-each))
(defparameter *possible-replace-flags* 
  '((:strict :relaxed) (:case-sensitive)))
(defparameter *possible-replace-keys* 
  `((:from) (:to) (:at) 
    ,*possible-replace-replacing-modes*
    ,*possible-replace-with-modes*
    ))

(defparameter *possible-insert-keys* `((:before) (:after) (:each)))
(defparameter *possible-insert-flags* 
  '((:strict :relaxed) (:case-sensitive)))

(defparameter *bbl-replace-list-comparison-function*
  (lambda (x y) 
    (cond
     ((and (not (stringp x)) (not (stringp y))
           (not (characterp x)) (not (characterp y)))
      (equal x y))
     (t 
      (ecase *bbl-current-case-mode*
        (:case-sensitive (equal x y))
        (:case-insensitive (equalp x y))
        )))))
      
(defun find-allowable-keys (function-args possible-keys)
  (let ((findings nil)
        (last-arg-position 0))
    (loop for keyset in possible-keys do
          (loop 
           for key in keyset do
           (loop for arg in function-args by #'cddr 
                 for pos from 0 by 2
                 do
                 (when (symbol= key arg) 
                   (push (list key pos) findings)
                   (setq last-arg-position (max last-arg-position (1+ pos)))
                   ))))
    (loop for keyset in possible-keys
          as found-one-in-set = nil
          do
          (loop for key in keyset do 
                (when (find key findings :test 'symbol= :key 'first)
                  (if found-one-in-set 
                      (error "Illegal key! using ~A and ~A"
                             key found-one-in-set)
                    (setq found-one-in-set key)
                    ))))
    (values findings last-arg-position)
    ))

(defun arglist-key-and-data (findings keyset function-args)
  (loop for key in keyset do
        (let* ((data (find key findings :test 'symbol= :key 'first)))
          (when data 
            (let ((pos (second data)))
              (return (list key (nth (1+ pos) function-args)))
              )))
        finally (return nil)
        ))

(defun replace-keys (findings replace-args)
  (let ((from-data (arglist-key-and-data findings '(:from) replace-args))
        (to-data (arglist-key-and-data findings '(:to) replace-args))
        (at-data (arglist-key-and-data findings '(:at) replace-args))
        (replacing-mode-data 
         (arglist-key-and-data 
          findings *possible-replace-replacing-modes* replace-args))
        (with-mode-data
         (arglist-key-and-data
          findings *possible-replace-with-modes* replace-args
          )))
    (values 
     (first from-data) (second from-data)
     (first to-data) (second to-data)
     (first at-data) (second at-data)
     (first replacing-mode-data) (second replacing-mode-data)
     (first with-mode-data) (second with-mode-data)
     )))

(defmacro bbl::replace (&rest replace-args)
  "Replace part of a string or list with something else"
  (let ((len (length replace-args))
        (mode (first replace-args))
        (target (second replace-args))
        (error-mode nil)
        (case-mode nil))
    (unless (plusp len) 
      (error "You must provide arguments to REPLACE!"))
    (labels
        ((snarf-mode () 
           (if (member mode *possible-replace-modes* :test 'utils::symbol=)
               (progn
                 (maybe-next-oops "a target")
                 (setq mode (first replace-args))
                 (setq target (second replace-args))
                 (setq replace-args (cddr replace-args)))
             (progn
               (setq mode :into)
               (setq target (first replace-args))
               (pop replace-args))))
         (verify-flags ()
           (let ((n-possible-flags (length *possible-replace-flags*)))
             (unless (<= (length replace-args) n-possible-flags)
               (error "Too many flags in REPLACE!"))
             (loop for flags in *possible-replace-flags* 
                   with count = 0
                   do
                   (loop for flag in flags
                         do 
                         (when (find flag replace-args :test 'symbol=)
                           (incf count)
                           (return)
                           ))
                   finally 
                   (unless (= count (length replace-args))
                     (error "Invalid flag in REPLACE!")
                     ))))
         (maybe-oops (missing-stuff)
           (unless replace-args
             (error (s+ "You must provide " missing-stuff "!"))))
         (maybe-next-oops (missing-stuff)
           (unless (cdr replace-args)
             (error (s+ "You must provide " missing-stuff "!")))))
      
      (snarf-mode)
      (maybe-oops "a replacing mode or range")
      (multiple-value-bind (replace-key-findings last-arg-position)
          (find-allowable-keys replace-args *possible-replace-keys*)
        (multiple-value-bind 
            (from? from-arg to? to-arg at? at-arg 
                   replacing-mode? replacing-mode-arg 
                   with-mode? with-mode-arg)
            (replace-keys replace-key-findings replace-args)
          (unless (= (* 2 (length replace-key-findings)) (1+ last-arg-position))
            (error "Unrecognized REPLACE key or keys!"))
          (setq replace-args (nthcdr (1+ last-arg-position) replace-args))
          (verify-flags)
          (setq error-mode 
                (or (find :strict replace-args :test 'symbol=)
                    (find :relaxed replace-args :test 'symbol=)
                    ))
          (setq case-mode 
                (if (find :case-sensitive replace-args :test 'symbol=)
                    :case-sensitive 
                  :case-insensitive))
          `(LET ((*bbl-level* (INCREMENT-BBL-LEVEL)))
	        (replace-function 
            ,(utils::keywordize mode) 
            ,target
            ,@(if replacing-mode? 
                  (list (utils::keywordize replacing-mode?) replacing-mode-arg)
                (list nil nil))
            ,(if from? from-arg nil)
            ,(if to? to-arg nil)
            ,(if at? at-arg nil)
            ,@(if with-mode? 
                  (list (utils::keywordize with-mode?) with-mode-arg)
                (list nil nil))
            ,(if error-mode (utils::keywordize error-mode) nil)
            ,(not (null replacing-mode?)) ,from? ,to? ,at? 
            ,(not (null with-mode?))
            ,(utils::keywordize case-mode)
            nil
            )))))))

;; (replacey :into "xyzzy" :replacing-every "z" :with "a")

;; when both replacing-each and with-each are used, THEN there's a 
;; single loop representing the 1:1 correspondence between the elements
;; of the with-each and replacing-each
(defun replace-function 
       (
        ;; one of into, into-each, into-copy-of, into-copy-of-each
        mode 
        target
        ;; one of replacing-every, replacing-first, replacing-each, 
        ;; or NIL 
        replacing-mode 
        stuff-to-be-replaced 
        ;; either both from and to are present, or from only is present 
        ;; or to only is present, or at only is present
        from
        to
        at
        ;; with or with-each
        with-mode
        ;; stuff being used to replace with
        replacing-stuff
        ;; one of strict or relaxed
        error-mode 
        ;; anything else
        replacing-mode?
        from?
        to?
        at?
        with-mode?
        case-mode
        other-goo
        &aux 
        (*bbl-current-case-mode* case-mode)
        )
  
  (declare (ignorable replacing-mode? from? to? at? with-mode?))
  
  (when (equal mode :into-copy-of)
    (setf target (utils::copy-seq target)))
  (when (equal mode :into-each)
    (return-from replace-function
      (loop for thing in target
            collect
            (replace-function
             :into thing replacing-mode stuff-to-be-replaced from to at
             with-mode replacing-stuff error-mode
             replacing-mode? from? to? at? with-mode? case-mode other-goo))))
  (when (equal mode :into-copy-of-each)
    (return-from replace-function
      (loop for thing in target
            collect
            (replace-function
             :into-copy-of
             thing replacing-mode stuff-to-be-replaced
             from to at with-mode replacing-stuff error-mode
             replacing-mode? from? to? at? with-mode? case-mode
             other-goo))))

  (let ((len (length target)))
    (when (equal to *end*)
      (setf to len))
    (when (equal at *end*)
      (setf at len))
    (when (null error-mode)
      (setf error-mode :relaxed))
    (when from 
      (case error-mode
        (:strict 
         (when (or (< from 1) (> from len))
           (error "Invalid range!")))
        (:relaxed
         (cond
          ((< from 1) (setf from 1))
          ((> from len) (setf from len))
          (t nil)
          ))))
    (when to
      (case error-mode
        (:strict
         (when (or (< to 1) (> to len))
           (error "Invalid range!")))
        (:relaxed
         (cond
          ((< to 1) (setf from 1))
          ((> to len) (setf to len))
          (t nil)
          ))))
    (when at
      (when (or from to)
        (error "You can only use AT without using FROM or TO!"))
      (when (member
             replacing-mode
             '(:replacing-every :replacing-first :replacing-each))
        (error 
         #.(one-string-nl
            "You cannot combine the AT keyword with a replacing-mode"
            "such as replacing-every, replacing-first, or replacing-each"
            )))
        
      (case error-mode
        (:strict 
         (when (or (< at 1) (> at len))
           (error "Invalid range!")))
        (:relaxed
         (cond
          ((< at 1) (return-from replace-function target))
          ((> at len) (return-from replace-function target))
          (t nil)
          )))))

  (when (and (equal replacing-mode :replacing-every) 
             (equal with-mode :with))
    (return-from replace-function
      (cond
       ((stringp target)
        (string-replacex 
         stuff-to-be-replaced replacing-stuff target from to at))
       ((listp target)
        (list-replacex 
         stuff-to-be-replaced replacing-stuff target
         from to at replacing-mode?))
       (t
        (error "Target must be a list or string"))
       )))

  (when (and (equal replacing-mode :replacing-every) 
             (equal with-mode :with-each))
    (unless (listp replacing-stuff)
      (error "If you are using :WITH-EACH, you must pass a list"))
    (return-from replace-function
      (loop for little-replacing-stuff in replacing-stuff
            as target-copy = (copy-seq target)
            collect
            (cond
             ((stringp target-copy)
              (string-replacex
               stuff-to-be-replaced little-replacing-stuff 
               target-copy from to at))
             ((listp target-copy)
              (list-replacex
               stuff-to-be-replaced 
               little-replacing-stuff target-copy from to at replacing-mode?))
             (t
              (error "Target must be a list or string"))
             ))))

  (when (and (equal replacing-mode :replacing-first) 
             (equal with-mode :with))
    (return-from replace-function
      (cond
       ((stringp target) 
        (setq stuff-to-be-replaced (bb-convert stuff-to-be-replaced 'string))
        (setq replacing-stuff (bb-convert replacing-stuff 'string))
        (let ((lsr (length stuff-to-be-replaced))
              (lrs (length replacing-stuff)))
          (cond
           ((and (= lsr 1) (= lrs 1)) 
            (do-1-1-string-replace 
             target stuff-to-be-replaced replacing-stuff from to at))
           (t 
            (do-complex-string-replace
             target stuff-to-be-replaced replacing-stuff from to at)))))
       ((listp target) 
        (do-list-thing
         target stuff-to-be-replaced replacing-stuff from to at))
       (t 
        (error "Cannot replace into something that isn't a string or list!"))
       )))

  (when (and (equal replacing-mode :replacing-first) 
             (equal with-mode :with-each))
    (unless (listp replacing-stuff)
      (error "If you are using :WITH-EACH, you must pass a list"))
    (return-from replace-function
      (loop 
       for little-replacing-stuff in replacing-stuff
       as target-copy = (copy-seq target)
       collect
       (cond
        ((stringp target-copy) 
         (setq stuff-to-be-replaced
               (bb-convert stuff-to-be-replaced 'string))
         (setq little-replacing-stuff 
               (bb-convert little-replacing-stuff 'string))
         (let ((lsr (length stuff-to-be-replaced))
               (lrs (length little-replacing-stuff)))
           (cond
            ((and (= lsr 1) (= lrs 1)) 
             (do-1-1-string-replace 
              target-copy stuff-to-be-replaced
              little-replacing-stuff from to at))
            (t 
             (do-complex-string-replace
              target-copy stuff-to-be-replaced
              little-replacing-stuff from to at)))))
        ((listp target-copy) 
         (do-list-thing
          target-copy stuff-to-be-replaced little-replacing-stuff from to at))
        (t 
         (error
          "Cannot replace into something that isn't a string or list!"))
        )

       )))
  (when (and (equal replacing-mode :replacing-each) 
             (equal with-mode :with))
    (unless (listp stuff-to-be-replaced)
      (error
       (one-string-nl
        "When you are using :REPLACING-EACH, you must pass"
        "a list of things to be replaced.  You passed in '~A'")
       stuff-to-be-replaced
       ))
    (return-from replace-function
      (loop for little-stuff-to-be-replaced in stuff-to-be-replaced
            do
            (cond
             ((stringp target)
              (setq target 
                    (string-replacex 
                     little-stuff-to-be-replaced
                     replacing-stuff target from to at)))
             ((listp target)
              (setq target
                    (list-replacex 
                     little-stuff-to-be-replaced 
                     replacing-stuff target from to at replacing-mode?)))
             (t
              (error "Target must be a list or string"))
             )
            finally (return target)
            )))
  
  (when (and (equal replacing-mode :replacing-each) 
             (equal with-mode :with-each))
    (return-from replace-function
      (loop 
       for little-stuff-to-be-replaced in stuff-to-be-replaced
       for little-replacing-stuff in replacing-stuff
       do
       (cond
        ((stringp target)
         (setq target 
               (string-replacex 
                little-stuff-to-be-replaced little-replacing-stuff
                target from to at)))
        ((listp target)
         (setq target
               (list-replacex 
                little-stuff-to-be-replaced little-replacing-stuff 
                target from to at replacing-mode?)))
        (t
         (error "Target must be a list or string"))
        )
       finally (return target)
       )))

  (when (null replacing-mode) 
    (unless (or from to at)
      (error 
       "If there's no replacing mode, you must specify some kind of range!"))
    (cond
     ((stringp target)
      (string-replacex stuff-to-be-replaced replacing-stuff 
                       target from to at))
     ((listp target)
      (list-replacex stuff-to-be-replaced replacing-stuff 
                     target from to at replacing-mode?))
     ))

  
  
  )

(defun do-1-1-string-replace 
       (target stuff-to-be-replaced replacing-stuff from to at)
  (let* ((test
          (ecase *bbl-current-case-mode*
            (:case-sensitive 'lisp:string=)
            (:case-insensitive 'lisp:string-equal)))
         (real-stuff-to-be-replaced (coerce stuff-to-be-replaced 'character))
         (real-replacing-stuff (coerce replacing-stuff 'character))
         (pos 
          (cond 
           ((and from to) 
            (position real-stuff-to-be-replaced target
                      :start (1- from) :end to :test test))
           (from
            (position real-stuff-to-be-replaced target 
                      :start (1- from) :test test))
           (to
            (position real-stuff-to-be-replaced target 
                      :end to :test test))
           (t 
            (position real-stuff-to-be-replaced target 
                      :test test)))))
    (when at
      (setq pos 
            (position real-stuff-to-be-replaced target :start (1- at) :end at
                      :test test)
            ))
    (when (null pos) (return-from do-1-1-string-replace target))
    (setf (elt target pos) real-replacing-stuff)
    (return-from do-1-1-string-replace target)
    ))

(defun do-complex-string-replace 
       (target stuff-to-be-replaced replacing-stuff from to at)
  (let* ((test
          (ecase *bbl-current-case-mode*
            (:case-sensitive 'lisp:string=)
            (:case-insensitive 'lisp:string-equal)))
         (pos 
          (cond
           ((and from to)
            (search
             stuff-to-be-replaced target :start2 from :end2 to :test test))
           (from
             (search
              stuff-to-be-replaced target :start2 from :test test))
           (to 
            (search
             stuff-to-be-replaced target :end2 to :test test))
           (t 
            (search
             stuff-to-be-replaced target :test test)))))
    (when (or (null pos) (zerop (length stuff-to-be-replaced)))
      (return-from do-complex-string-replace target))
    (let* ((len (length stuff-to-be-replaced)) ;; not length of replacing-stuff
           (before (lisp:subseq target 0 pos))
           (after (lisp:subseq target  (+ pos (if (zerop len) 1 len)))))
      (when at
        (setq pos 
              (position stuff-to-be-replaced target :start (1- at) :end at
                        :test test)
              ))
    
      (utils::s+ before replacing-stuff after))))

(defun do-list-thing (target stuff-to-be-replaced replacing-stuff from to at)
  (let* ((pos 
          (cond 
           ((and from to) 
            (position stuff-to-be-replaced target
                      :start (1- from) :end to
                      :test *bbl-replace-list-comparison-function*))
           (from
            (position stuff-to-be-replaced target 
                      :start (1- from)
                      :test *bbl-replace-list-comparison-function*))
           (to
            (position stuff-to-be-replaced target 
                      :end to
                      :test *bbl-replace-list-comparison-function*))
           (t 
            (position stuff-to-be-replaced target 
                      :test *bbl-replace-list-comparison-function*)))))
    (when at
      (setq pos 
            (position stuff-to-be-replaced target :start (1- at) :end at
                      :test *bbl-replace-list-comparison-function*)
            ))
    (when pos
      (lisp:setf (lisp:nth pos target) replacing-stuff))
    target
    ))

(defun list-replacex 
       (replace-what with-what in-what from to at replace-what-provided?)
  ;; (replace 2 4 '(1 2 3 4 5)) --> (1 4 3 4 5)
  (cond
   ((null in-what) nil)
   (replace-what-provided?
    (if (or from to at)
        (multiple-value-bind (from to)
            (zero-based-from-and-to from to at in-what)
          (append
           (subseq in-what 0 from)
           (list-replacex-aux 
            replace-what with-what (subseq in-what from to))
           (lisp:subseq in-what to (length in-what))
           ))
      (list-replacex-aux replace-what with-what in-what)
      ))
   ((null replace-what)
    (unless (or from to at) 
      (error "If you don't provide REPLACE-WHAT, you must provide a range!"))
    (multiple-value-bind (from to)
        (zero-based-from-and-to from to at in-what)
      (list-replacex-range with-what in-what from to)
      ))))

(defun list-replacex-aux (replace-what with-what in-what)
  (loop for sublist on in-what 
        as item = (first sublist)
        when (funcall *bbl-replace-list-comparison-function*
                      replace-what item)
        do
        (lisp:setf (first sublist) with-what) 
        finally (return in-what)
        ))

(defun zero-based-from-and-to (from to at in-what)
  (let* ((from (if from (1- from) 0))
         (to (if to to (length in-what))))
    (when at
      (setf from (1- at))
      (setf to at)
      )
    (values from to)
    ))

(defun string-replacex (replace-what with-what in-what from to at)
  (typecase with-what
    (string nil)
    (otherwise (setq with-what (bbi::bb-convert with-what 'string))))
  (cond
   ((zerop (length in-what)) "")
   ;; (replace "abc" "z" "1abc2abc3") --> "1z2z3"
   ((and (stringp replace-what)
         (or (> (lisp:length replace-what) 1)
             (> (lisp:length with-what) 1)
             (zerop (lisp:length with-what)))) 
    (if (or from to at)
        (multiple-value-bind (from to)
            (zero-based-from-and-to from to at in-what)
          (utils::s+
           (lisp:subseq in-what 0 from)
           (hard-string-replacex 
            replace-what with-what (lisp:subseq in-what from to))
           (lisp:subseq in-what to (length in-what))
           ))
      (hard-string-replacex replace-what with-what in-what)
      ))
   ;; (replace "1" "z" "abc1def1") --> "abczdefz"
   ((and (stringp replace-what) (= (lisp:length with-what) 1))
    (if (or from to at)
        (multiple-value-bind (from to)
            (zero-based-from-and-to from to at in-what)
          (utils::s+
           (lisp:subseq in-what 0 from)
           (easy-string-replacex 
            replace-what with-what (lisp:subseq in-what from to) nil)
           (lisp:subseq in-what to (length in-what))
           ))
      (easy-string-replacex replace-what with-what in-what nil))
    )
   ((null replace-what)
    (string-replacex-range with-what in-what from to at))
   ))

(defun hard-string-replacex 
   (replace-what with-what in-what &KEY case-mode recursive)
  (LET* ((*bbl-current-case-mode* (OR case-mode *bbl-current-case-mode*))
         (target (copy-seq in-what))
         (len (length replace-what))
         (test
          (ecase *bbl-current-case-mode*
            (:case-sensitive 'lisp:string=)
            (:case-insensitive 'lisp:string-equal)))
         (pos nil)
         (start 0)
		 (result)
		 (change-made))
    (SETF result
      (LOOP
        UNTIL 
         (NULL 
	       (SETQ pos 
		        (SEARCH replace-what target :TEST test :START2 start)))
        DO
		  (SETF change-made T)
          (SETF target 
            (utils::S+ (lisp:SUBSEQ target 0 pos)
                       with-what
                      (lisp:SUBSEQ target (+ pos len) (LENGTH target))
                    ))
          (SETQ start (+ pos (LENGTH with-what)))
        FINALLY (RETURN target)))
    (IF (AND recursive change-made)
	    (FORWARD-FUNCALL 'HARD-STRING-REPLACEX replace-what with-what result
		    :CASE-MODE case-mode :RECURSIVE T)
	    result)
     ))

(defun easy-string-replacex (replace-what with-what in-what copy?)
  (when copy? 
    (setq in-what (copy-seq in-what)))
  (unless (zerop (length replace-what))
    (let* ((test 
            (ecase *bbl-current-case-mode*
              (:case-sensitive 'lisp:char=)
              (:case-insensitive 'lisp:char-equal)))
           (replace-what-char (char replace-what 0))
           (with-what-char (char with-what 0)))
      (loop for char across in-what 
            for j from 0
            when (lisp:funcall test replace-what-char char)
            do
            (lisp:setf (lisp:aref in-what j) with-what-char)
            )))
  in-what
  )

(defun list-replacex-range (with-what in-what from to)
  ;; at this point, from and to are zero-based
  (let ((sublist (nthcdr from in-what)))
    (loop for j from from to (1- to)
          for cdrlist on sublist
          do
          (setf (first cdrlist) with-what)
          ))
  in-what
  )

(defun string-replacex-range (with-what in-what from to at)
  (cond
   ((> (length with-what) 1)
    (multiple-value-bind (from to)
        (zero-based-from-and-to from to at in-what)
      (let ((before (subseq in-what 0 from))
            (after (subseq in-what to)))
        (s+ before with-what after)
        )))
   (t
    (let ((new-with-what (coerce with-what 'character))
          (new-from (if from (1- from) 0))
          (new-to (if to (1- to) (1- (length in-what)))))
      (when at 
        (setq new-from (1- at))
        (setq new-to (1- at)))
      (loop for pos from new-from to new-to
            do
            (setf (lisp:char in-what pos) new-with-what)
            finally (return in-what)
            )))))




(defun insert-function 
       (
        ;; one of into, into-each, into-copy-of, into-copy-of-each
        mode 
        target
        each?
        stuff-to-be-inserted
        before-or-after
        before-or-after-position
        after-end?
        error-mode
        case-mode
        &aux
        (*bbl-current-case-mode* case-mode)
        )

  (block exit  

    (when (equal error-mode :strict)
      (check-insert-for-errors 
       mode target each? stuff-to-be-inserted before-or-after
       before-or-after-position after-end?))
  
    (when (equal mode :into-copy-of)
      (setf target (utils::copy-seq target)))
  
    ;; here were checking for 1:1
    (when (and (or (equal mode :into-copy-of-each) (equal mode :into-each))
               each?)
      (return-from exit
        (loop
         for little-target in target
         for little-stuff in stuff-to-be-inserted
         collect
         (insert-function
          (ecase mode 
            (:into-copy-of-each :into-copy-of)
            (:into-each :into))
          little-target nil little-stuff before-or-after
          before-or-after-position after-end? error-mode case-mode
          ))))

    (when (equal mode :into-each)
      (return-from exit
        (loop for thing in target
              collect
              (insert-function
               :into thing each? stuff-to-be-inserted before-or-after
               before-or-after-position after-end? error-mode case-mode))))
  
    (when (equal mode :into-copy-of-each)
      (return-from exit
        (loop for thing in target
              collect
              (insert-function
               :into-copy-of thing each? stuff-to-be-inserted before-or-after
               before-or-after-position after-end? error-mode case-mode))))
    
    ;; at this point the mode must be :into
    (when each?
      (return-from exit
        (loop for little-stuff in stuff-to-be-inserted
              collect
              (insert-function
               :into (copy-seq target) nil little-stuff before-or-after 
               before-or-after-position after-end? error-mode case-mode
               ))))
      

  
    (when after-end?
      (setq before-or-after :after)
      (setq before-or-after-position (length target)))
  
    (when (equal before-or-after-position *end*)
      (setq before-or-after-position (length target)))
  
    (when (numberp before-or-after-position)
      (when (< before-or-after-position 1)
        (setq before-or-after-position 1))
      (when (> before-or-after-position (length target))
        (setq before-or-after-position (length target))))

    (typecase target
      (list 
       (handle-list-insert 
        mode target each? stuff-to-be-inserted before-or-after
        before-or-after-position after-end? error-mode case-mode))
      (string
       (handle-string-insert 
        mode target each? stuff-to-be-inserted before-or-after
        before-or-after-position after-end? error-mode case-mode))
      (otherwise 
       (error "Can't insert into something that isn't a string or list"))
      )))

(defun handle-list-insert
       (mode target each? stuff-to-be-inserted before-or-after
             before-or-after-position after-end? error-mode case-mode)
  (if (not (integerp before-or-after-position))
      (list-position-isnt-number 
       mode target each? stuff-to-be-inserted before-or-after
       before-or-after-position after-end? error-mode case-mode)
    (list-position-is-number
     mode target each? stuff-to-be-inserted before-or-after
     before-or-after-position after-end? error-mode case-mode
     )))


(defun list-position-isnt-number
       (mode target each? stuff-to-be-inserted before-or-after
             before-or-after-object after-end? error-mode case-mode)
  (declare (ignore mode each? after-end? error-mode case-mode))
  (let ((pos (search (list before-or-after-object) target 
                     :test *bbl-replace-list-comparison-function*)))
    (when (null pos)
      (return-from list-position-isnt-number target))
    (when (equal before-or-after :before)
      (let ((before (subseq target 0 pos))
            (middle (list stuff-to-be-inserted))
            (end (subseq target pos)))
        (setq target (nconc before middle end))))
    (when (equal before-or-after :after)
      (let ((before (subseq target 0 (1+ pos)))
            (middle (list stuff-to-be-inserted))
            (end (subseq target (1+ pos))))
        (setq target (nconc before middle end))))
    target
    ))

(defun list-position-is-number
       (mode target each? stuff-to-be-inserted before-or-after
               before-or-after-position after-end? error-mode case-mode)
  (declare (ignore mode each? after-end? error-mode case-mode))
  (when (null target)
    (return-from list-position-is-number (list stuff-to-be-inserted)))
  (when (equal before-or-after :before)
    (let ((before (subseq target 0 (1- before-or-after-position)))
          (middle (list stuff-to-be-inserted))
          (end (subseq target (1- before-or-after-position))))
      (setq target (append before middle end))))
  (when (equal before-or-after :after)
    (let ((before (subseq target 0 before-or-after-position))
          (middle (list stuff-to-be-inserted))
          (end (subseq target before-or-after-position)))
      (setq target (append before middle end))))
  target
  )

(defun handle-string-insert
       (mode target each? stuff-to-be-inserted before-or-after
               before-or-after-position after-end? error-mode case-mode)
  (if (not (integerp before-or-after-position))
      (string-position-isnt-number 
       mode target each? stuff-to-be-inserted before-or-after
       before-or-after-position after-end? error-mode case-mode)
    (string-position-is-number
     mode target each? stuff-to-be-inserted before-or-after
     before-or-after-position after-end? error-mode case-mode
     )))

(defun string-position-isnt-number 
       (mode target each? stuff-to-be-inserted before-or-after
               before-or-after-object after-end? error-mode case-mode)
  (declare (ignore mode each? after-end? error-mode))
  (when (characterp before-or-after-object)
    (setq before-or-after-object (string before-or-after-object)))
  (let* ((test-mode 
          (ecase case-mode
            (:case-sensitive 'char=)
            (:case-insensitive 'char-equal)))
         (pos (search before-or-after-object target :test test-mode)))
    (when (null pos)
      (return-from string-position-isnt-number target))
    (when (equal before-or-after :before)
      (let ((before (subseq target 0 pos))
            (middle stuff-to-be-inserted)
            (end (subseq target pos)))
        (setq target (utils::s+ before middle end))))
    (when (equal before-or-after :after)
      (let ((before (subseq target 0 (+ pos (length before-or-after-object))))
            (middle stuff-to-be-inserted)
            (end (subseq target (+ pos (length before-or-after-object)))))
        (setq target (utils::s+ before middle end))))
    target
    ))


(defun string-position-is-number 
       (mode target each? stuff-to-be-inserted before-or-after
             before-or-after-position after-end? error-mode case-mode)
  (declare (ignore mode each? after-end? error-mode case-mode))
  (if (equal target "")
      (copy-seq (string stuff-to-be-inserted))
    (cond
     ((equal before-or-after :before)
      (let ((before (subseq target 0 (1- before-or-after-position)))
            (middle stuff-to-be-inserted)
            (end (subseq target (1- before-or-after-position))))
        (utils::s+ before middle end)))
     ((equal before-or-after :after)
      (let ((before (subseq target 0 before-or-after-position))
            (middle stuff-to-be-inserted)
            (end (subseq target before-or-after-position)))
        (utils::s+ before middle end)))
     (t (error "Internal error."))
     )))

(defun check-insert-for-errors
       (mode target each? stuff-to-be-inserted before-or-after
             before-or-after-position after-end?)
  (declare (ignore stuff-to-be-inserted each? mode))  
  (when (and (or before-or-after before-or-after-position) after-end?)
    (error 
     (one-string-nl
      "You cannot specify both AFTER-END and a BEFORE or AFTER position"
      "You specified both AFTER-END and and ~A ~D")
     before-or-after before-or-after-position
     ))
  (when (and (numberp before-or-after-position))
    (when (or (< before-or-after-position 1)
              (> before-or-after-position (length target)))
      (error
       (one-string-nl
        "You specified ~A ~D.  Please specify a number between"
        "one and the length of the sequence, which is ~D")
       before-or-after before-or-after-position (length target))))
  )


#||
   (INSERT (INTO | INTO-EACH | INTO-COPY-OF | INTO-COPY-OF-EACH) target
           (EACH) string-or-item
      plus required choice of one of the following:
           [BEFORE | AFTER | AFTER-END]
      plus other options:
           [RELAXED | STRICT]
           [CASE-SENSITIVE]
                 )
||#



(defun insert-keys (findings insert-args)
  (let ((before-data (arglist-key-and-data findings '(:before) insert-args))
        (after-data (arglist-key-and-data findings '(:after) insert-args))
        (each-data (arglist-key-and-data findings '(:each) insert-args))
        )
    (values 
     (first before-data) (second before-data)
     (first after-data) (second after-data)
     (first each-data) (second each-data)
     )))


(defmacro bbl::insert (&rest insert-args)
  "Insert something into something else"
  (let ((len (length insert-args))
        (mode nil)
        (target nil)
        (each? nil)
        (thing-to-insert nil)
        (before? nil)
        (after? nil)
        (after-end? nil)
        (where nil)
        (error-mode nil)
        (case-mode nil))
    (unless (plusp len) 
      (error "You must provide arguments to INSERT!"))
    (labels
        ((snarf-mode () 
           ;; same exact modes as for replace
           (if (member (first insert-args)
                       *possible-replace-modes* :test 'utils::symbol=)
               (progn
                 (unless (cdr insert-args)
                   (error "You must provide a target!"))
                 (setq mode (first insert-args))
                 (setq target (second insert-args))
                 (setq insert-args (cddr insert-args)))
             (progn
               (setq mode :into)
               (setq target (first insert-args))
               (pop insert-args))))
         (snarf-thing-to-insert () 
           (if (symbol= (first insert-args) :each)
               (progn
                 (setq each? t)
                 (setq thing-to-insert (second insert-args))
                 (setq insert-args (cddr insert-args)))
             (progn
               (setq each? nil)
               (setq thing-to-insert (pop insert-args))
               )))
         (snarf-where ()
           (cond
            ((symbol= (first insert-args) :before)
             (setq before? t)
             (setq where (second insert-args))
             (setq insert-args (cddr insert-args)))
            ((symbol= (first insert-args) :after)
             (setq after? t)
             (setq where (second insert-args))
             (setq insert-args (cddr insert-args)))
            ((symbol= (first insert-args) :after-end)
             (setq after-end? t)
             (setq where nil)
             (setq insert-args (cdr insert-args)))
            (t (error "Must specify where insert is to take place!"))
            ))
         (verify-flags ()
           (let ((n-possible-flags (length *possible-insert-flags*)))
             (unless (<= (length insert-args) n-possible-flags)
               (error "Too many flags in INSERT!"))
             (loop for flags in *possible-insert-flags* 
                   with count = 0
                   do
                   (loop for flag in flags
                         do 
                         (when (find flag insert-args :test 'symbol=)
                           (incf count)
                           (return)
                           ))
                   finally 
                   (unless (= count (length insert-args))
                     (error "Invalid flag in INSERT!")
                     ))))
         (maybe-oops (missing-stuff)
           (unless insert-args
             (error (s+ "You must provide " missing-stuff "!"))))
         (insert-place? (x) (and (symbolp x) (not (constantp x))))
         )
      
      (snarf-mode)
      (maybe-oops "an insert mode")
      (snarf-thing-to-insert)
      (maybe-oops "something to insert")
      (snarf-where)
      (verify-flags)
      (setq error-mode 
            (or (find :strict insert-args :test 'symbol=)
                (find :relaxed insert-args :test 'symbol=)
                ))
      (setq case-mode 
            (if (find :case-sensitive insert-args :test 'symbol=)
                :case-sensitive 
              :case-insensitive))

      (let ((insert-call 
             `(LET ((*bbl-level* (INCREMENT-BBL-LEVEL)))
	           (insert-function 
               ,(utils::keywordize mode) 
               ,target
               ,each?
               ,thing-to-insert
               ,(cond
                 (before? :before)
                 (after? :after)
                 (t nil))
               ,where
               ,after-end?
               ,(if error-mode (utils::keywordize error-mode) nil)
               ,(utils::keywordize case-mode)
               ))))
        (if (and (symbol= mode :into) (insert-place? target) (null each?))
            `(setf ,target ,insert-call)
          insert-call
          )))))
