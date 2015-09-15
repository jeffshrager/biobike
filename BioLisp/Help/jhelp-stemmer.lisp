;;; -*- Package: help; mode: lisp; base: 10; Syntax: Common-Lisp; -*-
(in-package :help)
;;  Requires:  jhelp-structs.lisp.  That's about it.



;; !!! ONLY WORKS WHEN LIST IS NOT EMPTY!!!
(defun pull (item list) (nconc list (list item)))


;; Puts one word into the state-machine tree, indexed by its sequence of letters.
;; Note:  (list) is equivalent to () is equivalent to NIL, it tests to FALSE.  Watch yourself.
;; Also:  nconc with a nil list seed does not save the pointer, it gets lost as a new list.
;;  ...So as long as the current-assoc list is never empty, using nconc to suck onto the back should be destructive enough for you.

(defun insert-reversed-word-into-suffix-taster (reversed-word current-assoc)

    (loop for letter across reversed-word
          with newtree
          with isthere
        do

        (if (setq isthere (assoc letter current-assoc :test #'string-equal))

            ;yes, is there.  ASSOC RETURNS DOTTED PAIR, NOT CONTENTS.  MUST INDIRECT ONE MORE LEVEL.
            (setq newtree (cdr isthere))

          ;no, is not there.  Put it in.
          (progn   ; Here we have to give the index letter, along with a stub NON-EMPTY assoc list.
            (setq newtree (cons letter (list '(#\~ . #\-))))   ;  ( (L.( (~.~) )) )
            (pull newtree current-assoc)
            (setq newtree (cdr newtree))
            )
          )

        (setq current-assoc newtree)   ; Continue on.  Yes this could be put inside, but it's easier to understand/maintain this way.
  )
)



(defun find-stem (probe-word)
;;  Very primitive first-pass version.
;;  To do this for real, we'd really need a list of all the morphemes in English.  See Chompsky for something approaching this.
;;  Only handles SUFFIXES.
;; Returns the stem of the probe-word.  This is defined as:
;;   a string being the front of the word, after subtracting out the longest rear of the word that matches a known suffix; or
;;   NIL, if no known suffix matches.

;;  Current version is an industrial-grade character-parsing dictionary tree.  This is probably overkill.
;;  It uses assoc's for filing, which are O(13) per letter expected time in a 26-letter alphabet for the first three letters, way down after that.
;;  Will still handle unicode chars, but the expectation goes up from there.
;;  With 500-odd suffixes, it could be faster to do 8 brute-force searches on hashes next time...h'mmmm.
;
;  Cases:
;   No suffix at all,no match.
;   Suffix less than word, returns early.
;   Suffix length equal to word.
;   Suffix LONGER than word, we're still in the middle.
;   First half of suffix matches, but no second half.
;   First half of short suffix matches, longer suffix does not match.
;
  (let* ( ( reverse-probe-word (reverse probe-word)) 
         ( current-assoc  (stemmer-tree *JHelp-suffix-taster*)) 
         (newtree) 
         (isthere)
         (best-i-so-far 0)
         )
  (loop for letter across reverse-probe-word   for i from 0  do

      ;;  (format T "~a ? ~a: ~a~%~%" letter (assoc "*" current-assoc :test #'string-equal) current-assoc)

          (if  (assoc "*" current-assoc :test #'string-equal)  ;; Indicates win at PREVIOUS level.
              (progn
               ;; (format T "Win at previous level ~a.~%" i)
              (setq best-i-so-far i)      ;; This needs to be a constant fallback, as we may have garden path matches that fail.
                )
            )
        (if (setq isthere (assoc letter current-assoc :test #'string-equal))
            ;yes, is there.  ASSOC RETURNS DOTTED PAIR, NOT CONTENTS.  MUST INDIRECT ONE MORE LEVEL.
            (setq newtree (cdr isthere))
          ;no, is not there at all.  First character that's different. 
          ; Get index, return rest, reverse it.
          (progn

          (if  (> best-i-so-far 0)
              ;; Woohoo.  Win.
              (progn
                (return-from find-stem (reverse (subseq reverse-probe-word best-i-so-far)))
                )
            ;else, fail out
               (return-from find-stem NIL)
            )
            ); progn
          )
        (setq current-assoc newtree)   ; Continue on.  Yes this could be put inside, but it's easier to understand/maintain this way.
     
  ))
)

(defun init-jhelp-stemmer ()

  (setq *JHelp-suffix-taster*     
          (make-stemmer	:tree    (list '(#\~ . ~))   ;; we use assoc lists here.  Big O(26/2) hit for first letter, but faster than hash for the rest.

  ))
  ;; Note that equalp is agnostic w/r/t lower and upper case, whereas equal is strict.
  ;; So we don't need to smallify.    Fix this if this becomes significant for this application.

  (loop for reversed-word in *jhelp-reversed-English-Suffixes* 
        for count from 1
        for reversed-word* = (concatenate 'string reversed-word "*")
        do
          (insert-reversed-word-into-suffix-taster  reversed-word*  (stemmer-tree *JHelp-suffix-taster*) )
        finally (return count)
  )
)



(init-jhelp-stemmer)
