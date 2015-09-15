;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: user; -*-

;;; Author:  JP Massar.

(in-package :cl-user)

(defmacro emtest (n pattern text result-list &optional (just-first? nil))
  `(deftest ,(intern (format nil "EXACT-MATCH-~D" n))
            (exact-matches ,pattern ,text ,just-first?) ,result-list
            :comparison #'equal :chapter :match))

(emtest 1 "" "whatever" nil)
(emtest 2 "" "whatever" nil)
(emtest 3 "x" "xxxaaaxxx" '(1 2 3 7 8 9))
(emtest 4 "x" "this string has no x" '(20))
(emtest 5 "x" "really not here" nil)
(emtest 6 "abc" "xyzabcfoo" '(4))
(emtest 7 "abc" "xyzzyfoobarbaz" nil)
(emtest 8 "matching string" "matching string" '(1))
(emtest 9 "too long for you" "blarf and foo" nil)
(emtest 10 "aba" "xyzababaaaaba" '(4 6 11))
(emtest 11 "xxxx" "abcxxxxxx" '(4 5 6))
(emtest 12 "aaa" "aaaaafredaaaaa" '(1 2 3 10 11 12))
(emtest 13 "start" "start of a long string" '(1))
(emtest 14 "end" "after all this we come to the end" '(31))
(emtest 15 "middle" "in the middle of a string" '(8))
(emtest 16 "repeat" "a repeat a repeat" '(3 12))
;; JUST-FIRST? = T tests.
(emtest 17 "just first" "This is just first the just first" 9 t)
(emtest 18 "abc" "xyuzzy" nil t)


(defmacro stmtest (n pattern text result-list)
  `(deftest ,(intern (format nil "SUFFIX-TREE-MATCH-~D" n))
            (let ((st (naive-suffix-tree-build ,text)))
              (sort (st-match ,pattern st) #'<))
            ,result-list
            :comparison #'equal :chapter :match))

(stmtest 1 "abc" "abcdefg" '(1))
(stmtest 2 "abc" "xyzabcdefg" '(4))  
(stmtest 3 "abc" "xyzzyabc" '(6))
(stmtest 4 "abc" "zyabcabcefgabc" '(3 6 12))
(stmtest 5 "abc" "zyabcabcefgabc" '(3 6 12))
(stmtest 6 "blarf" "zyabcabcefgabc" nil)
(stmtest 7 "abcz" "zyabcabcefgabc" nil)
(stmtest 8 "a very long string" "a short string" nil)
(stmtest 9 "xyzzyzyx" "xyzzyzyx" '(1))
(stmtest 10 "z" "xyzzyzyx" '(3 4 6))
(stmtest 11 "z" "zzzzzz" '(1 2 3 4 5 6))
(stmtest 12 "att" "cgattcgatttaggcccaaaattcga" '(3 8 21))


(deftest edit-distance-1 (compute-edit-distance "FIREMAN" "POLICEMAN") 4
         :comparison #'= :chapter :match)
(deftest edit-distance-2
         (compute-edit-distance "If I were a rich man" "If I were a poor man")
         4 :comparison #'= :chapter :match)
(deftest edit-distance-3 (compute-edit-distance "xyzzy" "xyzzy") 0
         :comparison #'= :chapter :match)
(deftest edit-distance-4 (compute-edit-distance "" "xyzzy") 5
         :comparison #'= :chapter :match)

(defmacro nwtest (name a b common-alignment)
  (let ((gname (intern (concatenate 'string "NW-" (string name) "-GENERAL")))
        (lname (intern (concatenate 'string "NW-" (string name) "-LINEAR"))))
    `(progn
       (deftest ,gname (values (nw-common ',a ',b :general)) 
                ',common-alignment
                :comparison #'equal :chapter :match)
       (deftest ,lname (values (nw-common ',a ',b :linear-gap)) 
                ',common-alignment
                :comparison #'equal :chapter :match)
       )))

(nwtest identity
         (A C G T A C G T) (A C G T A C G T)
         ((A A) (C C) (G G) (T T) (A A) (C C) (G G) (T T)))
(nwtest one-gap-down-row
        (A C G A C G T) (A C G T A C G T)
        ((A A) (C C) (G G) (- T) (A A) (C C) (G G) (T T)))
(nwtest one-gap-across-col
        (A C G T A C G T) (A C G A C G T)
        ((A A) (C C) (G G) (T *) (A A) (C C) (G G) (T T)))
(nwtest two-gap-down-row
        (A C G C G T) (A C G T A C G T)
        ((A A) (C C) (G G) (- T) (- A) (C C) (G G) (T T)))
(nwtest two-gap-across-col
        (A C G T A C G T) (A C G C G T)
        ((A A) (C C) (G G) (T *) (A *) (C C) (G G) (T T)))
(nwtest double-gap-across-col
        (A C G T A C G T) (A G T A G T)
        ((A A) (C *) (G G) (T T) (A A) (C *) (G G) (T T)))
(nwtest one-gap-each
        (A C G T T G C A) (A G T T G A C A)
        ((A A) (C *) (G G) (T T) (T T) (G G) (- A) (C C) (A A)))
(nwtest down-row-shifted
        (C G T A C G T A C) (A C G T A C G T A)
        ((C C) (G G) (T T) (A A) (C C) (G G) (T T) (A A)))
(nwtest across-column-shifted
        (A C G T A C G T) (C G T A C G T A)
        ((C C) (G G) (T T) (A A) (C C) (G G) (T T)))
