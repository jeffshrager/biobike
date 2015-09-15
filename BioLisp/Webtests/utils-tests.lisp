;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: weblistener; -*-

;;; Author:  JP Massar. 

(in-package :weblistener)

(defutiltest memoize-1 
  (memoized-distance-test-function 2 2) 
  (distance-test-function 2 2)
  :comparison #'=)

(defutiltest memoize-2 
  (memoized-distance-test-function 2 2) 
  (distance-test-function 2 2)
  :comparison #'=)

(defutiltest memoize-3 
  (memoized-distance-test-function 3 4) 
  (distance-test-function 3 4)
  :comparison #'=)

(defutiltest memoize-4
  (memoized-distance-test-function 3 4) 
  (distance-test-function 3 4)
  :comparison #'=)

(defutiltest memoize-1-arg-1 
  (1-arg-test-function 9) 
  3)

(defutiltest memoize-1-arg-2 
  (1-arg-test-function 9) 
  3)

(defutiltest one-string-1
  (let ((x "x") (y "y")) (one-string x y "zz" y "plugh")) 
  "xyzzyplugh"
  :comparison 'string=)

(defutiltest mvsetf1
  (let ((x (make-array 10)) (y 5) (z (make-hash-table)))
    (mvsetf ((aref x 0) y (gethash :fred z)) (values 1 2 3))
    (list (aref x 0) y (gethash :fred z)))
  (list 1 2 3)
  :comparison 'equal)

(defutiltest insert-into-ordered-list-1
  (insert-into-ordered-list 5 '< (copy-list '(1 4 7 10))) '(1 4 5 7 10)
  :comparison 'equal)
(defutiltest insert-into-ordered-list-2
  (insert-into-ordered-list 5 '< (copy-list '(7 10))) '(5 7 10)
  :comparison 'equal)
(defutiltest insert-into-ordered-list-3
  (insert-into-ordered-list 5 '< (copy-list '(1 4))) '(1 4 5)
  :comparison 'equal)
(defutiltest insert-into-ordered-list-3
  (insert-into-ordered-list 5  '< nil) '(5)
  :comparison 'equal)

(defutiltest flatten-1 (flatten '(((a) (((b)))))) '(a b)
  :comparison 'equal)

(defutiltest mapcarnn-1
  (mapcarnn 'identity '(a nil b c 1 nil)) '(a b c 1)
  :comparison 'equal)
(defutiltest mapcarnn-4
  (mapcarnn 
   (lambda (a b c d) (and a b c d))
   '(1 2 3 4) '(nil 2 3 4) '(x y z nil) '(foo bar baz quux))
  '(bar baz)
  :comparison 'equal)
                   
(defutiltest string-split-1 
  (string-split "This is a test") '("This" "is" "a" "test")
  :comparison 'equal)
(defutiltest string-split-2
  (string-split "" #\x) '("")
  :comparison 'equal)
(defutiltest string-split-3
  (string-split "This is a test" #\t) '("This is a " "es" "")
  :comparison 'equal)
(defutiltest simple-string-split-1
  (simple-string-split "A B C  ") '("A" "B" "C" "" "")
  :comparison 'equal)

(defutiltest string-join-1
  (string-join '("Bob" "Ted" "Mary" "Alice") " and ")
  "Bob and Ted and Mary and Alice"
  :comparison 'string=)
(defutiltest string-join-2
  (string-join '("Bob" "Ted" "Mary" "Alice") "") "BobTedMaryAlice"
  :comparison 'string=)
(defutiltest string-join-3
  (string-join '("Bob") "Fred") "Bob"
  :comparison 'string=)

(defutiltest s+1 
  (s+ "a" "b" "c")
  "abc"
  :comparison 'string=)

(defutiltest s+2
  (let ((x (mapcar
            'anything-stringlike-to-string
            (iota call-arguments-limit))))
    (plusp (length (eval `(s+ ,@x)))))
  t
  )

(defutiltest s+3 (s+) "" :comparison 'string=)

(defutiltest s+4 (s+ #\Space) " " :comparison 'string=)

(defutiltest s+5 (s+ :foo :bar) "FOOBAR" :comparison 'string=)

(defutiltest ntranslate-1
  (ntranslate-string (copy-seq "0123456789") "9876543210" "876543210-")
  "-012345678"
  :comparison 'string=)
(defutiltest ntranslate-2
  (ntranslate-string
   (copy-seq
    (one-string
     "This IS SOME TEXT TO LOWERCASE !!! It has to be 100 characters."
     "This IS SOME MORE TEXT TO LOWERCASE !!!  FOR THE TEST TO TEST"
     "the hash table algorithm for NTRANSLATE-2."))
   "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
   "abcdefghijklmnopqrstuvwxyz")
  (one-string
   "this is some text to lowercase !!! it has to be 100 characters."
   "this is some more text to lowercase !!!  for the test to test"
   "the hash table algorithm for ntranslate-2.")
  :comparison 'string=)

(defutiltest limited-string-1 (limited-string "1234567890" 5) "12345..."
  :comparison 'string=)

(defutiltest set-equal-1 
  (set-equal '("ab" "cd" "ef" "gh" "ij")
             '("IJ" "CD" "EF" "GH" "AB")
             :test 'equalp
             :hash-threshold 10)
  t)
(defutiltest set-equal-2
  (set-equal '("ab" "cd" "ef" "gh" "ij")
             '("IJ" "CD" "EF" "GH" "XB")
             :test 'equalp
             :hash-threshold 10)
  nil)
(defutiltest set-equal-3
  (set-equal '("ab" "cd" "ef" "gh" "ij")
             '("IJ" "CD" "EF" "GH" "AB")
             :test 'equalp
             :hash-threshold 100)
  T)         

(defutiltest binsearch-1 
  (binsearch 3 #(0 1 2 3 4 5 6 7 8 9 0)) 3)
(defutiltest binsearch-2
  (binsearch #\X (reverse "abcdefghijklmnopqrstuvwxyz0123456789")
             :test 'char-greaterp :start 5)
  12)

(defutiltest correlate-1
  (correlate '(0 1 2 3 4 5) '(1 2 3 4 5 6)) '(:r 1.0 :r2 1.0)
  :comparison 'equal)                  
(defutiltest correlate-2
  (correlate-fast '(0 1 2 3 4 5) '(1 2 3 4 5 6)) '(:r 1.0 :r2 1.0)
  :comparison 'equal)

(defutiltest purge-duplicates-1
  (purge-duplicates '("a" "A" "B" "b") :test 'string-equal)
  '("a" "B")
  :comparison 'equal)
(defutiltest purge-duplicates-2
  (purge-duplicates '("a" "A" "B" "b") 
                    :test 'string-equal :hash-threshold 2)
  '("a" "B")
  :comparison 'equal)

(defutiltest purge-duplicates-3
  (purge-duplicates 
   '((1 "abc") (2 "def") (3 "ABC") (4 "Def") 
     (5 "foo") (6 "fooz") (7 "ABc"))
   :test 'string-equal :key 'cadr)
  '((1 "abc") (2 "def") (5 "foo") (6 "fooz"))       
  :comparison 'equal)
(defutiltest purge-duplicates-4
  (purge-duplicates 
   '((1 "abc") (2 "def") (3 "ABC") (4 "Def") 
     (5 "foo") (6 "fooz") (7 "ABc"))
   :test 'string-equal :key 'cadr :hash-threshold 2)
  '((1 "abc") (2 "def") (5 "foo") (6 "fooz"))       
  :comparison 'equal)

(defutiltest find-duplicates-1
  (find-duplicates '(1 2 1 3 1 2))
  '(1 1 2)
  :comparison 'equal)
(defutiltest find-duplicates-2
  (find-duplicates '(1 2 1 3 1 2) :hash-threshold 2)
  '(1 1 2)
  :comparison 'equal)

(defutiltest find-duplicates-3
  (find-duplicates '(1 2 1 3 1 2) :return-exactly-one-duplicate? t)
  '(1 2)
  :comparison 'equal)
(defutiltest find-duplicates-4
  (find-duplicates '(1 2 1 3 1 2) 
                   :hash-threshold 2 :return-exactly-one-duplicate? t)
  '(1 2)
  :comparison 'equal)
(defutiltest find-duplicates-5
  (find-duplicates '("a" "A" "B" "b" "c") :test 'string-equal)
  '("A" "b")
  :comparison 'equal)
(defutiltest find-duplicates-6
  (find-duplicates '("a" "A" "B" "b" "c") 
                   :test 'string-equal :hash-threshold 2)
  '("A" "b")
  :comparison 'equal)
(defutiltest find-duplicates-7
  (find-duplicates '((1 1) (2 2) (3 3) (4 2)) :key 'cadr)
  '((4 2))
  :comparison 'equal)
(defutiltest find-duplicates-8
  (find-duplicates '((1 1) (2 2) (3 3) (4 2)) 
                   :key 'cadr :hash-threshold 2)
  '((4 2))
  :comparison 'equal)
(defutiltest find-duplicates-9
  (find-duplicates '((1 1) (2 2) (3 3) (4 4)) :key 'cadr)
  nil
  :comparison 'equal)
(defutiltest find-duplicates-0
  (find-duplicates '((1 1) (2 2) (3 3) (4 4)) 
                   :key 'cadr :hash-threshold 2)
  nil
  :comparison 'equal)

(defutiltest compile-word-1 
  (compile-word "antecedent") (compile-word "antecedent")
  :comparison 'equal)
(defutiltest compile-word-2 (compile-word "x") (compile-word "x")
  :comparison 'equal)

(defutiltest score-homology-1 
  (score-homology "xyzzy and plugh" "magic words and dragons")
  (score-homology "xyzzy and plugh" "magic words and dragons")
  :comparison 'equal)

(defutiltest score-homology-2 (score-homology "xyzzy" "xyzzy") 1.0  
  :comparison 'equal)

(defutiltest create-hash-table-1 
  (sort
   (ht-contents (create-hash-table '((a . b) (c . d)) :mode :dotted-pair))
   (lambda (x y) (string-lessp (string x) (string y)))
   :key 'car)
  '((a b) (c d))
  :comparison 'equal)

(defutiltest create-hash-table-2
  (sort
   (ht-contents 
    (create-hash-table '(a b c d) :mode :singleton :default-value 3))
   (lambda (x y) (string-lessp (string x) (string y)))   
   :key 'car)
  '((a 3) (b 3) (c 3) (d 3))
  :comparison 'equal)

(defutiltest intersection-size-1
  (intersection-size '(a b c d) '(d c x y)) 2)

(defutiltest intersection-size-2
  (intersection-size
   '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28)
   '(822 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2)
   :hash-threshold 10000)
  17)

;;; This one tests the hash

(defutiltest intersection-size-3
  (intersection-size
   '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28)
   '(822 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2))
  17)

(defutiltest intersection-size-4
  (intersection-size
   '(("a" "b") ("c" "d")) '(("C" "foo")) :key 'car :test 'string-equal)
  1)

(defutiltest intersection-size-5
  (intersection-size
   '(("a" "b") ("c" "d")) '(("C" "foo") ("D" "a")) 
   :key 'car :test 'string-equal :hash-threshold 1)
  1)

(defutiltest union-size-1
  (union-size '(a b c d) '(d c x y)) 6)

(defutiltest union-size-2
  (union-size
   '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28)
   '(-1 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 -2)
   :hash-threshold 10000)
  30)

;;; This one tests the hash

(defutiltest union-size-3
  (union-size
   '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28)
   '(-1 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 -2))
  30)

(defutiltest union-size-4
  (union-size
   '(("a" "b") ("c" "d")) '(("C" "foo")) :key 'car :test 'string-equal)
  2)

(defutiltest union-size-5
  (union-size
   '(("a" "b") ("c" "d")) '(("C" "foo") ("D" "a")) 
   :key 'car :test 'string-equal :hash-threshold 1)
  3)




;; begin pred
(tests:deftest coll/proc-1
         (collect-and-process-subsequences 
          '(1 2 3 4 5 6 7 8 9 0) 'oddp nil 'r+)
         '(3 7 11 15 9)
         :chapter :utils :comparison 'equal)
;; end pred
(tests:deftest coll/proc-2
         (collect-and-process-subsequences 
          '(1 2 3 4 5 6 7 8 9 0) nil 'oddp 'r+)
         '(1 5 9 13 17)
         :chapter :utils :comparison 'equal)
;; begin and end preds
(tests:deftest coll/proc-3
         (collect-and-process-subsequences 
          '(1 2 3 4 5 6 7 8 9 0) 'oddp  'squarep 'r+)
         '(6 26)
         :chapter :utils :comparison 'equal)
;; begin and end preds non-exclusive
(tests:deftest coll/proc-4
         (collect-and-process-subsequences 
          '(1 2 3 4 5 6 7 8 9 0) 'oddp 'squarep 'r+ :exclusive? nil)
         '(10 35)
         :chapter :utils :comparison 'equal)
;; no beginning found
(tests:deftest coll/proc-5
         (collect-and-process-subsequences 
          '(1 2 3 4 5 6 7 8 9 0) 'floatp 'squarep 'r+ :exclusive? nil)
         nil
         :chapter :utils :comparison 'equal)
;; identity non-exclusive, groups into two element sublists
(tests:deftest coll/proc-6
         (collect-and-process-subsequences 
          '(1 2 3 4 5 6 7 8 9) 'identity 'identity 'identity :exclusive? nil)
         '((1 2) (3 4) (5 6) (7 8) (9))
         :chapter :utils :comparison 'equal)
;; single element, begin
(tests:deftest coll/proc-7
         (collect-and-process-subsequences 
          '(1) 'oddp nil 'identity :exclusive? nil)
         '((1))
         :chapter :utils :comparison 'equal)



;; begin pred
(tests:deftest coll/proc-1v
         (collect-and-process-subsequences 
          "this is some   text to   tokenize  " 
          'stoken? nil 'string-upcase :predicate-mode :binary)
         '("THIS " "IS " "SOME   " "TEXT " "TO   " "TOKENIZE  ")
         :chapter :utils :comparison 'equal)
;; begin and end preds
(tests:deftest coll/proc-2v
         (collect-and-process-subsequences 
          "this is some   text to   tokenize  " 
          'stoken? 'etoken?
          'string-upcase :predicate-mode :binary)
         '("THIS" "IS" "SOME" "TEXT" "TO" "TOKENIZE")
         :chapter :utils :comparison 'equal)
;; begin and end preds non-exclusive
(tests:deftest coll/proc-3v
         (collect-and-process-subsequences 
          "this is some   text to   tokenize  " 
          'stoken? 'etoken?
          'string-upcase :predicate-mode :binary :exclusive? nil)
         '("THIS " "IS " "SOME " "TEXT " "TO " "TOKENIZE ")
         :chapter :utils :comparison 'equal)
;; random
(tests:deftest coll/proc-4v
         (collect-and-process-subsequences 
          "this is some   text to   tokenize  " 
          (lambda (seq i) (eql (aref seq i) #\s)) nil
          'string-upcase :predicate-mode :binary)
         '("S I" "S " "SOME   TEXT TO   TOKENIZE  ")
         :chapter :utils :comparison 'equal)
;; no beginning
(tests:deftest coll/proc-5v
         (collect-and-process-subsequences 
          "this is some   text to   tokenize  " 
          (lambda (seq i) (eql (aref seq i) #\$)) nil
          'string-upcase :predicate-mode :binary)
         nil
         :chapter :utils :comparison 'equal)
;; no end
(tests:deftest coll/proc-6v
         (collect-and-process-subsequences 
          "this is some   text to   tokenize  " 
          nil (lambda (seq i) (eql (aref seq i) #\$))
          'string-upcase :predicate-mode :binary)
         '("THIS IS SOME   TEXT TO   TOKENIZE  ")
         :chapter :utils :comparison 'equal)
;; A single token, begin and end
(tests:deftest coll/proc-7v
         (collect-and-process-subsequences 
          "this" 'stoken? 'etoken? 'string-upcase :predicate-mode :binary)
         '("THIS")
         :chapter :utils :comparison 'equal)
;; A single token, begin
(tests:deftest coll/proc-8v
         (collect-and-process-subsequences 
          "this" 'stoken? nil 'string-upcase :predicate-mode :binary)
         '("THIS")
         :chapter :utils :comparison 'equal)
;; A single token, end
(tests:deftest coll/proc-9v
         (collect-and-process-subsequences 
          "this" nil 'etoken? 'string-upcase :predicate-mode :binary)
         '("THIS")
         :chapter :utils :comparison 'equal)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defutiltest= range0 (subranges (iota 5) 1 2) '(1 2))
(defutiltest= range1 (subranges (iota 5) '(1 4)) '(1 2 3 4))
(defutiltest= range2 (subranges (iota 10) '(1 3) 5 '(8 9)) '(1 2 3 5 8 9))
(defutiltest= range3 (subranges (iota 10) '(3 1) 5 '(9 8)) '(3 2 1 5 9 8))
(defutiltest= range4 (subranges (iota 10)) nil)
(defutiltest= range5 (subranges (iota 10) '(1 1) 7 5 '(1 2) '(1 1))
  '(1 7 5 1 2 1))

(defutiltest= range6 (subranges "abcdefghijkl" 1 2) "bc")
(defutiltest= range7 (subranges "abcdefghijkl" '(1 4)) "bcde")
(defutiltest= range8 (subranges "abcdefghijkl" '(1 3) 5 '(8 9)) "bcdfij")
(defutiltest= range9 (subranges "abcdefghijkl" '(3 1) 5 '(9 8)) "dcbfji")
(defutiltest= range10 (subranges "abcdefghijkl") "")
(defutiltest= range11 (subranges "abcdefghijkl" '(1 1) 7 5 '(1 2) '(1 1))
  "bhfbcb")

(defutiltest== range12 (subranges #(0 1 2 3 4 5 6 7 8 9) '(1 3) 5 '(8 9))
  #(1 2 3 5 8 9))
(defutiltest== range13 (subranges #(0 1 2 3 4 5 6 7 8 9) '(3 1) 5 '(9 8))
  #(3 2 1 5 9 8))



(defutiltest= srange0 (subranges-of (ilist 1 6) 1 2) '(1 2))
(defutiltest= srange1 (subranges-of (ilist 1 6) '(1 4)) '(1 2 3 4))
(defutiltest= srange2 (subranges-of (ilist 1 11) '(1 3) 5 '(8 9)) '(1 2 3 5 8 9))
(defutiltest= srange3 (subranges-of (ilist 1 11) '(3 1) 5 '(9 8)) '(3 2 1 5 9 8))
(defutiltest= srange4 (subranges-of (ilist 1 11)) nil)
(defutiltest= srange5 (subranges-of (ilist 1 11) '(1 1) 7 5 '(1 2) '(1 1))
  '(1 7 5 1 2 1))

(defutiltest= srange6 (subranges-of "abcdefghijkl" 1 2) "ab")
(defutiltest= srange7 (subranges-of "abcdefghijkl" '(1 4)) "abcd")
(defutiltest= srange8 (subranges-of "abcdefghijkl" '(1 3) 5 '(8 9)) "abcehi")
(defutiltest= srange9 (subranges-of "abcdefghijkl" '(3 1) 5 '(9 8)) "cbaeih")
(defutiltest= srange10 (subranges-of "abcdefghijkl") "")
(defutiltest= srange11 (subranges-of "abcdefghijkl" '(1 1) 7 5 '(1 2) '(1 1))
  "ageaba")

(defutiltest== srange12 (subranges-of #(1 2 3 4 5 6 7 8 9 10) '(1 3) 5 '(8 9))
  #(1 2 3 5 8 9))
(defutiltest== srange13 (subranges-of #(1 2 3 4 5 6 7 8 9 10) '(3 1) 5 '(9 8))
  #(3 2 1 5 9 8))

;; test setting subranges with a constant
(defutiltest= setf-range0 
  (let ((x (iota 5))) (setf (subranges x 3) 20) x)
  '(0 1 2 20 4))
(defutiltest= setf-range1
  (let ((x (iota 5))) (setf (subranges x '(1 3)) 4) x)
  '(0 4 4 4 4))
(defutiltest= setf-range2
  (let ((x (iota 5))) (setf (subranges x 1 3) 4) x)
  '(0 4 2 4 4))
(defutiltest= setf-range3
  (let ((x (iota 10))) (setf (subranges x '(4 6) 8 9) 25) x)
  '(0 1 2 3 25 25 25 7 25 25))
(defutiltest= setf-range4
  (let ((x (iota 10))) (setf (subranges x '(4 6) '(7 9)) 25) x)
  '(0 1 2 3 25 25 25 25 25 25))
(defutiltest= setf-range5
  (let ((x (iota 10))) (setf (subranges x '(0 0) '(1 9)) 25) x)
  '(25 25 25 25 25 25 25 25 25 25))
(defutiltest= setf-range6
  (let ((x (iota 10))) (setf (subranges x 6 3 1) 20) x)
  '(0 20 2 20 4 5 20 7 8 9))
(defutiltest= setf-range7
  (let ((x (iota 10))) (setf (subranges x '(6 4) '(3 2)) 20) x)
  '(0 1 20 20 20 20 20 7 8 9))
(defutiltest= setf-range8
  (let ((x (iota 10))) (setf (subranges x '(6 4) '(3 2) 0 '(7 9)) 40) x)
  '(40 1 40 40 40 40 40 40 40 40))

;; testing subranges with list of elements to set with
(defutiltest= setf-range9 
  (let ((x (iota 5))) (setf (subranges x 3) (list 20)) x)
  '(0 1 2 20 4))
(defutiltest= setf-range10
  (let ((x (iota 5))) (setf (subranges x '(1 3)) (list 20 21 22)) x)
  '(0 20 21 22 4))
(defutiltest= setf-range11
  (let ((x (iota 5))) (setf (subranges x 1 3) (list 20 21)) x)
  '(0 20 2 21 4))
(defutiltest= setf-range12
  (let ((x (iota 10))) (setf (subranges x '(4 6) 8 9) (ilist 20 25)) x)
  '(0 1 2 3 20 21 22 7 23 24))
(defutiltest= setf-range13
  (let ((x (iota 10))) (setf (subranges x '(4 6) '(7 9)) (ilist 90 96)) x)
  '(0 1 2 3 90 91 92 93 94 95))
(defutiltest= setf-range14
  (let ((x (iota 10))) (setf (subranges x '(0 0) '(1 9)) (ilist 10 20)) x)
  '(10 11 12 13 14 15 16 17 18 19))
(defutiltest= setf-range15
  (let ((x (iota 10))) (setf (subranges x 6 3 1) (list 99 98 97)) x)
  '(0 97 2 98 4 5 99 7 8 9))
(defutiltest= setf-range16
  (let ((x (iota 10))) (setf (subranges x '(6 4) '(3 2)) (ilist 20 25)) x)
  '(0 1 24 23 22 21 20 7 8 9))
(defutiltest= setf-range17
  (let ((x (iota 10)))
    (setf (subranges x '(6 4) '(3 2) 0 '(7 9)) (ilist 30 39)) x)
  '(35 1 34 33 32 31 30 36 37 38))

;; testing setting subranges of vectors with constants
(defutiltest== setf-range18
  (let ((x (coerce (iota 10) 'vector))) (setf (subranges x 3) 20) x)
  #(0 1 2 20 4 5 6 7 8 9))
(defutiltest== setf-range19
  (let ((x (coerce (iota 10) 'vector))) (setf (subranges x 1 3) 4) x)
  #(0 4 2 4 4 5 6 7 8 9))
(defutiltest== setf-range20
  (let ((x (coerce (iota 10) 'vector))) (setf (subranges x '(4 6) '(7 9)) 25) x)
  #(0 1 2 3 25 25 25 25 25 25))
(defutiltest== setf-range21
  (let ((x (coerce (iota 10) 'vector))) (setf (subranges x 6 3 1) 20) x)
  #(0 20 2 20 4 5 20 7 8 9))
(defutiltest== setf-range22
  (let ((x (coerce (iota 10) 'vector))) 
    (setf (subranges x '(6 4) '(3 2) 0 '(7 9)) 40) x)
  #(40 1 40 40 40 40 40 40 40 40))

;; testing setting subranges of vectors with lists of elements
(defutiltest== setf-range23
  (let ((x (coerce (iota 10) 'vector))) (setf (subranges x 3) (list 20)) x)
  #(0 1 2 20 4 5 6 7 8 9))
(defutiltest== setf-range24
  (let ((x (coerce (iota 10) 'vector))) (setf (subranges x 1 3) (list 20 21)) x)
  #(0 20 2 21 4 5 6 7 8 9))
(defutiltest== setf-range25
  (let ((x (coerce (iota 10) 'vector)))
    (setf (subranges x '(4 6) '(7 9)) (ilist 90 96)) x)
  #(0 1 2 3 90 91 92 93 94 95))
(defutiltest== setf-range26
  (let ((x (coerce (iota 10) 'vector)))
    (setf (subranges x 6 3 1) (list 99 98 97)) x)
  #(0 97 2 98 4 5 99 7 8 9))
(defutiltest== setf-range27
  (let ((x (coerce (iota 10) 'vector)))
    (setf (subranges x '(6 4) '(3 2) 0 '(7 9)) (ilist 30 39)) x)
  #(35 1 34 33 32 31 30 36 37 38))

;; testing setting subranges of vectors with vectors
(defutiltest= setf-range28 
  (let ((x (copy-seq "abcdefg")))
    (setf (subranges x '(6 0)) (copy-seq x)) x)
  "gfedcba")
(defutiltest= setf-range29
  (let ((x (copy-seq "abcdefg")))
    (setf (subranges x '(1 0) '(5 3)) "12345") x)
  "21c543g")

;; xxxxxxxxxxxxxxxxx

;; testing subranges-of with constants
(defutiltest= setf-srange0 
  (let ((x (iota 5))) (setf (subranges-of x 4) 20) x)
  '(0 1 2 20 4))
(defutiltest= setf-srange1
  (let ((x (iota 10))) (setf (subranges-of x '(5 7) 9 10) 25) x)
  '(0 1 2 3 25 25 25 7 25 25))
(defutiltest= setf-srange2
  (let ((x (iota 10))) (setf (subranges-of x 7 4 2) 20) x)
  '(0 20 2 20 4 5 20 7 8 9))

;; testing subranges-of with list of elements to set with
(defutiltest= setf-srange3
  (let ((x (iota 5))) (setf (subranges-of x 4) (list 20)) x)
  '(0 1 2 20 4))
(defutiltest= setf-srange4
  (let ((x (iota 10))) (setf (subranges-of x '(5 7) 9 10) (ilist 20 25)) x)
  '(0 1 2 3 20 21 22 7 23 24))
(defutiltest= setf-srange5
  (let ((x (iota 10))) (setf (subranges-of x 7 4 2) (list 99 98 97)) x)
  '(0 97 2 98 4 5 99 7 8 9))

;; testing setting subranges-of of vectors with constants
(defutiltest== setf-srange6
  (let ((x (coerce (iota 10) 'vector))) (setf (subranges-of x 4) 20) x)
  #(0 1 2 20 4 5 6 7 8 9))
(defutiltest== setf-srange7
  (let ((x (coerce (iota 10) 'vector))) (setf (subranges-of x 7 4 2) 20) x)
  #(0 20 2 20 4 5 20 7 8 9))
(defutiltest== setf-srange8
  (let ((x (coerce (iota 10) 'vector)))
    (setf (subranges-of x 2 4) (list 20 21)) x)
  #(0 20 2 21 4 5 6 7 8 9))
(defutiltest== setf-srange9
  (let ((x (coerce (iota 10) 'vector)))
    (setf (subranges-of x '(7 5) '(4 3) 1 '(8 10)) (ilist 30 39)) x)
  #(35 1 34 33 32 31 30 36 37 38))


;; testing setting subranges-of of vectors with vectors
(defutiltest= setf-srange10
  (let ((x (copy-seq "abcdefg")))
    (setf (subranges-of x '(7 1)) (copy-seq x)) x)
  "gfedcba")
(defutiltest= setf-srange11
  (let ((x (copy-seq "abcdefg")))
    (setf (subranges-of x '(2 1) '(6 4)) "12345") x)
  "21c543g")

;; testing overlap
(defutiltest= setf-srange12
  (let ((x (copy-seq "abcdefgh")))
    (setf (subranges-of x '(1 3) '(3 1) '(5 7) '(8 6)) "123456789xyz") x)
  "654d7zyx")
(defutiltest= setf-srange13
  (let ((x (ilist 1 9)))
    (setf (subranges-of x '(1 3) '(3 1) '(5 7) '(8 6)) (ilist 10 22)) x)
  '(15 14 13 4 16 21 20 19))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defutiltest ref0 (ref #(1 2 3) 1) 1)
(defutiltest cref0 (cref #(1 2 3) 1) 2)

(defutiltest ref1 (ref "abc" 1) "a" :comparison 'string=)
(defutiltest cref1 (cref "abc" 1) #\b)

(defutiltest ref2 (ref '(1 2 3) 1) 1)
(defutiltest cref2 (cref '(1 2 3) 1) 2)

(defutiltest ref2a (ref nil 3) nil)
(defutiltest cref2a (cref nil 3) nil)

(defutiltest ref3
  (ref (make-array '(2 2) :initial-contents '((1 2) (3 4))) 1 1)
  1)
(defutiltest cref3
  (cref (make-array '(2 2) :initial-contents '((1 2) (3 4))) 1 1)
  4)

(defutiltest ref4 (values (ref (create-hash-table '((a 1) (b 2))) 'a)) 1)
(defutiltest cref4 (values (cref (create-hash-table '((a 1) (b 2))) 'a 'b))
  (list 1 2)
  :comparison 'equal)

(defutiltest ref5 (ref "abcdefg" 1 3 5) "ace" :comparison 'string=)
(defutiltest cref5 (cref "abcdefg" 1 3 5) "bdf" :comparison 'string=)

(defutiltest ref6 (ref #(4 5 6 7 8) (list 1 3 2 4)) #(4 6 5 7) 
  :comparison 'equalp)
(defutiltest cref6 (cref #(4 5 6 7 8) (list 1 3 2 4)) #(5 7 6 8) 
  :comparison 'equalp)

(defutiltest ref7 (ref '(5 6 7 8 9) 1 3 5) '(5 7 9) :comparison 'equal)
(defutiltest cref7 (cref '(5 6 7 8 9) 0 2 4) '(5 7 9) :comparison 'equal)
(defutiltest ref7a (ref '(5 6 7 8 9) 5 1 3) '(9 5 7) :comparison 'equal)
(defutiltest cref7b (cref '(5 6 7 8 9) 4 0 2) '(9 5 7) :comparison 'equal)

(defutiltest ref8 (ref (create-hash-table '((a 1) (b 2) (c 4))) (list 'c 'b 'a))
  '(4 2 1) :comparison 'equal)
(defutiltest cref8 
  (cref (create-hash-table '((a 1) (b 2) (c 4))) (list 'c 'b 'a))
  '(4 2 1) :comparison 'equal)

(defutiltest= cref9 (cref (iota 20) 0 -> 3 6 -> 4)
  '(0 1 2 3 6 5 4))

(defutiltest= ref10 (ref "abcdefghijkl" -> 3 7 ->)
  "abcghijkl")

(defutiltest= cref11 (cref (iota 20) 0 -> 3 5 -> 7 9 10 18 ->)
  '(0 1 2 3 5 6 7 9 10 18 19))

(defutiltest sref0 
  (let ((v (vector 1 2 3))) (setf (ref v 3) 2) (ref v 3))
  2)
(defutiltest scref0 
  (let ((v (vector 1 2 3))) (setf (cref v 2) 2) (ref v 2))
  2)

(defutiltest sref1
  (let ((s (copy-seq "abcd"))) (setf (ref s 1) "q") (ref s 1))
  "q" :comparison 'string=)
(defutiltest sref1a
  (let ((s (copy-seq "abcd"))) (setf (ref s 1) #\q) (ref s 1))
  "q" :comparison 'string=)
(defutiltest scref1
  (let ((s (copy-seq "abcd"))) (setf (cref s 0) #\q) (cref s 0))
  #\q)


(defutiltest sref2
  (let ((v (list 1 2 3))) (setf (ref v 3) 2) (ref v 3))
  2)
(defutiltest scref2 
  (let ((v (list 1 2 3))) (setf (cref v 2) 2) (ref v 2))
  2)

(defutiltest sref2a
  (let ((v (list 1 2 3))) (setf (ref v 2 3) '(1 2)) v)
  '(1 1 2) :comparison 'equal)
(defutiltest scref2a
  (let ((v (list 1 2 3))) (setf (cref v 1 2) '(1 2)) v)
  '(1 1 2) :comparison 'equal)

(defutiltest sref2b
  (let ((v (list 1 2 3))) (setf (ref v (list 2 3)) '(1 2)) v)
  '(1 1 2) :comparison 'equal)
(defutiltest scref2b
  (let ((v (list 1 2 3))) (setf (cref v (list 1 2)) '(1 2)) v)
  '(1 1 2) :comparison 'equal)

(defutiltest sref3
  (let ((a (make-array '(2 2) :initial-contents '((1 2) (3 4)))))
    (setf (ref a 1 1) 5)
    (ref a 1 1))
  5)
(defutiltest scref3
  (let ((a (make-array '(2 2) :initial-contents '((1 2) (3 4)))))
    (setf (cref a 0 0) 5)
    (cref a 0 0))
  5)

(defutiltest sref4 
  (let ((h (create-hash-table '((a 1) (b 2) (c 3)))))
    (setf (ref h 'a 'c) #(9 10))
    (ref h 'a 'b 'c))
  '(9 2 10) 
  :comparison 'equal)
(defutiltest scref4 
  (let ((h (create-hash-table '((a 1) (b 2) (c 3)))))
    (setf (cref h 'a 'c) #(9 10))
    (cref h 'a 'b 'c))
  '(9 2 10) 
  :comparison 'equal)

(defutiltest sref4a 
  (let ((h (create-hash-table '((a 1) (b 2) (c 3)))))
    (setf (ref h (list 'a 'c)) #(9 10))
    (ref h 'a 'b 'c))
  '(9 2 10) 
  :comparison 'equal)
(defutiltest scref4a 
  (let ((h (create-hash-table '((a 1) (b 2) (c 3)))))
    (setf (cref h (list 'a 'c)) #(9 10))
    (cref h 'a 'b 'c))
  '(9 2 10) 
  :comparison 'equal)

(defutiltest sref5
  (let ((s (copy-seq "abcd"))) (setf (ref s 2 1) "qr") s)
  "rqcd" :comparison 'string=)
(defutiltest scref5
  (let ((s (copy-seq "abcd"))) (setf (cref s 1 0) "qr") s)
  "rqcd" :comparison 'string=)

(defutiltest sref5a
  (let ((s (copy-seq "abcd"))) (setf (ref s (list 2 1)) "qr") s)
  "rqcd" :comparison 'string=)
(defutiltest scref5a
  (let ((s (copy-seq "abcd"))) (setf (cref s (list 1 0)) "qr") s)
  "rqcd" :comparison 'string=)


(defutiltest= sref6 
  (let ((x (iota 20)))
    (setf (cref x 0 -> 3 6 -> 4) 22) x)
  '(22 22 22 22 22 22 22 7 8 9 10 11 12 13 14 15 16 17 18 19))


(defutiltest= sref7
  (let ((x "abcdefghijkl"))
    (setf (ref x -> 3 7 ->) "123456789") x)
  "123def456789")

(defutiltest= sref8 
  (let ((x (iota 20)))
    (setf (cref x 0 -> 3 5 -> 7 9 10 18 ->) 
          '(a b c d e f g h i j k l m n o p q)) x)
  '(a b c d 4 e f g 8 h i 11 12 13 14 15 16 17 j k))

(defutiltest= sref9
  (let ((x "abcdefg"))
    (setf (ref x 3 4 5) "z") x)
  "abzzzfg")

(defutiltest= sref10
  (let ((x "abcdefg"))
    (setf (ref x 3 4 5) #\a) x)
  "abaaafg")


(defutiltest= sref11
  (let ((x "abcdefg"))
    (setf (cref x 3 4 5) #\a) x)
  "abcaaag")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defutiltest iter1 
  (let* ((list (list 1 2 3 4 5)) (s (iter-init list)))
    (loop until (not (iter-next? s)) sum (iter-next s)))
  (reduce '+ (list 1 2 3 4 5)))

(defutiltest iter-on-1
  (let* ((list (list 1 2 3 4 5)) (s (iter-init (create-list-on-iterator list))))
    (loop until (not (iter-next? s)) sum (first (iter-next s))))
  (reduce '+ (list 1 2 3 4 5)))

(defutiltest iter-by-1
  (let* ((list (list 1 2 3 4 5)) 
         (s (iter-init (create-general-list-iterator list 'cddr nil))))
    (loop until (not (iter-next? s)) sum  (iter-next s)))
  9)

(defutiltest iter-by-on-1
  (let* ((list (list 1 2 3 4 5)) 
         (s (iter-init (create-general-list-iterator list 'cdddr t))))
    (loop until (not (iter-next? s)) sum (first (iter-next s))))
  5)

(defutiltest iter-by-on-2
  (let* ((v (vector 1 2 3 4 5)) 
         (s (iter-init v)))
    (loop until (not (iter-next? s)) sum  (iter-next s)))
  (reduce '+ (vector 1 2 3 4 5)))

(defutiltest iter-array-1
  (let* ((ss (make-array 
              10 :element-type 'character :adjustable t :initial-element #\a)) 
         (s (iter-init ss)))
    (loop until (not (iter-next? s)) sum (char-code (iter-next s))))
    (reduce
     '+
     (map 'list 'char-code
          (make-array 
           10 :element-type 'character :adjustable t :initial-element #\a))))

(defutiltest iter-string-1
  (let* ((ss "abcde") 
         (s (iter-init ss)))
    (loop until (not (iter-next? s)) sum (char-code (iter-next s))))
  (reduce '+ (map 'list 'char-code "abcde")))

(defutiltest iter-array-2
  (let* ((aa (make-array '(2 2) :initial-contents '((1 2) (3 4))))
         (s (iter-init aa)))
    (loop until (not (iter-next? s)) sum (iter-next s)))
  10)

(defutiltest iter-null-vector 
  (let* ((v #()) (s (iter-init v)))
    (loop until (not (iter-next? s)) sum (iter-next s)))
  0)

(defutiltest iter-range-1
  (let* ((range (create-range-iterator 3 15 4 t :up))
         (s (iter-init range)))
    (loop until (not (iter-next? s)) sum (iter-next s)))
  (+ 3 7 11 15))

(defutiltest iter-range-2
  (let* ((range (create-range-iterator 15 3 4 t :down))
         (s (iter-init range))) 
    (loop until (not (iter-next? s)) sum (iter-next s)))
  (+ 3 7 11 15))

(defutiltest iter-range-3
  (let* ((range (create-range-iterator 0 3 1 nil :up))
         (s (iter-init range))) 
    (loop until (not (iter-next? s)) sum (iter-next s)))
  3)
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defutiltest combinations-of-1
  (combinations-of 0 0)
  1
  )

(defutiltest combinations-of-2 
  (combinations-of 10 0)
  1
  )

(defutiltest combinations-of-3
  (combinations-of 3 2)
  3
  )

(defutiltest combinations-of-4
  (combinations-of 5 5)
  1
  )

(defutiltest combinations-of-5
  (combinations-of 5 1)
  5
  )

(defutiltest combinations-of-6
  (combinations-of 50 20)
  47129212243960
  )

(defutiltest integrate-numerically-1
  (values (round (integrate-numerically (lambda (x) x) 0 2 0.01)))
  2)

(defutiltest integrate-numerically-2
  (values (round (integrate-numerically (lambda (x) (* x x)) 0 3 0.01)))
  9
  )



