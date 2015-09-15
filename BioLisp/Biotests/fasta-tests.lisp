;;; -*- Package: bioutils; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

;;; Author: JP Massar.

(in-package :bioutils)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Tests


;; Non-blank text w/o '>'
(tests:deftest fasta-parse-1
         (write-fasta-file-and-test-parse
          (list "This file" "has no records")) 0 :chapter :fasta)
;; No records found.
(tests:deftest fasta-parse-2
         (write-fasta-file-and-test-parse (list "   " "    "))
         0 :chapter :fasta)
;; Duplicate keys.
(tests:deftest fasta-parse-3
         (write-fasta-file-and-test-parse 
          (list ">key" "data" ">key" "data"))
         0 :chapter :fasta)
;; No text for key.
(tests:deftest fasta-parse-3.1
         (write-fasta-file-and-test-parse 
          (list ">" "data" ">key" "data"))
         0 :chapter :fasta)
;; Key does not immediately follow '>'
(tests:deftest fasta-parse-3.2
         (write-fasta-file-and-test-parse 
          (list "> key" "data" ">key" "data"))
         0 :chapter :fasta)

;; Warning about blank line.
(tests:deftest fasta-parse-4
         (write-fasta-file-and-test-parse 
          (list ">key1" "data" "" "more data" ">key2" "data"))
         1 :chapter :fasta)
;; Warn about no data.
(tests:deftest fasta-parse-5
         (write-fasta-file-and-test-parse 
          (list ">key1" "data" "more data" ">key2" ">key3" "key3 data"))
         1 :chapter :fasta)

;; Simple one-record correct file.
(tests:deftest fasta-parse-6
         (write-fasta-file-and-test-parse 
          (list ">key1" "data"))
         2 :chapter :fasta)
;; Two records with blanks at top correct parse.
(tests:deftest fasta-parse-7
         (write-fasta-file-and-test-parse 
          (list "" "   " ">key1" "data1" "data2" ">key2" "data1"))
         2 :chapter :fasta)

;; Test keys are parsed correctly
(tests:deftest fasta-parse-output-1
         (write-fasta-file-and-test-output
          (list ">xyzzy" "23" ">plugh and more stuff" "24"))
         (list "xyzzy" "plugh")
         :chapter :fasta :comparison 'equal)

(tests:deftest fasta-parse-output-2
         (write-fasta-file-and-test-output
          (list ">xyz" "23" "more data" 
                ">plugh-and-you" "and more stuff" "24"
                ">ggg" "a" "b" "c" "d" "e"))
         (list "xyz" "plugh-and-you" "ggg")
         :chapter :fasta :comparison 'equal)

;; Test start position computed correctly.
(tests:deftest fasta-parse-output-3
         (write-fasta-file-and-test-output
          (list ">xyz" "abc" ">abc" "xyz")
          :function
          #'(lambda (d) (fri-startfp (fasta-index-datum-recordinfo d))))
         (list 0 (1- (+ 7 (* 2 newline-file-position-increment) 1)))
         :chapter :fasta :comparison 'equal)

;; Test sequence length computed correctly.
(tests:deftest fasta-parse-output-4
         (write-fasta-file-and-test-output
          (list ">xyz" "ab" ">abc" "xyz" "defg" "x")
          :function
          #'(lambda (d) 
              (fri-sequence-length (fasta-index-datum-recordinfo d))))
         (list 2 (+ 3 4 1))
         :chapter :fasta :comparison 'equal)

;; Test data-linelen computed correctly.
(tests:deftest fasta-parse-output-5
         (write-fasta-file-and-test-output
          (list ">xyz" "ab" ">abc" "xyz" "def" "xyz" ">foo" "a" "bc" "def")
          :function
          #'(lambda (d) 
              (fri-data-linelen (fasta-index-datum-recordinfo d))))
         (list 2 3 nil)
         :chapter :fasta :comparison 'equal)

;; Test data line sizes computed correctly.
(tests:deftest fasta-parse-output-5
         (write-fasta-file-and-test-output
          (list ">xyz" "ab" ">abc" "qxyz" "def" "xyz" 
                ">foo" "a" "bc" "def" ">bar" "aa" "bb")
          :function
          #'(lambda (d) 
              (fri-data-line-sizes (fasta-index-datum-recordinfo d))))
         (list nil #(4 3 3) #(1 2 3) nil)
         :chapter :fasta :comparison 'equalp)

;; Test data records can be extracted from fasta file using index file,
;; after fasta file is parsed and index file created.
(tests:deftest fasta-extract-1
         (write-fasta-file-and-find-fasta-record
          '("a" "xyz" "blarfbag")
          (list ">xyz" "ab c" "def" 
                ">blarfbag" "a lot of stuff"
                ">a foo bar" "a" "bc" "defgh"))
          (list "abcdefgh" "ab cdef" "a lot of stuff")
         :chapter :fasta :comparison 'equalp)          

(tests:deftest fasta-extract-2
         (write-fasta-file-and-find-fasta-record
          '("a" "xyz" "blarfbag" "cowabunga")
          (list ">xyz" "abc" "def" 
                ">blarfbag"
                ">a foo bar" "a" "b" "c"))
          (list "abc" "abcdef" "" nil)
         :chapter :fasta :comparison 'equalp)

(tests:deftest fasta-extract-3
         (write-fasta-file-and-find-fasta-header
          '("a" "xyzzy")
          (list ">xyz" "foo" "barbaz"
                ">a header" "data" "more" "data"
                ">xyzzy" "the last record"))
         (list "a header" "xyzzy")
         :chapter :fasta :comparison 'equalp)
                


;; Test FIND-FASTA-SUBSEQUENCE.  Recall the START and END are 1-based
;; and inclusive.

;; Extract a single character.
(tests:deftest fasta-extract-3
         (write-fasta-file-and-find-fasta-subsequences
          '(("a" 1 1 :forward) ("a" 2 2 :forward) ("a" 10 10 :forward))
          (list ">a gene 'a'" "acgtacgt" "tgcatgca"))
         '("a" "c" "g") 
         :chapter :fasta :comparison 'list-string=)

;; Extract a sequence forward, no wrap.
(tests:deftest fasta-extract-6
         (write-fasta-file-and-find-fasta-subsequences
          '(("a" 1 4 :forward) ("a" 10 15 :forward) ("a" 6 10 :forward))
          (list ">a gene 'a'" "acgtacgt" "tgcatgca"))
         '("acgt" "gcatgc" "cgttg")
         :chapter :fasta :comparison 'list-string=)

;; Extract a sequence backward, no wrap.
(tests:deftest fasta-extract-9
         (write-fasta-file-and-find-fasta-subsequences
          '(("a" 6 10 :backward) ("a" 1 16 :backward) ("a" 1 1 :backward))
          (list ">a gene 'a'" "acgtacgt" "tgcatgca"))
         (list (reverse "cgttg") (reverse "acgtacgttgcatgca") (reverse "a"))
         :chapter :fasta :comparison 'list-string=)

;; Extract from multiple varying length record forward
(tests:deftest fasta-extract-12
         (write-fasta-file-and-find-fasta-subsequences
          '(("a" 10 10 :forward) ("a" 9 14 :forward) ("bigone" 13 14 :forward))
          (list ">b gene called bee" "foo" "barbaz"
                ">a gene 'a'" "acgtacgt" "tgca" "abcdefghij"
                ">bigone no description" "xyz" "xyzzy" "plover"))
         '("g" "tgcaab" "er")
         :chapter :fasta :comparison 'list-string=)

;; Extract from multiple varying length record backward
(tests:deftest fasta-extract-15
         (write-fasta-file-and-find-fasta-subsequences
          '(("a" 10 10 :backward)
            ("a" 9 14 :backward) 
            ("bigone" 13 14 :backward)
            ("b" 1 9 :backward))
          (list ">b gene called bee" "foo" "barbaz"
                ">a gene 'a'" "acgtacgt" "tgca" "abcdefghij"
                ">bigone no description" "xyz" "xyzzy" "plover"))
         (mapcar 'reverse '("g" "tgcaab" "er" "foobarbaz"))
         :chapter :fasta :comparison 'list-string=)

;; Extract with wrapping fixed length 
(tests:deftest fasta-extract-18
         (write-fasta-file-and-find-fasta-subsequences
          '(("a" 16 2 :forward) ("a" 8 7 :forward) ("a" 14 2 :backward))
          (list ">a gene 'a'" "acgtacgt" "tgcatgca"))
         (list "aac" "ttgcatgcaacgtacg"  (reverse "gcaac"))
         :chapter :fasta :comparison 'list-string=)

;; Extract with wrapping varying length 
(tests:deftest fasta-extract-21
         (write-fasta-file-and-find-fasta-subsequences
          '(("a" 22 3 :backward)
            ("a" 13 1 :forward) 
            ("bigone" 4 3 :backward)
            ("b" 3 1 :forward))
          (list ">b gene called bee" "foo" "barbaz"
                ">a gene 'a'" "acgtacgt" "tgca" "abcdefghij"
                ">bigone no description" "xyz" "xyzzy" "plover"))
         (list (reverse "jacg") "abcdefghija" 
               (reverse "xyzzyploverxyz") "obarbazf")
         :chapter :fasta :comparison 'list-string=)

(tests:deftest fasta-other-extension-1
         (write-fasta-file-and-find-fasta-subsequences
          '(("a" 16 2 :forward) ("a" 8 7 :forward) ("a" 14 2 :backward))
          (list ">a gene 'a'" "acgtacgt" "tgcatgca")
          :testfile *fasta-other-extension*)
         (list "aac" "ttgcatgcaacgtacg"  (reverse "gcaac"))
         :chapter :fasta :comparison 'list-string=)

(tests:deftest fasta-no-extension
         (write-fasta-file-and-find-fasta-subsequences
          '(("a" 16 2 :forward) ("a" 8 7 :forward) ("a" 14 2 :backward))
          (list ">a gene 'a'" "acgtacgt" "tgcatgca")
          :testfile *fasta-no-extension*)
         (list "aac" "ttgcatgcaacgtacg"  (reverse "gcaac"))
         :chapter :fasta :comparison 'list-string=)

(tests:deftest fasta-iterate
         (progn
           (write-test-strings-to-file 
            (fasta-test-file)
            '(">xyz foo" "bar" ">a" "b" ">blahblah" "blah"))
           (let ((key-list nil))
             (with-fasta-db (db (fasta-test-file))
               (with-fasta-db-keys (key db) (push key key-list))
               (setq key-list (sort key-list 'string<))
               key-list
               )))
         '("a" "blahblah" "xyz")
         :chapter :fasta :comparison 'list-string=)

(tests:deftest fasta-miscellaneous
         (progn
           (write-test-strings-to-file 
            (fasta-test-file)
            (list
             ">xyz foo" "bar" "bazaba" "foo"
             ">a" "b" "c" "d" 
             ">blah" "blah" "blah" "blah" "blahblah"))
           (with-fasta-db (db (fasta-test-file))
             (list (fasta-record-data-length db "xyz")
                   (fasta-record-data-length db "a")
                   (fasta-record-data-length db "blah")
                   (fasta-record-data-nlines db "xyz")
                   (fasta-record-data-nlines db "a")
                   (fasta-record-data-nlines db "blah")
                   (fasta-record-data-line-size db "xyz")
                   (fasta-record-data-line-size db "a")
                   (fasta-record-data-line-size db "blah")
                   (fasta-record-data-line-sizes db "xyz")
                   (fasta-record-data-line-sizes db "a")
                   (fasta-record-data-line-sizes db "blah")
                   (fasta-record-data-format db "xyz")
                   (fasta-record-data-format db "a")
                   (fasta-record-data-format db "blah")
                   (fasta-record-data-last-line-size db "xyz")
                   (fasta-record-data-last-line-size db "a")
                   (fasta-record-data-last-line-size db "blah"))))
         (list 12 3 20
               3 3 4
               nil 1 4
               #(3 6 3) nil nil
               :variable-width :fixed-width :fixed-width
               3 1 8
               )
         :chapter :fasta :comparison 'equalp)
                   
(tests:deftest fasta-miscellaneous-no-data
         (progn
           (write-test-strings-to-file 
            (fasta-test-file)
            (list
             ">xyz foo"
             ">a"
             ">blah" "blah" "blah" "blah" "blahblah"))
           (with-fasta-db (db (fasta-test-file))
             (list (fasta-record-data-length db "xyz")
                   (fasta-record-data-length db "a")
                   (fasta-record-data-length db "blah")
                   (fasta-record-data-nlines db "xyz")
                   (fasta-record-data-nlines db "a")
                   (fasta-record-data-nlines db "blah")
                   (fasta-record-data-line-size db "xyz")
                   (fasta-record-data-line-size db "a")
                   (fasta-record-data-line-size db "blah")
                   (fasta-record-data-line-sizes db "xyz")
                   (fasta-record-data-line-sizes db "a")
                   (fasta-record-data-line-sizes db "blah")
                   (fasta-record-data-format db "xyz")
                   (fasta-record-data-format db "a")
                   (fasta-record-data-format db "blah")
                   (fasta-record-data-last-line-size db "xyz")
                   (fasta-record-data-last-line-size db "a")
                   (fasta-record-data-last-line-size db "blah"))))
         (list 0 0 20
               0 0 4
               0 0 4
               nil nil nil
               :fixed-width :fixed-width :fixed-width
               0 0 8
               )
         :chapter :fasta :comparison 'equalp)


(tests:deftest fasta-cleanup-files
         (progn
           (delete-no-error (fasta-test-file))
           (delete-no-error *fasta-other-extension*)
           (delete-no-error *fasta-no-extension*)
           t)
         t :chapter :fasta)

(tests:deftest fasta-t1 
         (mapcar #'gene-name (fasta-t1 :testdriver t)) 
         '("two-gene" nil "four-gene")
         :comparison #'equal :chapter :fasta)
(tests:deftest fasta-t2
         (mapcar #'gene-name (fasta-t2 :testdriver t)) 
         '("x-gene" "unknown-gene")
         :comparison #'equal :chapter :fasta)
(tests:deftest fasta-t3
         (mapcar #'gene-name (fasta-t3 :testdriver t)) 
         '("two-gene" nil "four-gene" "x-gene" "unknown-gene")
         :comparison #'equal :chapter :fasta)
(tests:deftest fasta-t4
         (mapcar #'gene-name (fasta-t4 :testdriver t)) 
         '("two-gene" "x-gene" "hair-color-gene")
         :comparison #'equal :chapter :fasta)
(tests:deftest fasta-t5
         (mapcar #'gene-name (fasta-t5 :testdriver t)) 
         '("two-gene" "x-gene" "hair-color-gene")
         :comparison #'equal :chapter :fasta)
(tests:deftest fasta-t6
         (mapcar #'gene-name (fasta-t6 :testdriver t)) 
         '("two-gene" "x-gene" "hair-color-gene")
         :comparison #'equal :chapter :fasta)

