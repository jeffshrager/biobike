;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: bioutils; -*-

;;; Author:  JP Massar.

(in-package :bioutils)

(tests:deftest td2-keys (progn (ltt2) (table-data-keycols *td*)) 
         '("NAME" "SSN" "R1")
         :chapter :td :comparison #'equal)         
(tests:deftest td2-header (progn (ltt2) (table-data-header *td* 3)) "LEVEL0"
         :chapter :td :comparison #'equal)         
(tests:deftest :td2-nrows (progn (ltt2) (table-data-nrows *td*)) 4
         :chapter :td)

(tests:deftest td2-elem1 (progn (ltt2) (table-data-element *td* 0 1)) #$Fred
         :chapter :td :comparison #'equal)
(tests:deftest td2-elem-missing 
               (progn (ltt2) (table-data-element *td* 1 5)) 17.0
         :chapter :td :comparison #'equal)
(tests:deftest td2-elem-coerced 
               (progn (ltt2) (table-data-element *td* 2 6)) 0.0
         :chapter :td :comparison #'equal)
(tests:deftest td2-elem-hdr 
               (progn (ltt2) (table-data-element *td* 0 "SSN")) "62-055"
         :chapter :td :comparison #'equal)
(tests:deftest td2-elem-key1
         (progn (ltt2) 
           (table-data-element *td* (table-key #$Barney "NAME") "SSN"))
         "12-171" 
         :chapter :td :comparison #'equal)
(tests:deftest td2-elem-key2
         (progn (ltt2) (table-data-element *td* (table-key #$Barney) "SSN"))
         "12-171" 
         :chapter :td :comparison #'equal)
(tests:deftest td2-elem-key3
         (progn (ltt2) 
           (table-data-element *td* (table-key #$Barney "NAME" *td*) "SSN"))
         "12-171" 
         :chapter :td :comparison #'equal)
(tests:deftest td2-elem-dupkey
         (progn (ltt2)
           (multiple-value-bind (result error?)
               (ignore-errors
                 (table-data-element *td* (table-key #$Betty "NAME") "LEVEL2"))
             (declare (ignore result))
             (if error? :error :ok)))
         :error
         :chapter :td :comparison #'equal)

(tests:deftest td2-elems-rows-col 
         (progn (ltt2) (table-data-elements *td* '(1 3) 4))
         '(3.0 21.0)
         :chapter :td :comparison #'equal)
(tests:deftest td2-elems-rows-hdr 
         (progn (ltt2) (table-data-elements *td* '(1 3) "LEVEL0"))
         '(2.05 11.0)
         :chapter :td :comparison #'vsf-equiv)
(tests:deftest td2-elems-row-cols
         (progn (ltt2) (table-data-elements *td*  1 '(3 4 5 6)))
         '(2.05 3.0 17.0 0.50)
         :chapter :td :comparison #'vsf-equiv)
(tests:deftest td2-elems-row-hdrs
         (progn (ltt2) (table-data-elements *td*  2 '("NAME" "LEVEL0")))
         '(#$Barney 3.0)
         :chapter :td :comparison #'equal)
(tests:deftest td2-elems-key-hdrs
         (progn (ltt2) 
           (table-data-elements *td* (table-key #$Barney) '("SSN" "LEVEL0")))
         '("12-171" 3.0)
         :chapter :td :comparison #'equal)
(tests:deftest td2-elems-keys-hdr
         (progn (ltt2) 
           (table-data-elements 
            *td* (list (table-key #$Fred) (table-key #$Barney))
            "LEVEL0"))
         '(1.0 3.0)
         :chapter :td :comparison #'equal)         
         
(tests:deftest td2-drow
         (progn (ltt2) (table-data-data-row *td* 2))
         #(3.0 4.0 5.0 0.0) :chapter :td :comparison #'vsf-equiv)
(tests:deftest td2-dcol-col
         (progn (ltt2) (table-data-data-col *td* 0))
         #(1.0 2.05 3.0 11.0)
         :chapter :td :comparison #'vsf-equiv)
(tests:deftest td2-dcol-hdr
         (progn (ltt2) (table-data-data-col *td* "LEVEL2"))
         #(-3.0 17.0 5.0 31.0)
         :chapter :td :comparison #'vsf-equiv)
(tests:deftest :td2-drows-keys
         (progn (ltt2) 
           (table-data-data-rows 
            *td* (list #$Fred #$Barney)))
         (list #(1.0 2.0 -3.0 50.0e-2) #(3.0 4.0 5.0 0.0))
         :chapter :td
         :comparison
         #'(lambda (x y)
             (and (vsf-equiv (first x) (first y)) 
                  (vsf-equiv (second x) (second y)))))

(tests:deftest td2-key-present?-yes-1
         (progn (ltt2) 
           (not (null (table-data-key-present? 
                       *td* (table-key "12-171" "SSN"))))) t
         :chapter :td :comparison #'eql)
(tests:deftest td2-key-present?-yes-2
         (progn (ltt2) 
           (not (null (table-data-key-present? *td* (table-key #$Fred))))) t
         :chapter :td :comparison #'eql)
(tests:deftest td2-key-present?-no
         (progn (ltt2) 
           (not (null (table-data-key-present? *td* (table-key #$Astro))))) nil
         :chapter :td :comparison #'eql)

(tests:deftest td2-key-row-indices
         (progn (ltt2) 
           (table-data-key-row-indices *td* (table-key "37-999"))) '(1 3)
         :chapter :td :comparison #'equal)

(tests:deftest td2-data-subarray-1
         (progn (ltt2) 
           (let ((sa (table-data-data-subarray *td* 2 2 :as :matrix)))
             (list (aref sa 0 0) (aref sa 0 1) (aref sa 1 0) (aref sa 1 1))))
         '(5.0 0.0 31.0 0.50)
         :chapter :td :comparison #'equal)
(tests:deftest td2-data-subarray-2
         (progn (ltt2) 
           (let ((sa (table-data-data-subarray 
                      *td* 0 1 :end-row 1 :end-col 2 :as :matrix)))
             (list (aref sa 0 0) (array-dimensions sa))))
         '(2.0 (1 1))
         :chapter :td :comparison #'equal)
(tests:deftest td2-data-subarray-null
         (progn (ltt2) 
           (let ((sa (table-data-data-subarray 
                      *td* 0 1 :end-row 0 :end-col 1 :as :matrix)))
             (array-dimensions sa)))
         '(0 0)
         :chapter :td :comparison #'equal)
(tests:deftest td2-data-subarray-rows-col
         (progn (ltt2) 
           (let ((sa (table-data-data-subarray *td* '(0 1) '(2) :as :matrix)))
             (list (aref sa 0 0) (aref sa 1 0) (array-dimensions sa))))
             '(-3.0 17.0 (2 1))
         :chapter :td :comparison #'equal)
(tests:deftest td2-data-subarray-row-cols
         (progn (ltt2) 
           (let ((sa (table-data-data-subarray 
                      *td* '(1) '(0 1 2 3) :as :matrix)))
             (list (aref sa 0 1) (aref sa 0 2) (aref sa 0 3) 
                   (array-dimensions sa))))
             '(3.0 17.0  0.50 (1 4))
         :chapter :td :comparison #'equal)
(tests:deftest td2-data-subarray-rows-cols-unordered
         (progn (ltt2) 
           (let ((sa (table-data-data-subarray 
                      *td* '(1 0) '(2 1) :as :matrix)))
             (list (aref sa 0 0) (aref sa 0 1) (aref sa 1 0) (aref sa 1 1)
                   (array-dimensions sa))))
         '(17.0 3.0 -3.0 2.0 (2 2))
         :chapter :td :comparison #'equal)
(tests:deftest td2-data-subarray-rows-cols-dup
         (progn (ltt2) 
           (let ((sa (table-data-data-subarray 
                      *td* '(1 1) '(2 1) :as :matrix)))
             (list (aref sa 0 0) (aref sa 0 1) (aref sa 1 0) (aref sa 1 1)
                   (array-dimensions sa))))
         '(17.0 3.0 17.0 3.0 (2 2))
         :chapter :td :comparison #'equal)

(tests:deftest td2-table-data-row-1
         (progn (ltt2) (table-data-row *td* 0))
         #("a" #$Fred "62-055" 1.0 2.0 -3.0 0.50 "23" "the big man")
         :chapter :td :comparison #'equalp)
(tests:deftest td2-table-data-row-2
         (progn (ltt2) (table-data-row *td* 3 :list))
         '("d" #$Dino "37-999" 11.0 21.0 31.0 0.50 "10" "his dog")
         :chapter :td :comparison #'equalp)
(tests:deftest td2-table-data-col-1
         (progn (ltt2) (table-data-col *td* 4 :vector))
         #(2.0 3.0 4.0 21.0)
         :chapter :td :comparison #'vsf-equiv)
(tests:deftest td2-table-data-col-2
         (progn (ltt2) (table-data-col *td* "NAME" :list))
         '(#$Fred #$Wilma #$Barney #$Dino)
         :chapter :td :comparison #'equalp)

(tests:deftest td2-rows-table
         (progn (ltt2) 
           (let ((st (table-data-rows-table *td* '(1 2))))
             (list (subseq (table-data-headers st) 2 4)
                   (table-data-keycols st)
                   (table-data-element st 0 1)
                   (table-data-element st 1 "LEVEL3")
                   )))
         '(("SSN" "LEVEL0") ("NAME" "SSN" "R1") #$Wilma 0.0)
         :chapter :td :comparison #'equalp)

(tests:deftest td2-cols-table
         (progn (ltt2) 
           (let ((st (table-data-cols-table *td* '(2 3))))
             (list (table-data-headers st)
                   (table-data-keycols st)
                   (table-data-element st 0 0)
                   (table-data-element st 2 1)
                   )))
         '(("SSN" "LEVEL0") ("SSN") "62-055" 3.0)
         :chapter :td :comparison #'equalp)

(tests:deftest td2-subtable-1
         (progn (ltt2) 
           (let ((st (table-data-subtable *td* '(2 3) '(2 3))))
             (list (table-data-headers st)
                   (table-data-keycols st)
                   (table-data-element st 0 0)
                   (table-data-element st 1 1)
                   )))
         '(("SSN" "LEVEL0") ("SSN") "12-171" 11.0)
         :chapter :td :comparison #'equalp)                   

(tests:deftest td2-select-1
         (progn (ltt2)
           (table-data-select
            *td*
            #'(lambda (v) (>= (count-if #'(lambda (x) (> x 3.5)) v) 2))
            :from :data
            :return :name
            :tag :row
            ))
         '((2 #$Barney) (3 #$Dino))
         :chapter :td :comparison #'equalp)

(tests:deftest td2-select-2
         (progn (ltt2)
           (table-data-select
            *td*
            #'(lambda (v) (eql (char (aref v 2) 0) #\W))
            :from :other
            :return :all
            :index-predicate #'(lambda (x) (> x 1))
            ))
         nil
         :chapter :td :comparison #'equalp)



