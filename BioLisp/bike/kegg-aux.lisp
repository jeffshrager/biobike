;;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: bbi; -*-

(in-package :bbi)

; *************** KEGG-ID-OF AND ... **************
(DEFPARAMETER *KEGG-line-format*
  (FORMAT NIL "~&~{~4<~a~>~50<~a~>~21<~a~>~6<~a~>~10<~a~>~10<~a~>~10<~a~>~6<~a~>~6<~a~>~6<~a~>~6<~a~>~6<~a~>~6<~a~>~6<~a~>~6<~a~>    ~a~}" 
    (LIST "---" "--------------" "-----" "--" "---" "--" "-----" "---" "----" "---" "----" "----" "----" "--" "--" "----------"))) 
				  
(DEFPARAMETER *KEGG-label-format*
  (FORMAT NIL "~&~{~4<~a~>~50<~a~>~21<~a~>~6<~a~>~10<~a~>~10<~a~>~10<~a~>~6<~a~>~6<~a~>~6<~a~>~6<~a~>~6<~a~>~6<~a~>~6<~a~>~6<~a~>    ~a~}" 
    (LIST "ORF" "SBJCT-ORGANISM" "SBJCT" "SW" "BIT" "ID" "OVLAP" "ST1" "END1" "ST2" "END2" "1->2" "2->1" "L1" "L2" "DEFINITION")))
	

(defun kegg-org-of (bborg)
  (or (#^kegg-id bborg)
      (let* ((orgpref (ref bborg #$organism-prefix))
             (org (first (split orgpref at ".")))
             (kegg-org (gethash org *kegg-org-of-mapping*)))
        (or kegg-org
            (format
             nil 
             "BioBIKE doesn't yet allow you to use this function for ~A"
             bborg
             )))))

;; ******** KEGG-NEIGHBORS-OF ********

(defun kegg-neighbors-of-aux 
       (program-type bborf OFFSET LIMIT +orf-id +gene-id1 +gene-id2
          +sbjct-organism 
          +sw-score +bit-score +identity +overlap +start-position1
          +end-position1 +start-position2 +end-position2 +best-flag-1to2
          +best-flag-2to1 +length1 +length2 +definition1 +definition2)
  (LET* ((program
           (WHEN-VALUE-OF program-type 
              IS "best" THEN 'kegg::GET-BEST-NEIGHBORS-BY-GENE
              IS "best-best" THEN 'kegg::GET-BEST-BEST-NEIGHBORS-BY-GENE
              OTHERWISE (ERROR "KEGG program '~A' unknown" program-type)))
         (kegg-name (kegg-id-of-aux bborf))
         (results 
          (SECOND
           (SECOND
            ;; soap call
            (FUNCALL program
             :GENES_ID kegg-name :OFFSET OFFSET :LIMIT LIMIT)
            )))
         (results1 (bbi::ARRAY-TO-LIST results))
         (line *KEGG-line-format*)
         (labels *KEGG-label-format*)
         (fields (LIST "orf-id" "genes_id1" "genes_id2" "sbjt-organism" "sw_score" 
                 "bit_score" "identity" "overlap"
                 "start_position1" "end_position1" "start_position2" "end_position2"
                 "best_flag_1to2" "best_flag_2to1" "definition1" "definition2"
                 "length1" "length2"))
         (field-values (MAKE-LIST (LENGTH fields) :INITIAL-ELEMENT "-"))
         (field-desires (LIST +orf-id +gene-id1 +gene-id2 +sbjct-organism +sw-score
                   +bit-score +identity +overlap +start-position1 +end-position1
                   +start-position2 +end-position2 +best-flag-1to2 +best-flag-2to1
                   +definition1 +definition2 +length1 +length2))

         (results2 
          (PROGRAM	     
		   (DISPLAY line *newline* labels *newline* line *newline*)
           (LOOP 
            FOR ortholog-info IN results1
            DO (LOOP FOR (field-label value) IN ortholog-info
                     AS field-string = (BB-STRING-OF field-label)
                     AS field-position = (POSITION field-string fields :TEST 'EQUAL)
                     DO (IF field-position
                            (SETF (Nth field-position field-values) value)
                            (WARN "Unknown field label (~A)" field-label)))
               (SETF (REF field-values 1) bborf)
               (SETF (Nth (POSITION "sbjt-organism" fields :TEST 'EQUAL)
                             field-values) 
                     (kegg-organisms-of (REF field-values 3)))
               (FORMAT T (S+ "~& ~{~4<~a~>~38<~a~>~21<~a~>~6<~a~>~10<~a~>"
                            "~10<~a~>~10<~a~>~6<~a~>~6<~a~>~6<~a~>~6<~a~>"
                            "~6<~a~>~6<~a~>~6<~a~>~6<~a~>    ~a~}") 
                  (SUBLIST field-values ITEM (LIST 1 4 3 5 6 7 8 9 10 11 12 13 14 17 18 15)))
            COLLECT 
              (FLATTEN (LOOP FOR desire IN field-desires
                             FOR value IN field-values
                             WHEN desire COLLECT value)))))
      )
    results2
    ))