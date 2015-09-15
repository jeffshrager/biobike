(IN-PACKAGE BBI)

; (DEFCONSTANT kegg-id (NEW-TABLE (LIST $) INITIALIZE NIL))
  (DEFCONSTANT kegg-id 
      (MAKE-GARRAY '($) :IF-ACCESSED-LOCATION-DOES-NOT-EXIST NIL))

(LET ((kegg-categories-raw 
    (STRING-TO-LIST
      (FILE-TO-STRING 
         (MERGE-PATHNAMES "kegg-categories-and-genes.txt" 
               *load-pathname*)))))

;; file has format (by example):
;;   ((("syn" "Synechocystis PCC 6803" "S6803" "00010"
;;            "Glycolysis / Gluconeogenesis")
;;       (("sll0018" . "cbbA, cfxA, fbaA, fda; fructose-bisphosphate aldol")
;;        ("sll0395" . "hypothetical protein [EC:5.4.2.1]; K01834 phosphog")
;;        ...))
;;    (("syn" "Synechocystis PCC 6803" "S6803" "00020"
;;            "Citrate cycle (TCA cycle)")
;;       (("sll0401" . "gltA, gluT; citrate synthase [EC:2.3.3.1]; K01647 ")
;;        ("sll0823" . "sdhB; succinate dehydrogenase iron-sulfur protein ")
;;        ...))
;;    (("syw" "Synechococcus WH 8102" "S8102" "00561"
;;            "Glycerolipid metabolism")
;;       (("SYNW0167" . "utative glycosyl transferase family protein [EC:2 ")
;;        ("SYNW0939" . "utative glycerol dehydrogenase [EC:1.1.1.6]; K000 ")
;;        ...)))

(LOOP FOR (cat-info gene-info) IN kegg-categories-raw
      WITH *suppress-warnings* = T
      AS org = (REF cat-info 1)
      AS cat-number = (PARSE-INTEGER (REF cat-info 4))
      AS cat-ID = (S+ "KO" (REF cat-info 4))
      AS cat-name = (REF cat-info 5)
;      WHEN (NOT (EQUAL org "syf"))  ; Our version of syf differs from KEGG's
;        DO (LOOP FOR gene-name-source IN gene-info
;                AS gene = (GENE-NAMED (REF gene-name-source 1))
;                AS old-kegg-id = (REF gene #$kegg-id)
;                DO (DEF-FRAME gene #$kegg-id (APPEND old-kegg-id (LIST cat-ID))))
      DO 
         (SETF (REF kegg-id cat-ID) cat-name)
         (SETF (REF kegg-id cat-number) cat-name))

 )

(EXPORT 'kegg-id)
(EXPORT 'kegg-id 'bbl)

