;;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: bbi; -*-

(IN-PACKAGE :bbi)

(DEFUN bidirectional-best-hit (gene org2 &OPTIONAL (threshold 1e-10))
  (LET* ((org1 (ORGANISM-OF gene))
         (protein1 (PROTEIN-OF gene))
         (pre-prot-gene2 
            (IF protein1
                (SEQUENCE-SIMILAR-TO protein1 IN org2 PROTEIN-VS-PROTEIN
                    RETURN 1 NO-DISPLAY THRESHOLD threshold)
                (GENE/S-SIMILAR-TO gene IN org2 RETURN 1 
                    THRESHOLD threshold NO-DISPLAY)))
         (prot-gene2 
            (IF (LISTP pre-prot-gene2)
                (FIRST pre-prot-gene2)
                pre-prot-gene2))
         (pre-prot-gene-1b 
            (IF prot-gene2
                (IF protein1
                    (SEQUENCE-SIMILAR-TO prot-gene2 IN org1 PROTEIN-VS-PROTEIN
                        RETURN 1 NO-DISPLAY THRESHOLD threshold)
                    (GENE/S-SIMILAR-TO prot-gene2 IN org1 RETURN 1 
                        THRESHOLD threshold NO-DISPLAY))))
         (prot-gene-1b
            (IF (LISTP pre-prot-gene-1b)
                (FIRST pre-prot-gene-1b)
                pre-prot-gene-1b))
         )
    (IF (OR (AND protein1 (EQUAL protein1 prot-gene-1b))
            (AND (NOT protein1) (EQUAL gene prot-gene-1b)))
        prot-gene2)
 ))