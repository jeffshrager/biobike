;;; -*- Package: bio; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

;;; Author: JP Massar.

(in-package :bio)

;;;; To use these tests you must load the :NOS29133 and :ANA7120 organisms.  


;;;; Amino acid conversion package tests


(tests:deftest aa-dna1 (dna-to-rna-sequence "AaX9TUt") "AaX9UUu" 
         :comparison 'equal :chapter :organisms)
(tests:deftest aa-rna1 (rna-to-dna-sequence "AaX9UUu") "AaX9TTt"
         :comparison 'equal :chapter :organisms)
(tests:deftest aa-desig1 
         (mapcar 'amino-acid-designator? 
                 '(bioutils::y "Met" "lysine" 
                               bioutils::valine :K #\a "Stop" :fiddle #\4))
         (list t t t t t t t nil nil)
         :comparison 'equal :chapter :organisms)         
(tests:deftest aa-long1
         (mapcar 
          (lambda (x) (aa-to-long-name :val x))
          '(:string :lcstring :capstring :symbol :keyword))
         (list "VALINE" "valine" "Valine" 'bioutils::valine :valine)
         :comparison 'equal :chapter :organisms)
(tests:deftest aa-codon1
         (mapcar
          (lambda (x) (codon-to-aa :tta x))
          (list :char :string :lcstring :symbol :keyword))
         (list #\L "L" "l" 'bioutils::l :l)
         :comparison 'equal :chapter :organisms)

(tests:deftest aa-codon2 (codon-to-aa "XYZ" :symbol) '- :chapter :organisms)

(tests:deftest 
 aa-mol1
 (molecular-weight-of (translate-d/rna-to-aa "AAAAGTUCG"))
 (- (+ (aa-to-mw :aaa) (aa-to-mw "AGT") (aa-to-mw 'bioutils::ucg))
    ;; less the water molecules between the amino acids
    (* 18 (- 3 1)))
 :comparison 'equal :chapter :organisms)

(tests:deftest aa-translate1
         (translate-d/rna-to-aa "AAAAGTTCG" :as :3codes :separator ",")
         "LYS,SER,SER"
         :comparison 'equalp :chapter :organisms)

(tests:deftest aa-translate2
         (translate-d/rna-to-aa 
          "???AAAAGTTCGA" :as :1codes :separator " " 
          :if-partial-codon :ignore :if-unknown-codon "$")
         "$ K S S"
         :comparison 'equalp :chapter :organisms)


;;;; BEST-BLAST-ORTHOLOG

(tests:deftest bbo-a7120-npun-1
         (mapcar 
          (lambda (protein)
            (best-blast-ortholog-of protein (organism :npun) 1.0e-10))
          '(#$A7120.p-All4813 #$A7120.p-Alr4240 #$A7120.p-Alr5028)
          )
         '(
           #$Npun.p-NpR6625           
           #$Npun.p-NpR6584
           #$Npun.p-NpR6536
           )
         :comparison 'equal :chapter :organisms)

(tests:deftest bbo-a7120-npun-2
         (mapcar 
          (lambda (protein)
            (best-blast-ortholog-of protein (organism :npun) 1.0e-3))
          '(#$A7120.p-All4813 #$A7120.p-Alr4240 #$A7120.p-Alr5028)
          )
         '(
           #$Npun.p-NpR6625           
           #$Npun.p-NpR6584
           #$Npun.p-NpR6536
           )
         :comparison 'equal :chapter :organisms)

(tests:deftest bbo-a7120-npun-3
         (mapcar 
          (lambda (protein)
            (best-blast-ortholog-of protein (organism :npun) 1.0d-50))
          '(#$A7120.p-All4813 #$A7120.p-Alr4240 #$A7120.p-Alr5028)
          )
         '(
           #$Npun.p-NpR6625           
           #$Npun.p-NpR6584
           #$Npun.p-NpR6536
           )
         :comparison 'equal :chapter :organisms)


;; Basic test
(tests:deftest 2wayortho-1 
         (2wayo #$A7120.p-Alr2328 (organism :syn6803))
         #$S6803.p-Slr1756
         :chapter :organisms)

;; Test that gene works, canonicalization works.
(tests:deftest 2wayortho-2
         (2wayo :A7120.Alr2328 (organism :syn6803))
         #$S6803.p-Slr1756
         :chapter :organisms)

;; Variations, with different organisms.
(tests:deftest 2wayortho-3
         (2wayo :A7120.Alr2328 (organism :ana29413b))
         #$A29413B.p-Ava0147
         :chapter :organisms)

(tests:deftest 2wayortho-4
         (2wayo "A7120.p-Alr2328" (organism :ter))
         #$ter.p-Tery_3834
         :chapter :organisms)

(tests:deftest 2wayortho-5
         (2wayo 'A7120.p-Alr2328 (organism :npun))
         #$Npun.p-NpR5387
         :chapter :organisms)

;; A different protein in a different organism.
(tests:deftest 2wayorth-6
         (2wayo #$A7120.p-Alr2339 (organism :ana29413b))
         #$A29413B.p-Ava0158
         :chapter :organisms)


;; A gene with no ortholog.
(tests:deftest 2wayorth-7
         (2wayo #$A7120.p-Alr2339 (organism :syn6803))
         nil
         :chapter :organisms)

;; Another one
(tests:deftest 2wayorth-8
         (2wayo #$A7120.p-Asl0095 (organism :ter))
         nil
         :chapter :organisms)


#|

1. #$A7120.pAlr2328 (Anabaena PCC7120) is orthologous to:
   #$S6803.pSlr1756 Synechocystis_PCC6803
   #$a29413.pAv?1894 Anabaena_variabilis_ATCC29413
   #$Tery.pTe?0579  Trichodesmium_erythraeum
   #$Npun.pNpR5387 Nostoc_punctiforme_ATCC29133

2. #$A7120.pAlr2339 (Anabaena PCC7120) is orthologous to:
   #$a29413.pAv?1902 Anabaena_variabilis_ATCC19413
   #$Tery.pTe?4067 Trichodesmium_erythraeum
   #$Npun.pNpR1722 Nostoc_punctiforme_ATCC29133

   Synechocystis_PCC6803 has no ortholog

3. #$A7120.pAsl0095 Anabaena_PCC7120 is orthologous to:
   #$a29413.pAv?0358 Anabaena_variabilis_ATCC19413
   #$Npun.pNpF1835 Nostoc_punctiforme_ATCC29133

   Trichodesmium_erythraeum has no ortholog
   Synechocystis_PCC6803 has no ortholog

|#


