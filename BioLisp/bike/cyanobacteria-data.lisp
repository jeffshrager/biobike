;;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: bbi; -*-

(IN-PACKAGE :bbi)

(defmethod vpl-data-menu-variables 
           ((instance cl-user:biolingua)
            (organism-descriptor (eql :cyanobacteria)))
  (append 
   *common-vpl-data-menu-variables* 
   ;; put any variables specific to cyanobacteria in a quoted list here 
   nil
   ))

(defmethod organism-subset-variables ((descriptor (eql :cyanobacteria)))
  (copy-list
   '(
     *all-cyanobacteria*
     *complete-genomes*
     ;; Marine-cyanobacteria
     *marine-cyanobacteria*
     *marine-unicellular-cyanobacteria*
     *marine-Prochlorococcus-and-Synechococcus*
     *marine-Prochlorococcus*
     *marine-Synechococcus*
     *marine-filamentous-cyanobacteria*
     *marine-n-fixing-cyanobacteria*
     ;; Terrestrial-and-limnetic-cyanobacteria
     *terrestrial-and-limnetic-cyanobacteria*
     *terrestrial-and-limnetic-unicellular-cyanobacteria*
     *terrestrial-and-limnetic-Synechococcus*
     *terrestrial-and-limnetic-filamentous-cyanobacteria*
     *terrestrial-and-limnetic-n-fixing-cyanobacteria*
     ;; Unicellular-cyanobacteria
     *unicellular-cyanobacteria*
     ;;    *marine-unicellular-cyanobacteria*
     ;;    *marine-Prochlorococcus-and-Synechococcus*
     ;;    *marine-Prochlorococcus*
     ;;    *marine-Synechococcus*
     ;;    *terrestrial-and-limnetic-unicellular-cyanobacteria*
     ;;    *terrestrial-and-limnetic-Synechococcus*
     *unicellular-n-fixing-cyanobacteria*
     ;; Filamentous-cyanobacteria
     *filamentous-cyanobacteria*
     ;;    *marine-filamentous-cyanobacteria*
     ;;    *terrestrial-and-limnetic-filamentous-cyanobacteria*
     *filamentous-n-fixing-cyanobacteria*
     *heterocystous-cyanobacteria*
     ;; N-fixing-cyanobacteria
     *n-fixing-cyanobacteria*
     ;;    *marine-n-fixing-cyanobacteria*
     ;;    *terrestrial-and-limnetic-n-fixing-cyanobacteria*
     ;;    *unicellular-n-fixing-cyanobacteria*
     ;;    *filamentous-n-fixing-cyanobacteria*
     ;;    *heterocystous-cyanobacteria*

      *microarrays*

     )))


;;; Stuff specific to the :cyanobacteria *organisms-descriptor* value


(define-symbol-macro *all-cyanobacteria* (compute-all-cyanobacteria))



(define-symbol-macro *complete-genomes* (compute-complete-genomes))
(define-symbol-macro *marine-cyanobacteria* (compute-marine-cyanobacteria))
(define-symbol-macro *marine-unicellular-cyanobacteria*
                     (compute-marine-unicellular-cyanobacteria))
(define-symbol-macro *marine-Prochlorococcus-and-Synechococcus* 
                     (compute-marine-Prochlorococcus-and-Synechococcus))
(define-symbol-macro *marine-Prochlorococcus* 
                     (compute-marine-Prochlorococcus))
(define-symbol-macro *marine-Synechococcus* (compute-marine-Synechococcus))
(define-symbol-macro *marine-filamentous-cyanobacteria* 
                     (compute-marine-filamentous-cyanobacteria))
(define-symbol-macro *marine-n-fixing-cyanobacteria* 
                     (compute-marine-n-fixing-cyanobacteria))
(define-symbol-macro *terrestrial-and-limnetic-cyanobacteria* 
                     (compute-terrestrial-and-limnetic-cyanobacteria))
(define-symbol-macro 
 *terrestrial-and-limnetic-unicellular-cyanobacteria* 
 (compute-terrestrial-and-limnetic-unicellular-cyanobacteria))
(define-symbol-macro *terrestrial-and-limnetic-Synechococcus* 
                     (compute-terrestrial-and-limnetic-Synechococcus))
(define-symbol-macro 
 *terrestrial-and-limnetic-filamentous-cyanobacteria* 
 (compute-terrestrial-and-limnetic-filamentous-cyanobacteria))
(define-symbol-macro *terrestrial-and-limnetic-n-fixing-cyanobacteria* 
                     (compute-terrestrial-and-limnetic-n-fixing-cyanobacteria))
(define-symbol-macro *unicellular-cyanobacteria* 
                     (compute-unicellular-cyanobacteria))
(define-symbol-macro *unicellular-n-fixing-cyanobacteria* 
                     (compute-unicellular-n-fixing-cyanobacteria))
(define-symbol-macro *filamentous-cyanobacteria*
                     (compute-filamentous-cyanobacteria))
(define-symbol-macro *filamentous-n-fixing-cyanobacteria* 
                     (compute-filamentous-n-fixing-cyanobacteria))
(define-symbol-macro *heterocystous-cyanobacteria* 
                     (compute-heterocystous-cyanobacteria))
(define-symbol-macro *n-fixing-cyanobacteria* 
                     (compute-n-fixing-cyanobacteria))


(defun compute-all-cyanobacteria ()
    *all-organisms*)
     
(defun compute-complete-genomes ()
  (organism-symbol-values '(:pmn :pmi :pmc :pme :p9601 :pmg :pmf :PMED4 :PRO1375 :P9313 
                            :syg :S107 :S9916 :S9917 :S5701 :S7805 :S8102 :S9605 :S9902 :syr :syx :syp
                            :amar :cyt
                            :Tery :ter 
                            :Gvi :TeBP1 :S6803 :S6301 :S7942B :S7942 :cyb :cya :mae
                            :A7120 :A29413 :A29413b :Npun)))
     

(defun compute-marine-cyanobacteria ()
  (organism-symbol-values 
   '(:pmn :pmi :pmz :pmc :pme :p9601 :pmg :pmf :PMED4 :PRO1375 :P9313
     :syg :S107 :S9916 :S9917 :S5701 :S7805 :S8102 :S9605 :S9902 :syr :syx :syp
     :ctc :cwat :amar :cyt
     :lyn :Tery :ter :nod)))
(defun compute-marine-unicellular-cyanobacteria ()
  (organism-symbol-values 
   '(:pmn :pmi :pmz :pmc :pme :p9601 :pmg :pmf :PMED4 :PRO1375 :P9313
     :syg :S107 :S9916 :S9917 :S5701 :S7805 :S8102 :S9605 :S9902 :syr :syx :syp
     :ctc :cwat :amar :cyt)))
(defun compute-marine-Prochlorococcus-and-Synechococcus ()
  (organism-symbol-values
   '(:pmn :pmi :pmz :pmc :pme :p9601 :pmg :pmf :PMED4 :PRO1375 :P9313
     :syg :S107 :S9916 :S9917 :S5701 :S7805 :S8102 :S9605 :S9902 :syr :syx :syp)))
(defun compute-marine-Prochlorococcus ()
  (organism-symbol-values 
   '(:pmn :pmi :pmz :pmc :pme :p9601 :pmg :pmf :PMED4 :PRO1375 :P9313)))
(defun compute-marine-Synechococcus ()
  (organism-symbol-values 
   '(:syg :S107 :S9916 :S9917 :S5701 :S7805 :S8102 :S9605 :S9902 :syr :syx :syp)))
(defun compute-marine-filamentous-cyanobacteria ()
  (organism-symbol-values '(:Tery :ter :lyn :nod)))
(defun compute-marine-n-fixing-cyanobacteria ()
  (organism-symbol-values '(:Tery :ter :ctc :Cwat :lyn :nod :cyt)))
(defun compute-terrestrial-and-limnetic-cyanobacteria ()
  (organism-symbol-values 
   '(:Gvi :TeBP1 :S6803 :S6301 :S7942B :S7942 :cyb :cya :mae :cyr
     :A7120 :A29413 :A29413b :Npun)))
(defun compute-terrestrial-and-limnetic-unicellular-cyanobacteria ()
  (organism-symbol-values 
   '(:Gvi :TeBP1 :S6803 :S6301 :S7942B :S7942 :cyb :cya :mae :cyr)))
(defun compute-terrestrial-and-limnetic-Synechococcus ()
  (organism-symbol-values '(:S6803 :S6301 :S7942B :S7942 :cyb :cya)))
(defun compute-terrestrial-and-limnetic-filamentous-cyanobacteria ()
  (organism-symbol-values '(:A7120 :A29413 :A29413b :Npun)))
(defun compute-terrestrial-and-limnetic-n-fixing-cyanobacteria ()
  (organism-symbol-values '(:A7120 :A29413 :A29413b :Npun :cyb :cya :cyr)))
(defun compute-unicellular-cyanobacteria ()
  (organism-symbol-values 
   '(:pmn :pmi :pmz :pmc :pme :p9601 :pmg :pmf :PMED4 :PRO1375 :P9313
     :syg :S107 :S9916 :S9917 :S5701 :S7805 :S8102 :S9605 :S9902 :syr :syx :syp
     :amar :ctc :cwat :cyt
     :Gvi :TeBP1 :S6803 :S6301 :S7942B :S7942 :cyb :cya :cyt :mae)))
(defun compute-unicellular-n-fixing-cyanobacteria ()
  (organism-symbol-values '(:Cwat :ctc :cya :cyb :cyt :cyr)))
(defun compute-filamentous-cyanobacteria ()
  (organism-symbol-values 
   '(:A7120 :A29413 :A29413b :Npun :nod :Tery :ter :lyn)))
(defun compute-filamentous-n-fixing-cyanobacteria ()
  (organism-symbol-values '(:A7120 :A29413 :A29413b :Npun :nod :Tery :ter :lyn)))
(defun compute-heterocystous-cyanobacteria ()
  (organism-symbol-values '(:A7120 :A29413 :A29413b :Npun :nod)))
(defun compute-n-fixing-cyanobacteria ()
  (organism-symbol-values 
   '(:A7120 :A29413 :A29413b :Npun :nod :Tery :ter :Cwat :ctc
     :cya :cyb :lyn :cyt :cyr)))


(setq 
 *kegg-org-of-mapping* 
 (create-hash-table 
  '(("A29413" "ava")
    ("ava" "A29413")
    ("A7120" "ana")
    ("ana" "A7120")
    ("Amar" "amr")
    ("amr" "Amar")
    ("cthe" "Chro7203")
    ("Chro7203" "cthe")
    ("cya" "cya")
    ("cyb" "cyb")
    ("cyt" "cyt")
    ("cyp" "cyr")
    ("cyr" "cyp")
    ("cyc" "cyan7424")
    ("cyan7424" "cyc")
    ("cyn" "cyan7425")
    ("cyan7425" "cyn")
    ("cyh" "cyan8802")
    ("cyan8802" "cyh")
    ("cyj" "cyan7822")
    ("cyan7822" "cyj")
    ("cgc" "cyagr")
    ("cyagr" "cgc")
    ("can" "cyan10605")
    ("cyan10605" "can")
    ("csn" "cyast")
    ("cyast" "csn")
    ("dsl" "dacsa")
    ("dacsa" "dsl")
    ("glp" "glo7428")
    ("glo7428" "glp")
    ("cmp" "Cha6605")
    ("Cha6605" "cmp")
    ("cyu" "ucyn-a")
    ("ucyn-a" "cyu")
    ("lep" "lepto7376")
    ("lepto7376" "lep")
    ("gei" "gei7407")
    ("gei7407" "gei")
    ("osc" "osc6304")
    ("osc6304" "osc")
    ("oni" "osc7112")
    ("osc7112" "oni")
    ("pseu" "pse7367")
    ("pse7367" "pseu")
    ("cep" "cri9333")
    ("cri9333" "cep")
    ("mic" "mic7113")
    ("mic7113" "mic")
    ("arp" "Apla39")
    ("Apla39" "arp")
    ("nos" "nos7107")
    ("nos7107" "nos")
    ("nop" "nos7524")
    ("nos7524" "nop")
    ("anb" "ana90")
    ("ana90" "anb")
    ("acy" "Anacy")
    ("Anacy" "acy")
    ("naz" "aazo")
    ("aazo" "naz")
    ("csg" "cylst")
    ("cylst" "csg")
    ("calo" "cal7507")
    ("cal7507" "calo")
    ("calt" "cal6303")
    ("cal6303" "calt")
    ("ceo" "etsb")
    ("etsb" "ceo")
    ("riv" "riv7116")
    ("riv7116" "riv")
    ("Gvi" "gvi")
    ("gvi" "Gvi")
    ("glj" "gkil")
    ("gkil" "glj")
    ("hao" "halo7418")
    ("halo7418" "hao")
    ("mae" "mar")
    ("mar" "mae")
    ("Npun" "npu")
    ("npu" "Npun")
    ("plp" "ple7327")
    ("ple7327" "plp")
    ("P9313" "pmt")
    ("pmt" "P9313")
    ("pmb" "pmb")
    ("pmc" "pmc")
    ("pme" "pme")
    ("PMED4" "pmm")
    ("pmm" "PMED4")
    ("pmf" "pmf")
    ("pmg" "pmg")
    ("pmi" "pmi")
    ("pmn" "pmn")
    ("pmj" "pmz")
    ("pmz" "pmj")
    ("PRO1375" "pma")
    ("pma" "PRO1375")
    ("scs" "sta7437")
    ("sta7437" "scs")
    ("S6301" "syc")
    ("syc" "S6301")
    ("S6803" "syn")
    ("syn" "S6803")
    ("S7942" "syf")
    ("syf" "S7942")
    ("S8102" "syw")
    ("syw" "S8102")
    ("S9605" "syd")
    ("syd" "S9605")
    ("S9902" "sye")
    ("sye" "S9902")
    ("syg" "syg")
    ("syp" "syp")
    ("syr" "syr")
    ("syx" "syx")
    ("TeBP1" "tel")
    ("tel" "TeBP1")
    ("ter" "ter")
    ("thn" "tsynnk55")
    ("tsynnk55" "thn")
    ("ctep" "cte")
    ("cte" "ctep")
    ("cpha" "cph")
    ("cph" "cpha")
    ("pvib" "pvi")
    ("pvi" "pvib")
    ("cchl" "cch")
    ("cch" "cchl")
    ("plut" "plt")
    ("plt" "plut")
    ("rrs1" "rrs")
    ("rrs" "rrs1")
    ("rcas" "rca")
    ("rca" "rcas")
    ("deb" "deb")
    ("deh" "deh")
    ("det" "det")
    ("rsq" "rsq")
    ("rsp" "rsp")
    ("rsh" "rsh")
    ("rpc" "rpc")
    ("rpd" "rpd")
    ("rpe" "rpe")
    ("rpb" "rpb")
    ("rpa" "rpa")
    ("rru" "rru")
    ("hha" "hha")
    ;;   ("ctc" "---")
    ;;   ("Cwat" "___")
    ;;   ("cyr" "___")
    ;;   ("lyn" "___")
    ;;   ("N9414" "___")
    ;;   ("pmh" "pmh") or P9215 must be added in BioBIKE
    ;;   ("pmj" "pmj") still pmz or P9211 in BioBIKE, must be upgraded
    ;;   ("S107" "___")
    ;;   ("S5701" "___")
    ;;   ("S7805" "___")
    ;;   ("S9916" "___")
    ;;   ("S9917" "___")
    ;;   ("cphab" "___")
    ;;   ("clim" "___")
    ;;   ("cpar" "___")
    ;;   ("ppha" "___")
    ;;   ("paes" "___")
    )
  :test 'equal
  ))