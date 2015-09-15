;;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: bbi; -*-

(IN-PACKAGE :bbi)

(defmethod vpl-data-menu-variables 
           ((instance cl-user:biolingua)
            (organism-descriptor (eql :photosynthetic-bacteria)))
  (append 
   *common-vpl-data-menu-variables* 
   ;; put any variables specific to photosynthetic in a quoted list here 
   nil
   ))

(defmethod organism-subset-variables
           ((descriptor (eql :photosynthetic-bacteria)))
  (copy-list
   '(
     *all-cyanobacteria*
     *marine-cyanobacteria*
     *marine-unicellular-cyanobacteria*
     *marine-Prochlorococcus-and-Synechococcus*
     *marine-Prochlorococcus*
     *marine-Synechococcus*
     *marine-filamentous-cyanobacteria*
     *marine-n-fixing-cyanobacteria*
     *terrestrial-and-limnetic-cyanobacteria*
     *terrestrial-and-limnetic-unicellular-cyanobacteria*
     *terrestrial-and-limnetic-Synechococcus*
     *terrestrial-and-limnetic-filamentous-cyanobacteria*
     *terrestrial-and-limnetic-n-fixing-cyanobacteria*
     *unicellular-cyanobacteria*
     *unicellular-n-fixing-cyanobacteria*
     *filamentous-cyanobacteria*
     *filamentous-n-fixing-cyanobacteria*
     *heterocystous-cyanobacteria*
     *n-fixing-cyanobacteria*
     *all-green-bacteria*
     *chlorobi*
     *chloroflexi*
     *purple-bacteria*
     )))


;;; Stuff specific to the :cyanobacteria *organisms-descriptor* value


(define-symbol-macro *all-cyanobacteria* (compute-all-cyanobacteria))
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
  (organism-symbol-values
   '(:pmn :pmi :pmz :pmc :pme :p9601 :pmg :pmf :PMED4 :PRO1375 :P9313 
     :syg :S107 :S9916 :S9917 :S5701 :S7805 :S8102 :S9605 :S9902 :syr :syx :syp
     :ctc :cwat :amar :cyt :cyr
     :lyn :Tery :ter 
     :Gvi :TeBP1 :S6803 :S6301 :S7942B :S7942 :cyb :cya :mae
     :A7120 :A29413 :A29413b :Npun :nod)))

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


;;; Stuff specific to the :photosynthetic-bacteria *organisms-descriptor* value

(define-symbol-macro *all-photosynthetic-bacteria* (biolisp::loaded-organisms))
(define-symbol-macro *all-green-bacteria* (compute-all-green-bacteria))
(define-symbol-macro *chlorobi* (compute-chlorobi))
(define-symbol-macro *chloroflexi* (compute-chloroflexi))
(define-symbol-macro *purple-bacteria* (compute-purple-bacteria))

(defun compute-all-green-bacteria ()
  (organism-symbol-values 
   '(:ctep :cpha :cphab :pvib :cchl :plut :clim :cpar :ppha :paes
     :rrs1 :rcas :deb :deh :det)))

(defun compute-chlorobi ()
  (organism-symbol-values
   '(:ctep :cpha :cphab :pvib :cchl :plut :clim :cpar :ppha :paes)))

(defun compute-chloroflexi ()
  (organism-symbol-values '(:rrs1 :rcas :deb :deh :det)))

(defun compute-purple-bacteria ()
  (organism-symbol-values
   '(:rsq :rsp :rsh :rpc :rpd :rpe :rpb :rpa :rru :hha)))


