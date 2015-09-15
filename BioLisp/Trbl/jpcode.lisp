;;; -*- Package:pb; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :pb)

(defvar arab)
(defvar *year-frames*)

;;;; Listing of info functions:

;;; pubs-per-year
;;; number-of-gene-models-with-publications
;;; go-nodes-mentioned
;;; publications-with-gene-models
;;; gene-model-publications-per-year
;;;   gene-models-ranked-by-number-of-publications
;;;   top-n-gene-models-per-year
;;;   mentions-per-year
;;;   new-mentions-per-year
;;; unique-pis
;;; unique-pis-per-year
;;; publications-without-pis
;;; publications-with-pis-who-are-not-authors
;;; new-pis
;;; pis-for-all-gene-models-per-year
;;; pis-for-gene-model-per-year
;;; pis-ranked-by-publications-per-year
;;; top-n-pis-by-publications-per-year
;;; fringe-go-nodes-of-gene-model
;;; * assign-fringe-go-node-slot-to-gene-models
;;; fringe-nodes
;;; * arab-ancestor-bindings
;;; gene-model-references-per-second-level-go-node
;;; all-authors-list
;;; * author-publications-per-year
;;; average-number-of-authors-per-year
;;; top-n-authors-per-year
;;; * author-info
;;; fringe-go-node-reference-counts
;;; calculate-closeness-of-mentioned-go-nodes

;;; Information about the TAIR/publications database
;;; 
;;; There are 30644 genes in Ar.
;;; There are 37852 gene-models in Ar.
;;; 
;;; There are 9065 publications about these gene-models 
;;; from 1991 through mid-2003.
;;; (just total # of pubs, regardless of whether they mention the gene models)


(defparameter *base-year* 1991)
(cformatt " *** Warning: Base year is ~D ***" *base-year*)

;;; Publications per year:
;;; ((1991 166) (1992 205) (1993 325) (1994 397) (1995 519) (1996 659)
;;;  (1997 732) (1998 790) (1999 1134) (2000 1353) (2001 1113) (2002 1242)
;;;  (2003 430))

(defun pubs-per-year ()
  (frameloop (y *year-frames*) (year pubs) collect (list year (length pubs))))

;;; Of the 37852 gene-models only 2323 have publications that mention them,
;;; approximately 6%.

(defun number-of-gene-models-with-publications ()
  (count-if #'#^pubs (#^gene-models arab)))

;;; All the go nodes that have gene models that are mentioned
;;; in publication.  There are 1738 of these (at the moment)

(defun go-nodes-mentioned ()
  (purge-duplicates
   (flatten 
    (copy-tree
     (loop for gm in (remove-if-not #'#^pubs (#^gene-models arab))
           collect (#^go-id gm))))))

;;; There are 7307 distinct publications that have gene-models 
;;; that reference them.

(defun publications-with-gene-models ()
  (purge-duplicates 
   (frameloop (gene-model (#^gene-models arab)) (pubs) nconc 
     (copy-list pubs))))
        

;;; Returns a list of the number of mentions of each gene model
;;; in each year, as: 
;;; ((#$At.CAF (0 0 0 0 0 0 0 0 1 1 1 4 1))
;;;  ...)

(defun gene-model-publications-per-year ()
  (remove-if
   'null
   (loop for gene-model in (#^gene-models arab) nconc
         (let ((count-data
                (frameloop (yf *year-frames*) ((yearnum year)) collect
                  (loop for pub in (#^pubs gene-model) sum
                        (if (= yearnum (#^year pub)) 1 0)
                        ))))
	   (if (some #'plusp count-data)
	       (list (list gene-model count-data))
	     nil)))))

;;; This function takes as input the result of the above function as gpy.  

(defun gene-models-ranked-by-number-of-publications (gpy year)
  (let ((copy (copy-list gpy)))
    (cond
     ((or (eq t year) (eq :all year)) 
      (sort copy '> :key (lambda (x) (reduce '+ (second x)))))
     ((numberp year) 
      (let ((index (- year *base-year*)))
        (sort copy '> :key (lambda (x) (nth index (second x))))))
     (t (error "Year must be :all or a year between 1991 and 2003"))
     )))

(defun top-n-gene-models-per-year (gpy n)
  (frameloop (yf *year-frames*) (year) collect
    (list 
     year 
     (first-n n (gene-models-ranked-by-number-of-publications gpy year))
     )))


#+data
(setq top-gene-models
      '((1991
         ((#$At.GRF6 (14 3 6 7 5 9 1 2 0 3 4 3 0))
          (#$At.AG (9 3 8 10 16 18 16 12 24 33 12 14 5))
          (#$At.ER (7 6 7 18 12 17 23 20 31 32 29 26 12))
          (#$At.ATR1 (7 5 2 16 11 12 19 23 31 40 21 16 2))
          (#$At.AP2 (7 2 1 3 0 5 5 4 13 13 5 8 5))))
        (1992
         ((#$At.CAL (3 11 9 9 17 18 21 16 23 39 29 22 2))
          (#$At.CSR1 (2 10 7 6 3 3 1 0 5 2 0 0 1))
          (#$At.ASA1 (0 8 0 8 16 21 0 22 1 10 7 3 1))
          (#$At.PHYA (4 6 6 8 16 19 26 27 23 42 30 26 8))
          (#$At.ER (7 6 7 18 12 17 23 20 31 32 29 26 12))))
        (1993
         ((#$At.CO (5 3 10 13 20 20 28 32 69 84 55 92 38))
          (#$At.CAL (3 11 9 9 17 18 21 16 23 39 29 22 2))
          (#$At.AG (9 3 8 10 16 18 16 12 24 33 12 14 5))
          (#$At.ER (7 6 7 18 12 17 23 20 31 32 29 26 12))
          (#$At.CSR1 (2 10 7 6 3 3 1 0 5 2 0 0 1))))
        (1994
         ((#$At.ER (7 6 7 18 12 17 23 20 31 32 29 26 12))
          (#$At.ATR1 (7 5 2 16 11 12 19 23 31 40 21 16 2))
          (#$At.CO (5 3 10 13 20 20 28 32 69 84 55 92 38))
          (#$At.GRF2 (0 1 2 12 2 4 5 8 1 3 6 4 1))
          (#$At.PHYB (4 5 5 10 18 22 23 25 27 29 20 19 7))))
        (1995
         ((#$At.CO (5 3 10 13 20 20 28 32 69 84 55 92 38))
          (#$At.PHYB (4 5 5 10 18 22 23 25 27 29 20 19 7))
          (#$At.CAL (3 11 9 9 17 18 21 16 23 39 29 22 2))
          (#$At.PHYA (4 6 6 8 16 19 26 27 23 42 30 26 8))
          (#$At.ASA1 (0 8 0 8 16 21 0 22 1 10 7 3 1))))
        (1996
         ((#$At.PHYB (4 5 5 10 18 22 23 25 27 29 20 19 7))
          (#$At.ASA1 (0 8 0 8 16 21 0 22 1 10 7 3 1))
          (#$At.CA2 (1 3 1 10 8 20 24 25 22 21 13 18 6))
          (#$At.CO (5 3 10 13 20 20 28 32 69 84 55 92 38))
          (#$At.PHYA (4 6 6 8 16 19 26 27 23 42 30 26 8))))
        (1997
         ((#$At.CO (5 3 10 13 20 20 28 32 69 84 55 92 38))
          (#$At.PHYA (4 6 6 8 16 19 26 27 23 42 30 26 8))
          (#$At.CA2 (1 3 1 10 8 20 24 25 22 21 13 18 6))
          (#$At.PHYB (4 5 5 10 18 22 23 25 27 29 20 19 7))
          (#$At.ER (7 6 7 18 12 17 23 20 31 32 29 26 12))))
        (1998
         ((#$At.CO (5 3 10 13 20 20 28 32 69 84 55 92 38))
          (#$At.PHYA (4 6 6 8 16 19 26 27 23 42 30 26 8))
          (#$At.PHYB (4 5 5 10 18 22 23 25 27 29 20 19 7))
          (#$At.CA2 (1 3 1 10 8 20 24 25 22 21 13 18 6))
          (#$At.ETR1 (2 1 4 3 12 7 10 23 19 24 17 17 10))))
        (1999
         ((#$At.CO (5 3 10 13 20 20 28 32 69 84 55 92 38))
          (#$At.ER (7 6 7 18 12 17 23 20 31 32 29 26 12))
          (#$At.ATR1 (7 5 2 16 11 12 19 23 31 40 21 16 2))
          (#$At.PI (5 4 2 10 9 13 13 13 29 18 23 18 8))
          (#$At.PHYB (4 5 5 10 18 22 23 25 27 29 20 19 7))))
        (2000
         ((#$At.CO (5 3 10 13 20 20 28 32 69 84 55 92 38))
          (#$At.PHYA (4 6 6 8 16 19 26 27 23 42 30 26 8))
          (#$At.ATR1 (7 5 2 16 11 12 19 23 31 40 21 16 2))
          (#$At.CAL (3 11 9 9 17 18 21 16 23 39 29 22 2))
          (#$At.RPS2 (0 0 6 9 3 15 3 21 18 36 18 12 15))))
        (2001
         ((#$At.CO (5 3 10 13 20 20 28 32 69 84 55 92 38))
          (#$At.PHYA (4 6 6 8 16 19 26 27 23 42 30 26 8))
          (#$At.CAL (3 11 9 9 17 18 21 16 23 39 29 22 2))
          (#$At.ER (7 6 7 18 12 17 23 20 31 32 29 26 12))
          (#$At.TIP (4 4 6 7 5 12 18 16 21 27 25 28 13))))
        (2002
         ((#$At.CO (5 3 10 13 20 20 28 32 69 84 55 92 38))
          (#$At.TIP (4 4 6 7 5 12 18 16 21 27 25 28 13))
          (#$At.PHYA (4 6 6 8 16 19 26 27 23 42 30 26 8))
          (#$At.ER (7 6 7 18 12 17 23 20 31 32 29 26 12))
          (#$At.STM (0 0 1 2 0 9 2 9 15 15 12 24 7))))
        (2003
         ((#$At.CO (5 3 10 13 20 20 28 32 69 84 55 92 38))
          (#$At.RPS2 (0 0 6 9 3 15 3 21 18 36 18 12 15))
          (#$At.TIP (4 4 6 7 5 12 18 16 21 27 25 28 13))
          (#$At.ER (7 6 7 18 12 17 23 20 31 32 29 26 12))
          (#$At.ETR1 (2 1 4 3 12 7 10 23 19 24 17 17 10))))))


;;; This uses the output of gene-models-per-year to compute:
;;; Distinct gene-model mentions per year:
;;; ((#$Year.1991 222) (#$Year.1992 246) (#$Year.1993 422)
;;;   (#$Year.1994 628) (#$Year.1995 730) (#$Year.1996 1010)
;;;    (#$Year.1997 1131) (#$Year.1998 1449) (#$Year.1999 2070)
;;;     (#$Year.2000 2628) (#$Year.2001 2053) (#$Year.2002 2363)
;;;      (#$Year.2003 822))
;;; This should start out at ~2x/paper, bcs folks are just jumping in
;;; on their favorite gene, and maybe one interactor, but there are a 
;;; limited number of genes, and an unlimited possible number of papers
;;; BUT, the empirical evidence is that we're not at the top yet.

(defun mentions-per-year (gpy)
  (loop for year in *year-frames*
	for j from 0
	collect
	(list year
	      (loop for model-info in gpy
		    as year-data = (second model-info) sum
		    (nth j year-data)
		    ))))


;;; This uses the output of gene-models-per-year to compute:
;;; New gene-model mentions in a given year:
;;; ((#$Year.1991 222) (#$Year.1992 130) (#$Year.1993 195)
;;;   (#$Year.1994 197) (#$Year.1995 208) (#$Year.1996 227)
;;;    (#$Year.1997 249) (#$Year.1998 295) (#$Year.1999 291)
;;;     (#$Year.2000 369) (#$Year.2001 235) (#$Year.2002 289) (#$Year.2003 41))

(defun not-previously-mentioned? (index year-data)
  (or (zerop index)
      (loop for k from 0 below index do
	    (when (plusp (nth k year-data)) (return nil))
	    finally (return t)
	    )))

(defun new-mentions-per-year (gpy)
  (loop for year in *year-frames*
	for j from 0
	collect
	(list year
	      (loop for model-info in gpy
		    as year-data = (second model-info) sum
		    (if (not-previously-mentioned? j year-data)
			(nth j year-data)
		      0)))))

;;; There are 3362 distinct principal investigators for these 9065 publications
;;;  This sounds high, although not totally implausible...

(defun unique-pis ()
  (frameloop (yf *year-frames*) (pubs) 
    with pris = nil 
    do 
    (frameloop (pub pubs) ((pri #$pi)) 
      when pri 
      do 
      (push pri pris))
    finally (return (purge-duplicates pris))))
 
;;; Returns a structure like:
;;;  (#$Year.1994
;;;    (#$Author.24071 #$Author.2965 #$Author.10474 #$Author.26291
;;;     ...) ...)
;;; To get this:
;;; Number of distinct PIs who have papers for a given year:
;;; 
;;; ((#$Year.1991 133) (#$Year.1992 161) (#$Year.1993 253)
;;;   (#$Year.1994 312) (#$Year.1995 414) (#$Year.1996 507)
;;;    (#$Year.1997 576) (#$Year.1998 619) (#$Year.1999 671)
;;;     (#$Year.2000 729) (#$Year.2001 890) (#$Year.2002 939)
;;;      (#$Year.2003 350))

;;; You can just do the obvious: (loop for (year pis) in * collect
;;; (list year (length pis))

(defun unique-pis-per-year ()
  (frameloop (yf *year-frames*) (pubs) 
    collect 
    (frameloop (pub pubs) ((pri #$pi)) 
      with pris = nil 
      when pri
      do 
      (push pri pris)
      finally (return (list yf (purge-duplicates pris)))    
      )))

(defun unique-pis-count-per-year ()
  (mapcar (lambda (x) (list (first x) (length (second x)))) 
          (unique-pis-per-year)))


;;; Of the 9065 publications, 958 have no principal investigator listed,
;;; approximately 11%.

(defun publications-without-pis ()
  (frameloop (yf *year-frames*) (pubs)
    with no-pi-pubs = nil
    do 
    (frameloop (pub pubs) ((pri #$pi))
      unless pri 
      do
      (push pub no-pi-pubs))
    finally (return no-pi-pubs)))

;;; There are 367 publications which have a principal investigator 
;;; who is not listed as one of the authors, approximately 4%.
;;;  This helps answer the question of whether PIs mean 
;;; "the person running the lab",
;;;  v. the most important person on the paper (corresponding author).

(defun publications-with-pis-who-are-not-authors ()
  (frameloop (yf *year-frames*) (pubs)
    with pris = nil
    do 
    (frameloop (pub pubs) (authors (pri #$pi))
      when (and pri (not (member pri authors))) 
      do
      (push (list pub pri) pris))
    finally (return pris)))

;;; Number of new PIs who appear for the first time in a given year (that
;;; is, they have no publication in a previous year that they are the PI
;;; for)
;;; 
;;; (133 133 183 204 272 291 313 311 320 327 435 380 60)

(defun new-pis ()
  (let ((pi-hash (make-hash-table :test 'eq)))
    (frameloop (yf *year-frames*) (pubs) 
      collect 
      (frameloop (pub pubs) ((pri #$pi))
        with new-pis = nil
        when pri 
        do
        (unless (gethash pri pi-hash) (push pri new-pis))
        (setf (gethash pri pi-hash) pri)
        finally (return new-pis)
        ))))

;;; Gives something like:
;;;  (... (#$At.SR1
;;;         ((#$Year.1991 (#$Author.36658)) (#$Year.1992 NIL) (#$Year.1993 NIL)
;;;          (#$Year.1994 (#$Author.14225)) (#$Year.1995 (#$Author.24467))...]

(defun pis-for-all-gene-models-per-year ()
  (loop for gene-model in (#^gene-models arab)
	when (#^pubs gene-model)
	collect 
	(list gene-model (pis-for-gene-model-per-year gene-model))
	))

(defun pis-for-gene-model-per-year (gene-model)
  (let ((pubs (#^pubs gene-model)))
    (when pubs
      (frameloop (yf *year-frames*) (year)
        collect 
        (list
         yf
         (purge-duplicates
          (remove-if
           'null
           (frameloop (pub pubs) ((pub-year #$year) (pri #$pi)) 
             when (= pub-year year) 
             collect pri
             ))))))))

(defun pis-ranked-by-publications-per-year ()
  (let ((global-ht (make-hash-table :test 'eq)))
    (append 
     (loop for year-frame in *year-frames*
           as yearhash = (make-hash-table :test 'eq)
           collect
           (list 
            year-frame
            (progn 
              (loop for pub in (#^pubs year-frame)
                    as pri = (#^pi pub)
                    when pri 
                    do 
                    (incf (gethash pri yearhash 0))
                    (incf (gethash pri global-ht 0))
                    )
              (sort (hash-table-contents yearhash) '> :key 'second)
              )))
     (list 
      (list :all-years (sort (hash-table-contents global-ht) '> :key 'second)))
     )))


(defun top-n-pis-by-publications-per-year (n)
  (let ((raw-data 
         (mapcar 
          (lambda (x) (list (first x) (first-n (1+ n) (second x))))
          (pis-ranked-by-publications-per-year))))
    (loop for datum in raw-data collect
          (list (first datum) 
                ;; Add actual name of each PI.  
                ;; Remove entry for pubs without PI's
                (loop for item in (second datum) 
                      when (first item) 
                      collect
                      (append item (list (#^name (first item))))
                      )))))
       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun fringe-go-nodes-of-gene-model (gene-model children-function)
  (minimal (#^go-id gene-model) children-function #'eq))

;;; This function creates the #$fringe-go-nodes slot 

(defun assign-fringe-go-node-slot-to-gene-models 
       (children-function remove-obsolete?)
  (loop for g in (#^gene-models arab) do 
        (setf (slotv g #$fringe-go-nodes) nil))
  (loop for gene-model in (#^gene-models arab) 
        for count from 0 
        as fringe-nodes = (fringe-go-nodes-of-gene-model 
                           gene-model children-function)
        do 
        (when remove-obsolete? 
          (setq fringe-nodes (remove-if 'obsolete-go-node? fringe-nodes)))
        (if (/= (length fringe-nodes) (length (#^go-id gene-model)))
            (setf (slotv gene-model #$fringe-go-nodes) fringe-nodes)
          (setf (slotv gene-model #$fringe-go-nodes) (#^go-id gene-model))
          ))
  (loop for gene-model in (#^gene-models arab)
        when (not (= (length (#^fringe-go-nodes gene-model)) 
                     (length (#^go-id gene-model))))
        sum 1
        ))
  
(defun fringe-nodes (gene-model)
  (#^fringe-go-nodes gene-model))

       
;; There are 35383 gene models with bindings...

(defun gene-models-with-go-bindings ()
    (loop for gene-model in (#^gene-models arab) 
          when (#^go-id gene-model)
          count 1
          ))

;; There are 2469 gene models without bindings

(defun gene-models-without-go-bindings ()
  (loop for gene-model in (#^gene-models arab)
        unless (#^go-id gene-model) 
        collect gene-model))
       


;;; There are 2900 distinct go-bindings: 
;;; (length (gene-model-go-bindings-that-are-descendants-of 
;;;      #$go.gene_ontology 'go-parents-f3))
;;; Of the 2900 distinct go-bindings, there are 744 go-bindings that 
;;; are a subclass of #$go.reaction.  

(defun gene-model-go-bindings-that-are-descendants-of (go-node parent-function)
  (purge-duplicates 
   (loop for gene-model in (#^gene-models arab) 
         as go-bindings = (#^go-id gene-model)
         nconc 
         (loop for binding in go-bindings 
               when (ancestor? go-node binding parent-function)
               collect binding
               ))))
                  
(defparameter *next-level-molecular-function-go-nodes*
  '(#$GO.NutrientReservoirActivity 
    #$GO.TranslationRegulatorActivity 
    #$GO.TripletCodon-AminoAcidAdaptorActivity
    #$GO.TranscriptionRegulatorActivity
    #$GO.EnzymeRegulatorActivity
    #$GO.ChaperoneRegulatorActivity
    #$GO.AntioxidantActivity 
    #$GO.Molecular_FunctionUnknown
    #$GO.Binding
    #$GO.TransporterActivity
    #$GO.StructuralMoleculeActivity
    #$GO.SignalTransducerActivity 
    #$GO.CatalyticActivity
    #$GO.MotorActivity))

(defparameter *next-level-biological-process-go-nodes*
  '(#$GO.ViralLifeCycle 
    #$GO.RegulationOfBiologicalProcess
    #$GO.PhysiologicalProcess
    ;;; #$GO.ObsoleteBiologicalProcess
    #$GO.Development
    #$GO.CellularProcess
    #$GO.Biological_ProcessUnknown
    #$GO.Behavior))

(defparameter *next-level-cellular-component-go-nodes*
  '(#$GO.Virion 
    #$GO.Unlocalized
    ;;; #$GO.ObsoleteCellularComponent
    #$GO.ImmunoglobulinComplex
    #$GO.Extracellular
    #$GO.Cellular_ComponentUnknown
    #$GO.Cell))


(defparameter *second-level-go-nodes* 
  (append *next-level-molecular-function-go-nodes*
          *next-level-biological-process-go-nodes*
          *next-level-cellular-component-go-nodes*))

;;; This function computes a mapping from each gene-model to the subset of 
;;; ANCESTOR-NODES that are in fact ancestors of the go-nodes of the gene-model
;;; This mapping is optionally stored in the GENE-MODEL-SLOT slot of each 
;;; gene-model.  

;;; This function also computes a mapping from all ANCESTOR-NODES to
;;; all the child nodes of itself that are referenced by any of the gene-models

(defun arab-ancestor-bindings 
       (ancestor-nodes 
        parent-function
        &key 
        (gene-model-slot #$level-two-go-nodes)
        (clear-second-level-gene-model-go-bindings? t)
        (print-table? t)
        )
  (let ((ht (make-hash-table :test 'eq)))
    (loop for gene-model in (#^gene-models arab) 
          as go-bindings = (#^go-id gene-model)
          do 
          (when clear-second-level-gene-model-go-bindings?
            (setf (slotv gene-model gene-model-slot) nil))
          (loop for binding in go-bindings 
                as binding-found? = nil
                do
                (loop for ancestor in ancestor-nodes
                      do 
                      (when (ancestor? ancestor binding parent-function)
                        (setq binding-found? t)
                        (pushnew binding (gethash ancestor ht))
                        (pushnew ancestor (slotv gene-model gene-model-slot)))
                      finally 
                      (when (not binding-found?) 
                        (pushnew binding (gethash :none ht)))
                      )))
    (when print-table?
      (terpri)
      (mapcar 
       (lambda (data) 
         (formatt "~4D ~A ~%" (length (second data)) (first data)))
       (sort (hash-table-contents ht) '> :key (lambda (x) (length (second x))))
       )
      (terpri))
    ht
    ))

#|

(ARAB-ANCESTOR-BINDINGS
 (append 
  *next-level-molecular-function-go-nodes* 
  *next-level-biological-process-go-nodes*
  *next-level-cellular-component-go-nodes*))

1043 #$GO.PhysiologicalProcess 
1035 #$GO.CatalyticActivity 
 377 #$GO.CellularProcess 
 260 #$GO.Cell 
 193 #$GO.TransporterActivity 
 147 #$GO.Binding 
 123 #$GO.Development 
  49 #$GO.SignalTransducerActivity 
  36 #$GO.EnzymeRegulatorActivity 
  28 #$GO.Unlocalized 
  27 NONE 
  18 #$GO.ObsoleteBiologicalProcess 
  16 #$GO.TranscriptionRegulatorActivity 
  12 #$GO.RegulationOfBiologicalProcess 
  12 #$GO.ViralLifeCycle 
   9 #$GO.ObsoleteCellularComponent 
   8 #$GO.StructuralMoleculeActivity 
   7 #$GO.Extracellular 
   5 #$GO.TranslationRegulatorActivity 
   4 #$GO.AntioxidantActivity 
   4 #$GO.Behavior 
   2 #$GO.MotorActivity 

|#

(defun gene-model-references-per-second-level-go-node ()
  (let ((ht (make-hash-table :test 'eq)))
    (loop for gene-model in (#^gene-models arab) do
          (loop for go-node in (#^level-two-go-nodes gene-model) do
                (incf (gethash go-node ht 0))
                ))
    (sort (hash-table-contents ht) '> :key 'second)
    ))

;;; For each second level go-node, the number of gene-models in arab that
;;; reference go-nodes that are descendants of this second level go-node.  


#+data
((#$GO.Cell 22941)
 (#$GO.PhysiologicalProcess 21839)
 (#$GO.CatalyticActivity 13203) 
 (#$GO.Binding 12900)
 (#$GO.CellularProcess 7755)
 (#$GO.TransporterActivity 3050)
 (#$GO.TranscriptionRegulatorActivity 3026)
 (#$GO.Development 1335)
 (#$GO.StructuralMoleculeActivity 1326)
 (#$GO.SignalTransducerActivity 864)
 (#$GO.EnzymeRegulatorActivity 466)
 (#$GO.Unlocalized 399)
 (#$GO.TranslationRegulatorActivity 301)
 (#$GO.ObsoleteBiologicalProcess 181)
 (#$GO.AntioxidantActivity 147)
 (#$GO.Extracellular 140)
 (#$GO.ObsoleteCellularComponent 118)
 (#$GO.RegulationOfBiologicalProcess 108)
 (#$GO.Behavior 58)
 (#$GO.ViralLifeCycle 52)
 (#$GO.MotorActivity 17))




;;; There are 16623 distinct authors.

(defun all-authors-list ()
  (let ((ht (make-hash-table :test 'eq)))
    (frameloop (yf *year-frames*) (pubs) do
      (frameloop (pub pubs) (authors) do
        (frameloop (author authors) () do (setf (gethash author ht) t))
        ))
    (hash-table-keys ht)
    ))

;;; This sets the #$pub-count-by-year slot for each author.

(defun author-publications-per-year ()
  (loop for author in (all-authors-list)
        as count-list = nil 
        as author-pubs = (#^pubs author)
        do 
        (loop for year-frame in *year-frames* 
              as year = (#^year year-frame)
              do 
              (push (count-if (lambda (pub) (= year (#^year pub))) author-pubs)
                    count-list 
                    )
        finally (setf (slotv author #$pub-count-by-year) (nreverse count-list))
        )))

;;; ((#$Year.1991 3.2048192) (#$Year.1992 3.7560976)
;;;  (#$Year.1993 3.7323077) (#$Year.1994 3.7153652)
;;;  (#$Year.1995 3.8111753) (#$Year.1996 3.892261) (#$Year.1997 4.02459)
;;;  (#$Year.1998 4.386076) (#$Year.1999 3.4611993) (#$Year.2000 3.1256468)
;;;  (#$Year.2001 4.5768194) (#$Year.2002 4.811594) (#$Year.2003 5.0697675))

(defun average-number-of-authors-per-year ()
  (frameloop (yf *year-frames*) (pubs)
    as sum = 0 
    do
    (loop for pub in pubs 
          do 
          (incf sum (length (#^authors pub)))
          )
    collect
    (list yf (/ sum (float (length pubs))))
    ))

;;; For example: 
#+data 
(#$Year.1991
 ((#$Author.1405 "Quail, PH" 6) (#$Author.9184 "Haughn, GW" 5)
  (#$Author.18322 "VanMontagu, M" 5)
  (#$Author.2530 "Meyerowitz, EM" 5) (#$Author.984 "Ausubel, FM" 4)))

(defun top-n-authors-per-year (n)
  (let ((authors (all-authors-list)))
    (frameloop (yf *year-frames*) (year)
      as index = (- year *base-year*)
      collect
      (list 
       yf
       (mapcar 
        (lambda (a) (list a (#^name a) (nth index (#^pub-count-by-year a)))) 
        (first-n 
         n
         (sort (copy-list authors)
               '>
               :key (lambda (a) (nth index (#^pub-count-by-year a)))
               ))
        )))))


;;; This computes the list of publications of an author where that author
;;; is the first author (this assumes that the ordering of the #^authors slot
;;; for a publication is the same as that given by the real publication).
;;; It also computes the first year in which an author became a PI.  

#| 

Note: Order of authors isn't right; Needs to use the info in the
author_order slot:

describe map_auth;
+--------------+---------------+------+-----+---------+-------+
| Field        | Type          | Null | Key | Default | Extra |
+--------------+---------------+------+-----+---------+-------+
| pub_id       | int(12)       |      | MUL | 0       |       |
| author_id    | int(12)       | YES  | MUL | NULL    |       |
| author_order | int(3)        | YES  |     | NULL    |       |
| last         | enum('Y','N') | YES  |     | NULL    |       |
+--------------+---------------+------+-----+---------+-------+
4 rows in set (0.00 sec)

mysql> select * from map_auth where pub_id = 12345;
+--------+-----------+--------------+------+
| pub_id | author_id | author_order | last |
+--------+-----------+--------------+------+
|  12345 |     28450 |            1 | N    |
|  12345 |     28451 |            2 | N    |
|  12345 |     28452 |            3 | N    |
|  12345 |     28453 |            4 | N    |
|  12345 |     28454 |            5 | Y    |
+--------+-----------+--------------+------+
5 rows in set (0.07 sec)

|#

;;; This function sets the following slots for each author-frame: 
;;; #$first-author-pubs, #$first-author-year, #$first-pi-year, #$first-pub-year

(defun author-info ()
  (loop for author in (all-authors-list) 
        as first-author-list = nil 
        as first-author-year = 3000
        as pi-year = 3000
        as first-pub-year = 3000 
        do 
        (loop for pub in (#^pubs author) do
              (if (eq author (first (#^authors pub)))
                  (progn 
                    (setq first-author-year 
                          (min first-author-year (#^year pub)))
                    (push pub first-author-list))
                (setq first-pub-year (min first-pub-year (#^year pub))))
              (when (eq author (#^pi pub))
                (setq pi-year (min pi-year (#^year pub)))))
        (when first-author-list 
          (setf (#^first-author-pubs author) first-author-list))
        (when (/= first-author-year 3000) 
          (setf (#^first-author-year author) first-author-year))
        (when (/= pi-year 3000) 
          (setf (#^first-pi-year author) pi-year))
        (setf (#^first-pub-year author) first-pub-year)
        ))

;;; Of the 16623 authors, 3297 of them are or became PIs.  
;;; Of the 16623 authors, 6539 were listed as the first author on at least
;;; one paper.  

;;; (count-if (lambda (a) (#^first-pi-year a)) (all-authors-list))
;;; (count-if (lambda (a) (#^first-author-year a)) (all-authors-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; There are 157933 references to go-nodes in the gene-model frames of arab. 
;;; (loop for gm in (#^gene-models arab) sum (length (#^go-id gm)))
 
;;; There are 140374 references to fringe-go-nodes.  
;;; (reduce '+ (fringe-go-node-reference-counts) :key 'second)

(defun fringe-go-node-reference-counts ()
  (let ((ht (make-hash-table :test 'eq)))
    (loop for gene-model in (#^gene-models arab) do
          (loop for go-node in 
                (or (#^fringe-go-nodes gene-model) (#^go-id gene-model)) do
                (incf (gethash go-node ht 0))
                ))
    (let ((ref-counts (sort (hash-table-contents ht) '> :keys 'second)))
      ref-counts
      )))



;;; The top 50 go-nodes referenced by arab gene-models.                        
       

#+data
                
((#$GO.Biological_ProcessUnknown 11412) 
(#$GO.Molecular_FunctionUnknown 10198) 
(#$GO.Cellular_ComponentUnknown 10125) 
(#$GO.EndomembraneSystem 5624) 
(#$GO.Chloroplast 4861) 
(#$GO.Nucleus 3676) 
(#$GO.Mitochondrion 3557) 
(#$GO.TranscriptionFactorActivity 2768) 
(#$GO.Membrane 2526) 
(#$GO.RegulationOfTranscription,Dna-Dependent 2411) 
(#$GO.AtpBinding 2286) 
(#$GO.DnaBinding 2220) 
(#$GO.ElectronTransport 1672) 
(#$GO.IntegralToMembrane 1591) 
(#$GO.Cytoplasm 1387) 
(#$GO.ProteinAminoAcidPhosphorylation 1323) 
(#$GO.Intracellular 1305) 
(#$GO.ZincIonBinding 1288) 
(#$GO.ProteinBiosynthesis 1228) 
(#$GO.RnaBinding 1162) 
(#$GO.ProteinBinding 1150) 
(#$GO.StructuralConstituentOfRibosome 916) 
(#$GO.ProteolysisAndPeptidolysis 868) 
(#$GO.Ubiquitin-ProteinLigaseActivity 852) 
(#$GO.Ribosome 851) 
(#$GO.Extracellular 753) 
(#$GO.UbiquitinLigaseComplex 749) 
(#$GO.ProteinUbiquitination 736) 
(#$GO.NucleicAcidBinding 718) 
(#$GO.ProteinSerine/ThreonineKinaseActivity 707) 
(#$GO.RegulationOfTranscription 707) 
(#$GO.DnaRecombination 569) 
(#$GO.N-TerminalProteinMyristoylation 565) 
(#$GO.MonooxygenaseActivity 540) 
(#$GO.CarbohydrateMetabolism 535) 
(#$GO.Transport 529) 
(#$GO.ElectronTransporterActivity 515) 
(#$GO.GtpBinding 503) 
(#$GO.OxygenBinding 479) 
(#$GO.RnaDependentDnaReplication 473) 
(#$GO.Rna-DirectedDnaPolymeraseActivity 472) 
(#$GO.SignalTransduction 446) 
(#$GO.CalciumIonBinding 433) 
(#$GO.KinaseActivity 364) 
(#$GO.DefenseResponse 361) 
(#$GO.Ubiquitin-DependentProteinCatabolism 345) 
(#$GO.MotorActivity 339) 
(#$GO.DnaRepair 332) 
(#$GO.AminoAcidTransport 322) 
(#$GO.ProteinFolding 321)) 


(defun verify-pub-refs ()
  (let ((all-refs (xsql *pdb* "select pub_id, pub_id2 from pubmatch "))
        (valid-cites nil)
        (bad-cites nil)
        (same-cites nil)
	)
    (cformatt "~D citation references" (length all-refs))
    (loop for (citing-pubid original-pubid) in all-refs 
          as original-frame = (pubid->pubframe original-pubid)
          as citing-frame = (pubid->pubframe citing-pubid)
          as original-year = (and original-frame (#^year original-frame))
          as citing-year = (and citing-frame (#^year citing-frame))
          when (and original-frame citing-frame original-year citing-year)
          do 
          (let ((record (list original-frame 
                              original-year
                              citing-frame
                              citing-year
                              )))
	    (push record valid-cites)
            (when (= original-year citing-year) 
              (push record same-cites))
            (unless (<= original-year citing-year)
              (push record bad-cites)))
          (pushnew citing-frame (#^citing-publications original-frame))
          (pushnew original-frame (#^cited-publications citing-frame))
          )
    (cformatt "~D Total valid citations" (length valid-cites))
    (cformatt "~D Same citation years" (length same-cites))
    (cformatt "~D Bad citation references" (length bad-cites))
    (cformatt "ORIGINAL           CITING")
    (terpri)
    (loop for j below 10 for valid-cite in valid-cites do
          (print valid-cite))))



(defun analyze-citations ()
  (frameloop (y *year-frames*) (year pubs) 
    with most-cited-paper-of-all-time = nil
    with most-cites-of-all-time = 0
    as total-refs = 0
    as pub-with-most-refs = nil
    as most-refs = 0
    do
    (loop for pub in pubs 
          as len = (length (#^citing-publications pub))
          do 
          (when (> len most-cites-of-all-time)
            (setq most-cited-paper-of-all-time pub)
            (setq most-cites-of-all-time len))
          (incf total-refs len)
          (when (> len most-refs)
            (setq pub-with-most-refs pub)
            (setq most-refs len)
            ))
    (cformatt "~D: Total cites for this year's papers: (~D)."
              year total-refs)
    (when pub-with-most-refs
      (cformatt "  Pub with most references: ~A (~A)" 
                pub-with-most-refs 
                (string-capitalize (#^title pub-with-most-refs))))
    finally 
    (progn 
      (terpri)
      (cformatt "Most cited paper: ~A (~D), (~A)"
                most-cited-paper-of-all-time 
                most-cites-of-all-time
                (string-capitalize (#^title most-cited-paper-of-all-time))))))

(defun calculate-closeness-of-mentioned-go-nodes ()
  (loop for node in (go-nodes-mentioned)
        do (Setf (#^closeness node)
                 (closeness node 'go-parents-f3 'go-children-f3))))
