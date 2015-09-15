;;; -*- Package:pb; mode: lisp; base: 10; Syntax: Common-Lisp; -*-

(in-package :pb)

(defvar *agnigh* nil)
(defvar *obsolete-go-nodes* nil)
(defvar *gene-models-with-obsolete-go-nodes* nil)                

;;; There are 16799 go nodes in the go hierarchy that are linked together
;;; with either ISA or PARTOF links starting from #$Go.Gene_Ontology.

(defun all-go-nodes-in-go-hierarchy 
       (&key (root #$go.gene_ontology) (children-function 'go-children-f3))
  (flet ((all ()
          (loop with gf = (poset-node-generator root children-function nil t)
             as node = (funcall gf) 
             until (null node) 
             collect node 
             ))) 
    (cond 
     ((eq root #$go.gene_ontology) 
      (or *agnigh* (setq *agnigh* (all))))
     (t (all)))))

#|
CC : 1372, BP : 8187, MF : 7298 

CC and BP nodes in common : 0
NIL

CC and MF nodes in common : 22
(#$GO.CollagenTypeIv #$GO.CollagenTypeVi #$GO.CollagenTypeVii
 #$GO.CollagenTypeXiii #$GO.CollagenTypeXv #$GO.CollagenTypeIx
 #$GO.CollagenTypeXii #$GO.CollagenTypeXiv #$GO.CollagenTypeXvi
 #$GO.FacitCollagen #$GO.CollagenTypeI #$GO.CollagenTypeIi
 #$GO.CollagenTypeIii #$GO.CollagenTypeV #$GO.CollagenTypeXi
 #$GO.FibrillarCollagen #$GO.CollagenTypeViii #$GO.CollagenTypeX
 #$GO.Short-ChainCollagen #$GO.Collagen #$GO.Elastin
 #$GO.SmallNucleolarRna)

MF and BP nodes in common : 37
(#$GO.BentDnaBinding #$GO.ChromatinInsulatorSequenceBinding #$GO.Holin
 #$GO.JuvenileHormoneBinding #$GO.EndogenousLipidAntigenBinding
 #$GO.ExogenousLipidAntigenBinding #$GO.LipidAntigenBinding
 #$GO.AcylCarrierActivity #$GO.Acyl-CoaBinding
 #$GO.DiacylglycerolBinding #$GO.Acyl-CoaOrAcylBinding
 #$GO.5[S]-Hydroxyperoxy-6e,8z,11z,14z-IcosatetraenoicAcidBinding
 #$GO.5-Hydroxy-6e,8z,11z,14z-IcosatetraenoicAcidBinding
 #$GO.5-Oxo-6e,8z,11z,14z-IcosatetraenoicAcidBinding
 #$GO.ArachidonicAcidBinding #$GO.IcosatetraenoicAcidBinding
 #$GO.IcosanoidBinding #$GO.FattyAcidBinding
 #$GO.Calcium-DependentPhospholipidBinding
 #$GO.PhosphatidylethanolamineBinding #$GO.PhosphatidylserineBinding
 #$GO.PhosphatidylinositolBinding
 #$GO.Phosphatidylinositol-3,4,5-TriphosphateBinding
 #$GO.Phosphatidylinositol-4,5-BisphosphateBinding
 #$GO.PhosphoinositideBinding #$GO.PhospholipidBinding
 #$GO.SphingolipidBinding #$GO.TriglycerideBinding #$GO.LipidBinding
 #$GO.SatelliteDnaBinding #$GO.DnaStrandAnnealingActivity
 #$GO.Single-StrandedTelomericDnaBinding #$GO.Single-StrandedDnaBinding
 #$GO.VesicleTransport #$GO.PhospholipidScrambling #$GO.VesicleFusion
 #$GO.VesicleTargeting)
|#

;;; This shows that the three top level go-branches are not independent.
;;; The above uses both isa and partof, but if you use just isa 
;;; (aka go-children-f1)
;;; then while there are less nodes than common there are still common nodes.

;;; When we eliminate obsolete nodes from the hierarchy and run 
;;; mutual-go-nodes, it tells us that there is indeed no overlap between 
;;; the three branches of the go.
;;; The children-function to use in this case is go-children-f4.  

(defun mutual-go-nodes (&key (cf 'go-children-f3))
  (flet ((all (x) (all-go-nodes-in-go-hierarchy 
                   :root x :children-function cf)))
    (let* ((cc (all #$go.cellular_component))
           (bp (all #$go.biological_process))
           (mf (all #$go.molecular_function))
           (iccbp (intersection cc bp)) 
           (iccmf (intersection cc mf))
           (ibpmf (intersection bp mf)))
      (formatt "CC : ~d, BP : ~d, MF : ~d ~%"
               (length cc) (length bp) (length mf))
      (formatt "CC and BP nodes in common : ~d~%~a~%" 
               (length iccbp) iccbp)
      (formatt "CC and MF nodes in common : ~d~%~a~%"  
               (length iccmf) iccmf)
      (formatt "MF and BP nodes in common : ~d~%~a~%" 
               (length ibpmf) ibpmf))))

;;; There are 8545 go-nodes that have more than one level.

(defun assign-go-levels (&key (slot-level #$f3-levels) (cf 'go-children-f3))
  (loop for n in (all-go-nodes-in-go-hierarchy :children-function cf)
        do (setf (slotv n slot-level) nil))
  (loop with gf = (poset-node-level-generator #$go.gene_ontology cf nil t)
        do 
        (multiple-value-bind (depth new-nodes old-nodes)
            (funcall gf)
          (when (null depth) (return))
          (loop for n in new-nodes do (push depth (slotv n slot-level)))
          (loop for n in old-nodes do (push depth (slotv n slot-level)))
          )))


;;; There are 2918 distinct GO-nodes referenced by the gene-models of arab.

(defun go-nodes-referenced-by-gene-models ()
  (purge-duplicates 
   (frameloop (gene-model (#^gene-models arab)) (go-id)
     nconc (copy-list go-id))))


;;; There are (1730 1188) leaf and non-leaf, respectively, go-nodes in the set 
;;; of go-nodes that are referenced by the gene-models, 
;;; when using only #^isa links.
;;; There are (1600 1318) leaf and non-leaf nodes when using both #^isa 
;;; and #^partof links.
;;; There are (2589 329) leaf and non-leaf nodes when using only #^partof.  

(defun gene-model-go-node-properties 
       (&optional (children-function 'go-children-f3))
  (let ((leaf-count 0) (non-leaf-count 0))
    (loop for go-node in (go-nodes-referenced-by-gene-models) do
          (if (go-node-is-leaf? go-node children-function)
              (incf leaf-count)
            (incf non-leaf-count)))
    (list leaf-count non-leaf-count)))

(defun go-node-is-leaf? (go-node &optional (children-function 'go-children-f3))
  (null (funcall children-function go-node)))


(defun go-statistics (&key (root #$go.gene_ontology)
                           (children-function 'go-children-f3))
  (terpri)
  (cformatt "Statistics for root = ~a, children function = ~s" 
            root children-function)
  (terpri)
  (let ((gn (loop with gf = (poset-node-generator root children-function nil t)
                  as node = (funcall gf) 
                  until (null node)
                  collect node
                  )))
    (loop with gf = (poset-node-level-generator 
                     root children-function nil t)
          do 
          (multiple-value-bind (depth new-nodes old-nodes)
              (funcall gf)
            (when (null depth) (return))
            (cformatt "Depth: ~2d, new: ~4d, old: ~4d, total: ~d"
                      Depth (length new-nodes) (length old-nodes)
                      (+ (length new-nodes) (length old-nodes))
                      )))
    (cformatt "Number of leaves: ~d"
              (count-if (lambda (n) (null (go-children-f3 n))) gn))
    (cformatt "Number of non-leaves: ~d"
              (count-if 'go-children-f3 gn))
    (cformatt "Total number of nodes: ~d" (length gn))
    (loop for n in gn
          with branching-sum = 0
          with count = 0
          as children = (go-children-f3 n)
          do 
          (when children 
            (incf branching-sum (length children))
            (incf count)
            )
          finally 
          (cformatt "Average branching factor for non-leaves: ~f"
                    (/ branching-sum (float count)))
          )))

;; Statistics for root = #$Go.Gene_Ontology, 
;;   children function = GO-CHILDREN-F3 

;; Depth:  1, new:    1, old:    0, total: 1
;; Depth:  2, new:    3, old:    0, total: 3
;; Depth:  3, new:   31, old:    0, total: 31
;; Depth:  4, new: 1175, old:    0, total: 1175
;; Depth:  5, new: 1346, old:   24, total: 1370
;; Depth:  6, new: 3327, old:  159, total: 3486
;; Depth:  7, new: 7600, old:  982, total: 8582
;; Depth:  8, new: 6713, old: 2710, total: 9423
;; Depth:  9, new: 6178, old: 4734, total: 10912
;; Depth: 10, new: 3383, old: 6132, total: 9515
;; Depth: 11, new: 2303, old: 5262, total: 7565
;; Depth: 12, new:  400, old: 2811, total: 3211
;; Depth: 13, new:   60, old:  948, total: 1008
;; Depth: 14, new:   26, old:  519, total: 545
;; Depth: 15, new:    6, old:  176, total: 182
;; Depth: 16, new:    0, old:   36, total: 36
;; Depth: 17, new:    0, old:   24, total: 24
;; Number of leaves: 11152
;; Number of non-leaves: 5647
;; Total number of nodes: 16799
;; Average branching factor for non-leaves: 4.0056667

;; Statistics for root = #$GO.Cellular_Component, 
;;   children function = GO-CHILDREN-F3

;; Depth:  1, new:    1, old:    0, total: 1
;; Depth:  2, new:    7, old:    0, total: 7
;; Depth:  3, new:  215, old:    0, total: 215
;; Depth:  4, new:  147, old:    4, total: 151
;; Depth:  5, new:  374, old:   21, total: 395
;; Depth:  6, new:  530, old:   80, total: 610
;; Depth:  7, new:  377, old:  215, total: 592
;; Depth:  8, new:  159, old:  248, total: 407
;; Depth:  9, new:   30, old:  155, total: 185
;; Depth: 10, new:    0, old:   47, total: 47
;; Depth: 11, new:    0, old:    7, total: 7
;; Depth: 12, new:    0, old:    5, total: 5
;; Depth: 13, new:    0, old:    1, total: 1
;; Depth: 14, new:    0, old:    2, total: 2
;; Number of leaves: 912
;; Number of non-leaves: 460
;; Total number of nodes: 1372
;; Average branching factor for non-leaves: 3.7934783

;; Statistics for root = #$GO.Biological_Process, 
;;;  children function = GO-CHILDREN-F3

;; Depth:  1, new:    1, old:    0, total: 1
;; Depth:  2, new:    8, old:    0, total: 8
;; Depth:  3, new:  348, old:    0, total: 348
;; Depth:  4, new:  471, old:   12, total: 483
;; Depth:  5, new: 1609, old:  100, total: 1709
;; Depth:  6, new: 3692, old:  557, total: 4249
;; Depth:  7, new: 4777, old: 1967, total: 6744
;; Depth:  8, new: 5571, old: 4047, total: 9618
;; Depth:  9, new: 3006, old: 5617, total: 8623
;; Depth: 10, new: 2269, old: 5056, total: 7325
;; Depth: 11, new:  396, old: 2735, total: 3131
;; Depth: 12, new:   58, old:  924, total: 982
;; Depth: 13, new:   26, old:  516, total: 542
;; Depth: 14, new:    6, old:  174, total: 180
;; Depth: 15, new:    0, old:   36, total: 36
;; Depth: 16, new:    0, old:   24, total: 24
;; Number of leaves: 4331
;; Number of non-leaves: 3856
;; Total number of nodes: 8187
;; Average branching factor for non-leaves: 3.26167

;; Statistics for root = #$GO.Molecular_Function,  
;;   children function = GO-CHILDREN-F3

;; Depth:  1, new:    1, old:    0, total: 1
;; Depth:  2, new:   16, old:    0, total: 16
;; Depth:  3, new:  612, old:    0, total: 612
;; Depth:  4, new:  730, old:    6, total: 736
;; Depth:  5, new: 1356, old:   26, total: 1382
;; Depth:  6, new: 3394, old:  329, total: 3723
;; Depth:  7, new: 1573, old:  514, total: 2087
;; Depth:  8, new:  448, old:  439, total: 887
;; Depth:  9, new:  347, old:  360, total: 707
;; Depth: 10, new:   34, old:  159, total: 193
;; Depth: 11, new:    4, old:   69, total: 73
;; Depth: 12, new:    2, old:   19, total: 21
;; Depth: 13, new:    0, old:    2, total: 2
;; Number of leaves: 5955
;; Number of non-leaves: 1343
;; Total number of nodes: 7298
;; Average branching factor for non-leaves: 6.2099776

;; Statistics for root = #$GO.Cellular_Component, 
;;   children function = GO-CHILDREN-F1

;; Depth:  1, new:    1, old:    0, total: 1
;; Depth:  2, new:    7, old:    0, total: 7
;; Depth:  3, new:  192, old:    0, total: 192
;; Depth:  4, new:   21, old:    0, total: 21
;; Depth:  5, new:   11, old:    3, total: 14
;; Depth:  6, new:    9, old:    0, total: 9
;; Depth:  7, new:    4, old:    0, total: 4
;; Number of leaves: 195
;; Number of non-leaves: 42
;; Total number of nodes: 237
;; Average branching factor for non-leaves: 7.285714

;; Statistics for root = #$GO.Biological_Process, 
;;   children function = GO-CHILDREN-F1

;; Depth:  1, new:    1, old:    0, total: 1
;; Depth:  2, new:    8, old:    0, total: 8
;; Depth:  3, new:  340, old:    0, total: 340
;; Depth:  4, new:  334, old:   10, total: 344
;; Depth:  5, new: 1066, old:   52, total: 1118
;; Depth:  6, new: 2402, old:  267, total: 2669
;; Depth:  7, new: 3050, old: 1213, total: 4263
;; Depth:  8, new: 3315, old: 2471, total: 5786
;; Depth:  9, new: 1987, old: 2895, total: 4882
;; Depth: 10, new: 1602, old: 2758, total: 4360
;; Depth: 11, new:  152, old: 1240, total: 1392
;; Depth: 12, new:   37, old:  163, total: 200
;; Depth: 13, new:   16, old:    5, total: 21
;; Depth: 14, new:   14, old:    0, total: 14
;; Depth: 15, new:    4, old:    0, total: 4
;; Number of leaves: 3020
;; Number of non-leaves: 2867
;; Total number of nodes: 5887
;; Average branching factor for non-leaves: 3.4925008

;; Statistics for root = #$GO.Molecular_Function, 
;;  children function = GO-CHILDREN-F1

;; Depth:  1, new:    1, old:    0, total: 1
;; Depth:  2, new:   16, old:    0, total: 16
;; Depth:  3, new:  612, old:    0, total: 612
;; Depth:  4, new:  730, old:    6, total: 736
;; Depth:  5, new: 1356, old:   26, total: 1382
;; Depth:  6, new: 3394, old:  329, total: 3723
;; Depth:  7, new: 1573, old:  514, total: 2087
;; Depth:  8, new:  446, old:  439, total: 885
;; Depth:  9, new:  347, old:  360, total: 707
;; Depth: 10, new:   34, old:  159, total: 193
;; Depth: 11, new:    4, old:   69, total: 73
;; Depth: 12, new:    2, old:   19, total: 21
;; Depth: 13, new:    0, old:    2, total: 2
;; Number of leaves: 5953
;; Number of non-leaves: 1343
;; Total number of nodes: 7296
;; Average branching factor for non-leaves: 6.2099776




(defun various-go-statistics ()
  (go-statistics)
  (go-statistics :root #$Go.cellular_component)
  (go-statistics :root #$Go.biological_process)
  (go-statistics :root #$Go.molecular_function)
  (go-statistics 
   :root #$Go.cellular_component :children-function 'go-children-f1)
  (go-statistics 
   :root #$Go.biological_process :children-function 'go-children-f1)
  (go-statistics 
   :root #$Go.molecular_function :children-function 'go-children-f1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; There are 759 go-nodes that are labeled (or at least, have the string)
;;; obsolete.  

;;; These are the obsolete go-nodes that TAIR gene-models point to:

;;; #$GO.MrnaSplicing 
;;; #$GO.ProteinTaggingActivity
;;; #$GO.ProteinDegradationTaggingActivity


(defun obsolete-go-node? (n)
  (or (search "obsolete" (#^fname n) :test 'char-equal)
      (initial-subsequence-of? 
       (#^go.definition n) "obsolete" :element-test 'char-equal)))

(defun gene-model-obsolete-nodes ()
  (purge-duplicates 
   (frameloop (gm (#^gene-models arab)) (go-id) nconc
     (loop for id in go-id 
           when (obsolete-go-node? id)
           collect id
           ))))


(defun mark-obsolete-go-nodes ()
  (loop for n in (all-go-nodes-in-go-hierarchy) 
        when (obsolete-go-node? n)
        collect
        (progn (setf (#^obsolete n) t) n)
        ))
    
;;; This routine demonstrates there is no overlap between the isa and partof
;;; parents of any go-node.  
(defun verify-no-duplicate-parents ()
  (loop for n in (all-go-nodes-in-go-hierarchy) 
        do
        (vwhen (dups (intersection (go-parents-f1 n) (go-parents-f2 n)))
          (cformatt "Duplicate: ~A" dups))))
      


;;; Verify that there are no redundant arcs in the go. I.e., there are no arcs
;;; such that if c is a direct child of b, and b is a direct child of a, then 
;;; c is also a direct child of a.  

;;; In fact there are a number of such redundant arcs, many of them having 
;;; to do with #$go.reaction 

(defun determine-redundant-arcs (&key (pf 'go-parents-f3))
  (loop for n in (all-go-nodes-in-go-hierarchy)
        with redundant-arcs = nil
        as parents = (funcall pf n)
        do 
        (loop for p in parents do
              (loop for q in parents 
                    unless (eq p q) 
                    when (member p (ancestors q pf))
                    do
                    (push (list n p q) redundant-arcs)
                    (cformatt "Redundant arc between ~A and ~A via ~A" n p q)
                    ))
        finally (return redundant-arcs)
        ))
                    
(defun direct-link-type (child parent)
  (cond 
   ((member parent (#^isa child)) 
    (values #$isa #$subclasses #$redundant-isa #$redundant-subclasses))
   ((member parent (#^partof child))
    (values #$partof #$parts #$redundant-partof #$redundant-parts))
   (t nil)))


(defun analyze-redundant-link (link)
  (destructuring-bind (child ancestor intermediary)
      link
    (cformatt "Between ~A (child) and ~A (ancestor): ~A" 
              child ancestor (direct-link-type child ancestor))
    (cformatt "Between ~A (child) and ~A (intermediary): ~A" 
              child intermediary (direct-link-type child intermediary))
    (cformatt "Intermediary ~A is a direct child of ~A ? ~A"
              intermediary ancestor 
              (direct-link-type intermediary ancestor))
    (when (#^obsolete child) 
      (cformatt "Child ~A is obsolete."  child))
    (when (#^obsolete ancestor) 
      (cformatt "Ancestor ~A is obsolete."  ancestor))
    (when (#^obsolete intermediary) 
      (cformatt "Intermediary ~A is obsolete."  intermediary))
    (terpri)
    ))

(defun delete-and-store-slot-element (element frame delete-slot store-slot)
  (setf (slotv frame delete-slot) (delete element (slotv frame delete-slot)))
  (pushnew element (slotv frame store-slot)))


(defun remove-redundant-links-and-obsolete-nodes ()
  (let* ((ohash (create-hash-table *obsolete-go-nodes* :mode :singleton))
         (redundant-arcs (determine-redundant-arcs)))
    ;; Remove links from child to ancestor and vice versa.  Store these links
    ;; in #^redundant-* slots.  
    (loop for (child ancestor nil) in redundant-arcs do
          (multiple-value-bind
              (child-link ancestor-link child-store-slot ancestor-store-slot)
              (direct-link-type child ancestor)
            (delete-and-store-slot-element
             child ancestor ancestor-link ancestor-store-slot)
            (delete-and-store-slot-element 
             ancestor child child-link child-store-slot)))
    ;; Remove all pointers to every obsolete go-node by iterating through
    ;; all the go-nodes and looking in their #$isa, #$subclasses, #$parts, 
    ;; and #$partof slots for any mention of these obsolete nodes and 
    ;; deleting them.  The deletions are stored in #$obsolete-* slots of
    ;; the node from which they were deleted.  
    ;; This process is essentially redundant if the go-children-f4 and 
    ;; go-parents-f4 generators are used since they test for and eliminate
    ;; obsolete nodes anyway.  Once this function is run, 
    ;; then theoretically the f3 generators instead of the f4
    ;; generators could be used, which are more efficient.  
    (loop for go-node in (all-go-nodes-in-go-hierarchy) 
          unless (gethash go-node ohash)
          do
          (flet ((doit (list-slot store-slot)
                   (loop for node in (slotv go-node list-slot)
                         when (gethash node ohash)
                         do
                         (delete-and-store-slot-element 
                          node go-node list-slot store-slot))))
            (doit #$subclasses #$obsolete-subclasses)
            (doit #$isa #$obsolete-isa)
            (doit #$parts #$obsolete-parts)
            (doit #$partof #$obsolete-partof)
            ))))

  
(defun remove-obsolete-go-nodes-from-gene-models ()
  (let* ((ohash (create-hash-table *obsolete-go-nodes* :mode :singleton)))
    (flet ((obsolete? (x) (null (gethash x ohash))))
      (loop for gm in (#^gene-models arab) 
          as ids = (#^go-id gm)
          do
          (setf (#^obsolete-go-id gm) (remove-if #'obsolete? ids))
          (setf (#^go-id gm) (remove-if-not #'obsolete? ids))
          (when (#^obsolete-go-id gm) 
            (pushnew gm *gene-models-with-obsolete-go-nodes*)) 
          ))))



(defun restore-obsolete-go-nodes-to-gene-models ()
  (loop for gm in (#^gene-models arab) do
        (setf (#^go-id gm) (append (#^obsolete-go-id gm) (#^go-id gm)))
        (setf (#^obsolete-go-id gm) nil)))
       
                 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; We want to do go-processing using the parent and children functions which
;;; a) use both the #$isa and #$partof hierarchies.  
;;; b) do not consider obsolete nodes.


(defun trbl-go-processing 
       (&key (root #$go.gene_ontology) 
             (cf 'go-children-f3) 
             (pf 'go-parents-f3))
  
  ;; Step 0.546 remove $go.reaction from all isa lists
  ;; bcs #$go.reaction screws up pretty much everything. 
  (loop for node in (all-go-nodes-in-go-hierarchy :children-function cf)
        do (setf (#^isa node)
                 (remove #$go.reaction (#^isa node))))

  ;; step 1.  Find and tag all obsolete go-nodes.  
  ;; Obsolete go-nodes are those which either have the string "obsolete"
  ;; in their #$fname or the #$go.documentation slot has "obsolete" 
  ;; as its first characters.  
  (cformatt "Marking obsolete nodes")
  (setq *obsolete-go-nodes* (mark-obsolete-go-nodes))

  ;; step 1.5  Remove the obsolete go-nodes from the go-hierarchy
  ;; and delete redundant links (see below).  
  (cformatt "Removing redundant links and obsolete nodes.")
  (remove-redundant-links-and-obsolete-nodes)
  
  ;; step 2. Create a list of all go-nodes that are not obsolete.  
  ;; Since the obsolete nodes have already been removed above, we can
  ;; use the go-parents-f3 and go-children-f3 functions from this point on
  ;; and those nodes will not be processed.  
  ;; Note: The *agnigh* variable is set to this list of go-nodes.  
  (setq *agnigh* nil) 
  (cformatt "Number of non-obsolete go-nodes: ~D" 
            (length (all-go-nodes-in-go-hierarchy :children-function cf)))
  (cformatt "Number of obsolete go-nodes: ~D"
            (count-if (lambda (x) (#^obsolete x)) *agnigh*))
  
  ;; step 3.  Assign go-levels.  
  ;; There can be more than one go-level assigned to a given
  ;; node because the GO is not a tree but a POSET (partially ordered set).
  ;; We keep track of all the levels that a particular go-node belongs to.  
  (cformatt "Assigning levels to #$f3-level slot")
  (assign-go-levels :slot-level #$f3-levels :cf cf)
  (cformatt "Number of go-nodes with multiple levels: ~D"
            (loop for g in (all-go-nodes-in-go-hierarchy) 
                  when (> (length (slotv g #$f3-levels)) 1)
                  sum 1))

  ;; step 4. Print out summary of go-hierarchy statistics.  
  (go-statistics :children-function cf) 
  (go-statistics :root #$Go.cellular_component :children-function cf)
  (go-statistics :root #$Go.biological_process :children-function cf)
  (go-statistics :root #$Go.molecular_function :children-function cf)

  ;; step 5.  
  ;; Create a list of all the go-nodes at each level and store these 
  ;; lists in the #$go.gene_ontology node (the top level go-node). 
  ;; First get rid of existing depth-slots, then regenerate lists of nodes
  ;; at each depth.  
  (cformatt "Creating list of go-nodes at each depth")
  (loop for j from 1 to 40 
        as slot = (frame-fnamed (formatn "depth-~D" j))
        when slot 
        do (delete-slot root slot))
  (loop with gf = (poset-node-level-generator root cf nil t)
        do 
        (multiple-value-bind (depth new-nodes old-nodes)
            (funcall gf)
          (when (null depth) (return))
          (setf (slotv root (frame-fnamed (formatn "z-depth-~D" depth) t)) 
                (list depth new-nodes old-nodes)
                )))

  ;; step 7. Create for each go-node a list of all its ancestors.  
  (cformatt "Creating list of ancestors for every go-node")
  (loop for g in (all-go-nodes-in-go-hierarchy) 
        do 
        (setf (slotv g #$f4-ancestors) (ancestors g pf))
        )

  ;; step 8.  Creates set of fringe-nodes for each gene-model.
  ;; The fringe-nodes are all the go-nodes pointed to by a gene-model
  ;; except those which are ancestors of other go-nodes pointed to by the
  ;; same gene-model.  This takes a couple of minutes to run.  
  (cformatt "Creating fringe-nodes for gene-models.  Please be patient.")
  (cformatt "The fringe list is created with obsolete go-nodes removed.")
  (cformatt "There are ~D gene-models with fringe-node sets different than"
            (assign-fringe-go-node-slot-to-gene-models cf t))
  (cformatt "the list of Go Nodes in the go-id slot of the gene model")

  ;; step 9.  Remove obsolete go-nodes from gene-models.  
  (cformatt "Removing obsolete go-nodes from gene-models.")
  (remove-obsolete-go-nodes-from-gene-models)
  (cformatt "~D gene-models had obsolete go-nodes removed." 
            (length *gene-models-with-obsolete-go-nodes*))

  ;; step 10.  Compute second-level go-bindings for each gene-model
  (cformatt "Computing second level go bindings for gene models")
  (arab-ancestor-bindings
   (append 
    *next-level-molecular-function-go-nodes* 
    *next-level-biological-process-go-nodes*
    *next-level-cellular-component-go-nodes*) pf)

  ;; step 11.  Compute author information.  
  (cformatt "Computing author information.")
  (author-publications-per-year)
  (author-info)
  
  )

;;; Run over the whole graph and calculate degree; Returns the max.

(defun calculate-and-store-degrees 
       (&key (parent-function 'go-parents-f3)
             (children-function 'go-children-f3)
             &aux (max 0))
  (loop for node in *agnigh*
        as n =  (setf (#^degree node)
                      (+ (length (funcall parent-function node))
                         (length (funcall children-function node))))
        do (if (> n max) (setq max n)))
  ;; Second pass normalizes
  (loop for node in *agnigh*
        do  (setf (#^normalized-degree node)
                      (float (/ (#^degree node)
                                max))))
  ;; Avg the immed. neighborhood
  (loop for node in *agnigh*
        do  
        (setf (#^avg-normalized-degree node)
              ;; Include yourself.
              (let ((sum (#^normalized-degree node)) (n 1))
                (loop for node in (funcall parent-function node)
                      ;; Due to obsolete complexity some neighbors
                      ;; don't have values (cf. #$GO.ClathrinAdaptorComplex)
                      as nd = (#^normalized-degree node)
                      when nd
                      do 
                      (incf sum nd)
                      (incf n))
                (loop for node in (funcall children-function node)
                      ;; Due to obsolete complexity some neighbors
                      ;; don't have values (cf. #$GO.ClathrinAdaptorComplex)
                      as nd = (#^normalized-degree node)
                      when nd
                      do 
                      (incf sum nd)
                      (incf n))
                (float (/ sum n)))))
  max)
