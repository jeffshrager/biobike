;;; Copyright (c) 2002-2003 by Jeff Shrager and Mike Travers; All rights reserved.

;;; This software is made avilable for EDUCATIONAL PURPOSES
;;; ONLY, and WITHOUT ANY WARRANTY, express or implied, of
;;; its merchantability or fitness for a particular purpose. 

(in-package USER)

(defstruct reaction
      reactants  ; Lefthand side
      catalyst
      products   ; Righthand side
      )

(defstruct molecule
  abbrev     ; The abbreviation we'll use for convenience.
  fullname ; The pretty-printing name.
  type               ; S, NS, or INS
  )

(defvar *molecules* nil)
(defvar *reactions* nil)

(defmacro add-molecule (abbrev fullname type &rest constituents)
  `(push (make-molecule :abbrev ',abbrev
			:fullname ,fullname 
			:type ',type)
	 *molecules*))

(compile 
(defun find-molecules (abbrevs)
  (loop for molecule in *molecules*
        when (member (molecule-abbrev molecule) abbrevs)
        collect molecule)))

;;; Same thing, but only for one molcule:

(compile 
(defun find-molecule (abbrev)
  (loop for molecule in *molecules*
        when (eq (molecule-abbrev molecule) abbrev)
        do (return molecule))))

(defmacro add-reaction (reactants products enzyme)
  `(push (make-reaction :reactants (find-molecules ',reactants) 
			:catalyst ',enzyme
			:products (find-molecules ',products)
			)
	 *reactions*))

(defun init-objects ()
  (setq *molecules* nil)
  (add-molecule P "phosphatate" ins)
  (add-molecule C "carbon" ins)
  (add-molecule aden "adenosine" ns)
  (add-molecule guan "guanosine" ns)
  (add-molecule H+ "H+" ns)
  (add-molecule H2O "H2O" ns)
  (add-molecule NAD+ "NAD+" ns)
  (add-molecule FAD "FAD" ns)
  (add-molecule coa "CoA" s)
  (add-molecule atp "ATP" ins)
  (add-molecule adp "ADP" ins)
  (add-molecule gdp "GDP" ins)
  (add-molecule gtp "GTP" ins)
  (add-molecule NADH "NADH" ns)
  (add-molecule CO2 "Co2" ins)
  (add-molecule FADH2 "FADH2" ns)
  (add-molecule glu "glucose" s)
  (add-molecule fru "fructose" s)
  (add-molecule dha "dihydroxyacetone"  s)
  (add-molecule glh "glyceraldehyde" s)
  (add-molecule gla "glycerate" s)
  (add-molecule pyr "pyruvate" s)
  (add-molecule ace "acetyl" s)
  (add-molecule oxa "oxaloacetate" s)
  (add-molecule cit "citrate" s)
  (add-molecule ict "isocitrate" s)
  (add-molecule akg "a-ketoglutarate" s)
  (add-molecule suc "succinate" s)
  (add-molecule fum "fumarate" s)
  (add-molecule mal "malate" s)
  (add-molecule g6p "glucose 6-phosphate" s)
  (add-molecule f1p "fructose 1-phosphate" s)
  (add-molecule f6p "fructose 6-phosphate" s)
  (add-molecule fbp "fructose 1,6 bisphosphate" s)
  (add-molecule dap "dihydrozyacetone phosphate" s)
  (add-molecule g3p "glyceraldehyde 3-phosphate" s)
  (add-molecule bpg "1,3-bisphosphoglycerate" s)
  (add-molecule 3pg "3-phosphoglycerate" s)
  (add-molecule 2pg "2-phosphoglycerate" s)
  (add-molecule pep "phosphoenolpyruvate" s)
  (add-molecule aca "acetyl CoA" s)
  (add-molecule sca  "succinyl CoA" s)

  ;; Reactions:
  (setq *reactions* nil)
  (add-reaction (fru) (f1p) "Fructokinase")
  (add-reaction (f1p) (glh dap) "Fructose 1-phosphate aldolase")
  (add-reaction (glh) (g3p) "G3p kinase")
  (add-reaction (glu atp) (g6p adp) "Hexokinase")
  (add-reaction (g6p) (f6p) "Phosphoglucomutase")
  (add-reaction (f6p atp) (fbp adp) "Phosphofructokinase")
  (add-reaction (fbp) (dap g3p) "Aldolase")
  (add-reaction (dap) (g3p) "Isomerase")
  (add-reaction (P NAD+ g3p) (NADH H+ bpg) "G3p dehydrogenase")
  (add-reaction (bpg adp) (3pg atp) "Phosphoglycerate kinase")
  (add-reaction (3pg) (2pg) "Phosphoglyceromutase")
  (add-reaction (2pg) (pep H2O) "Enolase")
  (add-reaction (pep atp) (pyr adp) "Pyruvate kinase")
  (add-reaction (pyr NAD+ coa) (NADH H+ CO2 aca) "Citrate synthase")
  (add-reaction (cit) (ict) "Aconitase")
  (add-reaction (ict NAD+) (akg NADH H+ CO2) "Isocitrate dehydrogenase")
  (add-reaction (akg NAD+ coa) (sca NADH H+ CO2) "a-ketogluterate dehydrogenase complex")
  (add-reaction (sca gdp P) (suc gtp coa) "Succinyl CoA synthase")
  (add-reaction (suc FAD) (fum FADH2) "Succinate dehydrogenase")
  (add-reaction (fum H2O) (mal) "Fumerase")
  (add-reaction (mal NAD+) (oxa NADH H+) "Malate dehydrogenase")
  )	


(defun pp-reaction (r)
  (let* ((catalyst (reaction-catalyst r))
	 (reactants (reaction-reactants r))
	 (products (reaction-products r))
	 )
    (pp-mols reactants)
    (format t " --[~a]--> " catalyst)
    (pp-mols products)
    (format t "~%"))
  r)

(defun pp-mols (m*)
  (loop for m+ on m*
	do (format t "~A" (molecule-fullname (car m+)))
	(when (cdr m+)
	      (format t " + "))))

(defmacro pp-reactions (reactions)
  `(loop for reaction in ,reactions
	do (pp-reaction reaction)))


(defvar *env* nil)

(defun init-env ()
  (setq *env* 
	(find-molecules '(frk f1pa tk atp adp gdp gtp fru glu NAD+ FAD P hxk pfk pgm ald 
			      gapdh pgk pym eno pyk iso csy act idh adhc scs sdh fas mdh))))

(defvar *goal* nil)

(defun init-goal ()
  (setq *goal* (find-molecule 'mal)))

(defun init ()
  (init-objects)
  (init-env)
  (init-goal))

(compile 
(defun possible-reactions (env)
  (loop for reaction in *reactions*
	when (loop for reactant in (reaction-reactants reaction)
		   when (not (find reactant env))
		   do (return nil)
		   finally (return t))
	collect reaction)))

(compile 
(defun apply-reaction (reaction env)
  (append env
	  (loop for product in (reaction-products reaction)
		unless (member product env)
		collect product))))


(compile 
(defun paths (env goal &optional path)
  (cond ((find goal env) (list path))
	(t (loop for reaction in (possible-reactions env)
		 append (paths (apply-reaction reaction env) 
			       goal 
			       (cons reaction path)))))))

(compile 
(defun paths (env goal &optional path)
  (cond ((find goal env) (list path))
	(t (loop for reaction in (loop for reaction in (possible-reactions env)
				       when (not (member reaction path))
				       collect reaction)
		 append (paths (apply-reaction reaction env) goal (cons reaction path))))
	)))

(defmacro pathsto (goal)
  `(length (setq *pathways* 
                 (paths *env* (find-molecule ',goal)))))


(compile
(defun remove-duplicate-paths (path*)
  (loop for this-path in path*
	;; When the current path is NOT a superset of 
	;; anything else in the path list then collect it.
	;; (Careful not to remove it when it matches itself!)
	when (loop for target-path in path*
		   when (and (not (eq this-path target-path))
			     (superset? this-path target-path))
		   do (return nil)
		   finally (return t))
	collect this-path)))
		   
(compile
(defun superset? (s1 s2)
  (and (>= (length s1) (length s2))
       (loop for a in s2
	     when (not (member a s1))
	     do (return nil)
	     finally (return t)))))


(defmacro pathsto (goal)
  `(length (setq *pathways* 
		 (remove-duplicate-paths
		   (paths *env* (find-molecule ',goal))))))

(defun ppp (&optional (pwys *pathways*))
  (loop for path-n from 1 by 1
        as path in pwys
        do (format t "~%~%Pathway #~a:~%~%" path-n)
           ;; We copy the list 
           (pp-reactions (reverse (copy-list path)))))

;;; We have to come from a particular molecule, and we have to pass 
;;; the pathway down alongwith us, so that we can use the previous
;;; reaction to constrain the search.

(defun paths-between (from to &optional (env *env*) path)
  (cond ((member to env) (list path)) ; If we've found our goal, we're done! (LIST for appends up the stack.)
        (t (loop for reaction in (possible-reactions-from from env)
                 when (not (member reaction path))
                 append (paths-via (cons reaction path) to (apply-reaction reaction env))))
	))

;;; Track each SPECIFIC reaction product molecule into a new path.

(defun paths-via (path to env)
  (loop for link-mol in (reaction-products (car path))
        when (eq 's (molecule-type link-mol))
	append (paths-between link-mol to env path)))

;;; This is a constrained version of POSSIBLE-REACTIONS.
;;; The only difference is the (AND (MEMBER ...) ...) which 
;;; ensures that the reaction chosen also links to the FROM molecule.

(defun possible-reactions-from (from env)
  (loop for reaction in *reactions*
        as reactants = (reaction-reactants reaction)
        when (and (member from reactants)
                  (loop for reactant in reactants
                        when (not (find reactant env))
                        do (return nil)
                        finally (return t)))
       collect reaction))

;;; And we'll make a new PATHTO for this search as well; We'll
;;; call it FPATHSTO -- F for "Fast"!  This version doesn't even
;;; do the superset duplicate removal!

(defmacro fpathsto (from to)
  `(length (setq *pathways* 
                 (paths-between (find-molecule ',from)
                                (find-molecule ',to)
                                *env*))))

