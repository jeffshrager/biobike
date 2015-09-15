(in-package :bio)

(setq *dmode-exprs* '(

  "(setf syn (load-organism \"syn6803\"))"
  "(length (#^genes syn))"
  "(find-frames \"Glucokinase\")"
  "(df #$go.GlucokinaseActivity)"
  "(#^go.related-genes #$go.glycolysis)"
  "(#^go.related-genes #$go.kinaseactivity)"
  "(setf glycolytic-genes (#^go.related-genes #$go.glycolysis))"
  "(setf kinases (#^go.related-genes #$go.kinaseactivity))"
  "(intersection glycolytic-genes kinases)"
  "(intersection (#^go.related-genes #$go.glycolysis)
		(#^go.related-genes #$go.kinaseactivity))"
  "(setf gk-genes *)"
  "(setf g1 (first gk-genes)
	g2 (second  gk-genes))"
  "(extract-sequence g1)"
  "(setq g1s *)"
  "(setq g1-init (subseq g1s 0 30))" 
  "(nt-complement g1-init)"
  "(align '(\"the rain in spain falls mainly on the plain\" \"the plain in spain is mainly in the rain\"))"
  "(df *)"
  "(setq cr (align (list g1 g2)))"
  "(#^alignments cr)"
  "(#^consensus (align gk-genes))"
   "(find-frames \"glucokin\")"
   "(setf gkframe (first *))"
   "(#^go.related-genes gkframe)"
   "(#^alignments (align (#^go.related-genes gkframe)))"
   "(setq c (#^consensus (align (list g1 g2))))"
   "(count #\* c)"
   "(length c)"
   "(* 100.0 (/ (count #\* c) (length c)))"
   "(defun score-alignment (genes)
    (let ((c (#^consensus (align genes))))
      (* 100.0 (/ (count #\* c) (length c)))))"
  "(score-alignment (list g1 g2))"
  "(defun every-pair (list)
    (cond ((null (cdr list)) nil)
          (t (append 
               (mapcar (lambda (next) (list (car list) next))
                       (rest list))
               (every-pair (cdr list))))))"
   "(every-pair '(a s d f))"
   "gk-genes"
   "(length gk-genes)"
   "(setq gk-genes (subseq gk-genes 0 9))"
   "(loop for pair in (every-pair gk-genes)
           collect (list pair (score-alignment pair)))"
   "(setq scores *)"
   "(setq sorted-scores (sort scores '> :key 'second))"
  "(defun best-related-gene-pair (genes)
    \"Given a gene ontology category, like #$go.glycolysis,
     figure out the pair of genes among all those in the category
     that are most closely related, by alignment.\"
    (first 
      (sort 
        (loop for pair in (every-pair genes)
              collect (list pair (score-alignment pair))))))"
   "(best-related-gene-pair gk-genes)"

))
