;(load-module :snark)
;(use-module :snark)

(load-organism :p9313)
(load-organism :s6803)
(load-organism :pmed4)
(load "/usr/local/biolisp/data/hiharadat.lisp")

#|
Need to do:

(use-module :snark)
(do-snark-shadowing-imports)
(shadowing-import 'snark::assert)
(use-snark)

|#

(defun init-biodeducta ()
  (init-snark)
  (make-declarations)
  (make-assertions)
  )

(defun init-snark ()
  (setq prove-all nil)
  (setq evaluables 
        '(list genes orthologs intersection set-difference t1 project
               select selectneg select-pair select-pair-rel 
               map-compose neg-compose 
               mapcan function funcall of 
               evenp < hihara-exceeds-2))
  (initialize)
  (use-resolution t)
  (use-paramodulation nil)
  (use-hyperresolution nil)
  (use-negative-hyperresolution nil)
  (use-factoring t)
  (assert-supported nil)
  (use-numbers-as-constructors)
  (use-code-for-numbers)
  (use-lisp-types-as-sorts t) 
  (agenda-length-limit 10000)
  (print-rows-when-derived nil)
  (print-rows-when-given nil)
  (print-rows-when-processed nil)
  (print-final-rows)
  (use-literal-ordering-with-resolution 'literal-ordering-a)
  (use-literal-ordering-with-paramodulation 'literal-ordering-a)
  (use-literal-ordering-with-hyperresolution 'literal-ordering-p)
  (use-literal-ordering-with-negative-hyperresolution 'literal-ordering-n)
  (use-term-ordering :rpo)
  (use-default-ordering nil)
  (rpo-status :left-to-right)
  (ordering-functions>constants t)
  (use-subsumption nil)
  (use-answers-during-subsumption t)
  (use-simplification-by-equalities nil)
  )

(defun make-declarations ()
  (declare-sort 'thing)
  (declare-subsorts 'thing 'name 'entity)
  (declare-disjoint-subsorts 'entity 'bio-entity 'number 'pair 'bool)
  (declare-disjoint-subsorts 
   'bio-entity 
   'gene 'rna 'mrna 'protein 'organism 'list
   'molecule)
  (declare-subsorts 'organism 'species)
  (declare-subsorts 'protein 'enzyme)
  (declare-constant (f2s #$synechocystis_pcc6803) 
                    :sort 'species
                    :alias 's6803)
  (declare-constant (f2s #$prochlorococcus_marinus_med4)
                    :sort 'species
                    :alias 'med4) 
  (declare-constant (f2s #$prochlorococcus_marinus_mit9313) 
                    :sort 'species
                    :alias 'mit9313)
  (declare-relation 'gene-in-organism 2
                    :sort '((1 gene)
                            (2 organism)))
  (declare-relation 'gene-in-organism-code 2
                    :sort '((1 gene)
                            (2 organism))
                    :satisfy-code 
                    (list
                     (function-satisfies-relation #'genes 1) 
                     (function-satisfies-relation #'organisms 2))
                    :weight 13)
  (declare-relation 'ortholog 2 :sort '((t gene)))
  (declare-relation 'gene-has-ortholog-in-organism 3
                    :sort '((1 gene)
                            (2 gene)
                            (3 organism))
                    :satisfy-code 
                    (function-satisfies-relation #'orthologs-in-organism 2)
                    :falsify-code
                    (function-falsifies-relation #'orthologs-in-organism 2)
                    :weight 2)
  (declare-function 'hihara-mean-regulation-ratio 1
                    :sort '(number
                            (1 gene))
                    :rewrite-code
                    (function-rewrites-operator 
                     #'hihara-mean-regulation-ratio))
  (declare-relation 'photosynthesis-related 1
                    :sort '((1 gene))
                    :rewrite-code
                    (function-rewrites-operator #'photosynthesis-related)
                    :weight 1)
  (declare-ordering-greaterp                              
   'ortholog 'gene-in-organism 
   'gene-in-organism-code
   'photosynthesis-related
   'gene-has-ortholog-in-organism)
  )

(defun make-assertions ()
  (assert
   '(iff
     (gene-in-organism ?gene ?organism)
     (gene-in-organism-code ?gene ?organism))
   :sequential :uninherited
   :name 'definition-of-gene-in-organism-code)
  (assert
   '(iff
     (and
      (ortholog ?gene ?gene1)
      (gene-in-organism ?gene1 ?organism))
     (gene-has-ortholog-in-organism ?gene ?gene1 ?organism))
   :sequential :uninherited
   :name 'definition-of-gene-has-ortholog-in-organism)
  )

(defun test-gene-in-organism ()
  (init-biodeducta)
  (find-one 
   '(gene-in-organism ?gene s6803)
   :answer '?gene
   :time-limit 45
   ))

(defun test-organism-of-gene ()
  (init-biodeducta)
  (find-one 
   '(gene-in-organism slr1413 ?organism)
   :answer '?organism
   ))

(defun test-gene-has-ortholog-in-organism ()
  (init-biodeducta)
  (find-one '(and (gene-in-organism ?gene med4) 
                  (gene-has-ortholog-in-organism ?gene ?gene1 s6803 ))
            :answer '(pair ?gene ?gene1)))

(defun test-gene-has-no-ortholog-in-organism ()
  (init-biodeducta)
  (find-one '(and (gene-in-organism ?gene med4) 
                  (gene-has-ortholog-in-organism ?gene ?gene1 s6803)
                  (not (exists
                        ((gene3 :sort gene))
                        (gene-has-ortholog-in-organism ?gene gene3 mit9313))))
            :answer '(pair ?gene ?gene1))
  )

(defun test-hihara-ratio ()
  (init-biodeducta)
  (find-one '(and (gene-in-organism ?gene1 s6803)
                  (> (hihara-mean-regulation-ratio ?gene1) 5))
            :answer '(list ?gene1 (hihara-mean-regulation-ratio ?gene1))
           )
  )

(defun test-hihara-problem ()
  (init-biodeducta)
  (find-one '(and (gene-in-organism ?gene med4) 
		  (gene-has-ortholog-in-organism ?gene ?gene1 s6803)
                  (not (exists
                        ((gene3 :sort gene))
                        (gene-has-ortholog-in-organism ?gene gene3 mit9313)))
                   (> (hihara-mean-regulation-ratio ?gene1) 2))
             :answer '(list 
                      ?gene ?gene1 (hihara-mean-regulation-ratio ?gene1))
            :time-limit 35)
  )

(defun test-hihara ()
  (init-biodeducta)
  (find-one '(and (gene-in-organism ?gene med4)
                  (photosynthesis-related ?gene)
                  (ortholog ?gene ?gene1)
                  (gene-in-organism ?gene1 s6803)
                  (not (exists
                        ((gene3 :sort gene))
                        (and (ortholog ?gene gene3)
                             (gene-in-organism gene3 mit9313))))
                  (> (hihara-mean-regulation-ratio ?gene1) 2))
            :answer '(list ?gene ?gene1 (hihara-mean-regulation-ratio ?gene1))
            :time-limit 35
            ))

(defun test-gene-in-organism-has-no-ortholog-in-organism ()
  (init-biodeducta)
  (find-one '(and (gene-in-organism ?gene med4) 
                  (ortholog ?gene ?gene1)
                  (gene-in-organism ?gene1 s6803 )
                  (not (exists
                        ((gene3 :sort gene))
                        (and (ortholog ?gene gene3)
                             (gene-in-organism gene3 mit9313)))))
            :answer '(list ?gene ?gene1))
  )

(defun orthologs-in-organism (gne org)
  (let* ((ortho (and (#^encodes-protein gne)
                     (two-way-ortholog-of gne org 0.001))))  
    (and ortho (list (slotv ortho #$gene)))
    ))                

(defun photosynthesis-related (gne)
  (if (or (search "photo" (#^annotation gne) :test
                  #'char-equal)
          (search "light" (#^annotation gne) :test
                  #'char-equal))
      true
    nil))

(defun hihara-mean-regulation-ratio (gene-frame-or-name)
  (let* ((number-or-nil
          (let* ((name (cond ((isframe? gene-frame-or-name)
                              (second (string-split 
                                       (#^fname gene-frame-or-name) #\.)))
                             (t gene-frame-or-name)))
                 (values (assocdr name bio::*hihara-results*
                                  :test #'string-equal)))
            (when values (mean values)))))
    (if (numberp number-or-nil)
        number-or-nil
      0)))





