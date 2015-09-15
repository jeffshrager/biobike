(IN-PACKAGE :bbi)

(defparameter kegg-id nil)
(defvar kegg-categories nil)

(defun load-kegg-ids ()
  "loads file containing kegg id information, parses it,
  and stores an id in every gene it can.
;  in addition, creates a table, kegg-id, that contains
  in addition, creates a table, kegg-categories, that contains
  the english representation of the kegg categories"

  (when (null kegg-id) 
;    (setq kegg-id (make-garray '($) :if-accessed-location-does-not-exist nil)))
    (setq kegg-categories (make-garray '($) :if-accessed-location-does-not-exist nil)))

  (let ((kegg-categories-raw 
         (string-to-list
          (file-to-string 
           (translate-simple-lp "biol:kegg-categories-and-genes.txt"))))
        (organisms (loaded-organisms)))

    ;; file has format (by example):
    ;;   ((("syn" "synechocystis pcc 6803" "s6803" "00010"
    ;;            "glycolysis / gluconeogenesis")
    ;;       (("sll0018" . "cbba, cfxa, fbaa, fda; fructose-bisphosphate aldol")
    ;;        ("sll0395" . "hypothetical protein [ec:5.4.2.1]; k01834 phosphog")
    ;;        ...))
    ;;    (("syn" "synechocystis pcc 6803" "s6803" "00020"
    ;;            "citrate cycle (tca cycle)")
    ;;       (("sll0401" . "glta, glut; citrate synthase [ec:2.3.3.1]; k01647 ")
    ;;        ("sll0823" . "sdhb; succinate dehydrogenase iron-sulfur protein ")
    ;;        ...))
    ;;    (("syw" "synechococcus wh 8102" "s8102" "00561"
    ;;            "glycerolipid metabolism")
    ;;       (("synw0167" . "utative glycosyl transferase family protein [ec:2 ")
    ;;        ("synw0939" . "utative glycerol dehydrogenase [ec:1.1.1.6]; k000 ")
    ;;        ...)))

    (loop for (cat-info gene-info) in kegg-categories-raw
          with *suppress-warnings* = t
          with count = 0
          as org = (ref cat-info 1)
          as cat-number = (parse-integer (ref cat-info 4))
          as cat-id = (s+ "ko" (ref cat-info 4))
          as cat-name = (ref cat-info 5)
          when (not (equal org "syf"))  ; our version of syf differs from kegg's
          do 
          (when organisms
            (loop named fred
             for gene-name-source in gene-info
             as gene = (loop for organism in *loaded-organisms*
                             as prefix = (slotv organism #$organism-prefix)
                             as gene-name = (ref gene-name-source 1)
                             as frame = 
                             (frame-fnamed (s+ prefix gene-name))
                             when frame
                             do (return frame)
                             finally
                             (progn
                               (incf count)
                               (cond
                                ((< count 10) 
                                 (format t "~&gene ~a not found!" gene-name))
                                ((= count 10)
                                 (format t "~&and more genes not found..."))
                                (t nil))
                               (return-from fred nil))
                             )
             as old-kegg-id = (ref gene #$kegg-id)
             do
             (def-frame gene #$kegg-id (append old-kegg-id (list cat-id)))))
          do
    ;      (setf (ref bbi::kegg-id cat-id) cat-name)
          (setf (ref bbi::kegg-categories cat-id) cat-name)
    ;      (setf (ref bbi::kegg-id cat-number) cat-name))))
          (setf (ref bbi::kegg-categories cat-number) cat-name))))

