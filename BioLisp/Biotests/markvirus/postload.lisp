(in-package :bio)

(organism-postload-processing
    ;; Put the exact name of the organism here as the first list element,
    ;; and a symbol you want bound to the organism's frame when the body
    ;; below gets executed.
    (:markvirus orgf)
    (
     :contig-transforms
     ;; Transformations for #$Contiguous-Sequence slots go here.
     (circular circular-string-to-t-or-nil)
     :gene-transforms 
     ;; Transformations for #$Genes slots go here.
     (direction string-to-direction)
     (encodes-protein circular-string-to-t-or-nil)
     (start-unknown circular-string-to-t-or-nil)
     (end-unknown circular-string-to-t-or-nil)
     (architecture)
     :transcript-transforms
     ;; Transformations for #$Transcripts slots go here.  Currently none.
     :proteins-transforms
     ;; Transformations for #$Proteins slots go here.  Currently none.
     )

  ;; Any other code you want executed after the above transformations
  ;; have been done goes here.  The organism frame is bound to the
  ;; symbol which is the second element of the initial list above
  ;; while this code is being executed.

  (cformatt "Alias symbols for ~A: " orgf)
  (cformatt "  ~A" (slotv orgf #$Organism-symbols))

  )

