(in-package :pb)

(defvar *stop-table* 
  (create-hash-table 
   '("the"
     "of" "and" "in" "to" "a" "that" "is" "with" "by" "for" "are" 
     "was" "as" "gene" "plants" "we" "from" "this" "genes" "were" 
     "plant" "arabidopsis" "these" "protein" 
     "an" "expression" "on" "be" "have" "which" "at" "not" "proteins" 
     "cell" "or" "cells" "been" "has" "also" "but" "two" "development" 
     "analysis" "their" 
     "both" "results" "activity" "acid" "mutant" 
     "no" "growth" "all" "sequence" "mutants" 
     "other" "role" "rna" "it" "during" 
     "species" "than" "can" "leaves" "between" 
     "leaf" "identified" "may" "levels" "show" 
     "thaliana" "response" "function" "involved" "into" 
     "resistance" "using" "different" "expressed" "showed" 
     "genome" "its" "genetic" "found" "molecular" 
     "stress" "similar" "such" "one" "our" 
     "here" "dna" "sequences" "light" "root" 
     "high" "responses" "however" "used" "suggest" 
     "data" "transgenic" "family" "pathway" "important" 
     "more" "regulation" "higher" "signal" "studies" 
     "three" "abstract" "several" "only" "increased" 
     "isolated" "control" "mrna" "under" "within" 
     "most" "many" "revealed" "amino" "present" 
     "domain" "after" "cdna" "well" "level" 
     "silencing" "including" "rice" "when" "kinase" 
     "roots" "region" "known" "induced" "promoter" 
     "signaling" "conserved" "new" "functional" "transcription" 
     "auxin" "low" "system" "enzyme" "membrane" 
     "indicate" "functions" "addition" "formation" "rnas" 
     "model" "complex" "accumulation" "study" "pathways" 
     "conditions" "some" "mechanisms" "tobacco" "related" 
     "first" "recent" "number" "seed" "although" 
     "specific" "changes" "wild-type" "through" "highly" 
     "degradation" "required" "observed" "reduced" "suggesting" 
     "biosynthesis" "factors" "encoding" "evidence" "thus" 
     "mutations" "production" "novel" "treatment" "previously" 
     "developmental" "shown" "seedlings" "1" "small" 
     "whereas" "yeast" "they" "detected" "increase" 
     "processes" "among" "could" "tissues" "cellular" 
     "there" "type" "lines" "transcripts" "binding" 
     "regions" "genomic" "enzymes" "compared" "elsevier" 
     "characterized" "target" "single" "analyses"
     "associated" "each" "those" "based" "essential" 
     "potential" "aba" "provide" "active" "various" 
     "information" "mutation" "effects" "3" "further" 
     "2002" "presence" "science" "large" "early" 
     "patterns" "process" "mitochondrial" "pattern" "about" 
     "had"  "roles" "play" "putative" 
     "concentrations" "endogenous" "acids"  "possible" 
     "metabolism" "defense"  "signals" "encodes" 
     "components" "interaction" "major" "variation" "mechanism" 
     "elements" "ga" "against" "homologous" "h" 
     "members" "induction" "four" "tissue" "ethylene" 
     "environmental" "suggests" "nuclear" "regulated" "wild" 
     "understanding" "transduction" "evolution" "signalling" "vitro" 
     "phenotype" "b" "transcript" "contrast" "key" 
     "might" "tomato" "because" "did" "shoot" 
     "content" "interactions"  "organisms" 
     "demonstrated" "i" "size" "reserved" 
     "effect" "studied" "containing" "significant" 
     "rights" "animals"  "report" "structure" 
     "division" "domains" "demonstrate" "2" 
     "use" "respectively" "synthesis"  "very" 
     "products" "activities"  "elongation" "indicating" 
     "5"  "contains" "diverse" "furthermore" 
     "predicted" "will" "chromosome" "water" "discussed" 
     "class" "while" "clones" "produced" "genomes" 
     "human" "experiments" "infection" "either" "biochemical" 
     "phenotypes" "time" "concentration" "repeats" "pathogen" 
     "green" "virus" "identify" "group" "via" 
     "identification" "physiological" "recently" "stomatal" "flowers" 
     "review" "germination" "differences" "result" "differentiation" 
     "significantly" "sites" "encode" "regulate" "obtained" 
     "research" "less" "")
   :test #'equal
   :mode :singleton))

(defvar *term-table* (make-hash-table :test #'equal))

(defun hash-abstracts ()
  (clrhash *term-table*)
  (loop for (abstract pub-id) in 
        (xsql *pdb* "select abstract, pub_id from publication")
	do (hash-abstract abstract pub-id)))

(defun hash-abstract (abstract pub-id)
  (loop for cword in (parse-string abstract)
	as entry = (assoc pub-id (gethash cword *term-table*))
	do 
        (cond 
         (entry (incf (cdr entry)))
         (t (push (cons pub-id 1) (gethash cword *term-table*)))
         )))

(defun parse-string (string)
  (loop for word in (simple-string-split string)
	as cword = (cleanup-word word)
	unless (gethash cword *stop-table*)
	collect cword))

(defun cleanup-word (word)
  (string-trim "()[],;.'\"" (string-downcase word)))



(defun compose-stop-list ()
  (sort (lmaphash (lambda (k v) (list (cdar v) k)) pb::*term-table*)
	#'> :key #'car))

(defun match-go-definition (go-frame)
  (let* ((entries (loop for word in (parse-string (#^go.definition go-frame))
			as hashval = (gethash word *term-table*)
			when hashval
			collect hashval))
	 (temptbl (make-hash-table :test #'equal))
	 )
    (loop for entry in entries do
	  (loop for (pubid) in entry do
                (incf (gethash pubid temptbl 0))))
    (sort (lmaphash (lambda (k v) (cons v k)) temptbl)
	  #'> :key #'car)))
	 
(defun pair-up-go-frames ()
  (loop for frame in *go-frames*
	as i below 100
	as defn = (#^go.definition frame)
	as best-match = (when defn (cdar (match-go-definition frame)))
	when best-match
	collect 
        (list frame 
              best-match 
              (xsql *pdb* "select abstract from publication where pub_id = ~d" 
                    best-match))))
		  
	
