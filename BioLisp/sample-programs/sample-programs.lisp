;;; -*- mode: Lisp; Syntax: Common-Lisp; Package: biolisp; -*-

(in-package :bio)


;;; +=========================================================================+
;;; | Copyright (c) 2002, 2003, 2004 JP Massar, Jeff Shrager, Mike Travers    |
;;; |                                                                         |
;;; | Permission is hereby granted, free of charge, to any person obtaining   |
;;; | a copy of this software and associated documentation files (the         |
;;; | "Software"), to deal in the Software without restriction, including     |
;;; | without limitation the rights to use, copy, modify, merge, publish,     |
;;; | distribute, sublicense, and/or sell copies of the Software, and to      |
;;; | permit persons to whom the Software is furnished to do so, subject to   |
;;; | the following conditions:                                               |
;;; |                                                                         |
;;; | The above copyright notice and this permission notice shall be included |
;;; | in all copies or substantial portions of the Software.                  |
;;; |                                                                         |
;;; | THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,         |
;;; | EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF      |
;;; | MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  |
;;; | IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY    |
;;; | CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,    |
;;; | TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE       |
;;; | SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                  |
;;; +=========================================================================+


10. Pairwise sequence alignment (Blast) The most widely used
bioinformatic tool in the world is Blast, in its various flavors.
There's a good reason for this. It's very very useful. Suppose you
suspect that the automated annotation process would have missed a
certain small protein, one that was found by experimental means in
another organism.  You can use Blast to screen the genome of your
organism (translating every nucleotide in it) to see if it is capable
of encoding a similar protein.  

(ASSIGN my-favorite-gene (Get-gene patS FROM Ana7120)) 
(ASSIGN my-favorite-protein (Protein-of my-favorite-gene)) 
(DISPLAY (BLAST my-favorite-protein Nostoc29133)
            :FORMAT Blast-with-alignment))

Blast (query target &KEYS too-many-to-mention-here): The macro figures
out as best it can which of the various flavors of Blast is desired
from the nature of the query and target. The proper program is called,
and the output returned for parsing or display.

11. Find other instances in the genome of a known regulatory element
(Other side of the world from Example 2) Laboratory experiment has
identified a DNA sequence that is the target for the important
regulatory protein, NtcA. From a collection of known NtcA-binding
sites, you'd like to find other unknown binding sites in the genome.
Methods exist for this, and they have been incorporated into an
already-written BioLingua function:

    (Find-motif-in-genome "NtcA-sites.txt" IN Avar :THRESHOLD 0.7)

Translation: Analyze an alignment of NtcA-binding sites, and then go
through the set of upstream regions in the genome of A. variabilis
looking for similar sites. The closer the threshold is to 2, the more
you focus on those positions of the alignment where there is absolute
unanimity. When the threshold is set to 0, all positions in the
alignment contribute to the score.  Simple, but unfortunately not very
effective. This routine will find lots of plausible sites. Too many,
most of them probably fakes. What makes BioLingua powerful is the next
step. The real NtcA-binding sites have probably been conserved during
evolution. If you detect the NtcA-binding sites in front of the same
genes in related organisms, you can reasonably conclude that the sites
have a function that evolution has selected for. To assess this
possibility, you consider the genes of Anabaena variabilis that are
downstream from the sites found by Find-motif-in-genome, find the
orthologs (similar genes) of these genes in Anabaena PCC 7120 and
Nostoc punctiforme, and then see if they too possess NtcA-like sites:
    (Find-motif-in-genome "NtcA-sites.txt" IN Avar :THRESHOLD 0.7)
    (ASSIGN candidates-data *)

[Take what was found in the previous operation and call it
"candidates-data". Each hit consists of a triplet value: the contig
and coordinate (the FIRST and SECOND values) where the hit was found
and the score.]

    (LOOP FOR candidate IN candidates 
          DO (ASSIGN contig (FIRST candidate-data))
             (ASSIGN coordinate (SECOND candidate-data))
          COLLECT (Gene-downstream-from contig coordinate))
    (ASSIGN candidate-genes *)

	         	[Now I have the names of all genes in A.
variabilis with putative NtcA sites in front of them]

    (LOOP FOR organism IN (LIST 7120 Npun) 
          DO (LOOP FOR gene IN candidate-genes 
                   WHEN (> (Motif-score "NtcA-sites.txt" 
                           (Upstream-of (Ortholog-of gene IN organism)))
                          5)
                   COLLECT gene))

Translation: Go through A. 7120 and N. punctiforme and consider the
orthologs of the genes I identified using Find-motif. If the upstream
region of an ortholog has a good motif (better than a score of 5 -
might want to play with this), save the name of the gene in A.
variabilis.  Find-motif-in-genome (list-of-aligned-sequences [FROM|IN]
organism-frame &KEY (threshold 1.0) (keep 50)): Constructs
position-specific scoring matrix from aligned sequences and uses the
matrix to scan the genome of the given organism, returning for each of
the top hits: (contig coordinate score).  Gene-downstream-from (contig
coordinate): Finds the gene closest in the 5 direction to the given
coordinate in the given contig (and organism, since contig names are
unique). 5 means the direction moving backwards on the DNA from the
site.  Motif-score (motif target): Computes the score of the given
motif in the target sequence, according to the same method used by
Find-motif-in-genome.
