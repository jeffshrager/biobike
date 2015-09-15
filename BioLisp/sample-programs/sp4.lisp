
;;; Authors:  Jeff Elhai, JP Massar.

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


#|

4. Find the core set of metabolic pathways that defines cyanobacteria
The core set of genes is interesting but it is easier to interpret
what metabolic pathways are common to all cyanobacteria. This is
readily done by combining BioLingua's knowledge of metabolism with its
knowledge of cyanobacterial genomes and their annotation.

  (LOOP FOR pathway IN *pathway-models*
        WHEN (LOOP FOR organism IN *all-organisms*
                   IF (NOT (Get-pathway pathway organism)) (RETURN *failure*))
                   FINALLY (RETURN *success*))
        COLLECT pathway) 

Translation: For each known pathway, consider every available
organism. If, in that organism, the pathway does not exist, then
report failure. If the pathway does exist in every organism, then
report success, and collect that pathway. The result is a list of
those pathways common to all cyanobacteria.  Get-pathway
(pathway-name-or-descriptor [FROM|IN] organism-frame): (I need to look
at how pathways are stored).

|#

