;;; Stolen from ...acl62/src/aserve/main.cl

(in-package :net.aserve)

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

(defun parse-multipart-header (header)
  ;; look for known patterns in the mulitpart header and return
  ;; the information we can find.  Header is the return value from
  ;; get-multipart-header
  ;;
  ;; return values:
  ;; 1.  nil, :eof, :file, :data, :nofile   - description of the header
  ;; 2. name  - name of the item
  ;; 3. filename - if type is :file then this is the filename
  ;; 4. content-type - if type is :file this this is the content-type
  (if* (and (consp header) (consp (car header)))
     then (let ((cd (assoc :content-disposition header :test #'eq))
		(ct (assoc :content-type header :test #'eq))
		(name)
		(filename)
		(content-type))
	    (if* (and cd
		      (consp (cadr cd))
		      (eq :param (car (cadr cd)))
		      (equal "form-data" (cadr (cadr cd))))
	       then (let ((fd (cddr (cadr cd))))
		      (let ((aname (assoc "name" fd :test #'equal)))
			(if* aname then (setq name (cdr aname))))
		      (let ((afname (assoc "filename" fd :test #'equal)))
			(if* afname then (setq filename (cdr afname))))))
	    (if* (and (consp ct) (stringp (cadr ct)))
	       then (setq content-type (cadr ct)))
	    
	    (values (if* filename 
		       then (if* (equalp filename "")
			       then :nofile
			       else :file)
		       else :data)
		    name
		    filename
		    content-type))
   elseif (null header)
     then :eof
     else nil ; doesn't match anything we know about
	  ))

(defvar *default-aserve-external-format* :latin1-base) 

(defun get-all-multipart-data 
    (req &key (type :text) 
	      (size 4096)
	      (external-format 
	       *default-aserve-external-format*)
	      (limit nil)
	      )

  ;; retrieve all the data for one multipart item
  ;;
  (let (res buffer (total-size 0) index)
    (loop
      (if* (null buffer)
	 then (setq buffer 
		(ecase  type 
		  (:text (make-string size))
		  (:binary (make-array size :element-type '(unsigned-byte 8))))
		index 0))
      (let ((nextindex (get-multipart-sequence 
			req buffer 
			:start index
			:external-format external-format)))
	(if* (null nextindex)
	   then (if* (> index 0)
		   then (incf total-size index)
			(push buffer res))
		(return))
	(if* (>= nextindex (length buffer))
	   then				; full buffer
		(incf total-size (length buffer))
		(if* (and limit (> total-size limit))
		   then			; we in the overlimit stage, just
					; keep reading but don't save
			(setq index 0)
		   else			; save away this full buffer
			(push buffer res)
			(setq buffer nil))
	   else (setq index nextindex))))
      
					; read it all, data in res
    (if* (zerop total-size)
       then (case type
	      (:text "")
	      (:binary (make-array 0 :element-type '(unsigned-byte 8))))
     elseif (and limit (> total-size limit))
       then (values :limit total-size)	; over limit return
     elseif (null (cdr res))
       then				; just one buffer
	    (subseq (car res) 0 total-size)
       else				; multiple buffers, must build result
	    (let ((result (case type
			    (:text (make-string total-size))
			    (:binary (make-array total-size
						 :element-type
						 '(unsigned-byte 8))))))
	      (do ((to 0)
		   (buffs (nreverse res) (cdr buffs)))
		  ((null buffs))
		(replace result (car buffs)
			 :start1 to)
		(incf to (length (car buffs))))
	      result))))

(defmethod get-multipart-sequence 
  ((req http-request)
   buffer
   &key (start 0)
   (end (length buffer))
   (external-format 
    *default-aserve-external-format* 
    ef-spec))

  ;; fill the buffer with the chunk of data.
  ;; start at 'start' and go no farther than (1- end) in the buffer
  ;; return the index of the first character not placed in the buffer.
  
  ;; Since external-format not used in all versions
  (declare (ignorable external-format ef-spec))

  #-(and allegro (version>= 6 0 pre-final 1))
  (if* ef-spec
       then (warn "~
For this version of Lisp, external-format is ignored ~
in get-multipart-sequence"))

  (let* ((mp-info (getf (request-reply-plist req) 'mp-info))
	 mpbuffer 
	 cur
	 pos
	 kind
	 text-mode
	 after)

    
    (typecase buffer
      ((array (unsigned-byte 8) (*))
       )
      #+LISPWORKS
      (string (setq text-mode t))
      ((array character (*))
       (setq text-mode t))
      (t 
       (error 
	"This function only accepts (array (unsigned-byte 8)) or character arrays")))
    (if* (null mp-info)
	 then (error "get-multipart-sequence called before get-multipart-header"))
    
    (setq mpbuffer (mp-info-buffer mp-info)
	  cur      (mp-info-cur mp-info))

    (loop
     (case (mp-info-state mp-info)
       ((:header :boundary :last-boundary)
					; no data left
	(return-from get-multipart-sequence nil))
       (:start
	(error "get-multipart-sequence called before get-multipart-header"))
       ((:body :partial)
	(if* (eq (mp-info-state mp-info) :partial)
	     then     ; this was set below. we will return the partial
					; at then end of the buffer
	     (setf (mp-info-state mp-info) :body)
	     (setq pos (mp-info-end mp-info))
	     else (multiple-value-setq (pos kind after) (scan-forward mp-info))
	     (setf (mp-info-after mp-info) after)
	     (setq cur (mp-info-cur mp-info)) ; scan-forward can change
	     )
	 
	(if* (> pos cur)
	     then			; got something to return
	     (let* ((tocopy (min (- end start) (- pos cur)))
		    (items tocopy))
	       (if* text-mode
		    then 
					; here is where we should do
					; external format processing
		    #+(and allegro (version>= 6 0 pre-final 1))
		    (multiple-value-setq (buffer items tocopy)
		      (octets-to-string
		       mpbuffer
		       :string buffer
		       :start cur
		       :end pos 
		       :string-start start
		       :string-end (length buffer)
		       :external-format external-format
		       :truncate t))
		    #-(and allegro (version>= 6 0 pre-final 1))
		    (dotimes (i tocopy)
		      (setf (aref buffer (+ start i))
			    (code-char (aref mpbuffer (+ cur i)))))
		    else 
		    (dotimes (i tocopy)
		      (setf (aref buffer (+ start i))
			    (aref mpbuffer (+ cur i)))))
	       (if* (zerop items)
		    then	   ; didn't find enough bytes to make 
					; a character
		    (if* (null (shift-buffer-up-and-read mp-info))
			 then		; no more bytes available
			 (return-from get-multipart-sequence nil))
					; loop around
		    else (setf (mp-info-cur mp-info) (+ cur tocopy))
		    (return-from get-multipart-sequence 
		      (+ start items))))
	     elseif (eq kind :partial)
	     then		       ; may be a boundary, can't tell
	     (if* (null (shift-buffer-up-and-read mp-info))
		  then	      ; no more data, partial will never match
					; so return the partial, this special
					; state is recognized in this routine
		  (setf (mp-info-state mp-info) :partial)
					; loop around
		  )
	     elseif (or (eq kind :boundary)
			(eq kind :last-boundary))
	     then	      ; hit a boundary, nothing more to return
	     (setf (mp-info-state mp-info) kind
		   (mp-info-cur   mp-info) pos)
	     (return-from get-multipart-sequence nil)))))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 
   '(
     net.aserve::parse-multipart-header
     net.aserve::get-all-multipart-data
     net.aserve::get-multipart-sequence 
     )
   (find-package :net.aserve)))

