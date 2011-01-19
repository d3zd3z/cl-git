;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:cl-git)

(defun decode-object-type (type-code)
  (ecase type-code
    (-1 :bad)
    (0 :none)
    (1 :commit)
    (2 :tree)
    (3 :blob)
    (4 :tag)
    (6 :ofs-delta)
    (7 :ref-delta)
    (8 :any)))

(defclass packfile-v2 ()
  ((pack-stream :reader pack-stream)
   (index :initarg :index
	  :type pack-index-v2		; TODO: General type?
	  :reader pack-file-index)))

(defmethod initialize-instance :after ((instance packfile-v2)
				       &rest initargs
				       &key path)
  (declare (ignore initargs))
  (let ((stream (open path :element-type '(unsigned-byte 8)))
	(data (load-file-into-memory
	       (make-pathname :type "idx" :defaults path))))
    (setf (slot-value instance 'pack-stream) stream
	  (slot-value instance 'index) (load-pack-index
					(make-pathname :type "idx" :defaults path)
					(file-length stream)))))

(defun uncompress-data (data &key (start 0) end uncompressed-size)
  "Uncompress a segment of an array."
  (declare (ignore uncompressed-size))
  (chipz:decompress nil 'chipz:zlib data :input-start start :input-end end))

(defun patch-delta (base delta)
  (let ((delta-index 0)
	(delta-length (length delta))
	result-size base-size
	(result-index 0)
	result)
    (labels ((get-delta-byte ()
	       (prog1
		   (aref delta delta-index)
		 (incf delta-index)))
	     (get-header ()
	       (iter (with i = 0)
		     (with size = 0)
		     (for cmd = (get-delta-byte))
		     (setf size (logior size (ash (logand cmd 127) i)))
		     (incf i 7)
		     (while (and (>= cmd 128)
				 (< delta-index delta-length)))
		     (finally (return size))))
	     (copy-chunk (cmd)
	       (let ((copy-offset 0)
		     (copy-size 0))
		 (when (plusp (logand cmd #x01))
		   (setf copy-offset (get-delta-byte)))
		 (when (plusp (logand cmd #x02))
		   (setf copy-offset (logior copy-offset (ash (get-delta-byte) 8))))
		 (when (plusp (logand cmd #x04))
		   (setf copy-offset (logior copy-offset (ash (get-delta-byte) 16))))
		 (when (plusp (logand cmd #x08))
		   (setf copy-offset (logior copy-offset (ash (get-delta-byte) 24))))
		 (when (plusp (logand cmd #x10))
		   (setf copy-size (get-delta-byte)))
		 (when (plusp (logand cmd #x20))
		   (setf copy-size (logior copy-size (ash (get-delta-byte) 8))))
		 (when (plusp (logand cmd #x40))
		   (setf copy-size (logior copy-size (ash (get-delta-byte) 16))))
		 (when (zerop copy-size)
		   (setf copy-size #x10000))
		 ;; There are some checks here in the cgit code that
		 ;; seem to be checking for bounds and integer
		 ;; overflow, which we don't need to worry about here.
		 (replace result base :start1 result-index
			  :start2 copy-offset
			  :end2 (+ copy-offset copy-size))
		 (incf result-index copy-size)))
	     (inline-piece (length)
	       (replace result delta :start1 result-index
			:start2 delta-index
			:end2 (+ delta-index length))
	       (incf delta-index length)
	       (incf result-index length)))
      (setf base-size (get-header)
	    result-size (get-header)
	    result (make-array result-size
			       :element-type '(unsigned-byte 8)))
      (iter (while (< delta-index delta-length))
	    (for cmd = (get-delta-byte))
	    (if (>= cmd 128)
		(copy-chunk cmd)
		(inline-piece cmd))))
    result))

(defun decode-pack (packfile bytes this-offset)
  "Decode the type/size header from the stream of bytes.  Returns two
values, the type, and the uncompressed data.  THIS-OFFSET is the
offset of this object in the pack (the packs use deltas)."
  (let* ((index 1)
	 (c (aref bytes 0))
	 (type (decode-object-type (logand (ash c -4) 7)))
	 (size (logand c 15))
	 (shift 4))
    (iter (while (>= c #x80))
	  (setf c (aref bytes index))
	  (incf index)
	  (incf size (ash (logand c #x7f) shift))
	  (incf shift 7))
    (ecase type
      ((:commit :tree :blob :tag)
       (values (uncompress-data bytes :start index :uncompressed-size size) type))
      ((:ofs-delta)
       (setf c (aref bytes index))
       (incf index)
       (let ((base-offset (logand c 127)))
	 (iter (while (>= c 128))
	       (setf c (aref bytes index))
	       (incf index)
	       (setf base-offset (+ (ash (1+ base-offset) 7)
				    (logand c 127))))
	 (multiple-value-bind (base-data base-type)
	     (get-packdata-at-offset packfile (- this-offset base-offset))
	   (values (patch-delta base-data
				(uncompress-data bytes :start index))
		   base-type))))
      ((:ref-delta)
       (multiple-value-bind (base-data base-type)
	   (get-packfile-data packfile (subseq bytes index (+ index 20)))
	 (values (patch-delta base-data
			      (uncompress-data bytes :start (+ index 20)))
		 base-type))))))

(defun get-packdata-at-offset (packfile offset)
  (let* ((size (get-pack-size (pack-file-index packfile) offset))
	 (vector (make-array size :element-type '(unsigned-byte 8)))
	 (stream (pack-stream packfile)))
    (file-position stream offset)
    (read-sequence vector stream)
    (decode-pack packfile vector offset)))

(defgeneric get-packfile-data (packfile oid))

(defmethod get-packfile-data ((packfile packfile-v2) oid)
  "Search this packfile for the given oid data.  If present, returns
the first-level gunzip of the data, else NIL."
  (let ((offset (pack-find-oid (pack-file-index packfile) oid)))
    (when offset
      (get-packdata-at-offset packfile offset))))

(defclass loose-objects ()
  ((path :initarg :path :reader loose-objects-path)))

(defmethod initialize-instance :around ((instance loose-objects)
					&rest initargs
					&key path)
  (let ((true-path (fad:directory-exists-p path)))
    (assert true-path)
    (apply #'call-next-method instance :path true-path initargs)))

;;; The loose pack consists of a type at the beginning of the buffer,
;;; followed by an ascii size and a null.  Following the null is the
;;; rest of the payload.
(defun decode-loose-pack (data)
  (let* ((space-at (position 32 data))
	 (null-at (position 0 data))
	 (raw-kind (map 'string #'code-char (subseq data 0 space-at)))
	 (payload (make-array (- (length data) null-at 1)
			      :element-type '(unsigned-byte 8)
			      :displaced-to data
			      :displaced-index-offset (1+ null-at)))
	 (kind (cond ((string= raw-kind "commit") :commit)
		     ((string= raw-kind "tree") :tree)
		     ((string= raw-kind "blob") :blob)
		     ((string= raw-kind "tag") :tag)
		     (t (error "Unknown kind ~S in loose pack" raw-kind)))))
    (values payload kind)))

(defmethod get-packfile-data ((packfile loose-objects) oid)
  (let* ((textual (decode-oid oid))
	 (path (probe-file
		(merge-pathnames
		 (make-pathname :directory (list :relative (subseq textual 0 2))
				:name (subseq textual 2)
				:type nil)
		 (loose-objects-path packfile))))
	 (zdata (and path (load-file-into-memory path)))
	 (data (and zdata (uncompress-data zdata))))
    (and data (decode-loose-pack data))))

(defclass repo-packdata ()
  ((loose :reader repo-loose)
   (packs :reader repo-packs)))

(defmethod initialize-instance :after ((instance repo-packdata)
				       &rest initargs
				       &key path)
  (declare (ignore initargs))
  (setf (slot-value instance 'loose) (make-instance 'loose-objects :path path))
  (let ((packfiles (directory (merge-pathnames
			       (make-pathname :directory (list :relative "pack")
					      :name :wild
					      :type "pack")
			       path))))
    ;; TODO: Sort packfiles by newest first.
    (setf (slot-value instance 'packs)
	  (mapcar (lambda (name)
		    (make-instance 'packfile-v2 :path name))
		  packfiles))))

(defmethod get-packfile-data ((packfile repo-packdata) oid)
  (declare (optimize (debug 3)))
  (iter (for pack in (cons (repo-loose packfile)
			   (repo-packs packfile)))
	(for (values data kind) = (get-packfile-data pack oid))
	(when data
	  (return (values data kind)))))

(defun decode-oid (oid)
  "Decode an OID into a hex string representation."
  (check-type oid (array * (20)))
  (iter (with result = (make-string 40))
	(for element in-vector oid)
	(for index from 0 by 2)
	(replace result (format nil "~(~2,'0x~)" element)
		 :start1 index)
	(finally (return result))))

(defun encode-oid (text-oid)
  "Encode a hex oid into raw format."
  (check-type text-oid (string 40))
  (iter (with result = (make-array 20 :element-type '(unsigned-byte 8)))
	(for src-index from 0 by 2)
	(for dest-index from 0 below 20)
	(setf (aref result dest-index)
	      (parse-integer text-oid
			     :start src-index
			     :end (+ src-index 2)
			     :radix 16))
	(finally (return result))))
