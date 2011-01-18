;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:cl-git.odb)

;;; First implementation will read the indexes into memory rather than
;;; using mmap.

(defun load-file-into-memory (path)
  (with-open-file (stream path :direction :input
			  :element-type '(unsigned-byte 8))
    (let ((vector (make-array (file-length stream)
			      :element-type '(unsigned-byte 8))))
      (read-sequence vector stream)
      vector)))

;;; There are two pack versions supported by git.  Version one has no
;;; header, and after the top-index, is just an array of hashes and
;;; 32-bit offsets.  This format is not currently supported by this
;;; code.
;;;
;;; Version 2 has a special header that can't occur in version one,
;;; followed by the top-index.  This is followed by 4 blocks of data,
;;; the first three sized by the element size times the number of
;;; entries, the last variable size, based on reference from one of
;;; the other entries.
;;;
;;; |-----------+----------|
;;; | Block     | Size     |
;;; |-----------+----------|
;;; | header    | 8        |
;;; | top-index | 4*256    |
;;; | oids      | 20*nr    |
;;; | crcs      | 4*nr     |
;;; | offset32  | 4*nr     |
;;; | offset64  | variable |
;;; | pack-sha  | 20       |
;;; | index-sha | 20       |
;;; |-----------+----------|
;;;
;;; The top-index is an index by first-byte of the SHA1 hash to speed
;;; up the binary search by hash.  Each element is the offset of the
;;; first entry that is greater than one starting with that byte.  As
;;; a consequence, the last entry of this table contains the number of
;;; entries.
;;;
;;; The offset32 values that are less than 2^31 are represented
;;; directly.  When packfile offsets are larger than 31-bits, offsets
;;; with the high-bit set indicate an offset into the offset64 table
;;; for the actual 64-bit offset.

(defmacro get-big-int-n (bytes data offset)
  (check-type bytes (integer 1 8))
  (let ((%data (gensym))
	(%offset (gensym)))
    `(let ((,%data ,data)
	   (,%offset ,offset))
       (logior ,@(iter (for pos from 0 below bytes)
		       (for shift from (* 8 (1- bytes)) by -8)
		       (for data = (if (zerop pos)
				       `(aref ,%data ,%offset)
				       `(aref ,%data (+ ,%offset ,pos))))
		       (collect (if (zerop shift) data
				    `(ash ,data ,shift))))))))

;; (defun get-big-int-n (bytes data offset)
;;   "Extract a big-endian number from the byte-array DATA starting at
;; OFFSET for the given number of BYTES."
;;   (reduce (lambda (a b)
;; 	    (logior (ash a 8) b))
;; 	  data
;; 	  :start offset
;; 	  :end (+ offset bytes)))

(defun index-version (data)
  "With the data of a loaded index file, return the version number for it."
  (if (= (get-big-int-n 4 data 0) #xFF744F63)
      (get-big-int-n 4 data 4)
      1))

(defun compare-oids (oid1 oid2
		     &key (start1 0) (start2 0))
  "Compare two OIDs, possibly as subsequences of another sequence.
Returns a number less than equal to or greater than zero corresponding
to how the OIDs themselves compare."
  (let ((result (mismatch oid1 oid2 :test #'= :start1 start1 :start2 start2
			  :end1 (+ start1 20)
			  :end2 (+ start2 20))))
    (if result
	(- (aref oid2 (+ result start2 (- start1)))
	   (aref oid1 result))
	0)))

;;; The operations are open-coded like this for efficiency, not good style.

(defmacro make-v2-index-function (data &body body)
  (let ((%data (gensym)))
  `(let* ((,%data ,data)
	  (top-base 8)
	  (oid-base (+ top-base (* 4 256)))
	  (number (get-big-int-n 4 ,%data (- oid-base 4)))
	  (crc-base (+ oid-base (* 20 number)))
	  (o32-base (+ crc-base (* 4 number)))
	  (o64-base (+ o32-base (* 4 number))))
     (labels ((get-top (index)
		(assert (<= -1 index 255))
		(if (minusp index)
		    0
		    (get-big-int-n 4 ,%data (+ top-base (* 4 index)))))
	      (get-oid (index)
		(assert (< -1 index number))
		(let ((base (+ oid-base (* 20 index))))
		  (subseq ,%data base (+ base 20))))
	      (get-offset (index)
		(assert (< -1 index number))
		(let ((o32 (get-big-int-n 4 ,%data (+ o32-base (* 4 index)))))
		  (if (< o32 #x80000000)
		      o32
		      (get-big-int-n 8 ,%data (+ o64-base (* 8 (logand o32 #x7fffffff)))))))
	      (get-crc (index)
		(assert (< -1 index number))
		(get-big-int-n 4 ,%data (+ crc-base (* 4 index))))
	      (compare-oid (oid index &key (oid-start 0))
		"Compare this OID against one in the table (this is passed in first)"
		(assert (< -1 index number))
		(compare-oids oid ,%data :start1 oid-start
			      :start2 (+ oid-base (* 20 index)))))
       (declare (ignorable (function get-crc))
		(ignorable (function get-top))
		(ignorable (function get-oid))
		(ignorable (function get-offset))
		(ignorable (function compare-oid)))
       ,@body))))

(defun make-v2-searcher (data)
  "Given a block of data for a v2 index, return a function that will
search that index for a given UID."
  ;; (declare (optimize (debug 3)))
  (make-v2-index-function data
    (labels ((oid-search (oid &key (oid-start 0))
	       "Scan for an OID, and return either its offset in the
packfile, or NIL if not found."
	       (labels ((scan (low high)
			  (if (>= high low)
			      (let* ((mid (+ low (ash (- high low) -1)))
				     (comp (compare-oid oid mid :oid-start oid-start)))
				(cond ((plusp comp)
				       (scan low (1- mid)))
				      ((minusp comp)
				       (scan (1+ mid) high))
				      (t mid))))))
		 (let* ((first-byte (aref oid oid-start))
			(index (scan (get-top (1- first-byte))
			  (1- (get-top first-byte)))))
		   (and index (get-offset index))))))
      #'oid-search)))

;;; Quoting from pack-revindex.c in git:
;;; "Pack index for existing packs give us easy access to the offsets
;;; into the corresponding pack file where each object's data starts,
;;; but the entries do not store the size of the compressed
;;; representation (uncompressed size is easily available by examining
;;; the pack entry header).  It is also rather expensive to find the
;;; sha1 for an object given its offset."
;;;
;;; Like the cgit code, we make an array of pairs of offsets and index
;;; numbers to be able to easily find the lengths or offsets of each
;;; pack entry.
;;;
;;; We need to know the packfile size since nothing stores the offset
;;; of the end of the last entry.  The packfile ends with a SHA1 of
;;; the whole packfile, so just subtract.

(defun create-pack-revindex (data packfile-size)
  (make-v2-index-function data
    (iter (for index from 0 below number)
	  (with result = (make-array (1+ number) :fill-pointer 0))
	  (vector-push (cons (get-offset index) index) result)
	  (finally
	   (vector-push (cons (- packfile-size 20) -1) result)
	   (return (sort result #'< :key #'car))))))

;;; Need to re-organize this.
;;; Alternate revindex, testing with a hashtable.
#|
(defun create-pack-revindex%% (data packfile-size)
  (make-v2-index-function data
    (iter (with table = (make-hash-table :size (1+ number)))
	  (for index from 0 below number)
	  (for offset (get-offset index))
	  (setf (gethash offset table) index)
	  (finally
	   (setf (gethash (- packfile-size 20) -1) -1))
)))
|#

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

(defun make-v2-oid-generator (data)
  "Return a function that will generate the OIDs, returning NIL when there are none."
  (make-v2-index-function data
    (let ((index 0))
      (lambda ()
	(if (= index number)
	    nil
	    (prog1
	      (get-oid index)
	      (incf index)))))))

(defun test-v2-lookup (data)
  "Test that v2 lookup finds all of the items that should be present, and doesn't find those that shouldn't."
  (let ((lookup (make-v2-searcher data))
	(gen (make-v2-oid-generator data)))
    (iter (for item = (funcall gen))
	  (while item)
	  (assert (funcall lookup item))
	  (for bad-item = (copy-seq item))
	  (setf (aref bad-item 19)
		(logxor (aref bad-item 19) 1))
	  (assert (not (funcall lookup bad-item))))))

(defclass packfile-v2 ()
  ((pack-stream :reader pack-stream)
   (searcher :reader packfile-searcher)
   (revindex :initform nil)))

(defmethod initialize-instance :after ((instance packfile-v2)
				       &rest initargs
				       &key path)
  (declare (ignore initargs))
  (setf (slot-value instance 'pack-stream)
	(open path :element-type '(unsigned-byte 8)))
  (let ((stream (open path :element-type '(unsigned-byte 8)))
	(data (load-file-into-memory
	       (make-pathname :type "idx" :defaults path))))
    (setf (slot-value instance 'pack-stream) stream
	  (slot-value instance 'searcher) (make-v2-searcher data)
	  (slot-value instance 'revindex)
	  (lambda ()
	    (create-pack-revindex data (file-length stream))))))

;;; The revindex is computed lazily.
(defun packfile-revindex (packfile)
  (with-slots (revindex) packfile
    (when (functionp revindex)
      (setf revindex (funcall revindex)))
    revindex))

(defun get-pack-size (packfile offset)
  "Compute the size of the element at OFFSET in the packfile"
  (let ((revindex (packfile-revindex packfile)))
    (labels ((scan (low high)
	       (assert (>= high low))
	       (let* ((mid (+ low (ash (- high low) -1)))
		      (other (car (aref revindex mid))))
		 (cond ((> offset other)
			(scan (1+ mid) high))
		       ((< offset other)
			(scan low (1- mid)))
		       (t (- (car (aref revindex (1+ mid))) offset))))))
      (scan 0 (1- (length revindex))))))

(defun uncompress-data (data &key (start 0) end uncompressed-size)
  "Uncompress a segment of an array."
  (declare (ignore uncompressed-size))
  (chipz:decompress nil 'chipz:zlib data :input-start start :input-end end))

(defun patch-delta (base delta)
  ;;; TODO implement patch-delta.
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

;;; TODO cache both relative and SHA1 based deltas.

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
  (let* ((size (get-pack-size packfile offset))
	 (vector (make-array size :element-type '(unsigned-byte 8)))
	 (stream (pack-stream packfile)))
    (file-position stream offset)
    (read-sequence vector stream)
    (decode-pack packfile vector offset)))

(defgeneric get-packfile-data (packfile oid))

(defmethod get-packfile-data ((packfile packfile-v2) oid)
  "Search this packfile for the given oid data.  If present, returns
the first-level gunzip of the data, else NIL."
  (let ((offset (funcall (packfile-searcher packfile) oid)))
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
