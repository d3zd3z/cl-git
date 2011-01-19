;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:cl-git)

;;;; The object database packfiles each have an associated index file.
;;;; This processes these pack files.

;;;; First implementation will read the indexes into memory rather
;;;; than using mmap.

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
  "Extract a big-endian BYTES-byte integer out of the vector of DATA
bytes starting at OFFSET."
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

(defun index-version (data)
  "With the data of a loaded index file, return the version number for it."
  (if (= (get-big-int-n 4 data 0) #xFF744F63)
      (get-big-int-n 4 data 4)
      1))

(defclass pack-index-v2 ()
  ((data :type (simple-array (unsigned-byte 8) (*))
	 :initarg :data
	 :reader pi-data)
   (top-base :type (unsigned-byte 32)
	     :initarg :top-base
	     :reader pi-top-base)
   (oid-base :type (unsigned-byte 32)
	     :initarg :oid-base
	     :reader pi-oid-base)
   (number :type (unsigned-byte 32)
	   :initarg :number
	   :reader pi-number)
   (crc-base :type (unsigned-byte 32)
	     :initarg :crc-base
	     :reader pi-crc-base)
   (o32-base :type (unsigned-byte 32)
	     :initarg :o32-base
	     :reader pi-o32-base)
   (o64-base :type (unsigned-byte 32)
	     :initarg :o64-base
	     :reader pi-o64-base)
   (pack-size :type (unsigned-byte 64)
	     :initarg :pack-size
	     :reader pi-pack-size)
   (offset-table :type (simple-array (unsigned-byte 64) (*))
		 :documentation "Offsets of each pack entry, followed
		 by the last entry, used to compute sizes.")))

(defun build-pack-index-v2 (data pack-size)
  (let* ((top-base 8)
	 (oid-base (+ top-base (* 4 256)))
	 (number (get-big-int-n 4 data (- oid-base 4)))
	 (crc-base (+ oid-base (* 20 number)))
	 (o32-base (+ crc-base (* 4 number)))
	 (o64-base (+ o32-base (* 4 number))))
    (make-instance 'pack-index-v2
		   :data data
		   :top-base top-base
		   :oid-base oid-base
		   :number number
		   :crc-base crc-base
		   :o32-base o32-base
		   :o64-base o64-base
		   :pack-size pack-size)))

(defun pack-get-offset (pack-index index)
  (let* ((data (pi-data pack-index))
	 (o32-base (pi-o32-base pack-index))
	 (o64-base (pi-o64-base pack-index))
	 (offset (get-big-int-n 4 data (+ o32-base (* 4 index)))))
    (when (>= offset #x80000000)
      (setf offset (get-big-int-n 8 data (+ o64-base
					    (* 8 (logand offset #x7fffffff))))))
    offset))

(defun pack-get-oid (pack-index index)
  "Return a copy of the OID at the given index in the pack."
  (let* ((data (pi-data pack-index))
	 (oid-base (pi-oid-base pack-index))
	 (base (+ oid-base (* 20 index))))
    (subseq data base (+ base 20))))

(defun pack-index-generator (pack-index)
  "Return a function that will generate the OIDS in the pack, returning NIL after the last."
  (let ((number (pi-number pack-index))
	(index 0))
    (lambda ()
      (when (< index number)
	(prog1
	    (pack-get-oid pack-index index)
	  (incf index))))))

;;; Why this is worse, I do not know.
#+nil
(defun %pack-offset-table (pack-index)
  "Get or compute the pack index."
  (when (slot-boundp pack-index 'offset-table)
    (return-from pack-offset-table
      (slot-value pack-index 'offset-table)))
  (let* ((data (pi-data pack-index))
	 (o32-base (pi-o32-base pack-index))
	 (o64-base (pi-o64-base pack-index))
	 (number (pi-number pack-index))
	 (table (make-array (1+ number)
			    :element-type '(unsigned-byte 64))))
    (iter (for index from 0 below number)
	  (for offset = (get-big-int-n 4 data (+ o32-base (* 4 index))))
	  (when (>= offset #x80000000)
	    (setf offset (get-big-int-n 8 data (+ o64-base
						  (* 8 (logand offset #x7fffffff))))))
	  (setf (aref table index) offset))
    (setf (aref table number) (- (pi-pack-size pack-index) 20))
    ;; (sort table #'<)
    (qsort table)
    (setf (slot-value pack-index 'offset-table) table)))

(defun pack-offset-table (pack-index)
  "Get or compute the pack index."
  (when (slot-boundp pack-index 'offset-table)
    (return-from pack-offset-table
      (slot-value pack-index 'offset-table)))
  (let* ((number (pi-number pack-index))
	 (table (make-array (1+ number)
			    :element-type '(unsigned-byte 64))))
    (iter (for index from 0 below number)
	  (setf (aref table index) (pack-get-offset pack-index index)))
    (setf (aref table number) (- (pi-pack-size pack-index) 20))
    ;; (sort table #'<)
    (qsort table)
    (setf (slot-value pack-index 'offset-table) table)))

(defun get-pack-size (pack-index offset)
  "Compute the size of the object at OFFSET."
  (let ((rev-index (pack-offset-table pack-index)))
    (labels ((scan (low high)
	       (assert (>= high low))
	       (let* ((mid (+ low (ash (- high low) -1)))
		      (other (aref rev-index mid)))
		 (cond ((> offset other)
			(scan (1+ mid) high))
		       ((< offset other)
			(scan low (1- mid)))
		       (t (- (aref rev-index (1+ mid)) offset))))))
      (scan 0 (1- (length rev-index))))))

(defun pack-compare-oid (pack-index oid index &key (oid-start 0))
  "Compare the given OID with the OID at INDEX.  Returns an integer
less than, equal to or greater than zero if OID comes before, at, or
after the one at INDEX."
  (let* ((data (pi-data pack-index))
	 (oid-base (pi-oid-base pack-index))
	 (base (+ oid-base (* index 20)))
	 (result (mismatch oid data
			   :test #'=
			   :start1 oid-start
			   :start2 base
			   :end1 (+ oid-start 20)
			   :end2 (+ base 20))))
    (if result
	(- (aref data (+ result base (- oid-start)))
	   (aref oid result))
	0)))

(defun pack-first-index (pack-index first-byte)
  "The index contains a table at the start giving indices based on the
first byte of the SHA1 hash.  Accepts a value from -1 to 255 and
returns the packfile offset (not the index) for that value."
  (if (minusp first-byte)
      0
      (get-big-int-n 4 (pi-data pack-index)
		     (+ (pi-top-base pack-index) (* 4 first-byte)))))

(defun pack-find-oid (pack-index oid &key (oid-start 0))
  "Search this index for the given OID, returning it's offset if found, or NIL if not found."
  (labels ((scan (low high)
	     (if (>= high low)
		 (let* ((mid (+ low (ash (- high low) -1)))
			(comp (pack-compare-oid pack-index oid mid :oid-start oid-start)))
		   (cond ((plusp comp)
			  (scan low (1- mid)))
			 ((minusp comp)
			  (scan (1+ mid) high))
			 (t mid))))))
    (let* ((first-byte (aref oid oid-start))
	   (index (scan (pack-first-index pack-index (1- first-byte))
			(1- (pack-first-index pack-index first-byte)))))
      (and index (pack-get-offset pack-index index)))))

(defun benchmark-pack-lookup (pack-index)
  "Lookup all of the entries in the packfile."
  (let* ((oids (iter (with gen = (pack-index-generator pack-index))
		     (for oid = (funcall gen))
		     (while oid)
		     (collect oid result-type 'vector))))
    (fy-shuffle oids)
    (iter (for oid in-vector oids)
	  (assert (pack-find-oid pack-index oid))
	  (for oid2 = (copy-seq oid))
	  (setf (aref oid2 19)
		(logxor (aref oid2 19) 1))
	  (assert (not (pack-find-oid pack-index oid2))))))

(defun load-pack-index (path pack-size)
  "Read the pack index into memory, and return a pack-index object
that can handle queries upon it."
  (let ((data (load-file-into-memory path)))
    (ecase (index-version data)
      (2 (build-pack-index-v2 data pack-size)))))

;;; For debugging
(defun test-load-pack-index (path)
  (let ((pack-size (with-open-file (stream (make-pathname :type "pack"
							  :defaults path))
		     (file-length stream))))
    (load-pack-index path pack-size)))
