;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:cl-git)

;;; Because of the way deltas are stored, some objects will need to be
;;; computed repeatedly.  The cache is indexed by something 'equal
;;; (probably a cons of the pack object and an offset).  Currently, we
;;; do random replacement.

(defclass object-cache ()
  ((size :initarg :size)
   (table)
   (keys)
   (hits :initform 0)
   (misses :initform 0)))

;;; TODO: Make this scale by size of objects in the cache.

(defmethod initialize-instance :after ((instance object-cache)
				       &rest initargs
				       &key size)
  (declare (ignore initargs))
  (check-type size (integer 1 *))
  (with-slots (table keys) instance
    (setf table (make-hash-table :test #'equal :size size))
    (setf keys (make-array size :fill-pointer 0))))

(defun make-room-in-cache (cache)
  (let* ((table (slot-value cache 'table))
	 (keys (slot-value cache 'keys))
	 (length (length keys)))
    (when (= length (array-dimension keys 0))
      (let* ((offset (random length))
	     (discard-key (aref keys offset)))
	(setf (aref keys offset)
	      (aref keys (1- length)))
	(vector-pop keys)
	(remhash discard-key table)))))

(defun object-cache-lookup (cache key)
  (gethash key (slot-value cache 'table)))

(defun object-cache-add (cache key data)
  (with-slots (table keys) cache
    (make-room-in-cache cache)
    (setf (gethash key table) data)
    (vector-push key keys)))

(defmacro with-object-cache ((cache key) &body body)
  "Lookup the KEY in the CACHE.  If found, return the values from last
time.  Otherwise, evaluate body, capturing the values returned for
future lookups, then return them."
  (let ((%cache (gensym))
	(%key (gensym))
	(%tmp (gensym))
	(%result (gensym)))
    `(let* ((,%cache ,cache)
	    (,%key ,key)
	    (,%tmp (object-cache-lookup ,%cache ,%key)))
       (if ,%tmp
	   (progn
	     (incf (slot-value ,%cache 'hits))
	     (values-list ,%tmp))
	   (let ((,%result (multiple-value-list (progn ,@body))))
	     (incf (slot-value ,%cache 'misses))
	     (object-cache-add ,%cache ,%key ,%result)
	     (values-list ,%result))))))

#|
;;;; Benchmarking.
(defparameter *cache-data* (make-hash-table :test #'equal))

;;; To start with, let's log the queries and try to determine patterns.
(defun log-oid-lookup (packfile oid)
  (let ((key (cons packfile oid)))
    (setf (gethash key *cache-data*)
	  (1+ (gethash key *cache-data* 0)))))

(defun log-offset-lookup (packfile offset)
  (log-oid-lookup packfile offset))

;;; Grab the most common ones.
(defun scan-cache ()
  (iter (for (key value) in-hashtable *cache-data*)
	(collect (cons key value) into result)
	(finally (return (subseq (sort result #'> :key #'cdr)
				 0 10)))))
|#
