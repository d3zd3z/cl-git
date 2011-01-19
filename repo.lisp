;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Repository management.

(in-package #:cl-git)

(defvar *current-repository*)

(defstruct ref
  name tag commit)

(defun load-packed-refs (path)
  (setf path (probe-file path))
  (unless path
    (return-from load-packed-refs (make-hash-table :test 'equal)))
  (iter (with last-ref = nil)
	(with refs = (make-hash-table :test 'equal))
	(for line in-file path using #'read-line)
	(case (char line 0)
	  (#\# nil)
	  (#\^ (let ((commit (encode-oid (subseq line 1))))
		 (rotatef (ref-tag last-ref)
			  (ref-commit last-ref)
			  commit)))
	  (t (let* ((space (position #\Space line))
		    (oid (encode-oid (subseq line 0 space)))
		    (name (subseq line (1+ space)))
		    (ref (make-ref :name name :commit oid)))
	       (setf last-ref ref)
	       (setf (gethash name refs) ref))))
	(finally (return refs))))

(defclass repo ()
  ((odb :reader repo-odb)
   (path :reader repo-path)
   (packed-refs :reader repo-packed-refs)))

(defmethod initialize-instance :after ((instance repo)
				       &rest initargs
				       &key path)
  (declare (optimize debug))
  (declare (ignore initargs))
  (let ((path (fad:directory-exists-p path)))
    (unless path
      (error "Repo directory must exist"))
    (setf (slot-value instance 'path) path)
    (setf (slot-value instance 'packed-refs)
	  (load-packed-refs (merge-pathnames
			     (make-pathname :name "packed-refs"
					    :type nil)
			     path)))
    (setf (slot-value instance 'odb)
	  (make-instance 'repo-packdata
			 :path (merge-pathnames
				(make-pathname :directory (list :relative "objects"))
				path)))))

(defun load-loose-ref (path name)
  (with-open-file (stream path :direction :input)
    (let* ((text (read-line stream))
	   (oid (encode-oid (subseq text 0 40))))
      (make-ref :name name
		:commit oid))))

(defun lookup-repo-ref (repo ref-name)
  "Lookup a fully-qualified rename, and return a REF structure, or NIL
if not found."
  (let ((path (probe-file (merge-pathnames
			   (parse-namestring ref-name)
			   (repo-path repo)))))
    (if path (load-loose-ref path ref-name)
	(gethash ref-name (repo-packed-refs repo)))))

(defun repo-lookup (repo name)
  "Lookup a name in the repo, name can be a string, which will be
tried as a REF or a HEAD.  If it isn't either, try converting it to an
OID (which might fail).  If it is already an OID, just use it."
  (etypecase name
    (string (let ((resolved (lookup-repo-ref repo name)))
	      (if resolved (repo-lookup repo resolved)
		  (repo-lookup repo (encode-oid name)))))
    (ref (repo-lookup repo (ref-commit name)))
    ((simple-array (unsigned-byte 8) (20))
     (get-packfile-data (repo-odb repo) name))))

;;; Big TODO: repo operations need to rescan for changed packs and refs files.
