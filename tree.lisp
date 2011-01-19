;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tree decoding.

(in-package #:cl-git)

(defun mode-file-kind (mode)
  (ecase (logand mode #o170000)
    (#o040000 :dir)
    (#o100000 :reg)
    (#o120000 :link)
    (#o160000 :sub-repo)))

(defun mode-file-perm (mode)
  (ecase (logand mode #o007777)
    ((#o0000 #o0644) :normal)
    (#o0755 :execute)))

(defstruct tree-entry kind perm name sha children)

(defun decode-tree (vector)
  "Decode a git tree object in VECTOR."
  (iter (with length = (length vector))
	(for base first 0 then (+ null-position 21))
	(while (< base length))
	(for space-position = (position 32 vector :start base))
	(unless space-position
	  (error "Malformed git tree object"))
	(for null-position = (position 0 vector :start (1+ space-position)))
	(let* ((mode-text (try-decode-utf8 vector :start base :end space-position))
	       (mode (parse-integer mode-text :radix 8))
	       (kind (mode-file-kind mode))
	       (perm (mode-file-perm mode))
	       (name (try-decode-utf8 vector :start (1+ space-position)
				      :end null-position))
	       (sha (subseq vector (1+ null-position)
			    (+ null-position 21))))
	  (collect (make-tree-entry :kind kind
				    :perm perm
				    :name name
				    :sha sha)))))

(defun make-fake-root (sha)
  (make-tree-entry :kind :dir :perm :normal :name "." :sha sha))

(defun expand-node-children (node)
  "Walk through all of the nodes, expanding the 'children' field of any"
  (when (eq (tree-entry-kind node) :dir)
    (multiple-value-bind (object type)
	(repo-lookup *current-repository* (tree-entry-sha node))
      (assert (eq type :tree))
      (let ((children (decode-tree object)))
	(setf (tree-entry-children node) children)
	(mapc #'expand-node-children children)
	node))))

(defun bench1 (&optional (oid (encode-oid "9a11b8cefc692907e0a1802820689cc4798c8dd0")))
  (let ((root (make-fake-root oid)))
    (expand-node-children root)
    (values)))

(defun bench2 ()
  (let ((*current-repository* (make-instance 'repo :path #p"/usr/src/linux/.git/")))
    (bench1)
    (time (bench1))))

#|
(defun ptest ()
  (let* ((*current-repository* (make-instance 'repo :path #p"/usr/src/linux/.git/"))
	 (top (repo-lookup *current-repository* "refs/heads/master"))
	 (top-commit (decode-commit top))
	 (tree (commit-tree top-commit))
	 (count 0))
    ;; I know the sorts on the reverse lookups are taking most of the time.
    (bench1 tree)
    (sb-sprof:with-profiling (:mode :cpu
				    :reset t
				    :max-samples 1000)
      (bench1 tree)
      (incf count))
    count))
|#
