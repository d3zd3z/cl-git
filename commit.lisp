;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Commit management.

(in-package #:cl-git)

;;; Git commits are not required to be UTF8 encoded.  However, we have
;;; to use some kind of decoding when dealing with a string.  We'll
;;; deal with this by trying to convert the text using UTF-8, and if
;;; the conversion fails, revert to ISO-8859-1, which can represent
;;; any array of octets.

(defun try-decode-utf8 (vector &key (start 0) end)
  (handler-case
      (babel:octets-to-string vector :start start :end end)
    (babel-encodings:character-coding-error ()
      (warn "Invalid UTF8 in commit text")
      (map 'string #'code-char (subseq vector start end)))))

(defun decode-commit-id (vector start end)
  (encode-oid (try-decode-utf8 vector :start start :end end)))

(defstruct person
  name email time zone)

(defun decode-person (vector start end)
  ;;; TODO: Get more information out of this.
  (declare (optimize debug))
  (let ((text (try-decode-utf8 vector :start start :end end)))
    (or (ppcre:register-groups-bind (name email time zone)
	    ("^([^<]+) <([^>]+)> (\\d+) ([+-]\\d+)$" text)
	  (make-person :name name :email email
		       :time (parse-integer time) :zone zone))
	(error "Unable to parse person ~S" text))))

(defclass commit ()
  ((oid :type oid :reader commit-oid :initarg :oid)
   (tree :type oid :accessor commit-tree)
   (parents :accessor commit-parents :initform nil)
   (author :type person :accessor commit-author)
   (committer :type person :accessor commit-committer)
   (log :type string :accessor commit-log)))

(defun commit-newer (this other)
  "Return true is the commit THIS is newer than OTHER."
  (> (person-time (commit-committer this))
     (person-time (commit-committer other))))

(defvar *commit*)

(defun decode-header (vector start end)
  "Decode a single header from a commit message."
  (let ((space (position 32 vector :start start :end end)))
    (unless space
      (error "Commit header is malformed (no space)"))
    (let ((keyword (try-decode-utf8 vector :start start :end space)))
      (cond ((string= keyword "tree")
	     (setf (commit-tree *commit*)
		   (decode-commit-id vector (1+ space) end)))
	    ((string= keyword "parent")
	     (setf (commit-parents *commit*)
		   (nconc (commit-parents *commit*)
			  (list
			   (decode-commit-id vector (1+ space) end)))))
	    ((string= keyword "author")
	     (setf (commit-author *commit*)
		   (decode-person vector (1+ space) end)))
	    ((string= keyword "committer")
	     (setf (commit-committer *commit*)
		   (decode-person vector (1+ space) end)))
	    (t (error "Invalid header line ~S" keyword))))))

(defun parse-commit (oid vector &optional (class 'commit))
  (let ((*commit* (make-instance class :oid oid)))
    (iter (for base first 0 then (1+ newline))
	  (for newline = (position 10 vector :start base))
	  (unless newline
	    (error "Commit text does not have body"))
	  (when (= newline base)
	    ;; Blank line is end of header.
	    (setf (commit-log *commit*) (try-decode-utf8 vector :start (1+ newline)))
	    (return *commit*))
	  (decode-header vector base newline))))

(defun lookup-commit (ref &optional (class 'commit))
  (let ((oid (resolve-ref *current-repository* ref)))
    (multiple-value-bind (data type)
	(repo-lookup *current-repository* oid)
      (assert (eq type :commit))
      (parse-commit oid data class))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; There seems to be an assumption in the commit walking code that
;;;; commits are always newer than their parents.  This can be
;;;; overridden with GIT_COMMITTER_DATE when making the commit (or
;;;; just having a bad clock).  It seems that things like finding
;;;; merge bases and such will just not work very well if the times
;;;; are out of order, but the only other option would seem to be to
;;;; walk the entire tree.

;;; Insert a commit into the list of commits, keeping the list sorted
;;; with the newest commit at the head.  The list is non-destructively
;;; updated.
(defun insert-commit-by-date (commit list)
  (cond ((null list)
	 (list commit))
	((commit-newer commit (first list))
	 (cons commit list))
	(t (cons (first list)
		 (insert-commit-by-date commit (rest list))))))

;;; Walking revisions
(defclass revwalk ()
  ((seen :reader revwalk-seen
	 :initform (make-hash-table :test #'equalp))
   (heads :accessor revwalk-heads
	  :initform nil)))

(defun add-walk-head (revwalk ref)
  "Look up the given head, and add it as a commit"
  ;; TODO: method to handle merge if we're walking ancesstors.
  (let ((oid (resolve-ref *current-repository* ref)))
    (with-slots (seen heads) revwalk
      (unless (gethash oid seen)
	(let ((commit (lookup-commit oid)))
	  (setf (gethash oid seen) t)
	  (setf heads (insert-commit-by-date commit heads))))))
  (values))

(defun walk-next (revwalk)
  "Return the next commit in a revwalk, or NIL if there are none."
  (with-slots (heads) revwalk
    (when heads
      (let ((commit (pop heads)))
	(mapc #'(lambda (parent)
		  (add-walk-head revwalk parent))
	      (commit-parents commit))
	commit))))

(defun walk-all ()
  "Walk all of the nodes in the current repo."
  (let ((walker (make-instance 'revwalk)))
    (add-walk-head walker "refs/heads/master")
    (iter (for commit = (walk-next walker))
	  (while commit)
	  (for count from 1 to 10000)
	  (when (zerop (mod count 1000))
	    (format t "~A visited~%" count)
	    (force-output)))))

(defun oneline (commit)
  (let* ((hash (decode-oid (commit-oid commit)))
	 (log (commit-log commit))
	 (pos (position #\Newline log)))
    (when pos
      (setf log (subseq log 0 pos)))
    (format nil "~8@A ~A" (subseq hash 0 8) log)))

(defun show-log ()
  (let ((walker (make-instance 'revwalk)))
    (add-walk-head walker "refs/heads/master")
    (iter (for commit = (walk-next walker))
	  (while commit)
	  (for count from 1 to 100)
	  (format t "~A~%" (oneline commit)))))
