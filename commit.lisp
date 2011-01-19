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

(defstruct commit
  tree parents author committer log)

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

(defun decode-commit (vector)
  (let ((*commit* (make-commit)))
    (iter (for base first 0 then (1+ newline))
	  (for newline = (position 10 vector :start base))
	  (unless newline
	    (error "Commit text does not have body"))
	  (when (= newline base)
	    ;; Blank line is end of header.
	    (setf (commit-log *commit*) (try-decode-utf8 vector :start (1+ newline)))
	    (return *commit*))
	  (decode-header vector base newline))))
