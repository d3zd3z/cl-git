;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CL-git

(defsystem :cl-git
  :serial t
  :version "0.1"
  :depends-on (#+(or) :trivial-garbage
		 :chipz
		 :cl-fad
		 :babel
		 :cl-ppcre)
  :components ((:file "packages")
	       (:file "sorts")
	       (:file "odb-index2")
	       (:file "odb-pack")
	       (:file "repo")
	       (:file "commit")))
