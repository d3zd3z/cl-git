;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CL-git

(defsystem :cl-git
  :serial t
  :version "0.1"
  :depends-on (#+(or) :trivial-garbage
		 :chipz
		 :cl-fad
		 :babel
		 :cl-ppcre
		 :iterate)
  :components ((:file "packages")
	       (:file "sorts")
	       (:file "cache")
	       (:file "odb-index2")
	       (:file "odb-pack")
	       (:file "repo")
	       (:file "commit")))
