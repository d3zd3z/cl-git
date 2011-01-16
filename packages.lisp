;;; CL-GIT
;;; Binding and usage of libgit2

(defpackage #:cl-git.odb
  (:use #:cl
	#:iterate)
  (:export #:get-packfile-data
	   #:repo-packdata
	   #:encode-oid
	   #:decode-oid))

(defpackage #:cl-git.repo
  (:use #:cl
	#:iterate
	#:cl-git.odb)
  (:export #:repo
	   #:lookup-repo-ref
	   #:repo-lookup))

(defpackage #:cl-git
  (:nicknames #:git)
  (:use #:cl
	#+(or) #:cffi
	#:iterate
	#:cl-git.odb
	#:cl-git.repo))
