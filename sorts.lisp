;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Specialized sorts.

(in-package #:cl-git)

(defun fy-shuffle (vector)
  (iter (for n from (length vector) above 0)
	(for k = (random n))
	(rotatef (aref vector k) (aref vector (1- n)))))

(defun make-random-numbers (length)
  (iter (with result = (make-array length :element-type `(integer 0 ,(1- length))))
	(for i from 0 below length)
	(setf (aref result i) i)
	(finally (fy-shuffle result)
		 (return result))))

(defun sortedp (vector)
  (iter (for i from 1 below (length vector))
	(for b = (aref vector i))
	(for a previous b initially (aref vector 0))
	(when (< b a)
	  (return nil))
	(finally (return t))))

;;; Basic insertion sort.

(defun insertion-sort (vector start end)
  (iter (for j from (1+ start) below end)
	(for key = (aref vector j))
	(iter (for i from (1- j) downto start)
	      (for other = (aref vector i))
	      (while (> other key))
	      (setf (aref vector (1+ i)) other)
	      (finally (setf (aref vector (1+ i)) key)))))

(defun partition (vector start last pivot-point)
  "Partition the vector according to qsort.  Returns an element in the
array where all elements to its left are less than it, and all
elements to the right are greater than it.  The end point is
inclusive, unlike most CL ranges."
  (let ((pivot-value (aref vector pivot-point))
	(store-index start))
    (rotatef (aref vector pivot-point)
	     (aref vector last))
    (iter (for i from start below last)
	  (for tmp = (aref vector i))
	  (when (<= tmp pivot-value)
	    (setf (aref vector i) (aref vector store-index))
	    (setf (aref vector store-index) tmp)
	    (incf store-index)))
    (rotatef (aref vector store-index)
	     (aref vector last))
    store-index))

;;; Pick a pivot by finding the median element out of the first, mid,
;;; and last of the range.
(defun select-pivot (vector start last)
  (let* ((mid (+ start (floor (- last start) 2)))
	 (a (aref vector start))
	 (b (aref vector mid))
	 (c (aref vector last))
	 (b<=c (<= b c))
	 (a<=c (<= a c)))
    (if (<= a b)
	(if a<=c
	    (if b<=c mid last)
	    (if b<=c :bogus1 start))
	(if a<=c
	    (if b<=c start :bogus2)
	    (if b<=c last mid)))))

(defun %qsort (vector start last)
  (when (> last start)
    (if (<= (- last start) 10)
	(insertion-sort vector start (1+ last))
	(let* ((pivot-point (select-pivot vector start last))
	       (actual-pivot (partition vector start last pivot-point)))
	  (%qsort vector start (1- actual-pivot))
	  (%qsort vector (1+ actual-pivot) last)))))

(defun qsort (vector)
  (%qsort vector 0 (1- (length vector)))
  vector)

#|
(defmacro with-parallel-arrays ((all-accessor-name first-accessor-name &rest arrays) &body body)
  "Expand BODY in a context with ACCESSOR available as an array
accessor describing a multiple-value place for all of the arrays."
  (let ((names (mapcar (lambda (array)
			 (gensym (symbol-name array)))
		       arrays))
	(index (gensym "INDEX")))
    `(let ,(mapcar #'list names arrays)
       (macrolet ((,all-accessor-name (,index)
		    `(values ,,@(mapcar (lambda (name)
					  `(list 'aref ',name ,index))
					names)))
		  (,first-accessor-name (,index)
		    (list 'aref ',(first names) ,index)))
	 ,@body))))

;;; TODO: Try to understand nested quasiquotation.  It isn't doing at
;;; all what I think it should.

(defun zzz (a b)
  (with-parallel-arrays (all key a b)
    (setf (all 0) (all 1))
    (key 2)))

;;; Sort two arrays together, using the numbers in the first array as keys.
;;; Ugh, this is complex.
(defun parallel-sort (key-vector other-vector)
  (with-parallel-arrays (allvec keyvec key-vector other-vector)
    (labels ((insertion-sort (start end)
	       (iter (for j from (1+ start) below end)
		     (for key = (keyvec j))
		     (iter (for i from (1- j) downto start)
			   (while (> (keyvec i) key)))))))))
|#
