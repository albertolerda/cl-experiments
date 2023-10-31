(ql:quickload 'fiveam)
(defpackage :edit-distance
  (:use :cl :fiveam))
(in-package edit-distance)

(defun is-dist (s1 s2 n)
  (is (= (dist s1 s2) n)))

(def-test dist1 ()
  (is-dist "foo" "" 3)
  (is-dist "" "bar" 3)
  (is-dist "ba" "bar" 1)
  (is-dist "fomo" "foo" 1)
  (is-dist "fomobar" "foobar" 1))

(run 'dist1)

(defun dist (from to)
  (let ((dp (make-array (list (1+ (length from)) (1+ (length to))))))
    (loop for i from 0 to (length from) do
      (setf (aref dp i 0) i))
    (loop for i from 0 to (length to) do
      (setf (aref dp 0 i) i))
    (loop for i from 1 to (length from) do
      (loop for j from 1 to (length to) do
	(setf (aref dp i j)
	      (min (1+ (aref dp (1- i) j))
		   (1+ (aref dp i (1- j)))
		   (+ (aref dp (1- i) (1- j))
		      (if (equal (char from (1- i))
				 (char to (1- j)))
			  0 1))))))
    (aref dp (length from) (length to))))

(defun dist-optimized (from to)
  (declare (type string from to))  
  (let ((dp (make-array (list (1+ (length from)) (1+ (length to)))
			:element-type '(unsigned-byte 32))))
    (loop for i from 0 to (length from) do
      (setf (aref dp i 0) i))
    (loop for i from 0 to (length to) do
      (setf (aref dp 0 i) i))
    (loop for i from 1 to (length from) do
      (loop for j from 1 to (length to) do
	(setf (aref dp i j)
	      (min (1+ (aref dp (1- i) j))
		   (1+ (aref dp i (1- j)))
		   (+ (aref dp (1- i) (1- j))
		      (if (equal (char from (1- i))
				 (char to (1- j)))
			  0 1))))))
    (aref dp (length from) (length to))))
  

(with-open-file (fp "~/common-lisp/edit-distance/DNAs")
  (let ((dna-from (read-line fp))
	(dna-to (read-line fp)))
    (princ (time (dist-optimized-2 dna-from dna-to)))
    ))
