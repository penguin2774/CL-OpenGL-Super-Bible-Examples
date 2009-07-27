(in-package :gltools)


(defclass frame ()
  ((origin
    :initarg :origin
    :initform (make-array 3 :initial-contents '(0.0 0.0 0.0))
    :accessor origin)
   (forward
    :initarg :forward
    :initform (make-array 3 :initial-contents '(1.0 0.0 0.0))
    :accessor forward)
   (up
    :initarg :up
    :initform (make-array 3 :initial-contents '(0.0 1.0 0.0))
    :accessor up)))



(defgeneric x (object))
(defgeneric (setf x) (x object))


(defmethod x ((a sequence))
  (declare (inline))
  (elt a 0))

(defmethod (setf x) (x (a sequence))
  (declare (inline))
  (setf (elt a 0) x))

(defgeneric y (object))
(defgeneric (setf y) (y object))

(defmethod y ((a sequence))
  (declare (inline))
  (elt a 1))

(defmethod (setf y) (y (a sequence))
  (declare (inline))
  (setf (elt a 1) y))

(defgeneric z (object))
(defgeneric (setf z) (z object))

(defmethod z ((a sequence))
  (declare (inline))
  (elt a 2))

(defmethod (setf z) (z (a sequence))
  (declare (inline))
  (setf (elt a 2) z))
 

(defgeneric set-origin (f point &optional y z))

(defmethod set-origin ((f frame) point &optional y z)
  (with-slots (origin) f
      (if z
	  (progn 
	    (setf (x origin) point)
	    (setf (y origin) y)
	    (setf (z origin) z))
	  (setf origin (copy-seq point))))
  f)


(defgeneric set-forward (f point &optional y z))

(defmethod set-forward ((f frame) point &optional y zc)
  (with-slots (forward) f
      (if z
	  (progn 
	    (setf (x forward) point)
	    (setf (y forward) y)
	    (setf (z forward) z))
	  (setf forward (copy-seq point))))
  f)

(defgeneric set-up (f point &optional y z))

(defmethod set-up ((f frame) point &optional y z)
  (with-slots (up) f
      (if z
	  (progn 
	    (setf (x up) point)
	    (setf (y up) y)
	    (setf (z up) z))
	  (setf up (copy-seq point))))
  f)


(defgeneric z-axis (frame))
(defmethod z-axis ((f frame))
  (forward f))

(defgeneric y-axis (frame))
(defmethod y-axis ((f frame))
  (up f))

(defgeneric x-axis (frame))
(defmethod x-axis ((f frame))
  (m3d:cross-product (up f) (forward f)))