(defpackage :m3d
  (:use :cl)
  (:export m3d-pi
	   deg-to-rad
	   rad-to-deg
	   hr-to-deg
	   hr-to-rad
	   deg-to-hr
	   rad-to-hr
	   load-identity44
	   rotation-matrix44
	   transform-vector3
	   draw-torus
	   cross-product))

(in-package :m3d)

(defconstant m3d-pi 3.14159265358979323846)
(defconstant m3d-2pi (* 2.0 m3d-pi))
(defconstant m3d-pi-div-180 0.017453292519943296)
(defconstant m3d-inv-pi-div-180 57.2957795130823229)


;//////////////////////////////////////////////////////////////////////////////
;/ Useful shortcuts and macros
;/ Radians are king... but we need a way to swap back and forth
(defmacro deg-to-rad (x)	
  `(* ,x m3d-pi-div-180))

(defmacro rad-to-deg (x)	
  `(* , x m3d-inv-pi-div-180))

;; Hour angles
(defmacro hr-to-deg (x)
  `(* ,x (/ 1.0 15.0)))

(defmacro hr-to-rad (x)
  `(m3d-deg-to-rad (m3d-hr-to-deg ,x)))

(defmacro deg-to-hr (x)	
  `(*  ,x 15.0))

(defmacro rad-to-hr (x)
  `(m3d-deg-to-hr (m3d-rad-to-deg ,x)))



(defun load-identity44 ()
  (make-array 16 :initial-contents '(1.0 0.0 0.0 0.0
				     0.0 1.0 0.0 0.0
				     0.0 0.0 1.0 0.0
				     0.0 0.0 0.0 1.0)))

(defun rotation-matrix44 (m angle x y z)
 
 
  (macrolet ((elt-m (row col)
	       `(elt m ,(+ (* col 4) row))))
    (let* ((mag (sqrt (+ (* x x)  (* y y)  (* z z))))
	   (m (if (zerop mag)
		  (load-identity44)
		  m))
	   (s (sin angle))
	   (c (cos angle))
	   (x (/ x mag))
	   (y (/ y mag))
	   (z (/ z mag))
	   (xx (* x x))
	   (yy (* y y))
	   (zz (* z z))
	   (xy (* x y))
	   (yz (* y z))
	   (zx (* z x))
	   (xs (* x s))
	   (ys (* y s))
	   (zs (* z s))
	   (one-c (- 1.0 c)))

      (setf (elt-m 0 0)  (+ (* one-c xx)  c))
      (setf (elt-m 0 1)  (- (* one-c xy)  zs))
      (setf (elt-m 0 2)  (+ (* one-c zx) ys)) ;
      (setf (elt-m 0 3)  0.0)

      (setf (elt-m 1 0)  (+ (* one-c xy) zs))
      (setf (elt-m 1 1)  (+ (* one-c yy) c))
      (setf (elt-m 1 2)  (- (* one-c yz) xs))
      (setf (elt-m 1 3)  0.0)

      (setf (elt-m 2 0)  (- (* one-c zx) ys))
      (setf (elt-m 2 1)  (+ (* one-c yz) xs))
      (setf (elt-m 2 2)  (+ (* one-c zz) c))
      (setf (elt-m 2 3)  0.0)

      (setf (elt-m 3 0) 0.0)
      (setf (elt-m 3 1) 0.0)
      (setf (elt-m 3 2) 0.0)
      (setf (elt-m 3 3) 1.0)))  m)


(defun transform-vector3 (v m)
  "Transform - Does rotation and translation via a 4x4 matrix. Transforms a point or vector.
  (v is a 3 element sequence, m is a 16 element sequence)"  
  (values  
    (+ (* (elt m 0) (elt v 0)) 
       (* (elt m 4) (elt v 1)) 
       (* (elt m 8) (elt v 2))
       (elt m 12)) ; * (elt v 3);	 
    (+ (* (elt m 1) (elt v 0))  
       (* (elt m 5) (elt v 1))  
       (* (elt m 9) (elt v 2))
       (elt m 13)) ; * (elt v 3);	
    (+ (* (elt m 2) (elt v 0)) 
       (* (elt m 6) (elt v 1)) 
       (* (elt m 10) (elt v 2))
       (elt m 14)))) ;// * (elt v 3);	
;;   (elt vOut 3) = (+ (* (elt m 3) (elt v 0))  
;; 		      (* (elt m 7) (elt v 1))  
;; 		      (* (elt m 11) (elt v 2))  
;; 		      (* (elt m 15) (elt v 3)))

(defmacro within-2pi (num)
  `(coerce (mod ,num ,(* 2 PI)) '(float 0 ,(* 2 pi))))

(defun draw-torus (m-transform)
  (let* ((major-radius 0.35)
	(minor-radius 0.15)
	(num-major  40.0)
	(num-minor  20.0)
	(object-vertex (make-array 3 )) ; Vertex in object/eye space
	(major-step (coerce (/ (* 2.0 PI) num-major) 'single-float))
	(minor-step (coerce (/ (* 2.0 PI) num-minor) 'single-float)))
    (declare (type single-float major-radius)
	     (short-float minor-radius)
	     (short-float num-major)
	     (short-float num-minor)
	     (short-float major-step)
	     (short-float minor-step))
	     
	
	(loop for i from 0 to num-major
	   for a0 short-float = (within-2pi (* i major-step)) 
	   for a1 short-float = (within-2pi (+ a0 major-step))
	   for x0 short-float = (cos a0)
	   for y0 short-float = (sin a0)
	   for x1 short-float = (cos a1)
	   for y1 short-float = (sin a1)
	   do (gl:with-primitives :triangle-strip
		(loop for j from 0 to num-minor
		   for b  = (within-2pi (* j minor-step))
		   for c = (cos b)
		   for r = (+ (* minor-radius c) major-radius)
		   for z = (* minor-radius (sin b))

					; First point
		   do (setf (elt object-vertex 0)  (* x0 r))
		     (setf (elt object-vertex 1) (* y0 r))
		     (setf (elt object-vertex 2) z)
		     (multiple-value-bind (x y z) (m3d:transform-vector3 object-vertex m-transform)
		       (gl:vertex x y z))
		     
            ;; Second point
		     (setf (elt object-vertex  0)  (* x1 r))
		     (setf (elt object-vertex 1) (* y1 r))
		     (setf (elt object-vertex 2) z)
		     (multiple-value-bind (x y z) (m3d:transform-vector3 object-vertex m-transform)
		       (gl:vertex x y z)))))))


(defmacro with-xyzs (vectors &body body)
  `(symbol-macrolet ,(loop for i in vectors 
			   appending (loop for j in '(".X"
						     ".Y"
						     ".Z")
					for k in '(0 1 2)
					  collect `(,(intern (concatenate 'string 
							   (symbol-name i)
							   j))
					     (elt ,i ,k))))
     ,@body))
	  

(defun cross-product (u v &keys result)
    (let ((result (or result (make-array 3))))  
      (with-xyzs (u v result)
	(setf result.x  (- (* u.y v.z) (* v.y u.z)))
	(setf result.y (
  