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
	   with-xyzs
	   with-matrix-xyzs
	   cross-product
	   set-matrix-column44
	   set-matrix-column33
	   get-matrix-column44
	   get-matrix-column33
	   scale-vector2
	   scale-vector3
	   vector-length-squared
	   vector-length
	   normalize-vector
	   load-vector
	   mulf
	   divf
	   invert-matrix44
	   
	   ))

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


(defmacro with-matrix44-xyzs (matrices &body body)
  `(symbol-macrolet ,(loop for i in matrices
			   appending (loop for j in '(".XX" ".YX" ".ZX" ".TX"
						      ".XY" ".YY" ".ZY" ".TY"
						      ".XZ" ".YZ" ".ZZ" ".TZ"
						      ".XT" ".YT" ".ZT" ".TT")
					
					for k from 0
					  collect `(,(intern (concatenate 'string 
							   (symbol-name i)
							   j))
					     (elt ,i ,k))))
     ,@body))

(defmacro with-matrix3-xyzs (matrices &body body)
  `(symbol-macrolet ,(loop for i in matrices 
			appending (loop for j in '(".X"
						   ".Y"
						   ".Z")
				       
				     for k from 0
				     collect `(,(intern (concatenate 'string 
								     (symbol-name i)
								     j))
						(elt ,i ,k))))
     ,@body))

(defmacro with-xyzs (vectors &body body)
  (let ((matrix44-symbols '(".XX" ".YX" ".ZX" ".TX"
			    ".XY" ".YY" ".ZY" ".TY"
			    ".XZ" ".YZ" ".ZZ" ".TZ"
			    ".XT" ".YT" ".ZT" ".TT"))
	(matrix33-symbols '(".XX" ".YX" ".ZX" 
			    ".XY" ".YY" ".ZY" 
			    ".XZ" ".YZ" ".ZZ"))
	(matrix3-symbols '(".X"
			   ".Y"
			   ".Z")))

    (labels ((collect-matrix-macrolets (i symbols)
	       (loop for j in symbols
		  
		  for k from 0
		  collect `(,(intern (concatenate 'string 
						  (symbol-name i)
						  j))
			     (elt ,i ,k)))))
      `(symbol-macrolet
	   ,(loop for i in vectors
	      if (typep i 'list)
	      appending (case (second i)
			  (3 (collect-matrix-macrolets (first i) matrix3-symbols))
			  (33 (collect-matrix-macrolets (first i) matrix33-symbols))
			  (44 (collect-matrix-macrolets (first i) matrix44-symbols)))
	       else 
	       appending (collect-matrix-macrolets i matrix44-symbols) and
	       appending (collect-matrix-macrolets i matrix3-symbols))
	 ,@body))))
	  

(defun cross-product (u v &key result)
    (let ((result (or result (make-array 3))))  
      (with-xyzs (u v result)
	(setf result.x  (- (* u.y v.z) (* v.y u.z)))
	(setf result.y (+ (* (- u.x) v.z) (* v.x u.z)))
	(setf result.z (- (* u.x v.y) (* v.x u.y)))
	result)))

(defun get-matrix-column33 (src column)
  (subseq src (* 3 column) (+ (* 3 column) 3)))

(defun get-matrix-column44 (src column)
  (subseq src (* 4 column) (+ (* 4 column) 4)))

(defun set-matrix-column33 (dest src column)
  (setf (subseq dest (* 3 column) (+ (* 3 column) 3)) src)
  dest)

(defun set-matrix-column44 (dest src column)
  (setf (subseq dest (* 4 column) (+ (* 4 column) 4)) src)
  dest)

(defun scale-vector2 (v scale)
  (declare ((vector number 2) v)
	   (number scale)
	   (inline))
  (setf (elt v 0) (* (elt v 0) scale))
  (setf (elt v 1) (* (elt v 1) scale)))

(defun scale-vector3 (v scale)
  (declare ((vector number 3) v)
	   (number scale)
	   (inline))
  (setf (elt v 0) (* (elt v 0) scale))
  (setf (elt v 1) (* (elt v 1) scale))
  (setf (elt v 2) (* (elt v 2) scale)))


(defun vector-length-squared (u)
  (declare ((vector number  3) u)
	   (inline))
  (with-matrix3-xyzs (u)
    (+ (* u.x u.x) (* u.y u.y) (* u.z u.z))))
	   


(defun vector-length (u) ;; useful function?
  (declare ((vector number 3) u)
	   (inline))
  (sqrt (vector-length-squared u)))
    
  

(defun normalize-vector (u)
  (declare ((vector number 3) u)
	   (inline))
  (scale-vector3 u (/ 1.0 (vector-length u))))


(defmacro load-vector (x y &rest args)
  `(make-array ,(+ (length args) 2)
	       :initial-contents ',(cons x (cons y args))))

(defmacro mulf (form delta)
  `(setf ,form (* ,form ,delta)))

(defmacro divf (form delta)
  `(setf ,form (/ ,form ,delta)))
  
;; I realy dont know what this guy is doing, its way over my head,
;; so in all likelyhood this function has lots of typos.
(defun invert-matrix44 (src)
  (macrolet ((swap-rows (a b)
	       (let ((holder-gen (gensym "holder-")))
	       `(let ((,holder-gen ,a))
		  (setf ,a ,b)
		  (setf ,b ,holder-gen))))
	     (if-abs-greater-swap (a b)
	       `(if (> (abs ,a) (abs ,b))
		    (swap-rows ,a ,b))))
    (let ((r0 (make-array 8 :initial-element 0.0))
	  (r1 (make-array 8 :initial-element 0.0))
	  (r2 (make-array 8 :initial-element 0.0))
	  (r3 (make-array 8 :initial-element 0.0))
	  (dest (load-identity44))
	  m0 m1 m2 m3)
      (with-xyzs ((src 44) (dest 44))
	(setf (svref r0 0) src.xx) (setf (svref r0 1) src.xy)
	(setf (svref r0 2) src.xz) (setf (svref r0 3) src.xt)
	(setf (svref r0 4) 1.0)
	
	
	(setf (svref r1 0) src.yx) (setf (svref r1 1) src.yy)
	(setf (svref r1 2) src.yz) (setf (svref r1 3) src.yt)
	(setf (svref r1 5) 1.0)
	
	
	(setf (svref r2 0) src.zx) (setf (svref r2 1) src.zy)
	(setf (svref r2 2) src.zz) (setf (svref r2 3) src.zt)
	(setf (svref r2 6) 1.0)
	

	(setf (svref r3 0) src.tx) (setf (svref r3 1) src.ty)
	(setf (svref r3 2) src.tz) (setf (svref r3 3) src.tt)
	(setf (svref r3 7) 1.0)
	
					; Choose pivot or give up.
	(if-abs-greater-swap (svref r3 0) (svref r2 0))
	(if-abs-greater-swap (svref r2 0) (svref r1 0))
	(if-abs-greater-swap (svref r1 0) (svref r0 0))
	(if (zerop (svref r0 0)) (return-from invert-matrix44 nil))
	
					; eliminate first variable 
	
	(setf m1 (/ (svref r1 0)
		    (svref r0 0)))
	(setf m2 (/ (svref r2 0)
		    (svref r0 0)))
	(setf m3 (/ (svref r3 0)
		    (svref r0 0)))
	(loop for i from 0 to 3
	   for s = (svref r0 i)
	     do
	     (decf (svref r1 i) (* m1 s))
	     (decf (svref r2 i) (* m2 s))
	     (decf (svref r3 i) (* m3 s)))

	(loop for i from 4 to 7
	     for s = (svref r0 i)
	   do (when (not (zerop s))
		      (decf (svref r1 i) (* m1 s))
		      (decf (svref r2 i) (* m2 s))
		      (decf (svref r3 i) (* m3 s))))
					; choose pivot or give up
	(if-abs-greater-swap (svref r3 1) (svref r2 1))
	(if-abs-greater-swap (svref r2 1) (svref r1 1))
	(if (zerop (svref r1 1))
	    (return-from invert-matrix44 nil))
	
					; eliminate second variable
	
	(setf m2 (/ (svref r2 1) (svref r1 1)))
	(setf m3 (/ (svref r3 1) (svref r1 1)))
	(decf (svref r2 2) (* m2 (svref r1 2)))
	(decf (svref r2 3) (* m2 (svref r1 3)))
	(decf (svref r3 2) (* m3 (svref r1 2)))
	(decf (svref r3 3) (* m3 (svref r1 3)))
	
	(loop for i from 4 to 7
	     for s = (svref r1 i)
	     do (when (not (zerop s))
	       (decf (svref r2 i) (* m2 s))
	       (decf (svref r3 i) (* m3 s))))
	
					; choose pivot or give up
	(if-abs-greater-swap (svref r3 2) (svref r2 2))
	(if (zerop (svref r2 2))
	    (return-from invert-matrix44 nil))
	
					; eliminate third variable
	(setf m3 (/ (svref r3 2) (svref r2 2)))
	(loop for i from 3 to 7
	     do 
	     (decf (svref r3 i) (* m3 (svref r2 i))))

					; last check
	(if (zerop (svref r3 3))
	    (return-from invert-matrix44 nil))
					; now back substitute row 3
	(loop for i from 4 to 7  
	     with s = (/ 1.0 (svref r3 3))
	     do  (mulf (svref r3 i) s))
	
					; now back substitute row 2
	(setf m2 (svref r2 3))
	(loop for i from 4 to 7
	     with s = (/ 1.0 (svref r2 2))
	     do (setf (svref r2 i) (* s (- (svref r2 i) (* (svref r3 i) m2)))))
	
	(setf m1 (svref r1 3))
	(loop for i from 4 to 7
	   do (decf (svref r1 i) (* (svref r3 i) m1)))
	
	(setf m0 (svref r0 3))
	(loop for i from 4 to 7
	   do (decf (svref r0 i) (* (svref r3 i) m0)))
	
	
	(setf m1 (svref r1 2))
	(loop for i from 4 to 7
	     with s = (/ 1.0 (svref r1 1))
	     do (setf (svref r1 i) (* s (- (svref r1 i ) (* (svref r2 i) m1)))))
	
	(setf m0 (svref r0 2))
	(loop for i from 4 to 7
	     do (decf (svref r0 i) (* (svref r2 i) m0)))

	(setf m0 (svref r0 1))
	(loop for i from 4 to 7
	     with s = (/ 1.0 (svref r0 0))
	   do (setf (svref r0 i) (* s (- (svref r0 i) (* (svref r1 i) m0)))))

	
	
	(setf dest.xx (svref r0 4))
	(setf dest.xy (svref r0 5))
	(setf dest.xz (svref r0 6))
	(setf dest.xt (svref r0 7))
	
	(setf dest.yx (svref r1 4))
	(setf dest.yy (svref r1 5))
	(setf dest.yz (svref r1 6))
	(setf dest.yt (svref r1 7))

	(setf dest.zx (svref r2 4))
	(setf dest.zy (svref r2 5))
	(setf dest.zz (svref r2 6))
	(setf dest.zt (svref r2 7))

	(setf dest.tx (svref r3 4))
	(setf dest.ty (svref r3 5))
	(setf dest.tz (svref r3 6))
	(setf dest.tt (svref r3 7))
	dest))))
	
	     
	


	


	
	



    
    


