;;;
;;; shared/gltools lisp translation.
;;;
;;; Copyright (c) 2009 Nathanael Cunningham
;;; See LICENSE for full licensing details.
;;;

(defpackage :gltools
  (:use :cl)
  (:nicknames :glt)
  (:export 
   ;; from glframe.lisp
   forward
   up
   origin
   x
   y
   z
   frame
   set-origin
   set-forward
   set-up
   z-axis
   y-axis
   x-axis
   translate-world
   move-forward
   move-up
   move-right
   translate-local
   get-matrix
   get-camera-orientation
   apply-camera-transform
   apply-actor-transform
   rotate-local-x
   rotate-local-y
   rotate-local-z
   normalize
   rotate-world
   rotate-local
   local-to-world
   world-to-local
   transform-point
   rotate-vector   
   within-2pi
   draw-torus))

(in-package :gltools)

(defmacro within-2pi (num)
  `(coerce (mod ,num ,(* 2 PI)) '(float 0 ,(* 2 pi))))

(defun draw-torus (major-radius minor-radius num-major num-minor)
  (let* ((object-vertex (make-array 3 )) ; Vertex in object/eye space
	 (major-step (coerce  (/ (* 2.0 PI) num-major) 'short-float))
	 (minor-step (coerce (/ (* 2.0 PI) num-minor) 'short-float)))
    (declare (single-float major-radius)
	     (single-float minor-radius)
	     (single-float num-major)
	     (single-float num-minor))
	     
	
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
	       (gl:vertex (elt object-vertex 0) (elt object-vertex 1) (elt object-vertex 2))
		     
	       ;; Second point
	       (setf (elt object-vertex  0)  (* x1 r))
	       (setf (elt object-vertex 1) (* y1 r))
	       (setf (elt object-vertex 2) z)
	       (gl:vertex (elt object-vertex 0) (elt object-vertex 1) (elt object-vertex 2)))))))