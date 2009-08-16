;;;
;;; Bouncing Box Example (first example!)
;;;
;;; Copyright (c) 2009 Nathanael Cunningham
;;; See LICENSE for full licensing details.
;;;


(defpackage :opengl-test
  (:use :cl))

(in-package :opengl-test)

(defclass window ()
  ((rot-x
    :initform 0.0
    :accessor rot-x)
   (rot-y
    :initform 0.0
    :accessor rot-y)
   (drag-point
    :initform nil
    :accessor drag-point)))
  



(defun draw-rect (p size)
  (gl:clear :color-buffer-bit)
  (gl:color 0 1 0)
  (gl:rect (sdl:x p) (sdl:y p) (+ (sdl:x p) size)  (+ (sdl:y p) size)))


(defgeneric resize-window (window w h))

(defmethod resize-window ((win window) w h)
  (when (zerop h)
    (setf h 1))
  (let ((ratio (float (/ w h))))
    (gl:viewport 0 0 w h)
    (gl:matrix-mode :projection)
    (gl:load-identity)
    (glu:perspective 80.0 ratio 1.0 400.0)
    (gl:matrix-mode :modelview)
    (gl:load-identity)))

(defgeneric setup-RC (w))
(defmethod setup-RC ((w window))
  (gl:enable :depth-test)
  (gl:front-face :ccw)
  (gl:enable :cull-face)
  (gl:enable :lighting)
  (let ((white-light '(0.45 0.45 0.45 1.0))
	(source-light '(0.25 0.25 0.25 1.0))
	(light-pos '(-50.0 25.0 250.0 0.0)))
	  
    (gl:light-model :light-model-ambient white-light)
    (gl:light :light0 :ambient source-light)
    (gl:light :light0 :diffuse source-light)
    (gl:light :light0 :position light-pos)
    (gl:enable :light0))
  (gl:enable :color-material)
  (gl:color-material :front :ambient-and-diffuse)
  (gl:clear-color 0.0 0.0 0.0 1.0))


(defgeneric render-scene (w))
(defmethod render-scene ((w window))
  (gl:clear :color-buffer :depth-buffer-bit)
  (gl:with-pushed-matrix
    (gl:translate 0.0 0.0 -300.0)
    (gl:rotate (rot-x w) 1.0 0.0 0.0)
    (gl:rotate (rot-y w) 0.0 1.0 0.0)
    (gl:color 1.0 0.0 0.0)
    (let ((fz 100)
	  (bz -100))
      ;; Front Face ;;;;;;;;;;;;;;;;;;;;;;
      (gl:with-primitive :quads
	;; Pointing straight out Z ?
	(gl:normal 0.0 0.0 1.0)
	;; Left Panel
	(gl:vertex -50.0 50.0 fz)
	(gl:vertex -50.0 -50.0 fz)
	(gl:vertex -35.0 -50.0 fz)
	(gl:vertex -35.0 50.0 fz)
	;; Right Panel
	(gl:vertex 50.0 50.0 fz)
	(gl:vertex 35.0 50.0 fz)
	(gl:vertex 35.0 -50.0 fz)
	(gl:vertex 50.0 -50.0 fz)
      
	;; Top Panel
	(gl:vertex -35.0 50.0 fz)
	(gl:vertex -35.0 35.0 fz)
	(gl:vertex 35.0 35.0 fz)
	(gl:vertex 35.0 50.0 fz)
	;; Bottom Panel
	(gl:vertex -35.0 -35.0 fz)
	(gl:vertex -35.0 -50.0 fz)
	(gl:vertex 35.0 -50.0 fz)
	(gl:vertex 35.0 -35.0 fz)

	;; Top length section ;;;;;;;;;;;;;;;;
	;; Normal points up Y axis
	(gl:normal 0.0 1.0 0.0)
	(gl:vertex -50.0 50.0 fz)
	(gl:vertex 50.0 50.0 fz)
	(gl:vertex 50.0 50.0 bz)
	(gl:vertex -50.0 50.0 bz)
	
	;; Bottom Section
	(gl:normal 0.0 -1.0 0.0)
	(gl:vertex -50.0 -50.0 fz)
	(gl:vertex -50.0 -50.0 bz)
	(gl:vertex 50.0 -50.0 bz)
	(gl:vertex 50.0 -50.0 fz)
      
	;; Left Section
	(gl:normal 1.0 0.0 0.0)
	(gl:vertex 50.0 50.0 fz)
	(gl:vertex 50.0 -50.0 fz)
	(gl:vertex 50.0 -50.0 bz)
	(gl:vertex 50.0 50.0 bz)

	
	;; Right Section 
	(gl:normal -1.0 0.0 0.0)
	(gl:vertex -50.0 50.0 fz)
	(gl:vertex -50.0 50.0 bz)
	(gl:vertex -50.0 -50.0 bz)
	(gl:vertex -50.0 -50.0 fz))
      (gl:front-face :cw)
      ;; Back Section ;;;;;;;;;;;;;
      (gl:with-primitive :quads
	
	;; Back section
	;; Pointing straight out Z
	(gl:normal 0.0  0.0  -1.0)	;	
      
	;; Left Panel
	(gl:vertex -50.0  50.0  bz)
	(gl:vertex -50.0  -50.0  bz)
	(gl:vertex -35.0  -50.0  bz)
	(gl:vertex -35.0 50.0  bz)
      
	;; Right Panel
	(gl:vertex 50.0  50.0  bz)
	(gl:vertex 35.0  50.0  bz)
	(gl:vertex 35.0  -50.0  bz)
	(gl:vertex 50.0 -50.0 bz)
	
	;; Top Panel
	(gl:vertex -35.0  50.0  bz)
	(gl:vertex -35.0  35.0  bz)
	(gl:vertex 35.0  35.0  bz)
	(gl:vertex 35.0  50.0 bz)
	
	;; Bottom Panel
	(gl:vertex -35.0  -35.0  bz)
	(gl:vertex -35.0  -50.0  bz)
	(gl:vertex 35.0  -50.0  bz)
	(gl:vertex 35.0  -35.0 bz)
      
	;; Insides ;;;;;;;;;;;;;;;;;;;;;;;;;;;;/
	(gl:color 0.75  0.75  0.75)
      
	;; Normal points up Y axis
	(gl:normal 0.0  1.0  0.0)
	(gl:vertex -35.0  35.0  fz)
	(gl:vertex 35.0  35.0  fz)
	(gl:vertex 35.0  35.0  bz)
	(gl:vertex -35.0 35.0 bz)
      
	;; Bottom section
	(gl:normal 0.0  1.0  0.0)
	(gl:vertex -35.0  -35.0  fz)
	(gl:vertex -35.0  -35.0  bz)
	(gl:vertex 35.0  -35.0  bz)
	(gl:vertex 35.0  -35.0  fz)

	;; Left section
	(gl:normal 1.0  0.0  0.0)
	(gl:vertex -35.0  35.0  fz)
	(gl:vertex -35.0  35.0  bz)
	(gl:vertex -35.0  -35.0  bz)
	(gl:vertex -35.0  -35.0  fz)
	
	;; Right Section
	(gl:normal -1.0  0.0  0.0)
	(gl:vertex 35.0  35.0  fz)
	(gl:vertex 35.0  -35.0  fz)
	(gl:vertex 35.0  -35.0  bz)
	(gl:vertex 35.0  35.0  bz)))
    (gl:front-face :ccw)))



(defun bounce (p dir size)
  (let ((x (sdl:x p))
	(y (sdl:y p)))
    (if (and (> (- 320 size) x 0)
	     (> (- 320 size) y 0))
	dir
	(list (if (> (+ x size) 320) :w
		  (if (< x 0) :e
		      (first dir)))
	      (if (> (+ y size) 320) :s
		  (if (< y 0) :n
		      (second dir)))))))
		
      
(defgeneric mouse (window button state x y))

(defgeneric motion (w x y))
  

;; (defmethod mouse ((w window) button state x y)
;;   (when (eq button :left-button)
;;     (if (eq state :down)
;; 	(setf (drag-point w) (cons x y))
;; 	(setf (drag-point w) nil))))
      
  
(defmethod motion ((w window) x-rel y-rel)
  (incf (rot-x w) (* y-rel 0.1))
  (incf (rot-y w) (* x-rel 0.1)))
  
;;   (when (drag-point w)
;;     (setf (rot-y w) (+ (- (car (drag-point w)) x) (rot-y w)))
;;     (setf (rot-x w) (+ (- (cdr (drag-point w)) y) (rot-x w)))
;;     (setf (drag-point w) (cons x y))))

	

(defun sdl-opengl-test ()
  (let ((w (make-instance 'window))
	(x-sum 0)
	(y-sum 0))
    (sdl:with-init ()
      (sdl:window 640 640 :flags sdl:SDL-OPENGL :title-caption "Nate's amazing moving box")
    
      (resize-window w 640 640)
      
      (sdl:with-events ()
	  (:quit-event () t)
	  (:mouse-motion-event (:state state :x-rel x-rel :y-rel y-rel)
			       
			       (when (equal state 1)
				 (incf x-sum x-rel)
				 (incf y-sum y-rel)
				 
				 (motion w x-rel y-rel)))

	  (:idle ()
		 (setup-RC w)
		 (render-scene w)
		 ;; Start processing buffered OpenGL routines.
		 (gl:flush)
		 (sdl:update-display))
	  (:video-expose-event (sdl:update-display))))))
