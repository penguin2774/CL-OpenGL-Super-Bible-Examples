;;;
;;; Chapter 4 Perspective Example
;;;
;;; Copyright (c) 2009 Nathanael Cunningham
;;; See LICENSE for full licensing details.
;;;



(defpackage :opengl-test
  (:use :cl))

(in-package :opengl-test)

;(progn (asdf:oos 'asdf:load-op :cl-opengl)       (asdf:oos 'asdf:load-op :cl-glut))
(defclass hello-window (glut:window)
  ((depth
    :initform nil
    :accessor depth)
   (cull
    :initform nil
    :accessor cull)
   (outline
    :initform nil
    :accessor outline)
   (rot-x
    :initform 0.0
    :accessor rot-x)
   (rot-y
    :initform 0.0
    :accessor rot-y)
   (zfar
    :initform 400.0
    :accessor zfar)
   (drag-point
    :initform nil
    :accessor drag-point))
  (:default-initargs :pos-x 100 :pos-y 100 :width 800 :height 600
                     :mode '(:double :rgb  :depth) :title "So nice a window, I though you might enjoy it!"))




(defconstant +n-range+ 100.0)
(defparameter *watched-vars* (make-hash-table :test 'eq))

(defmacro watch-var (&rest vars)
  `(progn 
     ,@(loop for var in vars
	   collect `(setf (gethash ',var *watched-vars*) ,var))))

;;(cffi:defcallback handle-menu :void ((value :int))
;;  nil)





(defun draw-vars ()
  (gl:color 0 1 0)
  (loop for k being the hash-keys in *watched-vars*
     for line = 0 then (1+ line)
     do 
       (gl:window-pos 0 (- 590 (* line 12)))
       (glut:bitmap-string glut:+bitmap-8-by-13+ (format nil "~a : ~a" k (gethash k *watched-vars*)))))



      


(defparameter x-rot 0.0)
(defparameter y-rot 0.0)

(defmethod glut:keyboard ((w hello-window) key x y)
  (case key
    (#\+ (incf (zfar w) 10)
	 (glut:reshape w (glut:width w) (glut:height w)))
    (#\- (decf (zfar w) 10)
	 (glut:reshape w (glut:width w) (glut:height w)))
    (#\1 (setf (depth w) (not (depth w))))
    (#\2 (setf (cull w) (not (cull w))))
    (#\3 (setf (outline w) (not (outline w)))))
  (glut:post-redisplay))
    
(defmethod glut:special ((w hello-window) key x y)
  (case key
    (:key-left (setf (rot-y w) (+ (rot-y w) 5.0)))
    (:key-right (setf (rot-y w) (- (rot-y w) 5.0)))
    (:key-up (setf (rot-x w) (+ (rot-x w) 5.0)))
    (:key-down (setf (rot-x w) (- (rot-x w) 5.0))))
  
  (glut:post-redisplay))

(defmethod glut:reshape  ((window hello-window) w h)
  (let ((ratio (float (/ w h))))
    (watch-var ratio)
    (when (zerop h)
      (setf h 1))
    (gl:viewport 0 0 w h)
    (gl:matrix-mode :projection)
    (gl:load-identity)
    (glu:perspective 80.0 ratio 1.0 400.0)
    (gl:matrix-mode :modelview)
    (gl:load-identity)))

(defmethod glut:display-window :before ((w hello-window))
  ;; Select clearing color.
  (setup-RC))
  ;;(gl:clear-color 0 0 0 0)
  ;; Initialize viewing values.
  ;;(glut:reshape w (glut:width w) (glut:height w)))

(defun setup-RC ()
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
(defmethod render-scene ((w hello-window))
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

(defmethod glut:mouse ((w hello-window) button state x y)
  (when (eq button :left-button)
    (if (eq state :down)
	(setf (drag-point w) (cons x y))
	(setf (drag-point w) nil))))
      
  
(defmethod glut:motion ((w hello-window) x y)
  (when (drag-point w)
    (setf (rot-y w) (+ (- (car (drag-point w)) x) (rot-y w)))
    (setf (rot-x w) (+ (- (cdr (drag-point w)) y) (rot-x w)))
    (setf (drag-point w) (cons x y))
    (glut:post-redisplay)))
		       


(defmethod glut:display ((w hello-window))
  (watch-var (zfar w))
  (render-scene w)
  (draw-vars)
  (glut:swap-buffers))

(defun rb-hello ()
  (unwind-protect  
       (let ((window (make-instance 'hello-window)))
	 (glut:display-window window))
    (glut:destroy-current-window)))
  
;;(rb-hello)

