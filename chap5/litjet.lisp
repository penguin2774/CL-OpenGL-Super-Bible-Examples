;;;
;;; Chapter 5 Lit Jet Example
;;;
;;; Copyright (c) 2009 Nathanael Cunningham
;;; See LICENSE for full licensing details.
;;;


(defpackage :chapter5-litjet
  (:nicknames :litjet)
  (:use :cl)
  (:export :run))



(in-package :chapter5-litjet)


;(progn (asdf:oos 'asdf:load-op :cl-opengl)       (asdf:oos 'asdf:load-op :cl-glut))
(defclass hello-window (glut:window)
  ((rot-x
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




(defparameter *watched-vars* (make-hash-table :test 'eq))

(defmacro watch-var (&rest vars)
  `(progn 
     ,@(loop for var in vars
	   collect `(setf (gethash ',var *watched-vars*) ,var))))




(defun draw-vars (window-height)
  (gl:color 0 1 0)
  (loop for k being the hash-keys in *watched-vars*
     for line = 0 then (1+ line)
     do 
       (gl:window-pos 0 (- window-height 10 (* line 12)))
       (glut:bitmap-string glut:+bitmap-8-by-13+ (format nil "~a : ~a" k (gethash k *watched-vars*)))))



(defmethod glut:reshape ((window hello-window) w h)
  (when (zerop h)
    (setf h 1))
  (let ((aspect (/ w h))
	(light-pos #(-50.0 50.0 100.0 1.0)))
    (setf (glut:width window) w) 
    (setf (glut:height window) h)
    
    (gl:viewport 0 0 w h)
    (gl:matrix-mode :projection)
    (gl:load-identity)
    (glu:perspective 45.0 aspect 1.0 225.0)

    (gl:matrix-mode :modelview)
    (gl:load-identity)
    (gl:light :light0 :position light-pos)
    (gl:translate 0.0 0.0 -150.0)))


(defmethod glut:display-window :before ((w hello-window))
  ;;(glut:enable-tick w 33)
  ;; Select clearing color.
  (setup-RC w))
  ;;(gl:clear-color 0 0 0 0)
  ;; Initialize viewing values.
  ;;(glut:reshape w (glut:width w) (glut:height w)))

(defgeneric setup-RC (w))

(defmethod setup-RC ((w hello-window))
  (let ((ambient-light #(0.3 0.3 0.3 1.0))
	(diffuse-light #(0.7 0.7 0.7 1.0)))
    
  (gl:enable :depth-test)
  (gl:front-face :ccw)
  (gl:enable :cull-face)
  (gl:enable :lighting)
  
  (gl:light :light0 :ambient ambient-light)
  (gl:light :light0 :diffuse diffuse-light)
  (gl:enable :light0)
  
  (gl:enable :color-material)
  
  (gl:color-material :front :ambient-and-diffuse)
  
  (gl:clear-color 0.0 0.0 1.0 1.0)
  (gl:enable :normalize)))

	 
    

(defgeneric render-scene (w))

(defmethod render-scene ((w hello-window))
  ;; Clear the window with current clearing color
  (gl:clear :COLOR-BUFFER :DEPTH-BUFFER)

  ;; Save matrix state and do the rotation
  (gl:with-pushed-matrix
    (gl:rotate (rot-x w) 1.0 0.0 0.0)
    (gl:rotate (rot-y w) 0.0 1.0 0.0)


    ;; Nose Cone /////////////////////////////
    ;; White
    (gl:color 0.5 0.5 0.5)
    (gl:with-primitive :triangles
      (gl:normal 0.0 -1.0 0.0)
      (gl:vertex 0.0  0.0  60.0)
      (gl:vertex -15.0  0.0  30.0)
      (gl:vertex 15.0 0.0 30.0)
      
      
      (m3d:normalized-vertices
       (15.0 0.0 30.0)
       (0.0  15.0  30.0)
       (0.0  0.0  60.0))
      
     
      (m3d:normalized-vertices
       ( 0.0  0.0  60.0)
       ( 0.0  15.0  30.0)
       ( -15.0 0.0 30.0))


      ;; Body of the Plane ;;;;;;;;;;;;;;;;;;;;;;;;
     
      (m3d:normalized-vertices
       ( -15.0 0.0 30.0)
       ( 0.0  15.0  30.0)
       ( 0.0  0.0  -56.0))
		
      (m3d:normalized-vertices
       ( 0.0  0.0  -56.0)
       ( 0.0  15.0  30.0)
       ( 15.0 0.0 30.0))
	
      (m3d:normalized-vertices
       ( 15.0 0.0 30.0)
       ( -15.0  0.0  30.0)
       ( 0.0  0.0  -56.0))
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Left wing
      ;; Large triangle for bottom of wing
      (m3d:normalized-vertices
       ( 0.0 2.0 27.0)
       ( -60.0  2.0  -8.0)
       ( 60.0  2.0  -8.0))
	
      (m3d:normalized-vertices
       ( 60.0  2.0  -8.0)
       ( 0.0  7.0  -8.0)
       ( 0.0 2.0 27.0))
	
      (m3d:normalized-vertices
       ( 60.0  2.0  -8.0)
       ( -60.0  2.0  -8.0)
       ( 0.0 7.0 -8.0))
	
      ;; Other wing top section
      (m3d:normalized-vertices
       ( 0.0 2.0 27.0)
       ( 0.0  7.0  -8.0)
       ( -60.0  2.0  -8.0))
	
      ;; Tail section;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;/
      ;; Bottom of back fin
      (m3d:normalized-vertices
       ( -30.0  -0.50  -57.0)
       ( 30.0  -0.50  -57.0)
       ( 0.0 -0.50 -40.0))
	
      ;; top of left side
      (m3d:normalized-vertices
       ( 0.0 -0.5 -40.0)
       ( 30.0  -0.5  -57.0)
       ( 0.0  4.0  -57.0))
	
      ;; top of right side
      (m3d:normalized-vertices
       ( 0.0  4.0  -57.0)
       ( -30.0  -0.5  -57.0)
       ( 0.0 -0.5 -40.0))
	
      ;; back of bottom of tail
      (m3d:normalized-vertices
       ( 30.0 -0.5 -57.0)
       ( -30.0  -0.5  -57.0)
       ( 0.0  4.0  -57.0))
	
      ;; Top of Tail section left
      (m3d:normalized-vertices
       ( 0.0 0.5 -40.0)
       ( 3.0  0.5  -57.0)
       ( 0.0  25.0  -65.0))
	
      (m3d:normalized-vertices
       ( 0.0  25.0  -65.0)
       ( -3.0  0.5  -57.0)
       ( 0.0 0.5 -40.0))

      ;; Back of horizontal section
      (m3d:normalized-vertices
       ( 3.0 0.5 -57.0)
       ( -3.0  0.5  -57.0)
       ( 0.0  25.0  -65.0)))) ;; Of Jet
  ;; Display the results
  (glut:swap-buffers))

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

  (render-scene w)
  (draw-vars (glut:height w))

  (glut:swap-buffers))

(defmethod glut:tick ((w hello-window))
  
  (glut:post-redisplay))

(defun run ()
  (unwind-protect  
       (let ((window (make-instance 'hello-window)))
	 (glut:display-window window))
    (glut:destroy-current-window)))
  
;;(rb-hello)


