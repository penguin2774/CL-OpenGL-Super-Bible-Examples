;;;
;;; Chapter 4 SphereWorld example.
;;;
;;; Copyright (c) 2009 Nathanael Cunningham
;;; See LICENSE for full licensing details.
;;;


(defpackage :opengl-test
  (:nicknames :gltest)
  (:use :cl)
  (:export :rb-hello))



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
    :accessor drag-point)
   (spheres 
    :initform nil
    :accessor spheres)
   (camera
    :initform (make-instance 'glt:frame)
    :accessor camera)   
   (torus-rot-y
    :initform 0.0
    :accessor torus-rot-y))
  (:default-initargs :pos-x 100 :pos-y 100 :width 800 :height 600
                     :mode '(:double :rgb  :depth) :title "So nice a window, I though you might enjoy it!"))

(defclass sphere (glt:frame)
  ((size
    :initform 0.1
    :accessor size)))


(defconstant +n-range+ 100.0)
(defparameter *watched-vars* (make-hash-table :test 'eq))

(defmacro watch-var (&rest vars)
  `(progn 
     ,@(loop for var in vars
	   collect `(setf (gethash ',var *watched-vars*) ,var))))

;;(cffi:defcallback handle-menu :void ((value :int))
;;  nil)





(defun draw-vars (window-height)
  (gl:color 0 1 0)
  (loop for k being the hash-keys in *watched-vars*
     for line = 0 then (1+ line)
     do 
       (gl:window-pos 0 (- window-height 10 (* line 12)))
       (glut:bitmap-string glut:+bitmap-8-by-13+ (format nil "~a : ~a" k (gethash k *watched-vars*)))))



      


(defparameter x-rot 0.0)
(defparameter y-rot 0.0)

;; (defmethod glut:keyboard ((w hello-window) key x y)
;;   (case key
;;     (#\+ (incf (zfar w) 10)
;; 	 (glut:reshape w (glut:width w) (glut:height w)))
;;     (#\- (decf (zfar w) 10)
;; 	 (glut:reshape w (glut:width w) (glut:height w)))
;;     (#\1 (setf (depth w) (not (depth w))))
;;     (#\2 (setf (cull w) (not (cull w))))
;;     (#\3 (setf (outline w) (not (outline w)))))
;;   (glut:post-redisplay))
    
(defmethod glut:special ((w hello-window) key x y)
  (case key
    (:key-left (glt:rotate-local-y (camera w) 0.1))
    (:key-right (glt:rotate-local-y (camera w) -0.1))
    (:key-up (glt:move-forward (camera w) 0.1))
    (:key-down (glt:move-forward (camera w) -0.1)))
  
  (glut:post-redisplay))

(defmethod glut:reshape ((window hello-window) w h)
  (let ((ratio (float (/ w h))))
    (setf (glut:width window) w) ;; glut doesn't do this for us..?
    (setf (glut:height window) h)
    (when (zerop h)
      (setf h 1))
    (gl:viewport 0 0 w h)
    (gl:matrix-mode :projection)
    (gl:load-identity)
    (glu:perspective 35.0 ratio 1.0 50.0)
    (gl:matrix-mode :modelview)
    (gl:load-identity)))

(defmethod glut:display-window :before ((w hello-window))
  (glut:enable-tick w 33)
  ;; Select clearing color.
  (setup-RC w))
  ;;(gl:clear-color 0 0 0 0)
  ;; Initialize viewing values.
  ;;(glut:reshape w (glut:width w) (glut:height w)))

(defvar light-pos '(0.0 0.0 0.0 1.0))
(defvar spheres 50)
(defgeneric setup-RC (w))

(defmethod setup-RC ((w hello-window))
  ;(gl:enable :depth-test)
  ;(gl:front-face :ccw)
  ;(gl:enable :cull-face)
  
  (gl:clear-color 0.0 0.0 0.5 1.0)
  (%gl:polygon-mode :front-and-back :line)
  (setf (spheres w) (loop repeat  50
	collect (let ((result  (make-instance 'sphere)))
		  (setf (size result) (* (1+ (random 19)) 0.1))
		  (glt:set-origin result
		   (coerce (* (- (random 400) 200) 0.1) 'single-float)
		   (coerce (* (random 100) 0.01) 'single-float)
		   (coerce (* (- (random 400) 200) 0.1) 'single-float))))))


(defgeneric draw-ground (w))

(defmethod draw-ground ((w hello-window))
  (gl:with-primitive :lines
    (loop for line from -20.0 to 20.0
	 do
	 (gl:vertex line -0.4 20.0)
	 (gl:vertex line -0.4 -20.0)
	 
	 (gl:vertex 20.0 -0.4 line)
	 (gl:vertex -20.0 -0.4 line))))
	 
    

(defgeneric render-scene (w))

(let ((y-rot 0.0))
  (defmethod render-scene ((w hello-window))

    (incf y-rot 0.5)
    (watch-var (glt:origin (camera w))
	       (glt:forward (camera w))
	       (glt:up (camera w)))
    (gl:clear :color-buffer :depth-buffer)
    
    (gl:with-pushed-matrix
      (glt:apply-camera-transform (camera w))
      (draw-ground w)
      
      (loop for i in (spheres w)
	   	   do (gl:with-pushed-matrix 
		(glt:apply-actor-transform i)
		(glut:solid-sphere (size i) 13.0 26.0)))
      
      (gl:with-pushed-matrix 
	(gl:translate 0.0 0.0 -2.5)
	(gl:with-pushed-matrix 
	  (gl:rotate (* (- y-rot) 2.0) 0.0 1.0 0.0)
	  (gl:translate 1.0 0.0 0.0)
	  (glut:solid-sphere 0.1 13.0 26.0))
	(gl:rotate y-rot 0.0 1.0 0.0)
	(glt:draw-torus 0.35 0.15 40.0 20.0)))
	  
    
    (glut:swap-buffers)))

;; (defmethod glut:mouse ((w hello-window) button state x y)
;;   (when (eq button :left-button)
;;     (if (eq state :down)
;; 	(setf (drag-point w) (cons x y))
;; 	(setf (drag-point w) nil))))
      
  
;; (defmethod glut:motion ((w hello-window) x y)
;;   (when (drag-point w)
;;     (setf (rot-y w) (+ (- (car (drag-point w)) x) (rot-y w)))
;;     (setf (rot-x w) (+ (- (cdr (drag-point w)) y) (rot-x w)))
;;     (setf (drag-point w) (cons x y))
;;     (glut:post-redisplay)))
		       


(defmethod glut:display ((w hello-window))

  (render-scene w)
  (draw-vars (glut:height w))

  (glut:swap-buffers))

(defmethod glut:tick ((w hello-window))
  
  (glut:post-redisplay))

(defun rb-hello ()
  (unwind-protect  
       (let ((window (make-instance 'hello-window)))
	 (glut:display-window window))
    (glut:destroy-current-window)))
  
;;(rb-hello)


