;;;
;;; Chapter 4 atom example
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
   (electron
    :initform 0.0
    :accessor electron)
   (tick-time
    :initform (get-internal-run-time)
    :accessor tick-time)
   (drag-point
    :initform nil
    :accessor drag-point))
  (:default-initargs :pos-x 100 :pos-y 100 :width 320 :height 320
                     :mode '(:double :rgb :depth) :title "So nice a window, I though you might enjoy it!"))

     
	  

(defconstant +n-range+ 100.0)
(defparameter *watched-vars* (make-hash-table :test 'eq))

(defmacro watch-var (&rest vars)
  `(progn 
     ,@(loop for var in vars
	   collect `(setf (gethash ',var *watched-vars*) ,var))))

(defun draw-vars ()
  (gl:color 0 1 0)
  (loop for k being the hash-keys in *watched-vars*
     for line = 0 then (1+ line)
     do (gl:raster-pos -100 (- 93 (* line 8)))
       (glut:bitmap-string glut:+bitmap-8-by-13+ (format nil "~a : ~a" k (gethash k *watched-vars*)))))


(defmethod glut:reshape  ((window hello-window) w h)
  (let ((ratio (float (/ w h))))
    ;(watch-var ratio)
    (when (zerop h)
      (setf h 1))
    (gl:viewport 0 0 w h)
    (gl:matrix-mode :projection)
    (gl:load-identity)
    (if (<= w h)
	(gl:ortho -100 100 (/ -100 ratio) (/ 100 ratio) -100 100)
	(gl:ortho (* -100 ratio) (* 100 ratio) -100 100 -100 100))))
      
(defmethod glut:display-window :before ((w hello-window))
  ;; Select clearing color.
  (setup-RC)
  (gl:clear-color 0 0 0 0)
  ;; Initialize viewing values.
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:ortho 0 100 0 100 -100 100))

(defparameter x-rot 0.0)
(defparameter y-rot 0.0)


(defun setup-RC ()
  (gl:clear-color 0.0 0.0 0.0 0.0)
  (gl:shade-model :flat)
  (gl:front-face :cw))

(defgeneric render-scene (w))

(defmethod render-scene ((w hello-window))

  (gl:clear :color-buffer :depth-buffer-bit)
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  (gl:enable :depth-test)
  (gl:translate 0.0 0.0 0.0)
  
  ;; Red nucleus
  (gl:color 1 0 0)
  (glut:solid-sphere 10.0 15 15)
  (gl:color 1 1 0)
  (gl:with-pushed-matrix 
    (gl:rotate (electron w) 0.0 1.0 0.0)
    (gl:translate 90.0 0 0)
    (glut:solid-sphere 6.0 15 15))
  (gl:with-pushed-matrix 
    (gl:rotate 45.0 0.0 0.0 1.0)
    (gl:rotate (electron w) 0.0 1.0 0.0)
    (gl:translate -70.0 0 0)
    (glut:solid-sphere 6.0 15 15))
  (gl:with-pushed-matrix 
    (gl:rotate -45.0 0.0 0.0 1.0)
    (gl:rotate (electron w) 0.0 1.0 0.0)
    (gl:translate 0.0 0.0 60.0)
    (glut:solid-sphere 6.0 15 15))
  (incf (electron w) 10.0)
  (if (> (electron w) 360.0)
      (setf (electron w) 0.0)))

(assert (= internal-time-units-per-second 1000))


(defmethod glut:idle ((w hello-window))
  (when (>= (- (get-internal-real-time) (tick-time w)) 33)
    (setf (tick-time w) (get-internal-real-time))
    (glut:post-redisplay)))


(defmethod glut:display ((w hello-window))
  (render-scene w)  
  (draw-vars)
  (gl:flush)
  (glut:swap-buffers))

(defun rb-hello ()
  (let ((window (make-instance 'hello-window)))
    (glut:display-window window)
    (glut:destroy-current-window)))
  
(rb-hello)

