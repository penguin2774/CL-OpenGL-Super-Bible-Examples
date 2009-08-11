;;;
;;; Chapter 3 stencle example
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
   (box
    :initarg :box
    :accessor box)
   (tick-time
    :initform (get-internal-run-time)
    :accessor tick-time)
   (drag-point
    :initform nil
    :accessor drag-point))
  (:default-initargs :pos-x 100 :pos-y 100 :width 320 :height 320
                     :mode '(:single :rgb :depth :stencil) :title "So nice a window, I though you might enjoy it!"))

(defclass rect()
  ((x
    :initarg :x
    :accessor x)
   (y
    :initarg :y
    :accessor y)
   (w
    :initarg :w
    :accessor w)
   (h
    :initarg :h
    :accessor h)))

(defclass box (rect)
  ((heading-x 
    :initarg :heading-x
    :accessor heading-x)
   (heading-y
    :initarg :heading-y
    :accessor heading-y)
   (boundary
    :initarg :boundary
    :accessor boundary)
   (vol
    :initarg :vol
    :accessor vol)
   (color 
    :initarg :color
    :reader color)))

(defgeneric render (obj))
(defmethod render ((b box))
  (apply #'gl:color (color b))
  (gl:rect (x b) (y b) (+ (x b) (w b)) (+ (y b) (h b))))

(defgeneric bounce-check (b))
(defmethod bounce-check ((b box))
  (if (eq (heading-x b) :right)
      (if (>= (+ (x b) (w b)) (+ (x (boundary b)) (w (boundary b))))
	  (setf (heading-x b) :left))
      (if (<= (x b) (x (boundary b)))
	  (setf (heading-x b) :right)))
  (if (eq (heading-y b) :up)
      (if (>= (+ (y b) (h b)) (+ (y (boundary b)) (h (boundary b))))
	  (setf (heading-y b) :down))
      (if (<= (y b) (y (boundary b)))
	  (setf (heading-y b) :up))))

(defgeneric wander (obj))
(defmethod wander ((b box))
  (if (eq (heading-x b) :right)
      (incf (x b) (car (vol b)))
      (decf (x b) (car (vol b))))
  (if (eq (heading-y b) :up)
      (incf (y b) (cdr (vol b)))
      (decf (y b) (cdr (vol b)))))
      
	  

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
	(gl:ortho (* -100 ratio) (* 100 ratio) -100 100 -100 100))
    (gl:matrix-mode :modelview)
    (gl:load-identity)))
      
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
  (gl:color 0.0 1.0 0.0)
  (gl:shade-model :flat)
  (gl:front-face :cw))

(defgeneric render-scene (w))

(defmethod render-scene ((w hello-window))
  (if (depth w)
      (gl:enable :depth-test)
      (gl:disable :depth-test))
  (if (cull w)
      (gl:enable :cull-face)
      (gl:disable :cull-face))
  (if (outline w)
      (gl:polygon-mode :back :line)
      (gl:polygon-mode :back :fill))

  (gl:clear :color-buffer :depth-buffer-bit :stencil-buffer-bit)
  (gl:push-matrix)
  (gl:rotate (rot-x w) 1.0 0.0 0.0)
  (gl:rotate (rot-y w) 0.0 1.0 0.0)
  (gl:enable :stencil-test)
  (gl:stencil-func :never #x0 #x0)
  (gl:stencil-op :incr :incr :incr)
  (gl:color 1 1 1)

  (gl:with-primitive :line-strip
    (loop for angle from 0 to 400.0 by 0.1
	 for radious = 1.002 then (* radious 1.002)
	 do (gl:vertex (* radious (cos angle)) (* radious (sin angle)))))
  (gl:stencil-func :notequal #x1 #x1)
  (gl:stencil-op :keep :keep :keep)
  (render (box w))
;;  (watch-var (depth w))
;;  (watch-var (cull w))
;;  (watch-var (outline w))
  
  (gl:pop-matrix))

(assert (= internal-time-units-per-second 1000))

(defgeneric move-box (w))
(defmethod move-box ((w hello-window))
  (wander (box w))
  (bounce-check (box w)))

(defmethod glut:idle ((w hello-window))
  (when (>= (- (get-internal-real-time) (tick-time w)) 33)
    (setf (tick-time w) (get-internal-real-time))
    (move-box w)
    (glut:post-redisplay)))
    


(defmethod glut:display ((w hello-window))
  (render-scene w)  
  
  (draw-vars)
  (gl:flush))

(defun rb-hello ()
  (let ((window (make-instance 'hello-window :box (make-instance 'box  :x 20 :y 30 :w 50 :h 50 :heading-x :right :heading-y :up
								 :boundary (make-instance 'rect :x -100 :y -100 :w 200 :h 200)
								 :vol (cons 1 1) :color '(0 1 0)))))
    (glut:display-window window)
    (glut:destroy-current-window)))
  
(rb-hello)

