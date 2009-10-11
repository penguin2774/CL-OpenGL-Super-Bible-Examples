;;;
;;; Rendering a square with an image on it.
;;;
;;; Copyright (c) 2009 Nathanael Cunningham
;;; See LICENSE for full licensing details.
;;;


(defpackage :image-test
  (:use :cl)
  (:export :run))
(require 'lispbuilder-sdl)
(require 'lispbuilder-sdl-image)
(require 'cl-opengl)
(require 'cl-glu)
(load "/home/nathan/prj/lab/opengl/sdl/chap8/image.lisp")

(in-package :image-test)

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
 

(defparameter *ship*  (make-instance 'rielib:image))
(defparameter *turret* (make-instance 'rielib:image))


(defgeneric setup-RC (w))

(defmethod setup-RC ((w window))
  (gl:clear-color 1.0 1.0 1.0 0.0)
  (gl:enable :blend)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:enable :texture-2d))

   

(defun tex-point (x y)
  (gl:vertex x y)
  (gl:tex-coord x y))


(defgeneric render-scene (w))
(defmethod render-scene ((w window))
  (gl:clear :color-buffer)
  (gl:color 1.0 1.0 1.0)
  
  (rielib:render *ship*))
  



(defgeneric resize-window (window w h))
(defmethod resize-window ((win window) w h)
  (when (zerop h)
    (setf h 1))
  (gl:viewport 0 0 w h)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (glu:ortho-2d  0.0 (float w) 0.0 (float h))
  (gl:matrix-mode :modelview)
  (gl:load-identity))


(defun make-bounce-fn (low high &key (step 1) start-value (start-dir :up))
  (assert (or (eq start-dir :up)
	      (eq start-dir :down)))
  (let ((var (or start-value low))
	(dir start-dir))
    (lambda ()
      (if (eq dir :up)
	  (when (>= (incf var step) high)
	    (setf var high)
	    (setf dir :down))
	  (when (<= (decf var step) low)
	    (setf var low)
	    (setf dir :up)))
      var)))
	 
	 

(defun run ()
  (let ((w (make-instance 'window))
	(bounce-fn (make-bounce-fn 0.0 5.0 :step 0.5 :start-value 0.0)))
    (sdl:with-init ()
      (sdl:window 512 512 :flags sdl:SDL-OPENGL :title-caption "Nate's amazing moving box")
      (resize-window w 512 512)
      (setup-RC w)
      (sdl:with-surfaces ((ship (sdl-image:load-image "/home/nathan/prj/lab/opengl/sdl/chap8/dwarf-frigit-no-turret.png"))
			  (turret (sdl-image:load-image "/home/nathan/prj/lab/opengl/sdl/chap8/dwarf-frigit-turret.png")))
	(rielib:load-texture *ship* ship)
	(setf (rielib:location *ship*) '(100.0 100.0))
	(rielib:load-texture *turret* turret)
	(setf (rielib:location *turret*) '(0.0 43.0))

	(rielib:add-subimage *ship* *turret*))
      
      ;; Start processing buffered OpenGL routines.
      
      
      (sdl:with-events ()
	  (:quit-event () t)
	  (:idle ()
		 (rielib:rotate *turret* 6)
		 (rielib:rotate *ship* -3)
		 (render-scene w)
		 (gl:flush)
		 (sdl:update-display))
	  (:video-expose-event (sdl:update-display))))))
