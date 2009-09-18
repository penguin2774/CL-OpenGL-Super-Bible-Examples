;;;
;;; Rendering a square with an image on it.
;;;
;;; Copyright (c) 2009 Nathanael Cunningham
;;; See LICENSE for full licensing details.
;;;


(defpackage :chap8-sdl-simple
  (:nicknames :sdl-simple)
  (:use :cl)
  (:export :run))

(in-package :chap8-sdl-simple)

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
 

(defvar *ship* nil)


(defgeneric setup-RC (w))

(defmethod setup-RC ((w window))
  (gl:clear-color 0.0 0.0 0.0 0.0)
  (gl:enable :texture-2d))

   
(defun convert-image-for-gl (surface)
  (let ((result (sdl:create-surface (sdl:width surface) (sdl:height surface)
				    :bpp 24)))
    (sdl:blit-surface surface result)
    result))
  
(defun get-pixel-pointer (surface)
  (cffi:foreign-slot-value (sdl::fp surface) 'sdl-cffi::SDL-Surface 'sdl-cffi::pixels))

(defun load-texture (surface)
  (let ((texture (first (gl:gen-textures 1))))
    (gl:bind-texture :texture-2d texture)
    (gl:tex-parameter :texture-2d :texture-min-filter :linear)
    (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
    (gl:tex-image-2d :texture-2d 0
		     :rgb (sdl:width surface) (sdl:height surface)
		     0 :rgb :unsigned-byte (sdl-base::with-pixel (pixels (sdl::fp surface))
		       (sdl-base::pixel-data pixels)))
    texture))


(defun tex-point (x y)
  (gl:vertex x y)
  (gl:tex-coord x y))
(defgeneric render-scene (w))
(defmethod render-scene ((w window))
  (let ((points `((0.0 0.0)
		  (1.0 0.0)
		  (1.0 1.0)
		  (0.0 1.0)
		  )))
  (gl:clear :color-buffer)
  (gl:color 1.0 1.0 1.0)
  (gl:with-pushed-matrix 
    (gl:translate 10 10 0.0)
    (gl:scale 200 200 0.0)
    (gl:with-primitive :quads
      (apply #'gl:tex-coord (first points))
      (gl:vertex 0.0 0.0)
      
      (apply #'gl:tex-coord (second points))
      (gl:vertex 1.0 0.0)

      (apply #'gl:tex-coord (third points))
      (gl:vertex 1.0 1.0)

      (apply #'gl:tex-coord (fourth points))
      (gl:vertex 0.0 1.0)
))))
  


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


	

(defun run ()
  (let ((w (make-instance 'window)))
    (sdl:with-init ()
      (sdl:window 512 512 :flags sdl:SDL-OPENGL :title-caption "Nate's amazing moving box")
      (resize-window w 512 512)
      (setup-RC w)
      (setf *ship* (load-texture (sdl-image:load-image "/home/nathan/prj/lab/opengl/sdl/chap8/hartnell-1.png"  )))
      (render-scene w)
      ;; Start processing buffered OpenGL routines.
      (gl:flush)
      (sdl:update-display)
      (sdl:with-events ()
	  (:quit-event () t)
	  (:video-expose-event (sdl:update-display))))))
