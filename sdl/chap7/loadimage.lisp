;;;
;;; Bouncing Box Example (first example!)
;;;
;;; Copyright (c) 2009 Nathanael Cunningham
;;; See LICENSE for full licensing details.
;;;


(defpackage :chap7-sdl-loadimage
  (:nicknames :sdl-loadimage)
  (:use :cl)
  (:export :run))

(in-package :chap7-sdl-loadimage)

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
 

(defvar *fire* nil)


(defgeneric setup-RC (w))

(defmethod setup-RC ((w window))
  (gl:clear-color 0.0 0.0 0.0 0.0))

   
(defun convert-image-for-gl (surface)
  (let ((result (sdl:create-surface (sdl:width surface) (sdl:height surface)
				    :bpp 24)))
    (sdl:blit-surface surface result)
    result))
  
(defun get-pixel-pointer (surface)
  (cffi:foreign-slot-value (sdl::fp surface) 'sdl-cffi::SDL-Surface 'sdl-cffi::pixels))



(defgeneric render-scene (w))
(defmethod render-scene ((w window))
  
  (gl:clear :color-buffer)
;  (gl:pixel-store :unpack-alignment 1)
  (gl:raster-pos  0 (sdl:height *fire*) )
  (%gl:pixel-zoom 1.0 -1.0) ; SDL orients from the top left, where as gl orients from the bottem left, so we have to flip it. (theres gatta be a better way >.<)
   
  (%gl:draw-pixels (sdl:width *fire*) (sdl:height *fire*) :bgr :unsigned-byte
		     (sdl-base::with-pixel (pixels (sdl::fp *fire*))
		       (sdl-base::pixel-data pixels))))
  


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
      (setf *fire* (sdl-image:load-image "Fire.tga" :image-type :tga ))
      (resize-window w 512 512)
      (setup-RC w)
      (render-scene w)
      ;; Start processing buffered OpenGL routines.
      (gl:flush)
      (sdl:update-display)
      (sdl:with-events ()
	  (:quit-event () t)
	  (:video-expose-event (sdl:update-display))))))
