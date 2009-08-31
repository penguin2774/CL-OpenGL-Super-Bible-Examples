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


(defgeneric render-scene (w))
(defmethod render-scene ((w window))
  (gl:clear :color-buffer)
  (gl:pixel-store :unpack-alignment 1)
  (gl:raster-pos 0 0)
  (gl:draw-pixels (sdl:width *fire*) (sdl:height *fire*) )


(defgeneric resize-window (window w h))
(defmethod resize-window ((win window) w h)
  (when (zerop h)
    (setf h 1))
  (let ((ratio (float (/ w h))))
    (gl:viewport 0 0 w h)
    (gl:matrix-mode :projection)
    (gl:load-identity)
    (glu:ortho-2d 0.0 (float w) 0.0 (float h))
    (gl:matrix-mode :modelview)
    (gl:load-identity)))


	

(defun run ()
  (let ((w (make-instance 'window)))
    (sdl:with-init ()
      (setf *fire* (sdl-image:load-image "Fire.tga" :image-type :tga))
      (sdl:window 512 512 :flags sdl:SDL-OPENGL :title-caption "Nate's amazing moving box")
    
      (resize-window w 512 512)
      (setup-RC w)
      (render-scene w)
      ;; Start processing buffered OpenGL routines.
      (gl:flush)
      (sdl:update-display)
      (sdl:with-events ()
	  (:quit-event () t)
	  (:video-expose-event (sdl:update-display))))))
