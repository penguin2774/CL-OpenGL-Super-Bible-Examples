;;;
;;; Bouncing Box Example (first example!)
;;;
;;; Copyright (c) 2009 Nathanael Cunningham
;;; See LICENSE for full licensing details.
;;;


(defpackage :chap7-sdl-bitmaps
  (:nicknames :sdl-bitmaps)
  (:use :cl)
  (:export :run))

(in-package :chap7-sdl-bitmaps)

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
 

(defvar *fire* (cffi:foreign-alloc 
		'(:uint8) :initial-contents (list #X00 #X00 #X00 #X00 
						  #X00 #X00 #X00 #X00
						  #X00 #X00 #X00 #X00
						  #X00 #X00 #X00 #X00
						  #X00 #X00 #X00 #X00
						  #X00 #X00 #X00 #X00
						  #X00 #X00 #X00 #Xc0
						  #X00 #X00 #X01 #Xf0
						  #X00 #X00 #X07 #Xf0
						  #X0f #X00 #X1f #Xe0
						  #X1f #X80 #X1f #Xc0
						  #X0f #Xc0 #X3f #X80	
						  #X07 #Xe0 #X7e #X00
						  #X03 #Xf0 #Xff #X80
						  #X03 #Xf5 #Xff #Xe0
						  #X07 #Xfd #Xff #Xf8
						  #X1f #Xfc #Xff #Xe8
						  #Xff #Xe3 #Xbf #X70 
						  #Xde #X80 #Xb7 #X00
						  #X71 #X10 #X4a #X80
						  #X03 #X10 #X4e #X40
						  #X02 #X88 #X8c #X20
						  #X05 #X05 #X04 #X40
						  #X02 #X82 #X14 #X40
						  #X02 #X40 #X10 #X80 
						  #X02 #X64 #X1a #X80
						  #X00 #X92 #X29 #X00
						  #X00 #Xb0 #X48 #X00
						  #X00 #Xc8 #X90 #X00
						  #X00 #X85 #X10 #X00
						  #X00 #X03 #X00 #X00
						  #X00 #X00 #X10 #X00)))

(defgeneric setup-RC (w))

(defmethod setup-RC ((w window))
  (gl:clear-color 0.0 0.0 0.0 0.0))

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


(defgeneric render-scene (w))
(defmethod render-scene ((w window))
  (gl:clear :color-buffer)
  (gl:color 1.0 1.0 1.0)
  (loop for x from 0 to 15
     do (loop for y from 0 to 15
	     do (gl:raster-pos (* x 32) (* y 32))
	     (%gl:bitmap  32 32 0.0 0.0 32.0 0.0 *fire*))))



	

(defun run ()
  (let ((w (make-instance 'window)))
    (sdl:with-init ()
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
