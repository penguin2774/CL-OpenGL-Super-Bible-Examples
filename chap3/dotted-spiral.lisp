;;;
;;; Chapter 3 dotted spiral example 
;;;
;;; Copyright (c) 2009 Nathanael Cunningham
;;; See LICENSE for full licensing details.
;;;

(defpackage :opengl-test
  (:use :cl))

(in-package :opengl-test)

(defconstant +n-range+ 100.0)
(defparameter *watched-vars* (make-hash-table :test 'eq))

(defmacro watch-var (&rest vars)
  `(progn 
     ,@(loop for var in vars
	   collect `(setf (gethash ',var *watched-vars*) ,var))))

(defun change-size (w h)
  (let ((ratio (float (/ w h))))
    (watch-var ratio)
    (when (zerop h)
      (setf h 1))
    (gl:viewport 0 0 w h)
    (gl:matrix-mode :projection)
    (gl:load-identity)
    (if (<= w h)
	(gl:ortho -1 1 (/ -1 ratio) (/ 1 ratio) -1.0 1.0)
	(gl:ortho (* -1 ratio) (* 1 ratio) -1 1 -1 1))
    (gl:matrix-mode :modelview)
    (gl:load-identity)))

(defun draw-vars ()
  (loop for k being the hash-keys in *watched-vars*
       for line = 0 then (1+ line)
     do (sdl:with-font (font sdl:*font-8x8*)
	  (sdl:draw-string-shaded-* (format nil "~a : ~a" k (gethash k *watched-vars*))  10 (* 14 line) sdl:*green*  sdl:*black* ))))

(defun draw-thing (p)
  (gl:clear :color-buffer-bit)
  (gl:color 0 1 0)
  (gl:rect -0.25 -0.25 0.25 0.25)
  p)


	
      
(defun apply-motion (p dir)
  (setf (sdl:x p) (+ (sdl:x p) (if (eq (first dir) :e) 1 -1)))
  (setf (sdl:y p) (+ (sdl:y p) (if (eq (second dir) :s) -1 1))))
  
	

(defun sdl-opengl-test ()
  (sdl:with-init ()
    (sdl:window 320 320 :flags '(sdl:SDL-OPENGL sdl:SDL-RESIZABLE)  :title-caption "Nate's amazing moving box")
    (gl:clear-color 0 0 0 0)
    (change-size 320 320)
    (let ((p (sdl:point :x 0 :y 50))
	  ;(size 50)
	  ;(dir '(:n :e))
	  )
      (sdl:with-events ()
	(:quit-event () t)
	(:idle ()
	       (draw-thing p)
	       ;; Start processing buffered OpenGL routines.
	       (gl:flush)
	       (draw-vars)
	       (sdl:update-display))
	(:video-resize-event (:w w :h h)
	  (change-size w h) )
	(:video-expose-event (sdl:update-display))))))
