;;;
;;; Chapter 2 Bouncing Box Example
;;;
;;; Copyright (c) 2009 Nathanael Cunningham
;;; See LICENSE for full licensing details.
;;;

(defpackage :opengl-test
  (:use :cl))

(in-package :opengl-test)

(defun draw-rect (p size)
  (gl:clear :color-buffer-bit)
  (gl:color 0 1 0)
  (gl:rect (sdl:x p) (sdl:y p) (+ (sdl:x p) size)  (+ (sdl:y p) size)))


(defun bounce (p dir size)
  (let ((x (sdl:x p))
	(y (sdl:y p)))
    (if (and (> (- 320 size) x 0)
	     (> (- 320 size) y 0))
	dir
	(list (if (> (+ x size) 320) :w
		  (if (< x 0) :e
		      (first dir)))
	      (if (> (+ y size) 320) :s
		  (if (< y 0) :n
		      (second dir)))))))
		
      
(defun apply-motion (p dir)
  (setf (sdl:x p) (+ (sdl:x p) (if (eq (first dir) :e) 1 -1)))
  (setf (sdl:y p) (+ (sdl:y p) (if (eq (second dir) :s) -1 1))))
  
	

(defun sdl-opengl-test ()
  (sdl:with-init ()
    (sdl:window 320 320 :flags sdl:SDL-OPENGL :title-caption "Nate's amazing moving box")
    (gl:clear-color 0 0 0 0)
    (gl:matrix-mode :projection)
    (gl:load-identity)
    (gl:ortho 0 320 0 320 -320 320)
    (let ((p (sdl:point :x 0 :y 50))
	  (size 50)
	  (dir '(:n :e)))
      (sdl:with-events ()
	(:quit-event () t)
	(:idle ()
	       (setf dir (bounce p dir size))
	       (apply-motion p dir)
	       (draw-rect p size)
	       ;; Start processing buffered OpenGL routines.
	       (gl:flush)
	       (sdl:update-display))
	(:video-expose-event (sdl:update-display))))))
