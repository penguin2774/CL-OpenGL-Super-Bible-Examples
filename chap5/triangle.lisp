;;;
;;; Chapter 5 Triangle Example
;;;
;;; Copyright (c) 2009 Nathanael Cunningham
;;; See LICENSE for full licensing details.
;;;


(defpackage :chap5-triangle
  (:nicknames :triangle)
  (:use :cl)
  (:export :run))



(in-package :chap5-triangle)



(defclass hello-window (glut:window)
  ()
  (:default-initargs :pos-x 100 :pos-y 100 :width 800 :height 600
                     :mode '(:double :rgb  :depth) :title "Chapter 5 Triangle"))




(defparameter *watched-vars* (make-hash-table :test 'eq))

(defmacro watch-var (&rest vars)
  `(progn 
     ,@(loop for var in vars
	   collect `(setf (gethash ',var *watched-vars*) ,var))))




(defun draw-vars (window-height)
  (gl:color 0 1 0)
  (loop for k being the hash-keys in *watched-vars*
     for line = 0 then (1+ line)
     do 
       (gl:window-pos 0 (- window-height 10 (* line 12)))
       (glut:bitmap-string glut:+bitmap-8-by-13+ (format nil "~a : ~a" k (gethash k *watched-vars*)))))
      




(defmethod glut:reshape ((window hello-window) w h)
  (when (zerop h)
    (setf h 1))
  (let ((ratio (float (/ w h)))
	(ww (if (<=  w h) 
		250.0
		(* 250 (/ w h))))
	(wh (if (<=  w h) 
		250.0
		(* 250 (/ w h)))))
    (setf (glut:width window) w) ;; glut doesn't do this for us..?
    (setf (glut:height window) h)
    
    (gl:viewport 0 0 w h)

    (gl:load-identity)

    (%gl:ortho (- ww) ww (- wh) wh 1.0 -1.0)))

(defmethod glut:display-window :before ((w hello-window))
  (setup-RC w))
 

(defgeneric setup-RC (w))

(defmethod setup-RC ((w hello-window))
  ;(gl:enable :depth-test)
  ;(gl:enable :dither)
  (%gl:shade-model :smooth)
  
  (Gl:clear-color 0.0 0.0 0.0 1.0)
  )

	 
    

(defgeneric render-scene (w))

(defmethod render-scene ((w hello-window))
  (gl:clear :color-buffer); :depth-buffer)
  
  (gl:with-pushed-matrix
    
    (gl:with-primitive :triangles
      (gl:color 1.0 0.0 0.0)
      (gl:vertex 0.0 200.0 0.0)

      (gl:color 0.0 1.0 0.0)
      (gl:vertex 200.0 -70.0 0.0)

      (gl:color 0.0 0.0 1.0)
      (gl:vertex -200.0 -70.0 0.0))
    
    )
  (glut:swap-buffers))


		       


(defmethod glut:display ((w hello-window))

  (render-scene w)
  (draw-vars (glut:height w))

  (glut:swap-buffers))



(defun run ()
  (unwind-protect  
       (let ((window (make-instance 'hello-window)))
	 (glut:display-window window))
    (glut:destroy-current-window)))
  
;;(rb-hello)


