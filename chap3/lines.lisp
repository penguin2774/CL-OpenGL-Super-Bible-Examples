(defpackage :opengl-test
  (:use :cl))

(in-package :opengl-test)

;(progn (asdf:oos 'asdf:load-op :cl-opengl)       (asdf:oos 'asdf:load-op :cl-glut))
(defclass hello-window (glut:window)
  ()
  (:default-initargs :pos-x 100 :pos-y 100 :width 320 :height 320
                     :mode '(:single :rgb) :title "So nice a window, I though you might enjoy it!"))




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
     do (gl:raster-pos 0 (- 97 (* line 3.5)))
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
  (gl:clear-color 0 0 0 0)
  ;; Initialize viewing values.
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:ortho 0 100 0 100 -100 100))

(defparameter x-rot 0.0)
(defparameter y-rot 0.0)

(defmethod glut:special ((w hello-window) key x y)
  (case key
    (:key-left (setf y-rot (+ y-rot 5.0)))
    (:key-right (setf y-rot (- y-rot 5.0)))
    (:key-up (setf x-rot (+ x-rot 5.0)))
    (:key-down (setf x-rot (- x-rot 5.0))))
  (glut:post-redisplay))

(defun render-scene ()
  (gl:clear :color-buffer)
  (gl:push-matrix)
  (gl:rotate x-rot 1.0 0.0 0.0)
  (gl:rotate y-rot 0.0 1.0 0.0)
  (gl:color 1 1 1)
  (gl:with-primitive :lines
    (loop
	 with max = (* (* 2.0 pi) 3.0)
       for angle from 0.0 to max by 0.1
       for c = 0.0 then (+ c (float (/ 0.1 max)))
       for z = -50.0 then (+ z 0.5)
       for x = (* 50.0 (cos angle))
       for y = (* 50.0 (sin angle))
       for x2 = (* 50.0 (cos (+ angle pi)))
       for y2 = (* 50.0 (sin (+ angle pi)))
	 
       do  (gl:color (- 1.0 c) 1 (- 1.0 c))
	 (gl:vertex x y z)
	 (gl:vertex  x2 y2 z) ))
  (gl:pop-matrix))
  

(defmethod glut:display ((w hello-window))
  
  ;; Draw white polygon (rectangle) with corners at
  ;; (0.25, 0.25, 0.0) and (0.75, 0.75, 0.0).
  (render-scene)
    ;; Start processing buffered OpenGL routines.
  (draw-vars)
  (gl:flush))

(defun rb-hello ()
  (glut:display-window (make-instance 'hello-window)))

(rb-hello)

