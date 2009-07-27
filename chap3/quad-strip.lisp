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
   (drag-point
    :initform nil
    :accessor drag-point))
  (:default-initargs :pos-x 100 :pos-y 100 :width 320 :height 320
                     :mode '(:double :rgb :depth) :title "So nice a window, I though you might enjoy it!"))




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

(defmethod glut:keyboard ((w hello-window) key x y)
  (case key
    (#\1 (setf (depth w) (not (depth w))))
    (#\2 (setf (cull w) (not (cull w))))
    (#\3 (setf (outline w) (not (outline w)))))
  (glut:post-redisplay))
    
(defmethod glut:special ((w hello-window) key x y)
  (case key
    (:key-left (setf (rot-y w) (+ (rot-y w) 5.0)))
    (:key-right (setf (rot-y w) (- (rot-y w) 5.0)))
    (:key-up (setf (rot-x w) (+ (rot-x w) 5.0)))
    (:key-down (setf (rot-x w) (- (rot-x w) 5.0))))
  
  (glut:post-redisplay))

(defun setup-RC ()
  (gl:clear-color 0.0 0.0 0.0 0.0)
  (gl:color 0.0 1.0 0.0)
  (gl:enable :depth-test)
  (gl:front-face :ccw)
  (gl:enable :cull-face)
  (gl:shade-model :flat))

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

  (gl:clear :color-buffer :depth-buffer-bit)
  (gl:push-matrix)
  (gl:rotate (rot-x w) 1.0 0.0 0.0)
  (gl:rotate (rot-y w) 0.0 1.0 0.0)
  (gl:color 1 1 1)
  (macrolet ((pv (p)
	       (let* ((point-plist (list :p1 '(0.0 0.0 0.0)
					:p2 '(25.0 0.0 0.0)
					:p3 '(0.0 25.0 0.0)
					:p4 '(25.0 25.0 0.0)
					:p5 '(0.0 25.0 25.0)
					:p6 '(25.0 25.0 25.0)
					:p7 '(0.0 0.0 25.0)
					:p8 '(25.0 0.0 25.0)))
		     (point (getf point-plist p)))
		 `(gl:vertex ,(first point)
			     ,(second point)
			     ,(third point)))))
    (gl:with-primitive :quad-strip
      (pv :p1)
      (pv :p2)
      (pv :p3)
      (pv :p4)
      (gl:color 0 1 0)
      (pv :p5)
      (pv :p6)
      (gl:color 0 0 1)
      (pv :p7)
      (pv :p8)
      (gl:color 1 1 0)
      (pv :p1)
      (pv :p2)
      (gl:color 0 1 1)
      (pv :p2)
      (pv :p8)
      (pv :p4)
      (pv :p6)
      ))
    
    
    
  (watch-var (depth w))
  (watch-var (cull w))
  (watch-var (outline w))
  (gl:pop-matrix))

(defmethod glut:mouse ((w hello-window) button state x y)
  (when (eq button :left-button)
    (if (eq state :down)
	(setf (drag-point w) (cons x y))
	(setf (drag-point w) nil))))
      
  
(defmethod glut:motion ((w hello-window) x y)
  (when (drag-point w)
    (setf (rot-y w) (+ (- (car (drag-point w)) x) (rot-y w)))
    (setf (rot-x w) (+ (- (cdr (drag-point w)) y) (rot-x w)))
    (setf (drag-point w) (cons x y))
    (glut:post-redisplay)))
		       


(defmethod glut:display ((w hello-window))
  (render-scene w)  
  (draw-vars)
  (gl:flush))

(defun rb-hello ()
  (let ((window (make-instance 'hello-window)))
    
    (glut:display-window window)))
  
(rb-hello)

