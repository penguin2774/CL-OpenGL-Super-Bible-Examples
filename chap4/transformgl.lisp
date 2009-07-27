(defpackage :opengl-test
  (:use :cl))

(load "../m3d.lisp")

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
   (zfar
    :initform 400.0
    :accessor zfar)
   (drag-point
    :initform nil
    :accessor drag-point)
   (torus-rot-y
    :initform 0.0
    :accessor torus-rot-y))
  (:default-initargs :pos-x 100 :pos-y 100 :width 800 :height 600
                     :mode '(:double :rgb  :depth) :title "So nice a window, I though you might enjoy it!"))




(defconstant +n-range+ 100.0)
(defparameter *watched-vars* (make-hash-table :test 'eq))

(defmacro watch-var (&rest vars)
  `(progn 
     ,@(loop for var in vars
	   collect `(setf (gethash ',var *watched-vars*) ,var))))

;;(cffi:defcallback handle-menu :void ((value :int))
;;  nil)





(defun draw-vars (window-height)
  (gl:color 0 1 0)
  (loop for k being the hash-keys in *watched-vars*
     for line = 0 then (1+ line)
     do 
       (gl:window-pos 0 (- window-height 10 (* line 12)))
       (glut:bitmap-string glut:+bitmap-8-by-13+ (format nil "~a : ~a" k (gethash k *watched-vars*)))))



      


(defparameter x-rot 0.0)
(defparameter y-rot 0.0)

(defmethod glut:keyboard ((w hello-window) key x y)
  (case key
    (#\+ (incf (zfar w) 10)
	 (glut:reshape w (glut:width w) (glut:height w)))
    (#\- (decf (zfar w) 10)
	 (glut:reshape w (glut:width w) (glut:height w)))
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

(defmethod glut:reshape ((window hello-window) w h)
  (let ((ratio (float (/ w h))))
    (setf (glut:width window) w) ;; glut doesn't do this for us..?
    (setf (glut:height window) h)
    (when (zerop h)
      (setf h 1))
    (gl:viewport 0 0 w h)
    (gl:matrix-mode :projection)
    (gl:load-identity)
    (glu:perspective 80.0 ratio 1.0 400.0)
    (gl:matrix-mode :modelview)
    (gl:load-identity)))

(defmethod glut:display-window :before ((w hello-window))
  (glut:enable-tick w 33)
  ;; Select clearing color.
  (setup-RC))
  ;;(gl:clear-color 0 0 0 0)
  ;; Initialize viewing values.
  ;;(glut:reshape w (glut:width w) (glut:height w)))

(defvar light-pos '(0.0 0.0 0.0 1.0))

(defun setup-RC ()
  (gl:enable :depth-test)
  (gl:front-face :ccw)
  (gl:enable :cull-face)
  
  (gl:clear-color 0.0 0.0 0.5 1.0)
  (%gl:polygon-mode :front-and-back :line))

(defgeneric render-scene (w))


(defmethod render-scene ((w hello-window))
  (incf (torus-rot-y w) 0.5)
  (watch-var (torus-rot-y w))
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (gl:with-pushed-matrix 
    (let ((m (m3d:rotation-matrix44 (m3d:load-identity44) (m3d:deg-to-rad (torus-rot-y w)) 0.0 1.0 0.0)))
      (setf (elt m 12) 0.0)
      (setf (elt m 13) 0.0)
      (setf (elt m 14) -2.5)
      (gl:load-matrix m)
      (glt:draw-torus 0.35  0.15 40.0 20.0)))
    
    (glut:swap-buffers))

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
  (draw-vars (glut:height w))

  (glut:swap-buffers))

(defmethod glut:tick ((w hello-window))
  
  (glut:post-redisplay))

(defun rb-hello ()
  (unwind-protect  
       (let ((window (make-instance 'hello-window)))
	 (glut:display-window window))
    (glut:destroy-current-window)))
  
;;(rb-hello)


