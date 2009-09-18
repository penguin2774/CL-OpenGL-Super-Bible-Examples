(defpackage :rielib
  (:use :cl)
  (:export image
	   texture
	   scale
	   rotation
	   rotate
	   width
	   height
	   texture-rect
	   location
	   set-center
	   sub-images
	   load-texture
	   render
	   add-subimage))


(in-package :rielib)

(defclass 4matrix ()
  ((x1
    :accessor x1
    :initarg :x1
    :initform 0.0)
   (y1 
    :accessor y1
    :initarg :y1
    :initform 0.0)
   (x2
    :accessor x2
    :initarg :x2
    :initform 0.0)
   (y2
    :accessor y2
    :initarg :y2
    :initform 0.0)))

(defclass point ()
  ((x
   :accessor x
   :initform 0.0
   :initarg :x)
  (y
   :accessor y
   :initform 0.0
   :initarg :y)))

(defclass texture-matrix (4matrix) 
  ((x2 :initform 1.0)
   (y2 :initform 1.0)))

(defclass quad-points (4matrix) ())
    

(defclass image ()
  ((texture
    :reader texture)
   (scale
    :accessor scale
    :initform 1.0)
   (rotation
    :reader rotation
    :initform 0.0)
   (width 
    :accessor width)
   (height
    :accessor height)
   (texture-rect
    :reader texture-rect
    :initform (make-instance 'texture-matrix))
   (quad-points
    :reader quad-points
    :initform (make-instance 'quad-points))
    
   (location
    :accessor location
    :initform (make-instance 'point))
   (sub-images
    :reader sub-images
    :initform (list))))

    

(defun convert-image-for-gl (surface)
  (let ((result (sdl:create-surface (sdl:width surface) (sdl:height surface)
				    :bpp 32 :pixel-alpha t)))
    (sdl:blit-surface surface result)
    
    result))

(defgeneric set-center (image w-ratio h-ratio))
(defmethod set-center ((self image) w-ratio h-ratio)
  (with-slots (quad-points width height) self
    (with-slots (x1 y1 x2 y2) quad-points
      (setf x1 (- (* width w-ratio) width))
      (setf x2 (* width w-ratio))
      (setf y1 (- (* height h-ratio) height))
      (setf y2 (* height h-ratio)))))
  

(defgeneric load-texture (image surface))

(defmethod load-texture ((image image) surface)
  (sdl:with-surface (converted-surface (convert-image-for-gl surface))
    (let ((texture (first (gl:gen-textures 1))))
	
      (gl:bind-texture :texture-2d texture)
      (gl:tex-parameter :texture-2d :texture-min-filter :linear)
      (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
      (gl:tex-image-2d :texture-2d 0
		       :rgba (sdl:width surface) (sdl:height surface)
		       0 :rgba :unsigned-byte (sdl-base::with-pixel (pixels (sdl::fp converted-surface))
		       (sdl-base::pixel-data pixels)))
      (setf (width image) (sdl:width surface))
      (setf (height image) (sdl:height surface))
      (set-center image  0.5 0.5)
      (setf (slot-value image 'texture) texture))))

(defgeneric render (image))

(defmethod render ((image image))
  (with-slots (texture scale rotation texture-rect quad-points location sub-images width height) image 
    
    (gl:with-pushed-matrix
      
      (gl:translate (first location) (second location) (or (third location) 0.0))
      
      (if rotation 
	  (gl:rotate rotation 0.0 0.0 1.0))

      (if scale
	  (if (listp scale)
	      (gl:scale (first scale) (second scale) (or (third scale) 1.0))
	      (gl:scale scale scale 1.0)))
      
      (with-slots ((tr-x1 x1) (tr-y1 y1) (tr-x2 x2) (tr-y2 y2)) texture-rect
	(with-slots ((qp-x1 x1) (qp-y1 y1) (qp-x2 x2) (qp-y2 y2)) quad-points
	  (gl:bind-texture :texture-2d texture)
	  (gl:with-primitive :quads

	    (gl:tex-coord tr-x1 tr-y1)
	    (gl:vertex qp-x1 qp-y1)
;;	    (gl:vertex 0 0)
	    (gl:tex-coord tr-x2 tr-y1)
	    (gl:vertex qp-x2 qp-y1)
;;	    (gl:vertex width 0)
	
	    (gl:tex-coord tr-x2 tr-y2)
	    (gl:vertex qp-x2 qp-y2)
;;	    (gl:vertex width height)
	
	    (gl:tex-coord tr-x1 tr-y2)
	    (gl:vertex qp-x1 qp-y2)
;;	    (gl:vertex 0 height)
	    )))
	(loop for i in sub-images
	   do (render i)))))

(defgeneric add-subimage (image sub-image))

(defmethod add-subimage ((image image) sub-image)
  (push sub-image (slot-value image 'sub-images)))

(defgeneric rotate (image degrees))

(defmethod rotate ((image image) degrees)
  (incf (slot-value image 'rotation) degrees))