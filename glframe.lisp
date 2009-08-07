(in-package :gltools)


(defclass frame ()
  ((origin
    :initarg :origin
    :initform (make-array 3 :initial-contents '(0.0 0.0 0.0))
    :accessor origin)
   (forward
    :initarg :forward
    :initform (make-array 3 :initial-contents '(1.0 0.0 0.0))
    :accessor forward)
   (up
    :initarg :up
    :initform (make-array 3 :initial-contents '(0.0 1.0 0.0))
    :accessor up)))



(defgeneric x (object))
(defgeneric (setf x) (x object))


(defmethod x ((a sequence))
  (declare (inline))
  (elt a 0))

(defmethod (setf x) (x (a sequence))
  (declare (inline))
  (setf (elt a 0) x))

(defgeneric y (object))
(defgeneric (setf y) (y object))

(defmethod y ((a sequence))
  (declare (inline))
  (elt a 1))

(defmethod (setf y) (y (a sequence))
  (declare (inline))
  (setf (elt a 1) y))

(defgeneric z (object))
(defgeneric (setf z) (z object))

(defmethod z ((a sequence))
  (declare (inline))
  (elt a 2))

(defmethod (setf z) (z (a sequence))
  (declare (inline))
  (setf (elt a 2) z))
 

(defgeneric set-origin (f point &optional y z))

(defmethod set-origin ((f frame) point &optional y z)
  (with-slots (origin) f
      (if z
	  (progn 
	    (setf (x origin) point)
	    (setf (y origin) y)
	    (setf (z origin) z))
	  (setf origin (copy-seq point))))
  f)


(defgeneric set-forward (f point &optional y z))

(defmethod set-forward ((f frame) point &optional y z)
  (with-slots (forward) f
      (if z
	  (progn 
	    (setf (x forward) point)
	    (setf (y forward) y)
	    (setf (z forward) z))
	  (setf forward (copy-seq point))))
  f)

(defgeneric set-up (f point &optional y z))

(defmethod set-up ((f frame) point &optional y z)
  (with-slots (up) f
      (if z
	  (progn 
	    (setf (x up) point)
	    (setf (y up) y)
	    (setf (z up) z))
	  (setf up (copy-seq point))))
  f)


(defgeneric z-axis (frame))
(defmethod z-axis ((f frame))
  (forward f))

(defgeneric y-axis (frame))
(defmethod y-axis ((f frame))
  (up f))

(defgeneric x-axis (frame))
(defmethod x-axis ((f frame))
  (m3d:cross-product (up f) (forward f)))


(defmacro with-xyz-slots (slots instance &body body)
  `(with-slots ,(map 'list (lambda (x)
			     (if (typep x 'sequence)
				 (first x)
				 x)) slots) ,instance
     (m3d:with-xyzs ,slots
       ,@body)))

(defgeneric translate-world (frame x y z))
(defmethod translate-world ((f frame) x y z)
  (with-xyz-slots (origin) f
    (incf origin.x x)
    (incf origin.y y)
    (incf origin.z z)))

(defgeneric move-forward (frame delta))
(defmethod move-forward ((f frame) delta)
  (with-xyz-slots (origin forward) f
    (incf origin.x (* forward.x delta))
    (incf origin.y (* forward.y delta))
    (incf origin.z (* forward.z delta))))

(defgeneric move-up (frame delta))
(defmethod move-up ((f frame) delta)
  (with-xyz-slots (origin up) f
    (incf origin.x (* up.x delta))
    (incf origin.y (* up.y delta))
    (incf origin.z (* up.z delta))))

(defgeneric move-right (frame delta))
(defmethod move-right ((f frame) delta)
  (let ((cross (m3d:cross-product (slot-value f 'up) 
				  (slot-value f 'forward))))
    (with-xyz-slots (origin) f
      (m3d:with-xyzs (cross)
	(incf origin.x (* cross.x delta))
	(incf origin.y (* cross.y delta))
	(incf origin.z (* cross.z delta))))))
  


(defgeneric translate-local (frame x y z))
(defmethod translate-local ((f frame) x y z)
  (move-forward z)
  (move-up y)
  (move-right x))

(defgeneric get-matrix (frame  &optional rotation-only?))

(defmethod get-matrix ((f frame) &optional rotation-only?)
  (with-slots (up forward origin) f
    (let ((x-axis (m3d:cross-product up forward))
	  (result (make-array 16 :element-type 'float
			      :initial-element 0.0)))
      (m3d:set-matrix-column44 result x-axis 0)
      (setf (elt result 3) 0.0)

      (m3d:set-matrix-column44 result up 1)
      (setf (elt result 7) 0.0)
      
      (m3d:set-matrix-column44 result forward 2)
      (setf (elt result 11) 0.0)

      (if rotation-only?
	  (progn (setf (elt result 12) 0.0)
		 (setf (elt result 13) 0.0)
		 (setf (elt result 14) 0.0))
	  (m3d:set-matrix-column44 result origin 3))
      (setf (elt result 15) 1.0)
      result)))


	  
(defgeneric get-camera-orientation (frame))

(defmethod get-camera-orientation ((f frame))
  (with-slots (forward up) f
    (let* ((z (map 'sequence #'- forward))
	  (x (m3d:cross-product up z))
	  (result (m3d:load-identity44)))
      (m3d:set-matrix-column44 result x 0)
      (m3d:set-matrix-column44 result up 1)
      (m3d:set-matrix-column44 result z 2)
      result)))


(defgeneric apply-camera-transform (frame &optional rot-only?))
(defmethod apply-camera-transform ((f frame) &optional rot-only?)
  (with-xyz-slots (origin) f
    (gl:mult-matrix (get-camera-orientation f))
    (if rot-only?
	(gl:translate (- origin.x)
		      (- origin.y)
		      (- origin.z)))))

(defgeneric apply-actor-transform (frame &optional rot-only?))

(defmethod apply-actor-transform ((f frame) &optional rot-only?)
  (gl:mult-matrix (get-matrix f rot-only?)))

(defgeneric rotate-local-x (frame angle))

(defmethod rotate-local-x ((f frame) angle)
  (declare (float angle))
  (with-slots (up forward) f
    (let ((rot-mat (let ((cross (m3d:cross-product up forward)))
		     (m3d:rotation-matrix44 (m3d:load-identity44)
					    angle
					    (elt cross 0)
					    (elt cross 1)
					    (elt cross 2))))
	  (new-vect (make-array 3 :element-type 'float
				:initial-element 0.0)))
      (m3d:with-xyzs (new-vect up forward)
	(setf new-vect.x (+ (* (elt rot-mat 0) forward.x)
			    (* (elt rot-mat 4) forward.y)
			    (* (elt rot-mat 8) forward.z)))
	(setf new-vect.y (+ (* (elt rot-mat 1) forward.x)
			    (* (elt rot-mat 5) forward.y)
			    (* (elt rot-mat 9) forward.z)))
	(setf new-vect.z (+ (* (elt rot-mat 2) forward.x)
			    (* (elt rot-mat 6) forward.y)
			    (* (elt rot-mat 10) forward.z)))
	(setf forward (copy-seq new-vect))

	(setf new-vect.x (+ (* (elt rot-mat 0) up.x)
			    (* (elt rot-mat 4) up.y)
			    (* (elt rot-mat 8) up.z)))
	(setf new-vect.y (+ (* (elt rot-mat 1) up.x)
			    (* (elt rot-mat 5) up.y)
			    (* (elt rot-mat 9) up.z)))
	(setf new-vect.z (+ (* (elt rot-mat 2) up.x)
			    (* (elt rot-mat 6) up.y)
			    (* (elt rot-mat 10) up.z)))
	(setf up (copy-seq new-vect))
	nil))))

(defgeneric rotate-local-y (frame angle))

(defmethod rotate-local-y ((f frame) angle)
  (declare (float angle))
  (with-xyz-slots (up forward) f
    (let ((rot-mat (m3d:rotation-matrix44 (m3d:load-identity44)
					    angle
					    up.x up.y up.z))
	  (new-vect (make-array 3 :element-type 'float
				:initial-element 0.0)))
      (m3d:with-xyzs (new-vect rot-mat)
	(setf new-vect.x (+ (* rot-mat.xx forward.x)
			    (* rot-mat.xy forward.y)
			    (* rot-mat.xz forward.z)))
	(setf new-vect.y (+ (* rot-mat.yx forward.x)
			    (* rot-mat.yy forward.y)
			    (* rot-mat.yz forward.z)))
	(setf new-vect.z (+ (* rot-mat.zx forward.x)
			    (* rot-mat.zy forward.y)
			    (* rot-mat.zz forward.z)))
	(setf forward (copy-seq new-vect))
	nil))))

(defgeneric rotate-local-z (frame angle))

(defmethod rotate-local-z ((f frame) angle)
  (declare (float angle))
  (with-xyz-slots (up forward) f
    (let ((rot-mat (m3d:rotation-matrix44 (m3d:load-identity44)
					    angle
					    forward.x forward.y forward.z))
	  (new-vect (make-array 3 :element-type 'float
				:initial-element 0.0)))
      (m3d:with-xyzs (new-vect rot-mat)
	(setf new-vect.x (+ (* rot-mat.xx up.x)
			    (* rot-mat.xy up.y)
			    (* rot-mat.xz up.z)))
	(setf new-vect.y (+ (* rot-mat.yx up.x)
			    (* rot-mat.yy up.y)
			    (* rot-mat.yz up.z)))
	(setf new-vect.z (+ (* rot-mat.zx up.x)
			    (* rot-mat.zy up.y)
			    (* rot-mat.zz up.z)))
	
	(setf up (copy-seq new-vect))
	nil))))



(defgeneric normalize (frame))

(defmethod normalize ((f frame))
  (with-slots (up forward) f
    (let ((cross (m3d:cross-product up forward)))
      (m3d:cross-product cross up :result forward)
      (m3d:normalize-vector up)
      (m3d:normalize-vector forward)
      nil)))


(defgeneric rotate-world (frame angle x y z))
(defmethod rotate-world ((f frame) angle x y z)
  (declare (float angle))
  (with-slots (up forward) f
    (let ((rot-mat (m3d:rotation-matrix44 (m3d:load-identity44) angle x y z))
	  (new-vect (make-array 3 :element-type 'float
				:initial-element 0.0)))
      (m3d:with-xyzs (new-vect up forward rot-mat) ;; left off here

	(setf new-vect.x (+ (* rot-mat.xx up.x)
			    (* rot-mat.xy up.y)
			    (* rot-mat.xz up.z)))
	(setf new-vect.y (+ (* rot-mat.yx up.x)
			    (* rot-mat.yy up.y)
			    (* rot-mat.yz up.z)))
	(setf new-vect.z (+ (* rot-mat.zx up.x)
			    (* rot-mat.zy up.y)
			    (* rot-mat.zz up.z)))
	(setf up (copy-seq new-vect))

	(setf new-vect.x (+ (* rot-mat.xx forward.x)
			    (* rot-mat.xy forward.y)
			    (* rot-mat.xz forward.z)))
	(setf new-vect.y (+ (* rot-mat.yx forward.x)
			    (* rot-mat.yy forward.y)
			    (* rot-mat.yz forward.z)))
	(setf new-vect.z (+ (* rot-mat.zx forward.x)
			    (* rot-mat.zy forward.y)
			    (* rot-mat.zz forward.z)))
	(setf forward (copy-seq new-vect))
	nil))))


(defgeneric rotate-local (frame angle x y z))
(defmethod rotate-local ((f frame) angle x y z)
  (declare (single-float angle x y z))
  (let* ((local-vect (m3d:load-vector x y z))
	 (world-vect (local-to-world f local-vect)))
    
    (m3d:with-xyzs ((world-vect 3))
      (rotate-world f angle world-vect.x world-vect.y world-vect.z))))


(defgeneric local-to-world (frame local))
(defmethod local-to-world ((f frame) local)
  (let ((rot-mat (get-matrix f t))
	(world (make-array 3)))
    (with-xyz-slots ((origin 3)) f
	(m3d:with-xyzs ((world 3) (local 3) (rot-mat 44))
	  (setf world.x (+ (* rot-mat.xx local.x) 
			   (* rot-mat.xy local.y)
			   (* rot-mat.xz local.z)))
	  (setf world.y (+ (* rot-mat.yx local.x)
			   (* rot-mat.yy local.y)
			   (* rot-mat.yz local.z)))
	  (setf world.z (+ (* rot-mat.zx local.x)
			   (* rot-mat.zy local.y)
			   (* rot-mat.zz local.z)))
	  
	  (incf world.x origin.x)
	  (incf world.y origin.y)
	  (incf world.z origin.z)
	  world))))

(defgeneric world-to-local (frame world))
(defmethod world-to-local ((f frame) world)
  (let ((new-world (make-array 3))
	(local (make-array 3))
	(inv-mat (m3d:invert-matrix44 (get-matrix f t))))
    (with-xyz-slots ((origin 3)) f
      (m3d:with-xyzs ((new-world 3) (world 3) (inv-mat 44) (local 3))
	(setf new-world.x (- world.x origin.x))
	(setf new-world.y (- world.y origin.y))
	(setf new-world.z (- world.z origin.z))
	
	(setf local.x (+ (* inv-mat.xx new-world.x) 
			 (* inv-mat.xy new-world.y)
			 (* inv-mat.xz new-world.z)))
	(setf local.y (+ (* inv-mat.yx new-world.x)
			 (* inv-mat.yy new-world.y)
			 (* inv-mat.yz new-world.z)))
	(setf local.z (+ (* inv-mat.zx new-world.x)
			 (* inv-mat.zy new-world.y)
			 (* inv-mat.zz new-world.z)))
	local))))
	
(defgeneric transform-point (frame src))
(defmethod transform-point ((f frame) src)
  (let ((m (get-matrix f nil))
	(dest (make-array 3)))
    (m3d:with-xyzs ((src 3) (dest 3) (m 44))
      (setf dest.x (+ (* m.xx src.x) 
		      (* m.xy src.y)
		      (* m.xz src.z)))
      (setf dest.y (+ (* m.yx src.x) 
		      (* m.yy src.y)
		      (* m.yz src.z)))
      (setf dest.z (+ (* m.zx src.x) 
		      (* m.zy src.y)
		      (* m.zz src.z)))
      dest)))
		      
(defgeneric rotate-vector (frame src))
(defmethod rotate-vector ((f frame) src)
  (let ((m (get-matrix f t))
	(dest (make-array 3)))
    (m3d:with-xyzs ((src 3) (dest 3) (m 44))
      (setf dest.x (+ (* m.xx src.x) 
		      (* m.xy src.y)
		      (* m.xz src.z)))
      (setf dest.y (+ (* m.yx src.x) 
		      (* m.yy src.y)
		      (* m.yz src.z)))
      (setf dest.z (+ (* m.zx src.x) 
		      (* m.zy src.y)
		      (* m.zz src.z)))
      dest)))

  