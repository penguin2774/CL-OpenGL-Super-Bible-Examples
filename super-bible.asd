;;;
;;; An ASDF class for all the translated shared/ libraries.
;;;
;;; Copyright (c) 2009 Nathanael Cunningham
;;; See LICENSE for full licensing details.
;;;


(defsystem super-bible
  :depends-on (cl-opengl cl-glut cl-glu lispbuilder-sdl lispbuilder-sdl-image)
  :components
   ((:file "math3d")
    (:file "gltools" :depends-on ("math3d"))
    (:file "glframe" :depends-on ("math3d"))))