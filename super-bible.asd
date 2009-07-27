

(defsystem super-bible
  :depends-on (cl-opengl cl-glut cl-glu)
  :components
   ((:file "m3d")
   (:file "gltools" :depends-on ("m3d"))))