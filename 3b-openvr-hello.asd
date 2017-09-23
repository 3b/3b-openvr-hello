(defsystem :3b-openvr-hello
  :depends-on (3b-openvr cl-opengl 3bgl-shader glop sb-cga pngload)
  :serial t
  :components ((:file "shaders")
               (:file "hello-common")
               (:file "hello-glop")))
