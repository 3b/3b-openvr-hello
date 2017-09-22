(defsystem :3b-openvr-hello
  :depends-on (3b-openvr cl-opengl glop sb-cga pngload)
  :serial t
  :components ((:file "shaders.lisp")
               (:file "hello-common.lisp")
               (:file "hello-glop.lisp")))
