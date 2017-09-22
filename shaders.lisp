(defpackage 3b-openvr-hello/shaders
  (:use :3bgl-glsl/cl)
  (:export #:scene-vertex #:scene-fragment
           #:controller-transform-vertex #:controller-transform-fragment
           #:render-model-vertex #:render-model-fragment
           #:companion-window-vertex #:companion-window-fragment))
(in-package 3b-openvr-hello/shaders)

(uniform matrix :mat4)

(input position :vec4 :location 0)
(input uv-coords-in :vec2 :location 1)
(input normal-in :vec3 :location 2)

(output uv-coords :vec2)

(defun scene-vertex ()
  (setf uv-coords uv-coords-in)
  (setf gl-position (* matrix position)))

(uniform my-texture :sampler-2d)
(output output-color :vec4)

(defun scene-fragment ()
  (setf output-color (texture my-texture uv-coords)))


(input color-in :vec3 :location 1)
(output color :vec4)

(defun controller-transform-vertex ()
  (setf (.xyz color) color-in
        (.a color) 1)
  (setf gl-position (* matrix position)))

(input color :vec4)

(defun controller-transform-fragment ()
  (setf output-color color))


(uniform diffuse :sampler-2d)
(defun render-model-vertex ()
  (setf uv-coords uv-coords-in
        gl-position (* matrix (vec4 (.xyz position) 1))))

(defun render-model-fragment ()
  (setf output-color (texture diffuse uv-coords)))

(output uv :vec2 :qualifiers (:noperspective))

(defun companion-window-vertex ()
  (setf uv uv-coords-in
        gl-position position))
(input uv :vec2 :qualifiers (:noperspective))
(defun companion-window-fragment ()
  (setf output-color (texture my-texture uv)))
