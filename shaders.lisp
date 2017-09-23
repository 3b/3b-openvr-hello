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

(output uv-coords :vec2 :stage :vertex)
(input uv-coords :vec2 :stage :fragment)

(defun scene-vertex ()
  (setf uv-coords uv-coords-in)
  (setf gl-position (* matrix position)))

(uniform my-texture :sampler-2d)
(output output-color :vec4)

(defun scene-fragment ()
  (setf output-color (texture my-texture uv-coords)))


(input color-in :vec4 :location 1)

(defun controller-transform-vertex ()
  (setf (.xyz output-color) (.xyz color-in)
        (.a output-color) 1)
  (setf gl-position (* matrix position)))

(input color-in :vec4)

(defun controller-transform-fragment ()
  (setf output-color color-in))


(uniform diffuse :sampler-2d)
(defun render-model-vertex ()
  (setf uv-coords uv-coords-in
        gl-position (* matrix (vec4 (.xyz position) 1))))

(defun render-model-fragment ()
  (setf output-color (texture diffuse uv-coords)))

(output uv :vec2 :stage :vertex ;:qualifiers (:noperspective)
        )

(input position2 :vec2 :location 0)
(defun companion-window-vertex ()
  (setf uv uv-coords-in
        gl-position (vec4 position2 0 1)))
(input uv :vec2 :stage :fragment; :qualifiers (:noperspective)
       )
(defun companion-window-fragment ()
  (setf output-color (texture my-texture uv)))

#++
(progn
  (3bgl-shaders:generate-stage :vertex 'scene-vertex)
  (3bgl-shaders:generate-stage :fragment 'scene-fragment)
  (3bgl-shaders:generate-stage :vertex 'render-model-vertex)
  (3bgl-shaders:generate-stage :fragment 'render-model-fragment)
  (3bgl-shaders:generate-stage :vertex 'companion-window-vertex)
  (3bgl-shaders:generate-stage :fragment 'companion-window-fragment)
  (3bgl-shaders:generate-stage :vertex 'controller-transform-vertex)
  (3bgl-shaders:generate-stage :fragment 'controller-transform-fragment))
