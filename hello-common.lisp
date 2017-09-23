(defpackage 3b-openvr-hello
  (:use :cl)
  (:local-nicknames (:vr :3b-openvr)))
(in-package 3b-openvr-hello)

(defmacro with-accessors* ((&rest accessors) object &body body)
  `(with-accessors (,@ (loop for a in accessors
                             collect (if (consp a) a (list a a))))
       ,object
     ,@body))

(defclass render-model ()
  ((vert-buffer :accessor vert-buffer :initform 0)
   (index-buffer :accessor index-buffer :initform 0)
   (vert-array :accessor vert-array :initform 0)
   (texture :accessor texture :initform 0)
   (vertex-count :accessor vertex-count :initform 0)
   (model-name :accessor model-name :initform nil :initarg :name)))

(defparameter *bprintf* t)
(defmacro dprintf (format &rest r)
  `(when *bprintf* (format t ,format ,@r)))

(defclass main ()
  ((debug-opengl :accessor debug-opengl :initform nil :initarg :gldebug)
   (verbose :accessor verbose :initform nil :initarg :verbose)
   (perf :accessor perf :initform nil)
   (vblank :accessor vblank :initform nil :initarg :vblank)
   (gl-finish-hack :accessor gl-finish-hack :initform t
                   :initarg :gl-finish-hack)
   ;; m_pHMD = vr::*system*
   ;; m_pRenderModels = vr::*render-models*
   (driver :accessor driver :initform "No Driver")
   (display :accessor display :initform "No Device")
   (tracked-device-pose :reader tracked-device-pose
                        :initform (make-array vr::+max-tracked-device-count+
                                              :initial-element nil))
   (device-pose :reader device-pose
                :initform (make-array vr::+max-tracked-device-count+
                                      :initial-element nil))
   (show-tracked-device :reader show-tracked-device
                        :initform (make-array vr::+max-tracked-device-count+
                                              :initial-element nil))
   (companion-window :accessor companion-window :initform nil)
   (companion-window-width :accessor companion-window-width :initform 640)
   (companion-window-height :accessor companion-window-height :initform 320)
   (context :accessor context :initform nil)
   (tracked-controller-count :accessor tracked-controller-count :initform 0)
   (tracked-controller-count-last :accessor tracked-controller-count-last
                                  :initform -1)
   (valid-pose-count :accessor valid-pose-count :initform 0)
   (valid-pose-count-last :accessor valid-pose-count-last)
   (show-cubes :accessor show-cubes :initform t)
   ;; what classes we saw poses for this frame
   (pose-classes :accessor pose-classes :initform "")
   ;; for each device, a character representing its class
   (dev-class-char :accessor dev-class-char
                   :initform (make-array vr::+max-tracked-device-count+
                                         :initial-element nil))
   (scene-volume-width :accessor scene-volume-width)
   (scene-volume-height :accessor scene-volume-height)
   (scene-volume-depth :accessor scene-volume-depth)
   (scale-spacing :accessor scale-spacing)
   (scale :accessor scale)
   ;; if you want something other than the default 20x20x20
   (scene-volume-init :accessor scene-volume-init :initform 20
                      :initarg :cube-volume)
   (near-clip :accessor near-clip)
   (far-clip :accessor far-clip)
   (texture :accessor texture)
   (vertex-count :accessor vertex-count)
   (scene-vert-buffer :accessor scene-vert-buffer)
   (scene-vao :accessor scene-vao :initform 0)
   (companion-window-vao :accessor companion-window-vao)
   (companion-window-id-vert-buffer :accessor companion-window-id-vert-buffer)
   (companion-window-id-index-buffer :accessor companion-window-id-index-buffer)
   (companion-window-index-size :accessor companion-window-index-size)
   (controller-vert-buffer :accessor controller-vert-buffer :initform 0)
   (controller-vao :accessor controller-vao :initform 0)
   (controller-vert-count :accessor controller-vert-count)
   (hmd-pose :accessor hmd-pose :initform (sb-cga:identity-matrix))
   (eye-pos-left :accessor eye-pos-left)
   (eye-pos-right :accessor eye-pos-right)
   (projection-center :accessor projection-center)
   (projection-left :accessor projection-left)
   (projection-right :accessor projection-right)
   (scene-program-id :accessor scene-program-id :initform 0)
   (companion-window-program-id :accessor companion-window-program-id
                                :initform 0)
   (controller-transform-program-id :accessor controller-transform-program-id
                                    :initform 0)
   (render-model-program-id :accessor render-model-program-id
                            :initform 0)
   (scene-matrix-location :accessor scene-matrix-location :initform -1)
   (controller-matrix-location :accessor controller-matrix-location
                               :initform -1)
   (render-model-matrix-location :accessor render-model-matrix-location
                                 :initform -1)
   (scene-texture-location :accessor scene-texture-location :initform -1)
   (render-model-texture-location :accessor render-model-texture-location :initform -1)
   (companion-texture-location :accessor companion-texture-location :initform -1)
   (left-eye-desc :accessor left-eye-desc)
   (right-eye-desc :accessor right-eye-desc)
   (render-width :accessor render-width)
   (render-height :accessor render-height)
   (render-models :accessor render-models
                  :initform (make-hash-table :test 'equal)
                  #++(make-array 16 :fill-pointer 0 :adjustable t))
   (tracked-device-to-render-model :reader tracked-device-to-render-model
                                   :initform (make-array
                                              vr::+max-tracked-device-count+))))

;; subclasses should implement these ...
(defmethod (setf window-title) (title (o main)))
(defmethod swap-window ((o main)))

(defmethod init ((o main))
  ;; assumes vr and gl are initialized already, and companion window
  ;; is open, swap interval is set
  (setf (driver o) (or (vr::get-tracked-device-property
                        vr::+tracked-device-index-hmd+
                        :tracking-system-name-string)))
  (setf (display o) (or (vr::get-tracked-device-property
                         vr::+tracked-device-index-hmd+
                         :serial-number-string)))
  (setf (window-title (companion-window o))
        (format nil "hellovr - ~a ~a" (driver o) (display o)))
  (setf (scene-volume-width o) (scene-volume-init o))
  (setf (scene-volume-height o) (scene-volume-init o))
  (setf (scene-volume-depth o) (scene-volume-init o))

  (setf (scale o) 0.3)
  (setf (scale-spacing o) 4.0)

  (setf (near-clip o) 0.1)
  (setf (far-clip o) 30.0)

  (setf (texture o) 0)
  (setf (vertex-count o) 0)

  (init-gl o)
  (init-compositor o))

(cffi:defcallback debug-callback :void ((source %gl:enum)
                                        (type %gl:enum)
                                        (severity %gl:enum)
                                        (length %gl:sizei)
                                        (message :string)
                                        (user-param (:pointer :void)))
  (declare (ignorable source type severity length user-param))
  (dprintf "GL Error: ~a~%" message))

(defmethod init-gl ((o main))
  (when (debug-opengl o)
    (%gl:debug-message-callback (cffi:get-callback 'debug-callback)
                                (cffi:null-pointer))
    (%gl:debug-message-control :dont-care :dont-care :dont-care
                               0 (cffi:null-pointer) t)
    (gl:enable :debug-output-synchronous))

  (create-all-shaders o)

  (setup-texturemaps o)
  (setup-scene o)
  (setup-cameras o)
  (setup-stereo-render-targets o)
  (setup-companion-window o)
  (setup-render-models o))


(defmethod init-compositor ((o main))
  (vr::vr-compositor))

(defmethod shutdown ((o main))
  (map nil 'cleanup (alexandria:hash-table-values (render-models o)))
  (clrhash (render-models o))

  (when (context o)
    (gl:delete-buffers (list (shiftf (scene-vert-buffer o) nil)))
    (macrolet ((del (slot)
                 `(when (,slot o)
                    (gl:delete-program (shiftf (,slot o) nil)))))
      (del scene-program-id)
      (del controller-transform-program-id)
      (del render-model-program-id)
      (del companion-window-program-id))
    (macrolet ((del (f slot)
                 `(,f (list (shiftf (getf d ',slot) nil)))))
      (loop for d in (list (left-eye-desc o)
                           (right-eye-desc o))
            do (del gl:delete-renderbuffers depth-buffer-id)
               (del gl:delete-textures render-texture-id)
               (del gl:delete-framebuffers render-framebuffer-id)
               (del gl:delete-textures resolve-texture-id)
               (del gl:delete-framebuffers resolve-framebuffer-id)))
    (macrolet ((del (slot)
                 `(when (,slot o)
                    (gl:delete-vertex-arrays (list (shiftf (,slot o) nil))))))
      (del companion-window-vao)
      (del scene-vao)
      (del controller-vao))

    (when (debug-opengl o)
      (%gl:debug-message-control :dont-care :dont-care :dont-care
                                 0 (cffi:null-pointer) nil)
      (%gl:debug-message-callback (cffi:null-pointer) (cffi:null-pointer)))))


(defmethod handle-input ((o main))
  ;; process SteamVR events
  (loop for ev = (vr::poll-next-event)
        while ev
        do (process-vr-event o ev))

  ;; process SteamVR controller state
  (loop for device below vr::+max-tracked-device-count+
        for state = (vr::get-controller-state device)
        when state
          do (setf (aref (show-tracked-device o) device)
                   (zerop (getf state 'vr::button-pressed)))))

(defmethod process-vr-event ((o main) event)
  (let ((index (getf event :tracked-device-index)))
    (case (getf event :event-type)
      (:tracked-device-activated
       (setup-render-model-for-tracked-device o index)
       (dprintf "Device ~d attached. Setting up render model~%" index))
      (:tracked-device-deactivated
       (dprintf "Device ~d detached.~%" index))
      (:tracked-device-updated
       (dprintf "Device ~d updated.~%" index)))))

(defmethod render-frame ((o main))
  ;; for now as fast as possible
  (setf *w* o)
  (when vr::*system*
    (render-controller-axes o)
    (render-stereo-targets o)
    (render-companion-window o)

    (vr::submit :left (list 'vr::handle (getf (left-eye-desc o)
                                              'resolve-texture-id)
                            'vr::type :open-gl
                            'vr::color-space :gamma))
    (vr::submit :right (list 'vr::handle (getf (right-eye-desc o)
                                               'resolve-texture-id)
                             'vr::type :open-gl
                             'vr::color-space :gamma)))

  (when (and (vblank o) (gl-finish-hack o))
    (gl:finish))

  ;; SwapWindow
  (swap-window (companion-window o))

  ;; Clear
  (gl:clear-color 0 0 0 1)
  (gl:clear :color-buffer :depth-buffer)

  ;; flush and wait for swap
  (when (vblank o)
    (gl:flush)
    (gl:finish))

  ;; Spew out the controller and pose count whenever they change.
  (unless (eql (tracked-controller-count o) (tracked-controller-count-last o))
    (setf (valid-pose-count-last o) (valid-pose-count o)
          (tracked-controller-count-last o) (tracked-controller-count o))
    (dprintf "pose count: ~a(~a) Controllers:~a~%"
             (valid-pose-count o) (pose-classes o)
             (tracked-controller-count o)))
  (update-hmd-matrix-pose o))

(defun compile-gl-shader (name vertex-shader fragment-shader)
  (let ((program-id (gl:create-program)))
    (flet ((shader (stage entry)
             (let ((shader (gl:create-shader stage)))
               (gl:shader-source shader (3bgl-shaders:generate-stage
                                         stage entry :version 410))
               (gl:compile-shader shader)
               (unless (gl:get-shader shader :compile-status)
                 (dprintf "~a - Unable to compile ~a ~a!~%~a~%"
                          name stage shader
                          (gl:get-shader-info-log shader))
                 (gl:delete-program program-id)
                 (gl:delete-shader shader)
                 (return-from compile-gl-shader 0))
               (gl:attach-shader program-id shader)
               (gl:delete-shader shader))))
      (shader :vertex-shader vertex-shader)
      (shader :fragment-shader fragment-shader))
    (gl:link-program program-id)
    (unless (gl:get-program program-id :link-status)
      (dprintf "~a - Error linking program ~a~%~a~%" name program-id
               (gl:get-program-info-log program-id))
      (gl:delete-program program-id)
      (return-from compile-gl-shader 0))
    (gl:use-program program-id)
    (gl:use-program 0)
    program-id))

(defmethod create-all-shaders ((o main))
  (setf (scene-program-id o)
        (compile-gl-shader "Scene"
                           '3b-openvr-hello/shaders:scene-vertex
                           '3b-openvr-hello/shaders:scene-fragment))
  (setf (scene-matrix-location o)
        (gl:get-uniform-location (scene-program-id o) "matrix"))
  (setf (scene-texture-location o)
        (gl:get-uniform-location (scene-program-id o) "myTexture"))
  (when (= -1 (scene-matrix-location o))
    (dprintf "unable to find matrix unifomr for scene shader~%")
    (return-from create-all-shaders nil))

  (setf (controller-transform-program-id o)
        (compile-gl-shader
         "Controller"
         '3b-openvr-hello/shaders:controller-transform-vertex
         '3b-openvr-hello/shaders:controller-transform-fragment))
  (setf (controller-matrix-location o)
        (gl:get-uniform-location (controller-transform-program-id o) "matrix"))
  (when (= -1 (controller-matrix-location o))
    (dprintf "unable to find matrix unifomr for controller transform shader~%")
    (return-from create-all-shaders nil))

  (setf (render-model-program-id o)
        (compile-gl-shader
         "render model"
         '3b-openvr-hello/shaders:render-model-vertex
         '3b-openvr-hello/shaders:render-model-fragment))

  (setf (render-model-matrix-location o)
        (gl:get-uniform-location (render-model-program-id o) "matrix"))
  (setf (render-model-texture-location o)
        (gl:get-uniform-location (render-model-program-id o) "diffuse"))
  (when (= -1 (render-model-matrix-location o))
    (dprintf "unable to find matrix unifomr for render model shader~%")
    (return-from create-all-shaders nil))


  (setf (companion-window-program-id o)
        (compile-gl-shader
         "Companion Window"
         '3b-openvr-hello/shaders:companion-window-vertex
         '3b-openvr-hello/shaders:companion-window-fragment))
  (gl:use-program (companion-window-program-id o))
  (setf (companion-texture-location o)
        (gl:get-uniform-location (companion-window-program-id o) "myTexture"))
  (and (not (zerop (scene-program-id o)))
       (not (zerop (controller-transform-program-id o)))
       (not (zerop (render-model-program-id o)))
       (not (zerop (companion-window-program-id o)))))


(defmethod setup-texturemaps ((o main))
  (let ((path (asdf:system-relative-pathname '3b-openvr-hello "cube_texture.png")))
    (pngload:with-png-in-static-vector (png path :flip-y t)
      (setf (texture o) (gl:gen-texture))
      (gl:bind-texture :texture-2d (texture o))

      (gl:tex-image-2d :texture-2d 0 :rgba
                       (pngload:width png) (pngload:height png)
                       0 :rgb :unsigned-byte
                       (static-vectors:static-vector-pointer
                        (pngload:data png)))

      (gl:generate-mipmap :texture-2d)

      (gl:tex-parameter :texture-2d :texture-wrap-s :clamp-to-edge)
      (gl:tex-parameter :texture-2d :texture-wrap-t :clamp-to-edge)
      (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
      (gl:tex-parameter :texture-2d :texture-min-filter :linear-mipmap-linear)

      (gl:tex-parameter :texture-2d :texture-max-anisotropy-ext
                        (gl:get* :max-texture-max-anisotropy-ext))

      (gl:bind-texture :texture-2d 0)

      (not (zerop (texture o))))))

(defmethod setup-scene ((o main))
  (unless vr::*system*
    (return-from setup-scene))

  (let* ((vert-data-array (make-array 1024 :element-type 'single-float
                                           :adjustable t :fill-pointer 0))
         (scale (sb-cga:scale* (scale o) (scale o) (scale o)))
         (transform (sb-cga:translate* (/ (- (* (scene-volume-width o)
                                                (scale-spacing o)))
                                          2.0)
                                       (/ (- (* (scene-volume-height o)
                                                (scale-spacing o)))
                                          2.0)
                                       (/ (- (* (scene-volume-depth o)
                                                (scale-spacing o)))
                                          2.0)))
         (mat (sb-cga:matrix* scale transform)))
    (loop for z below (scene-volume-depth o)
          do (loop for y below (scene-volume-height o)
                   do (loop for x below (scene-volume-width o)
                            do (add-cube-to-scene mat vert-data-array)
                               (setf mat (sb-cga:matrix* mat
                                                         (sb-cga:translate*
                                                          (scale-spacing o)
                                                          0.0 0.0))))
                      (setf mat
                            (sb-cga:matrix* mat
                                            (sb-cga:translate*
                                             (- (* (scene-volume-width o)
                                                   (scale-spacing o)))
                                             (scale-spacing o)
                                             0.0))))
             (setf mat (sb-cga:matrix* mat
                                       (sb-cga:translate*
                                        0.0
                                        (- (* (scene-volume-height o)
                                              (scale-spacing o)))
                                        (scale-spacing o)))))

    (setf (vertex-count o) (/ (length vert-data-array) 5))

    (setf (scene-vao o) (gl:gen-vertex-array))
    (gl:bind-vertex-array (scene-vao o))

    (setf (scene-vert-buffer o) (gl:gen-buffer))
    (gl:bind-buffer :array-buffer (scene-vert-buffer o))
    (static-vectors:with-static-vector (sv (length vert-data-array)
                                           :element-type 'single-float)
      (replace sv vert-data-array)
      (%gl:buffer-data :array-buffer (* 4 (length sv))
                       (static-vectors:static-vector-pointer sv)
                       :static-draw))

    (let ((stride (* 4 (+ 3 2)))
          (offset 0))
      (gl:enable-vertex-attrib-array 0)
      (gl:vertex-attrib-pointer 0 3 :float nil stride offset)

      (incf offset (* 4 3))
      (gl:enable-vertex-attrib-array 1)
      (gl:vertex-attrib-pointer 1 2 :float nil stride offset)

      (gl:bind-vertex-array 0)
      (gl:disable-vertex-attrib-array 0)
      (gl:disable-vertex-attrib-array 1))))

(defun add-cube-vertex (f0 f1 f2 f3 f4 vert-data)
  (vector-push-extend (float f0 0.0) vert-data)
  (vector-push-extend (float f1 0.0) vert-data)
  (vector-push-extend (float f2 0.0) vert-data)
  (vector-push-extend (float f3 0.0) vert-data)
  (vector-push-extend (float f4 0.0) vert-data))

(defun add-cube-to-scene (mat vert-data)
  (labels ((v (a b c)
             (sb-cga:vec (float a 1.0) (float b 1.0) (float c 1.0)))
           (x (v) (aref v 0))
           (y (v) (aref v 1))
           (z (v) (aref v 2))
           (face (e f g h)
             (add-cube-vertex (x e) (y e) (z e) 0 1 vert-data)
             (add-cube-vertex (x f) (y f) (z f) 1 1 vert-data)
             (add-cube-vertex (x g) (y g) (z g) 1 0 vert-data)
             (add-cube-vertex (x g) (y g) (z g) 1 0 vert-data)
             (add-cube-vertex (x h) (y h) (z h) 0 0 vert-data)
             (add-cube-vertex (x e) (y e) (z e) 0 1 vert-data)))
    (let ((a (sb-cga:transform-point (v 0 0 0) mat))
          (b (sb-cga:transform-point (v 1 0 0) mat))
          (c (sb-cga:transform-point (v 1 1 0) mat))
          (d (sb-cga:transform-point (v 0 1 0) mat))
          (e (sb-cga:transform-point (v 0 0 1) mat))
          (f (sb-cga:transform-point (v 1 0 1) mat))
          (g (sb-cga:transform-point (v 1 1 1) mat))
          (h (sb-cga:transform-point (v 0 1 1) mat)))

      (face e f g h)    ;; front
      (face b a d c)    ;; back
      (face h g c d)    ;; top
      (face a b f e)    ;; bottom
      (face a e h d)    ;; left
      (face f b c g)))) ;; right

;; Draw all of the controllers as X/Y/Z lines
(defmethod render-controller-axes ((o main))
  (unless vr::*system*
    (return-from render-controller-axes))

  (let ((vertex-data-array (make-array 1024 :element-type 'single-float
                                            :adjustable t :fill-pointer 0)))
    (setf (controller-vert-count o) 0
          (tracked-controller-count o) 0)

    (loop for tracked-device from vr::+tracked-device-index-hmd+
            below vr::+max-tracked-device-count+
          when (and (vr::is-tracked-device-connected tracked-device)
                    (eql (vr::get-tracked-device-class tracked-device)
                         :controller))
            do (incf (tracked-controller-count o))
            and
              when (getf (aref (tracked-device-pose o) tracked-device)
                         'vr::pose-is-valid)
                do (let* ((mat (aref (device-pose o) tracked-device))
                          (center (sb-cga:transform-point
                                   (sb-cga:vec 0.0 0.0 0.0) mat)))
                     (flet ((w (v)
                              (loop for i below 3
                                    do (vector-push-extend (aref v i)
                                                           vertex-data-array))))
                       (loop for i below 3
                             for color = (sb-cga:vec 0.0 0.0 0.0)
                             for point = (sb-cga:vec 0.0 0.0 0.0)
                             do (incf (aref point i) 0.05)
                                (setf (aref color i) 1.0)
                                (setf point (sb-cga:transform-point point mat))

                                (w center)
                                (w color)
                                (w point)
                                (w color))
                       (incf (controller-vert-count o) 2)

                       (let ((start (sb-cga:transform-point
                                     (sb-cga:vec 0.0 0.0 -0.02) mat))
                             (end  (sb-cga:transform-point
                                    (sb-cga:vec 0.0 0.0 -39.0) mat))
                             (color (sb-cga:vec 0.92 0.92 0.71)))
                         (w start)
                         (w color)
                         (w end)
                         (w color)
                         (incf (controller-vert-count o) 2)))))

    ;; setup the VAO for the first time through.
    (when (zerop (controller-vao o))
      (setf (controller-vao o) (gl:gen-vertex-array))
      (gl:bind-vertex-array (controller-vao o))

      (setf (controller-vert-buffer o) (gl:gen-buffer))
      (gl:bind-buffer :array-buffer (controller-vert-buffer o))

      (let ((stride (* 2 3 4))
            (offset 0))
        (gl:enable-vertex-attrib-array 0)
        (gl:vertex-attrib-pointer 0 3 :float nil stride offset)

        (incf offset (* 3 4))
        (gl:enable-vertex-attrib-array 1)
        (gl:vertex-attrib-pointer 1 3 :float nil stride offset)

        (gl:bind-vertex-array 0)))

    (gl:bind-buffer :array-buffer (controller-vert-buffer o))

    ;; set vertex data if we have some
    (when (plusp (length vertex-data-array))
      (static-vectors:with-static-vector (sv (length vertex-data-array)
                                             :element-type 'single-float)
        (replace sv vertex-data-array)
        (%gl:buffer-data :array-buffer (* 4 (length sv))
                         (static-vectors:static-vector-pointer sv)
                         :stream-draw)))))

(defmethod setup-cameras ((o main))
  (setf (projection-left o) (get-hmd-matrix-projection-eye o :left))
  (setf (projection-right o) (get-hmd-matrix-projection-eye o :right))
  (setf (eye-pos-left o) (get-hmd-matrix-pose-eye o :left))
  (setf (eye-pos-right o) (get-hmd-matrix-pose-eye o :right)))

(defmethod create-frame-buffer ((o main) width height)
  (let ((render-framebuffer-id (gl:gen-framebuffer))
        (depth-buffer-id (gl:gen-renderbuffer))
        (render-texture-id (gl:gen-texture))
        (resolve-framebuffer-id (gl:gen-framebuffer))
        (resolve-texture-id (gl:gen-texture)))
    (gl:bind-framebuffer :framebuffer render-framebuffer-id)

    (gl:bind-renderbuffer :renderbuffer depth-buffer-id)
    (gl:renderbuffer-storage-multisample :renderbuffer 4 :depth-component
                                         width height)
    (gl:framebuffer-renderbuffer :framebuffer :depth-attachment
                                 :renderbuffer depth-buffer-id)

    (gl:bind-texture :texture-2d-multisample render-texture-id)
    (%gl:tex-image-2d-multisample :texture-2d-multisample 4 :rgba8
                                  width height t)
    (gl:framebuffer-texture-2d :framebuffer :color-attachment0
                               :texture-2d-multisample render-texture-id 0)

    (let ((cfs (gl:check-framebuffer-status :framebuffer)))
      (unless (gl::enum= cfs :framebuffer-complete)
        (error "render framebuffer incomplete ~s?" cfs)))

    (gl:bind-framebuffer :framebuffer resolve-framebuffer-id)

    (gl:bind-texture :texture-2d resolve-texture-id)
    (gl:tex-parameter :texture-2d :texture-min-filter :linear)
    (gl:tex-parameter :texture-2d :texture-max-level 0)
    (gl:tex-image-2d :texture-2d 0 :rgba8 width height 0 :rgba
                     :unsigned-byte nil)
    (gl:framebuffer-texture-2d :framebuffer :color-attachment0
                               :texture-2d resolve-texture-id 0)
    (gl:disable :depth-test)
    (let ((cfs (gl:check-framebuffer-status :framebuffer)))
      (unless (gl::enum= cfs :framebuffer-complete)
        (error "resolve framebuffer incomplete ~s?" cfs)))


    (gl:bind-framebuffer :framebuffer 0)
    (list 'depth-buffer-id depth-buffer-id
          'render-texture-id render-texture-id
          'render-framebuffer-id render-framebuffer-id
          'resolve-texture-id resolve-texture-id
          'resolve-framebuffer-id resolve-framebuffer-id)))

(defmethod setup-stereo-render-targets ((o main))
  (unless vr::*system*
    (return-from setup-stereo-render-targets nil))

  (destructuring-bind (w h) (vr::get-recommended-render-target-size)
    (setf (render-width o) w)
    (setf (render-height o) h)
    (setf (left-eye-desc o) (create-frame-buffer o w h))
    (setf (right-eye-desc o) (create-frame-buffer o w h)))
  t)

(defmethod setup-companion-window ((o main))
  (unless vr::*system*
    (return-from setup-companion-window nil))

  (static-vectors:with-static-vector (verts (* 4 8) :element-type 'single-float)
    (flet ((v (i &rest xyuv)
             (loop for j from (* i 4) repeat 4
                   for c in xyuv
                   do (setf (aref verts j) (float c 1.0)))))
      ;; left eye verts
      (v 0 -1 -1 0 1)
      (v 1 0 -1 1 1)
      (v 2 -1 1 0 0)
      (v 3 0 1 1 0)

      ;; right eye verts
      (v 4 0 -1 0 1)
      (v 5 1 -1 1 1)
      (v 6 0 1 0 0)
      (v 7 1 1 1 0)

      (static-vectors:with-static-vector (indices 12 :element-type
                                                  '(unsigned-byte 16))
        (replace indices '(0 1 3 0 3 2 4 5 7 4 7 6))
        (setf (companion-window-index-size o) (length indices))

        (setf (companion-window-vao o) (gl:gen-vertex-array))
        (gl:bind-vertex-array (companion-window-vao o))

        (setf (companion-window-id-vert-buffer o) (gl:gen-buffer))
        (gl:bind-buffer :array-buffer (companion-window-id-vert-buffer o))
        (%gl:buffer-data :array-buffer (* 4 6 (length verts))
                         (static-vectors:static-vector-pointer verts)
                         :static-draw)

        (setf (companion-window-id-index-buffer o) (gl:gen-buffer))
        (gl:bind-buffer :element-array-buffer
                        (companion-window-id-index-buffer o))
        (%gl:buffer-data :element-array-buffer (* 2 (length indices))
                         (static-vectors:static-vector-pointer indices)
                         :static-draw)
        (gl:enable-vertex-attrib-array 0)
        (gl:vertex-attrib-pointer 0 2 :float nil (* 4 4) 0)

        (gl:enable-vertex-attrib-array 1)
        (gl:vertex-attrib-pointer 1 2 :float nil (* 4 4) (* 2 4))

        (gl:bind-vertex-array 0)
        (gl:disable-vertex-attrib-array 0)
        (gl:disable-vertex-attrib-array 1)))))


(defmethod render-stereo-targets ((o main))
  (gl:clear-color (* 0.05 (abs (sin (* 0.002 (get-internal-real-time)))))
                  (* 0.05 (abs (sin (* 0.0015 (get-internal-real-time)))))
                  (* 0.05 (abs (sin (* 0.001 (get-internal-real-time)))))
                  1)

  (with-accessors ((left left-eye-desc) (right right-eye-desc)
                   (width render-width) (height render-height)) o

    (loop
      for desc in (list left right)
      for eye in '(:left :right)
      do
         (gl:enable :multisample)

         (gl:bind-framebuffer :framebuffer (getf desc 'render-framebuffer-id))
         (gl:viewport 0 0 width height)
         (render-scene o eye)
         (gl:bind-framebuffer :framebuffer 0)

         (gl:disable :multisample)

         (gl:bind-framebuffer :read-framebuffer
                              (getf desc 'render-framebuffer-id))
         (gl:bind-framebuffer :draw-framebuffer
                              (getf desc 'resolve-framebuffer-id))

         (%gl:blit-framebuffer 0 0 width height 0 0 width height
                               :color-buffer
                               :linear)

         (gl:bind-framebuffer :read-framebuffer 0)
         (gl:bind-framebuffer :draw-framebuffer 0)))
  (when *once*
    (setup-cameras o))
  (setf *once* nil))

(defmethod render-scene ((o main) eye)
                                        ;(gl:clear-color 1 0 1 1)
  (gl:clear :color-buffer :depth-buffer)
  (gl:enable :depth-test)
  (gl:enable :cull-face)

  (with-accessors* (show-cubes
                    scene-program-id scene-matrix-location scene-vao
                    texture vertex-count
                    controller-transform-program-id controller-matrix-location
                    controller-vao controller-vert-count
                    render-model-program-id
                    tracked-device-to-render-model show-tracked-device
                    tracked-device-pose device-pose
                    render-model-matrix-location) o
    (when show-cubes
      (gl:use-program scene-program-id)
      (gl:uniform-matrix scene-matrix-location 4
                         (vector (get-current-view-projection-matrix o eye))
                         nil)
      (gl:uniformi (scene-texture-location o) 0)
      (gl:bind-vertex-array scene-vao)
      (gl:active-texture :texture0)
      (gl:bind-texture :texture-2d texture)
                                        ;(gl:point-size 10)
      (gl:draw-arrays :triangles 0 vertex-count)
      (gl:bind-vertex-array 0))

    (let ((is-input-focus-captured-by-another-process
            (vr::is-input-focus-captured-by-another-process)))
      (unless is-input-focus-captured-by-another-process
        ;; draw the controller axis lines
        (gl:use-program controller-transform-program-id)
        (gl:uniform-matrix controller-matrix-location 4
                           (vector (get-current-view-projection-matrix o eye))
                           nil)
        (gl:bind-vertex-array controller-vao)
        (gl:draw-arrays :lines 0 controller-vert-count)
        (gl:bind-vertex-array 0))

      ;; render model rendering
      (gl:use-program render-model-program-id)
      (gl:uniformi (render-model-texture-location o) 0)
      (loop with pose = nil
            for tracked-device below vr::+max-tracked-device-count+
            when (and (aref tracked-device-to-render-model tracked-device)
                      (aref show-tracked-device tracked-device)
                      (setf pose (aref tracked-device-pose tracked-device))
                      (getf pose 'vr::pose-is-valid)
                      (not (and is-input-focus-captured-by-another-process
                                (eql (vr::get-tracked-device-class
                                      tracked-device)
                                     :controller)))
                      (not (eql (vr::get-tracked-device-class
                                 tracked-device)
                                :hmd)))
              do (let* ((device-to-tracking (aref device-pose tracked-device))
                        (mvp (sb-cga:matrix*
                              (get-current-view-projection-matrix o eye)
                              device-to-tracking)))
                   (gl:uniform-matrix render-model-matrix-location 4
                                      (vector mvp)
                                      nil)
                   (draw (aref tracked-device-to-render-model tracked-device))))))
  (gl:use-program 0))

(defmethod render-companion-window ((o main))
  (gl:disable :depth-test)
  (gl:viewport 0 0 (companion-window-width o) (companion-window-height o))
  (gl:bind-framebuffer :framebuffer 0)
  (gl:clear-color 1 0 0 1)
  (gl:clear :color-buffer)
  (gl:bind-vertex-array (companion-window-vao o))
  (gl:use-program (companion-window-program-id o))
  (gl:uniformi (companion-texture-location o) 0)
  (loop with c = (/ (companion-window-index-size o) 2)
        for desc in (list (left-eye-desc o) (right-eye-desc o))
        for start in (list 0 (* 2 c))
        do (gl:bind-texture :texture-2d (getf desc 'resolve-texture-id))
           (gl:tex-parameter :texture-2d :texture-wrap-s :clamp-to-edge)
           (gl:tex-parameter :texture-2d :texture-wrap-t :clamp-to-edge)
           (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
           (gl:tex-parameter :texture-2d :texture-min-filter :linear)
           (%gl:draw-elements :triangles c :unsigned-short start))

  (gl:bind-vertex-array 0)
  (gl:use-program 0))

(defmethod get-hmd-matrix-projection-eye ((o main) eye)
  (unless vr::*system*
    (return-from get-hmd-matrix-projection-eye (sb-cga:identity-matrix)))

  (vr::get-projection-matrix eye (near-clip o) (far-clip o)))

(defmethod get-hmd-matrix-pose-eye ((o main) eye)
  (unless vr::*system*
    (return-from get-hmd-matrix-pose-eye (sb-cga:identity-matrix)))

  (sb-cga:inverse-matrix
   (vr::get-eye-to-head-transform eye)))

(defparameter *once* t)
(defparameter *cc* 0)
(defmethod get-current-view-projection-matrix ((o main) eye)
  (when *once*
    (format t "~&~s~%~s~%~s~%"
            (projection-left o)
            (eye-pos-left o)
            (hmd-pose o)))
  (ecase eye
    (:left
     (sb-cga:matrix* (projection-left o)
                     (eye-pos-left o)
                     (hmd-pose o)))
    (:right
     (sb-cga:matrix* (projection-right o) (eye-pos-right o) (hmd-pose o)))))

(defmethod update-hmd-matrix-pose ((o main))
  (unless vr::*system*
    (return-from update-hmd-matrix-pose))

  (vr::wait-get-poses (tracked-device-pose o) nil)
  (setf (pose-classes o) "")
  (setf (valid-pose-count o) 0)
  (loop for device below vr::+max-tracked-device-count+
        for tracked-device = (aref (tracked-device-pose o) device)
        when (getf tracked-device 'vr::pose-is-valid)
          do (incf (valid-pose-count o))
             (setf (aref (device-pose o) device)
                   (getf tracked-device 'vr::device-to-absolute-tracking))
             (setf (aref (dev-class-char o) device)
                   (case (vr::get-tracked-device-class device)
                     (:controller #\C)
                     (:hmd #\H)
                     (:invalid #\I)
                     (:generic-tracker #\G)
                     (:tracking-reference #\T)
                     (t #\?)))
             (setf (pose-classes o) (format nil "~a~a" (pose-classes o)
                                            (aref (dev-class-char o) device))))
  (when (getf (aref (tracked-device-pose o) vr::+tracked-device-index-hmd+)
              'vr::pose-is-valid)
    (setf (hmd-pose o)
          (sb-cga:inverse-matrix
           (aref (device-pose o) vr::+tracked-device-index-hmd+)))))

;;; finds a render model we've already loaded or loads a new one
(defmethod find-or-load-render-model ((o main) name)
  (when (gethash name (render-models o))
    (return-from find-or-load-render-model (gethash name (render-models o))))

  ;; load the model if we didn't find one
  (let* ((model (loop when (vr::load-render-model-async name) return it
                        do (sleep 1)))
         (texture (loop when (vr::load-texture-async
                              (getf model 'vr::diffuse-texture-id))
                          return it
                        do (sleep 1)))
         (render-model (make-instance 'render-model
                                      :name name
                                      :model model :texture texture)))
    (vr::free-render-model model)
    (vr::free-texture texture)
    (setf (gethash name (render-models o)) render-model)))

;;; Create/destroy GL a Render Model for a single tracked device
(defmethod setup-render-model-for-tracked-device ((o main) tracked-device-index)
  (when (> tracked-device-index vr::+max-tracked-device-count+)
    (return-from setup-render-model-for-tracked-device))

  ;; try to find a model we've already set up
  (let* ((render-model-name (vr::get-tracked-device-property
                             tracked-device-index :render-model-name-string))
         (render-model (find-or-load-render-model o render-model-name)))
    (unless render-model
      (dprintf "unable to load render model for tracked device ~d (~a.~a)"
               tracked-device-index (driver o) render-model-name))
    (setf (aref (tracked-device-to-render-model o) tracked-device-index)
          render-model)
    (setf (aref (show-tracked-device o) tracked-device-index) t)))

;;; Create/destroy GL Render Models
(defmethod setup-render-models ((o main))
  (fill (tracked-device-to-render-model o) nil)
  (unless vr::*system*
    (return-from setup-render-models nil))

  (loop for tracked-device below vr::+max-tracked-device-count+
        when (vr::is-tracked-device-connected tracked-device)
          do (setup-render-model-for-tracked-device o tracked-device)))

;; allocates and populates the GL resources for a render model
(defmethod initialize-instance :after ((m render-model) &key model texture)
  ;; create and bind a VAO to hold state for this model
  (setf (vert-array m) (gl:gen-vertex-array))
  (gl:bind-vertex-array (vert-array m))

  (let ((size (cffi:foreign-type-size '(:struct vr::render-model-vertex-t))))

    ;; populate a vertex buffer
    (setf (vert-buffer m) (gl:gen-buffer))
    (gl:bind-buffer :array-buffer (vert-buffer m))
    (%gl:buffer-data :array-buffer
                     (* size (getf model 'vr::vertex-count))
                     (getf model 'vr::vertex-data)
                     :static-draw)

    ;; identify the components in the vertex buffer
    (flet ((offset (slot)
             (cffi:foreign-slot-offset
              '(:struct vr::render-model-vertex-t) slot)))
      (gl:enable-vertex-attrib-array 0)
      (gl:vertex-attrib-pointer 0 3 :float nil size (offset 'vr::position))
      (gl:enable-vertex-attrib-array 1)
      (gl:vertex-attrib-pointer 1 2 :float nil size (offset 'vr::texture-coord))
      (gl:enable-vertex-attrib-array 2)
      (gl:vertex-attrib-pointer 2 3 :float nil size (offset 'vr::normal))))

  ;; create and populate the index buffer
  (setf (index-buffer m) (gl:gen-buffer))
  (gl:bind-buffer :element-array-buffer (index-buffer m))
  (%gl:buffer-data :element-array-buffer
                   (* 2 (getf model 'vr::triangle-count) 3)
                   (getf model 'vr::index-data)
                   :static-draw)

  (gl:bind-vertex-array 0)

  ;; create and populate the texture
  (setf (texture m) (gl:gen-texture))
  (gl:bind-texture :texture-2d (texture m))

  (gl:tex-image-2d :texture-2d 0 :rgba
                   (getf texture 'vr::width) (getf texture 'vr::height)
                   0 :rgba :unsigned-byte (getf texture 'vr::texture-map-data))

  (gl:generate-mipmap :texture-2d)

  (gl:tex-parameter :texture-2d :texture-wrap-s :clamp-to-edge)
  (gl:tex-parameter :texture-2d :texture-wrap-t :clamp-to-edge)
  (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
  (gl:tex-parameter :texture-2d :texture-min-filter :linear-mipmap-linear)

  (gl:tex-parameter :texture-2d :texture-max-anisotropy-ext
                    (gl:get* :max-texture-max-anisotropy-ext))

  (gl:bind-texture :texture-2d 0)

  (setf (vertex-count m) (* 3 (getf model 'vr::triangle-count))))

(defmethod cleanup ((m render-model))
  (gl:delete-buffers (list (shiftf (index-buffer m) 0)))
  (gl:delete-vertex-arrays (list (shiftf (vert-array m) 0)))
  (gl:delete-buffers (list (shiftf (vert-buffer m) 0))))

(defmethod draw ((m render-model))
  (gl:bind-vertex-array (vert-array m))

  (gl:active-texture :texture0)
  (gl:bind-texture :texture-2d (texture m))

  (%gl:draw-elements :triangles (vertex-count m) :unsigned-short 0)

  (gl:bind-vertex-array 0))
