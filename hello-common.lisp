(defpackage 3b-openvr-hello
  (:use :cl)
  (:local-nicknames (:vr :3b-openvr)))
(in-package 3b-openvr-hello)

(defclass render-model ()
  ((vert-buffer :accessor vert-buffer)
   (index-buffer :accessor index-buffer)
   (vert-array :accessor vert-array)
   (texture :accessor texture)
   (vertex-count :accessor vertex-count)
   (model-name :accessor model-name)))

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
                           :initform (make-array vr::+max-tracked-device-count+))
   (device-pose :reader device-pose
                :initform (make-array vr::+max-tracked-device-count+))
   (show-tracked-device :reader show-tracked-device
                        :initform (make-array vr::+max-tracked-device-count+))
   (companion-window :accessor companion-window :initform nil)
   (companion-window-width :accessor companion-window-width :initform 640)
   (companion-window-height :accessor companion-window-height :initform 320)
   (context :accessor context :initform nil)
   (tracked-controller-count :accessor tracked-controller-count :initform 0)
   (tracked-controller-count-last :accessor tracked-controller-count-last
                                  :initform -1)
   (valid-pose-count :accessor valid-pose-count)
   (valid-pose-count-last :accessor valid-pose-count-last)
   (show-cubes :accessor show-cubes :initform t)
   ;; what classes we saw poses for this frame
   (pose-classes :accessor pose-classes :initform "")
   ;; for each device, a character representing its class
   (dev-class-char :accessor dev-class-char
                   :initform (make-array vr::+max-tracked-device-count+))
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
   (companion-id-vert-buffer :accessor companion-id-vert-buffer)
   (companion-id-index-buffer :accessor companion-id-index-buffer)
   (companion-window-index-size :accessor companion-window-index-size)
   (controller-vert-buffer :accessor controller-vert-buffer :initform 0)
   (controller-vao :accessor controller-vao :initform 0)
   (controller-vert-count :accessor controller-vert-count)
   (hmd-pose :accessor hmd-pose)
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
   (left-eye-desc :accessor left-eye-desc)
   (right-eye-desc :accessor right-eye-desc)
   (render-width :accessor render-width)
   (render-height :accessor render-height)
   (render-models :accessor render-models
                  :initform (make-array 16 :fill-pointer 0 :adjustable t))
   (tracked-device-to-render-model :accessor tracked-device-to-render-model)))

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
  (setf (window-title o)
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
  (setup-stereo-render-targets o)
  (setup-companion-window o)
  (setup-render-models o))


(defmethod init-compositor ((o main))
  (vr-compositor))

(defmethod shutdown ((o main))
  (map nil 'vr::free-render-model (render-models o))
  (setf (fill-pointer (render-models o)) 0)

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
                    (gl:delete-vertex-arrays (shiftf (,slot o) nil)))))
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
  (when vr::*system*
    (render-controller-axes)
    (render-stereo-targets)
    (render-companion-window)

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
  (swap-window o)

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
                 (dprintf "~a - Unable to compile ~a ~a!~%"
                          name stage shader)
                 (gl:delete-program program-id)
                 (gl:delete-shader shader)
                 (return-from compile-gl-shader 0))
               (gl:attach-shader program-id shader)
               (gl:delete-shader shader))))
      (shader :vertex-shader vertex-shader)
      (shader :fragment-shader fragment-shader))
    (gl:link-program program-id)
    (unless (gl:get-program program-id :link-status)
      (dprintf "~a - Error linking program ~a~%" name program-id)
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
        (gl:get-uniform-location (scene-program-id 0) "matrix"))
  (when (= -1 (scene-matrix-location o))
    (dprintf "unable to find matrix unifomr for scene shader~%")
    (return-from create-all-shaders nil))

  (setf (controller-transform-program-id o)
        (compile-gl-shader
         "Controller"
         '3b-openvr-hello/shaders:controller-transform-vertex
         '3b-openvr-hello/shaders:controller-transform-fragment))
  (setf (controller-matrix-location o)
        (gl:get-uniform-location (controller-transform-program-id 0) "matrix"))
  (when (= -1 (controller-matrix-location o))
    (dprintf "unable to find matrix unifomr for controller transform shader~%")
    (return-from create-all-shaders nil))

  (setf (render-model-program-id o)
        (compile-gl-shader
         "render model"
         '3b-openvr-hello/shaders:render-model-vertex
         '3b-openvr-hello/shaders:render-model-fragment))

  (setf (render-model-matrix-location o)
        (gl:get-uniform-location (render-model-program-id 0) "matrix"))
  (when (= -1 (render-model-matrix-location o))
    (dprintf "unable to find matrix unifomr for render model shader~%")
    (return-from create-all-shaders nil))


  (setf (companion-window-program-id o)
        (compile-gl-shader
         "Companion Window"
          '3b-openvr-hello/shaders:companion-window-vertex
          '3b-openvr-hello/shaders:companion-window-fragment))

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
                       0 :rgba :unsigned-byte
                       (static-vectors:static-vector-pointer png))

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
                            do (add-cube-to-scene mat vert-data-array))
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
      (gl:vertex-attrib-pointer 0 3 :float :falsse stride offset)

      (incf offset (* 4 3))
      (gl:enable-vertex-attrib-array 1)
      (gl:vertex-attrib-pointer 1 2 :float :falsse stride offset)

      (gl:bind-vertex-array 0)
      (gl:disable-vertex-attrib-array 0)
      (gl:disable-vertex-attrib-array 1))))

(defun add-cube-vertex (f0 f1 f2 f3 f4 vert-data)
  (vector-push-extend f0 vert-data)
  (vector-push-extend f1 vert-data)
  (vector-push-extend f2 vert-data)
  (vector-push-extend f3 vert-data)
  (vector-push-extend f4 vert-data))

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
;	for ( vr::TrackedDeviceIndex_t unTrackedDevice = vr::k_unTrackedDeviceIndex_Hmd + 1; unTrackedDevice < vr::k_unMaxTrackedDeviceCount; ++unTrackedDevice )
    (loop for tracked-device from vr::+tracked-device-index-hmd+
            below vr::+max-tracked-device-count+
          when (and (vr::is-tracked-device-connected tracked-device)
                    (eql (vr::get-tracked-device-class tracked-device)
                         :controller))
            do (incf (tracked-controller-count o))
            and
              when (getf (aref (tracked-device-pose o) tracked-device)
                         'vr::pose-is-valid)
                do (let ((mat (aref (device-pose o) tracked-device))
                         (center (sb-cga:transform-point
                                  mat (sb-cga:vec 0.0 0.0 0.0))))
                     (flet ((w (v)
                              (loop for i below 3
                                    do (vector-push-extend vertex-data-array
                                                           (aref v i)))))
                       (loop for i below 3
                             for color = (sb-cga:vec 0.0 0.0 0.0)
                             for point = (sb-cga:vec 0.0 0.0 0.0)
                             do (incf (aref point i) 0.05)
                                (setf (aref color i) 1.0)
                                (setf point (sb-cga:transform-point mat point))

                                (w center)
                                (w color)
                                (w point)
                                (w color))
                       (incf (controller-vert-count o) 2)

                       (let ((start (sb-cga:transform-point
                                     mat (sb-cga:vec 0.0 0.0 -0.02)))
                             (end  (sb-cga:transform-point
                                    mat (sb-cga:vec 0.0 0.0 -39.0)))
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
        (gl:vertex-attrib-pointer 0 3 :float :false :stride offset)

        (incf offset (* 3 4))
        (gl:enable-vertex-attrib-array 1)
        (gl:vertex-attrib-pointer 1 3 :float :false :stride offset)

        (gl:bind-vertex-array 0)))

    (gl:bind-buffer :array-buffer (controller-vert-buffer o))

    ;; set vertex data if we have some
    (when (plusp (length vert-data-array))
      (static-vectors:with-static-vector (sv (length vert-data-array)
                                             :element-type 'single-float)
        (replace sv vert-data-array)
        (%gl:buffer-data :array-buffer :* 4 (length sv)
                         (static-vectors:static-vector-pointer sv)
                         :stream-draw)))))

(defmethod setup-cameras ((o main))
  (setf (projection-left o) (get-hmd-matrix-projection-eye :left))
  (setf (projection-right o) (get-hmd-matrix-projection-eye :right))
  (setf (eye-pos-left o) (get-hmd-matrix-pose-eye :left))
  (setf (eye-pos-right o) (get-hmd-matrix-pose-eye :right)))

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
                               :texture-2d-multisample render-texture-id)


    (gl:bind-framebuffer :framebuffer resolve-framebuffer-id)

    (gl:bind-texture :texture-2d resolve-texture-id)
    (gl:tex-parameter :texture-2d :texture-min-filter :linear)
    (gl:tex-parameter :texture-2d :texture-max-level 0)
    (gl:tex-image-2d :texture-2d 0 :rgba8 width height 0 :rgba
                     :unsigned-byte nil)
    (gl:framebuffer-texture-2d :framebuffer :color-attachment0
                               :texture-2d resolve-texture-id)

    (if (eql (gl:check-framebuffer-status :framebuffer)
             :framebuffer-complete)
        (list 'depth-buffer-id depth-buffer-id
              'render-texture-id render-texture-id
              'render-framebuffer-id render-framebuffer-id
              'resolve-texture-id resolve-texture-id
              'resolve-framebuffer-id resolve-framebuffer-id)
        nil)))

(defmethod setup-stereo-render-targets ((o main))
  (unless vr::*system*
    (return-from setup-stereo-render-targets nil))

  (destructuring-bind (w h) (vr::get-recommended-render-target-size)
    (setf (left-eye-desc o) (create-frame-buffer o w h))
    (setf (right-eye-desc o) (create-frame-buffer o w h)))
  t)

(defmethod setup-companion-window ((o main))
  (unless vr::*system*
    (return-from setup-companion-window nil))

  

)
