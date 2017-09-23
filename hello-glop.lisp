;;(asdf:load-systems '3b-openvr-hello)
(in-package 3b-openvr-hello)

;; port of hellovr_opengl sample from OpenVR sdk

(defclass glop-window (glop:window)
  ((main-window :accessor main-window)))

(defclass main/glop (main)
  ((exit :accessor exit :initform nil)))

(defmethod (setf window-title) (title (w glop-window))
  (glop:set-window-title w title)
  title)

(defmethod swap-window ((w glop-window))
  (glop:swap-buffers w))

(defmethod glop:on-event ((w glop-window) e)) ;; do nothing

(defparameter *w* nil)

(defmethod glop:on-event ((gw glop-window) (e glop:key-press-event))
  (let ((w (main-window gw)))
    (case (glop:keysym e)
      ((:escape :q) (setf (exit w) t))
      (:c (setf (show-cubes w) (not (show-cubes w)))))))

(defmethod glop:on-event ((gw glop-window) (event glop:resize-event))
  (with-simple-restart (continue "continue")
    (let ((width (glop:width event))
          (height (glop:height event))
          (w (main-window gw)))

      (setf (companion-window-height w) height
            (companion-window-width w) width))))

(defun main/glop (&rest options &key gldebug verbose vblank gl-finish-hack)
  (declare (ignore  gldebug verbose vblank gl-finish-hack))
  (vr::with-vr ()
    (let ((w (apply #'make-instance 'main/glop options)))
      (glop:with-window (gw "hellovr"
                            (companion-window-width w) (companion-window-height w)
                            :x 700 :y 100
                            :win-class 'glop-window
                            :depth-size 0
                            :major 4 :minor 1 :profile :core
                            )
        (setf (companion-window w) gw
              (main-window gw) w)
        (glop::%swap-interval gw (if (vblank w) 1 0))
        #+win32
        (setf (glop::win32-window-swap-interval gw) 0)
        (init w)
        (loop
          until (exit w)
          while (with-simple-restart (Continue "Continue")
                  (glop:dispatch-events (companion-window w)
                                        :blocking nil :on-foo nil))
          do (with-simple-restart (Continue "Continue")
               (handle-input w))
             (with-simple-restart (Continue "Continue")
               (render-frame w)))
        (shutdown w)))))

#++
(main/glop :verbose t)
;(asdf:load-systems '3b-openvr-hello)(3b-openvr-hello::main/glop :verbose t)
