;;; The Common-Shooter
;;; Step1 <Game Frame>

;; Step1 <Game Frame>
;; -----------------------------------------------------------------------------------------------
(defun Common-shooter ()
  (sdl:with-init (sdl:sdl-init-video sdl:sdl-init-audio)  ; use video and audio
    (sdl:window 640 480 :position #(192 50)               ; size 640*480, position x(192) y(50)
                        :title-caption "THE SHOOTER"
                        :icon-caption "THE SHOOTER"
                        :double-buffer T)
                      ; :fullscreen T)

      (sdl:update-display)
      (sdl:with-events (:poll)
        (:quit-event ()
          t)
        (:idle ()
          ; Game body

          (sdl:update-display)))))

(Common-shooter)

