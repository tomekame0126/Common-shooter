;;; The Common-Shooter
;;; step1 <Game Frame>
;;; step2 <Audio>

;; step2 <Audio>
;; -----------------------------------------------------------------------------------------------
(defparameter *path-shot-sound*       "C:\\work\\sound\\tm2_shoot003.wav")
(defparameter *path-shipbom-sound*    "C:\\work\\sound\\bom01.wav")
(defparameter *path-enemybom-sound*   "C:\\work\\sound\\tm2_bom001.wav")
(defparameter *path-background-music* "C:\\work\\sound\\rbutai_xg.mid")
(defvar *shot-sound*)                   ; shot sound
(defvar *shipbom-sound*)                ; ship bom sound
(defvar *enemybom-sound*)               ; enemy bom sound
(defvar *bg-music*)                     ; background music

(defun Open-sound ()
 "load sound data and set"
  (sdl-mixer:open-audio :chunksize 1024 :channels 2)
  (sdl-mixer:allocate-channels 16)
  (setf *shot-sound* (sdl-mixer:load-sample *path-shot-sound*)
        *shipbom-sound* (sdl-mixer:load-sample *path-shipbom-sound*)
        *enemybom-sound* (sdl-mixer:load-sample *path-enemybom-sound*)
        *bg-music* (sdl-mixer:load-music *path-background-music*)))

(defun Stop-sound ()
 "sound stop"
  (when (sdl-mixer:music-playing-p)
        (sdl-mixer:halt-music))         ; BGM stop
  (when (sdl-mixer:sample-playing-p nil)
        (sdl-mixer:halt-sample)))       ; Shot,Bom sound stop  

(defun Close-sound ()
 "close sound file" 
  (sdl-mixer:free *shot-sound*)         ; free shot sound
  (sdl-mixer:free *shipbom-sound*)      ; freeship bom sound
  (sdl-mixer:free *enemybom-sound*)     ; free enemy bom sound
  (sdl-mixer:free *bg-music*)           ; free background music
  (sdl-mixer:close-audio))

(defun Play-music (music)
  "play music"
  (sdl-mixer:play-music music :loop t :position 0))  ; BGM start

(defun Play-sample (sample)
  "play sample"
  (sdl-mixer:play-sample sample)) ; shot sound     

;; step1 <Game Frame>
;; -----------------------------------------------------------------------------------------------
(defun Common-shooter ()
  (sdl:with-init (sdl:sdl-init-video sdl:sdl-init-audio)  ; use video and audio
    (sdl:window 640 480 :position #(192 50)               ; size 640*480, position x(192) y(50)
                        :title-caption "THE SHOOTER"
                        :icon-caption "THE SHOOTER"
                        :double-buffer T)
                      ; :fullscreen T)
      ; step 2 
      (Open-sound)                                        ; open audio and load sound data
      (Play-sample *shot-sound*)                          ; play sample
      (Play-music *bg-music*)                             ; play music

      (sdl:update-display)
      (sdl:with-events (:poll)
        (:quit-event ()
          ; step 2
          (Stop-sound)                                    ; sample and music stop
          (Close-sound)                                   ; close audio
          t)
        (:idle ()
          ; Game body

          (sdl:update-display)))))

(Common-shooter)

