;;;; The Common-Shooter
;;; step1  <Game Frame>
;;; step2  <Audio>
;;; step3  <Sprite Sheets> <Character Object> <Draw> <Initialize>

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
  (setf *shot-sound*     (sdl-mixer:load-sample *path-shot-sound*)
        *shipbom-sound*  (sdl-mixer:load-sample *path-shipbom-sound*)
        *enemybom-sound* (sdl-mixer:load-sample *path-enemybom-sound*)
        *bg-music*       (sdl-mixer:load-music  *path-background-music*)))

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
  (sdl-mixer:play-sample sample))                    ; shot sound     

;; step3 <Sprite sheets>
;; -----------------------------------------------------------------------------------------------
(defparameter *path-size32* "C:\\work\\graphics\\size32.bmp")   ; set path to size32.bmp
(defparameter *path-size64* "C:\\work\\graphics\\size64.bmp")   ; set path to size64.bmp
(defparameter *path-size16* "C:\\work\\graphics\\size16.bmp")   ; set path to size16.bmp
(defvar *image0*)
(defvar *image1*)
(defvar *image2*)

(defun Set-imageid ()
  "load image data and set id"
  (setf *image0* (sdl:load-image *path-size32* :color-key(sdl:color :r 0 :g 0 :b 0)) 
        *image1* (sdl:load-image *path-size64* :color-key(sdl:color :r 0 :g 0 :b 0))
        *image2* (sdl:load-image *path-size16* :color-key(sdl:color :r 0 :g 0 :b 0)))
  (let ((temp0 (append (loop for x from 0 to 128 by 32  
                        collect (list x 0 32 32))))      
        (temp1 (append (loop for x from 0 to 128 by 64  
                        collect (list x 0 64 64))))          
        (temp2 (append (loop for x from 0 to 16 by 16   
                        collect (list x 0 16 16)))))
        (setf (sdl:cells *image0*) temp0     ; id 0-4 ship,shot,enemy1,enemy2,enemy3 
              (sdl:cells *image1*) temp1     ; id 0-2 mapchip,enemy-explosion,my-explosion
              (sdl:cells *image2*) temp2)))  ; id 0-1 pointer,enemy-shot

;;step3 <Character object>
;; -----------------------------------------------------------------------------------------------
(defclass object ()
  ((%imageid     :initarg :imageid     :initform 0 :accessor imageid)     ; 3 imageid available
   (%id          :initarg :id          :initform 0 :accessor id)          ; graphic id in imageid
   (%x           :initarg :x           :initform 0 :accessor x)           ; x (upper left corner)
   (%y           :initarg :y           :initform 0 :accessor y)           ; y (upper left corner)
   (%width       :initarg :width       :initform 0 :accessor width)       ; width (from upper left corner)
   (%height      :initarg :height      :initform 0 :accessor height))     ; height (from upper left corner)
  (:documentation "The Object Class"))

(defclass entity (object)
  ((%dx          :initarg :dx          :initform 0 :accessor dx)          ; x direction speed
   (%dy          :initarg :dy          :initform 0 :accessor dy)          ; y direction speed
   (%move-cnt    :initarg :move-cnt    :initform 0 :accessor move-cnt)    ; moving counter(distance)
   (%revival-cnt :initarg :revival-cnt :initform 0 :accessor revival-cnt) ; revival counter(wait)
   (%explode-cnt :initarg :explode-cnt :initform 0 :accessor explode-cnt) ; explosion counter(wait)
   (%state       :initarg :state       :initform 0 :accessor state))      ; 0:dead 1:alive 2:explosion 3:revival
  (:documentation "The Entity Class"))

;; step3 <Draw images>
;; -----------------------------------------------------------------------------------------------  
(defun Draw (obj)
  "character draw"
  (ecase (imageid obj)
    (0 (sdl:draw-surface-at-* *image0* (x obj) (y obj) :cell (id obj)))
    (1 (sdl:draw-surface-at-* *image1* (x obj) (y obj) :cell (id obj)))
    (2 (sdl:draw-surface-at-* *image2* (x obj) (y obj) :cell (id obj)))))

;; step3 <Initialize>
;; -----------------------------------------------------------------------------------------------  
(defun Initialize ()
  "graphics initialize"
  (setf (sdl:frame-rate) 60)                             ; frame rate set
  (setf *random-state* (make-random-state t))            ; random set
  (Set-imageid)                                          ; imageid set
  (sdl:show-cursor nil))                                 ; cursor not show
    
;; step1 <Game Frame>
;; -----------------------------------------------------------------------------------------------
(defun Common-shooter ()
  "main routine"
  (sdl:with-init (sdl:sdl-init-video sdl:sdl-init-audio) ; use video and audio
    (sdl:window 640 480 :position #(192 50)              ; size 640*480, position x(192) y(50)
                        :title-caption "THE SHOOTER"
                        :icon-caption "THE SHOOTER"
                        :double-buffer T)
                     ;  :fullscreen T)
      ; step 3
      (Initialize)                                       ; graphics initialize
      ; step 2 
      (Open-sound)                                       ; open audio and load sound data 
      ; step 2
      (Play-music *bg-music*)                            ; play music
      ; step 3
      (let((ship (make-instance 'entity :imageid 0 :id 0 :x 224 :y 416 :width 32 :height 32 :dx 4 :dy 4 :state 1)))

      (sdl:update-display)
      (sdl:with-events (:poll)
        (:quit-event ()
          ; step 2
          (Stop-sound)                                   ; sample and music stop
          (Close-sound)                                  ; close audio
          t)
 
        (:idle ()
          ; Game body

          ; step 3
          (when (= (state ship) 1)
            (Draw ship))                                 ; draw ship

          (sdl:update-display))))))

(common-shooter)

