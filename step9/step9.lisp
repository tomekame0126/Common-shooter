;;;; The Common-Shooter

;;; step1  <Game Frame>
;;; step2  <Audio>
;;; step3  <Sprite Sheets> <Character Object> <Draw> <Initialize>
;;; step4  <Scroll> <Scroll Counter>
;;; step5  <Font> <Score Panel>
;;; step6  <Key State>
;;; step7  <Game Field>
;;; step8  <Shot>
;;; step9  <Move Enemy> <Set Enemy> <Remove Dead Eenemy>

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
  "load imagedata and set id"
  (setf *image0* (sdl:load-image *path-size32* :color-key(sdl:color :r 0 :g 0 :b 0)) 
        *image1* (sdl:load-image *path-size64* :color-key(sdl:color :r 0 :g 0 :b 0))
        *image2* (sdl:load-image *path-size16* :color-key(sdl:color :r 0 :g 0 :b 0)))
  (let ((temp0 (append (loop for x from 0 to 128 by 32  ; id 0-4 ship,shot,enemy1,enemy2,enemy3 
                        collect (list x 0 32 32))))      
        (temp1 (append (loop for x from 0 to 128 by 64  ; id 0-2 mapchip,enemy-explosion,my-explosion
                        collect (list x 0 64 64))))          
        (temp2 (append (loop for x from 0 to 16 by 16   ; id 0-1 pointer,enemy-shot
                        collect (list x 0 16 16)))))
        (setf (sdl:cells *image0*) temp0
              (sdl:cells *image1*) temp1 
              (sdl:cells *image2*) temp2)))

;;step3 <Character object>
;; -----------------------------------------------------------------------------------------------
(defclass object ()
  ((%imageid     :initarg :imageid     :initform 0 :accessor imageid)     ; 3 imageid available
   (%id          :initarg :id          :initform 0 :accessor id)          ; graphic id in imageid
   (%x           :initarg :x           :initform 0 :accessor x)           ; x (upper left corner)
   (%y           :initarg :y           :initform 0 :accessor y)           ; y (upper left corner)
   (%width       :initarg :width       :initform 0 :accessor width)       ; x (upper left corner)
   (%height      :initarg :height      :initform 0 :accessor height))     ; y (upper left corner)
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

;; step 4 <Scroll>
;; -----------------------------------------------------------------------------------------------  
(defclass timing ()
 ((%scroll   :initarg :scroll   :initform 0   :accessor scroll)     ; scroll counter
  (%interval :initarg :interval :initform 128 :accessor interval))  ; step 9 add senternce
 (:documentation "The timing Class"))

(defgeneric Scroll-background (mapchip timing))

(defmethod Scroll-background (mapchip timing)
  "draw background"
  (setf (y mapchip) (+ -64 (mod (scroll timing) 64)))            ; scroll start from y(-64) to y(0)
  (dotimes (i 9)
    (setf (x mapchip) 0)
      (dotimes (j 8)  
        (Draw mapchip)
          (incf (x mapchip) 64))
          (incf (y mapchip) 64)))

;; step5 <Font>
;; -----------------------------------------------------------------------------------------------
(defparameter *path-font72* "C:\\WINDOWS\\Fonts\\times.ttf")
(defparameter *path-font16* "C:\\WINDOWS\\Fonts\\msmincho.ttc")
(defparameter *font72* (make-instance 'sdl:ttf-font-definition
                                :size 72
                                :filename (sdl:create-path *path-font72*)))
(defparameter *font16* (make-instance 'sdl:ttf-font-definition
                                :size 16
                                :filename (sdl:create-path *path-font16*)))
(defvar *title-font*)                                    ; title font
(defvar *menu-font*)                                     ; menu font

(defun Set-font ()
  (setf *title-font* (sdl:initialise-font *font72*)
        *menu-font*  (sdl:initialise-font *font16*)))

;; step5 <Score panel>
;; -----------------------------------------------------------------------------------------------
(defclass score ()
 ((%score     :initarg :score     :initform 0    :accessor score)     ; score counter
  (%highscore :initarg :highscore :initform 5000 :accessor highscore) ; highscore
  (%n-ship    :initarg :n-ship   :initform 3     :accessor n-ship))   ; number of ship
 (:documentation "The Score Class"))

(defgeneric Score-panel (score score-ship))

(defmethod Score-panel (score score-ship)                             ; score panel message
  "draw score and ships of rest"
  (sdl:draw-box-* 480 0 160 480 :color (sdl:color :r 64 :g 64 :b 64))
  (sdl:draw-string-solid-* (format nil "SCORE:")      496 32 :color sdl:*white* :font *menu-font*)
  (sdl:draw-string-solid-* (format nil "HIGH-SCORE:") 496 72 :color sdl:*white* :font *menu-font*)
  (sdl:draw-string-solid-* (format nil "~7,'0d" (score score))     512 52 :color sdl:*white* :font *menu-font*)
  (sdl:draw-string-solid-* (format nil "~7,'0d" (highscore score)) 512 92 :color sdl:*white* :font *menu-font*)
  (dotimes (i (- (n-ship score) 1))
    (setf (x score-ship) (+ 496 (* 40 i)))
    (Draw score-ship)))

;; step6 <Key State>
;; -----------------------------------------------------------------------------------------------
(defclass keystate ()
 ((%right :initarg :right :initform nil :accessor right)
  (%left  :initarg :left  :initform nil :accessor left)
  (%up    :initarg :up    :initform nil :accessor up)
  (%down  :initarg :down  :initform nil :accessor down)
  (%z     :initarg :z     :initform nil :accessor z))
 (:documentation "The Keystate Class"))

(defgeneric Update-keystate (key boolean keystate))

(defmethod Update-keystate (key boolean keystate)  
  (cond ((sdl:key= key :SDL-KEY-RIGHT) (setf (right keystate) boolean))
        ((sdl:key= key :SDL-KEY-LEFT)  (setf (left keystate)  boolean))
        ((sdl:key= key :SDL-KEY-UP)    (setf (up keystate)    boolean))
        ((sdl:key= key :SDL-KEY-DOWN)  (setf (down keystate)  boolean))
        ((sdl:key= key :SDL-KEY-Z)     (setf (z keystate)     boolean))))

(defgeneric Move-ship (ship keystate))

(defmethod Move-ship (ship keystate)
  (when (or (= (state ship) 1)
            (= (state ship) 3))
    (cond ((right keystate) (incf (x ship) (dx ship)))
          ((left keystate)  (decf (x ship) (dx ship)))
          ((up keystate)    (decf (y ship) (dy ship)))
          ((down keystate)  (incf (y ship) (dy ship))))))

;; step7 <Game Field>
;; -----------------------------------------------------------------------------------------------
(defclass game-field ()
 ((%field-x :initarg :field-x :initform 0 :reader field-x)
  (%field-y :initarg :field-y :initform 0 :reader field-y)
  (%width   :initarg :width :initform 480 :reader width) 
  (%height  :initarg :height :initform 480 :reader height))
 (:documentation "The Game-field Class"))

(defgeneric Fix-ship-position (ship game-field))

(defmethod Fix-ship-position (ship game-field)
  "ship always inside game-field"
  (when (< (x ship) (field-x game-field)) (setf (x ship) (field-x game-field)))
  (when (< (y ship) (field-y game-field)) (setf (y ship) (field-y game-field)))
  (when (> (x ship) (- (width game-field) 32))  (setf (x ship) (- (width game-field) 32)))
  (when (> (y ship) (- (height game-field) 32)) (setf (y ship) (- (height game-field) 32))))

;; step8 <Shot>
;; -----------------------------------------------------------------------------------------------
(defgeneric Move-shot (shot))

(defmethod Move-shot (shot)
  "shot move"
  (when (= (state shot) 1)                ; shot is alive
           (decf (y shot) (dy shot)))     ; shot 16 dot up 
  (when (< (y shot) -32)                  ; out of screen 
    (setf (state shot) 0)))               ; shot off

(defgeneric Set-shot (shot ship keystate))

(defmethod Set-shot (shot ship keystate)
  (when (and (/= (state ship) 2)                ; if ship is not explode
             (= (state shot) 0)                 ; and shot is dead
             (eql (z keystate) t))              ; and set z key                    
    (setf (y shot) (- (y ship) (height shot)))  ; set shot position 
    (setf (x shot) (x ship))
    (setf (state shot) 1)                       ; shot on
    (Play-sample *shot-sound*)                  ; shot sound
    (setf (z keystate) nil)))                   ; reset z key

;; step9 <Move Enemy>
;; -----------------------------------------------------------------------------------------------
(defparameter *graphic-height* 480)
(defparameter *graphic-width* 480)

(defclass foe ()
 ((%enemy-list      :initarg :enemy-list      :initform nil :accessor enemy-list)
  (%enemy-shot-list :initarg :enemy-shot-list :initform nil :accessor enemy-shot-list))
 (:documentation "The foe Class"))

(defgeneric Move-enemy (ship foe))

(defmethod Move-enemy (ship foe)
  (dolist (enemy (enemy-list foe))               
    (when (= (state enemy) 1)
      (incf (x enemy) (dx enemy))
      (incf (y enemy) (dy enemy))
      (incf (move-cnt enemy) 1)
      (ecase (id enemy)
        (2 (progn ; enemy type1
             (when (> (y enemy) (- *graphic-height* 32))
               (if (> (x enemy) (x ship)) 
                 (setf (dx enemy) -2)
               (setf (dx enemy) 2))
               (setf (dy enemy) -2))
             (when (or (< (x enemy) -32)
                       (> (x enemy) *graphic-width*))
               (setf (state enemy) 0))))
        (3 (progn  ; enemy type2
             (when (or (< (x enemy) -32)
                       (> (x enemy) *graphic-width*))
               (setf (state enemy) 0))))
        (4 (progn  ; enemy type3
             (when (and (< (x enemy) -32)
                        (= (dx enemy) -3))
               (setf (state enemy) 0))
             (when (and (> (x enemy) *graphic-width*)
                        (= (dx enemy) 3))
               (setf (state enemy) 0))))))))

;; Step9 <Set Enemy>
;; -----------------------------------------------------------------------------------------------
(defgeneric Set-enemy (timing foe))

(defmethod Set-enemy (timing foe)
  "enemy appear position set"
  (when (= (mod (scroll timing) (interval timing)) 0)
    (let ((enemy (make-instance 'entity :id (+ (random 3) 2) :state 0)))
          (push enemy (enemy-list foe)))
    (dolist (enemy (enemy-list foe))
      (when (= (state enemy) 0) 
        (setf (state enemy) 1)                               ; set enemy state alive
        (ecase (id enemy)
          (2 (progn                                          ; enemy type1 appears from top
               (setf (x enemy) (random 448)) 
               (setf (y enemy) 0)
               (setf (dx enemy) 0)
               (setf (dy enemy) 2)))
          (3 (progn                                          ; enemy type2 appears from bottom
               (setf (x enemy) (random 448))
               (setf (y enemy) *graphic-height*)
               (if (> (x enemy) (- (/ *graphic-width* 2) 16))  
                 (setf (dx enemy) -2)                        ; if enemy type2 appears right half 
                 (setf (dx enemy) 2))                        ; if enemy type3 appears left half
                 (setf (dy enemy) -1)))
          (4 (progn                                          ; enemy type3 appears from left or right 
               (ecase (random 2) 
                 (0 (progn                                   ; from left  
                      (setf (x enemy) -32)
                      (setf (dx enemy) 3)))
                 (1 (progn                                   ; from right
                      (setf (x enemy) *graphic-width*)
                      (setf (dx enemy) -3)
                      (setf (y enemy) (random (/ *graphic-height* 2)))
                      (setf (dy enemy) 1)))))))))))

;; Step9 <Remove Dead Enemy>
;; -----------------------------------------------------------------------------------------------
(defgeneric Remove-dead-enemy (foe))

(defmethod Remove-dead-enemy (foe)
  "dead enemy remove from list"
  (setf (enemy-list foe)
	(delete-if #'(lambda (enemy) (= (state enemy) 0)) (enemy-list foe))))

;; Step 4 <Scroll Counter>
;; -----------------------------------------------------------------------------------------------
(defgeneric Scroll-counter (timing))

(defmethod Scroll-counter (timing)
  (incf (scroll timing))
  (when (= (scroll timing) 3072)                           ; mapchip 96 pieces move 
    (when (> (interval timing) 32)
      (decf (interval timing) 32))))

;; step1 <Game Frame>
;; -----------------------------------------------------------------------------------------------
(defun Common-shooter ()
  "main routine"
  (sdl:with-init (sdl:sdl-init-video sdl:sdl-init-audio) ; use video and audio
    (sdl:window 640 480 :position #(192 50)              ; size 640*480, position x(192) y(50)
                        :title-caption "THE SHOOTER"
                        :icon-caption "THE SHOOTER"
                        :double-buffer T)
                    ;   :fullscreen T)
      ; step 3
      (Initialize)                                       ; graphics initialize
      ; step 5
      (Set-font)                                         ; set font 
      ; step 2 
      (Open-sound)                                       ; open audio and load sound data 
      ; step 2
      (Play-music *bg-music*)                            ; play music
      ; step 3 - 9
      (let((ship (make-instance 'entity :imageid 0 :id 0 :x 224 :y 416 :width 32 :height 32 :dx 4 :dy 4 :state 1))   
           (mapchip (make-instance 'object :imageid 1 :id 0))
           (timing (make-instance 'timing))              ; step4 add sentence
           (score-ship (make-instance 'object :imageid 0 :id 0 :x 496 :y 128))        ;step5 add sentence
           (score (make-instance 'score))                ; step5 add sentence
           (keystate (make-instance 'keystate))          ; step6 add sentence
           (game-field (make-instance 'game-field))      ; step7 add sentence
           (shot (make-instance 'entity :imageid 0 :height 32 :dy 16 :id 1 :state 0)) ; step8 add sentence
           (foe (make-instance 'foe)))                   ; step9 add sentence 

      (sdl:update-display)
      (sdl:with-events (:poll)
        (:quit-event ()
          ; step 2
          (Stop-sound)                                   ; sample and music stop
          (Close-sound)                                  ; close audio
          t)
        ; step 6
        (:key-down-event (:key key)
          (if (sdl:key= key :SDL-KEY-ESCAPE)
              (sdl:push-quit-event)
	  (Update-keystate key t keystate)))
        (:key-up-event (:key key)
          (Update-keystate key nil keystate))

        (:idle ()
          ; Game body                  

          ; step 6
          (Move-ship ship keystate)

          ; step 7
          (Fix-ship-position ship game-field)

          ; step 8
          (Move-shot shot)
          (Set-shot shot ship keystate)

          ; step 4
          (Scroll-background mapchip timing)                    ; scroll background       

          ; step 3
          (when (= (state ship) 1)
            (Draw ship))                                 ; draw ship

          ; step 8
          (when (= (state shot) 1)
            (Draw shot))                                 ; draw shot

          ; step 9
          (Move-enemy ship foe)
          (Set-enemy timing foe)
          (dolist (enemy (enemy-list foe))
            (when (= (state enemy) 1)
              (Draw enemy)))                             ; draw enemy
          (Remove-dead-enemy foe)            
          
          ; step 5
          (Score-panel score score-ship)                       ; draw score panel 

          ; step 4
          (Scroll-counter timing)

          (sdl:update-display))))))

(Common-shooter)

