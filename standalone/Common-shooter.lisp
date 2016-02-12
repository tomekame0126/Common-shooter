;;;; The Common-Shooter

;;; step1  <Game Frame>
;;; step2  <Audio>
;;; step3  <Sprite Sheets> <Character Object> <Draw Images> <Initialize>
;;; step4  <Scroll> <Scroll Counter>
;;; step5  <Font> <Score Panel>
;;; step6  <Key State>
;;; step7  <Game Field>
;;; step8  <Shot>
;;; step9  <Move Enemy> <Set Enemy> <Remove Dead Eenemy>
;;; step10 <Move Enemy Shot> <Set Enemy Shot> <Remove Dead Enemy Shot>
;;; step11 <Judge Enemy Shot Hit> <Game Over Message> <Explode Ship> <Revive Ship>
;;; step12 <Judge Shot Hit> <Explode enemy>
;;; step13 <Stage Class> <Stage Start Message>
;;; step14 <Game Start Message>
;;; step15 <Define package>
;;; step16 <Macro>

;; step15 <Define package>
;; -----------------------------------------------------------------------------------------------
(defpackage :game
  (:use :common-lisp :lispbuilder-sdl)
  (:nicknames :shooting)
  (:export :Common-shooter))

(in-package :game)

;; step16 <Macro>
;; -----------------------------------------------------------------------------------------------
(defmacro define-class (name superclasses slots form)
  `(defclass ,name ,superclasses
    ,(mapcar (lambda (slot)
               (let ((keyword (intern (symbol-name slot) :keyword)))
               `(,slot :initarg ,keyword :initform ,form :accessor ,slot)))
              slots)))

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
;(defclass object ()
;  ((%imageid     :initarg :imageid     :initform 0 :accessor imageid)     ; 3 imageid available
;   (%id          :initarg :id          :initform 0 :accessor id)          ; graphic id in imageid
;   (%x           :initarg :x           :initform 0 :accessor x)           ; x (upper left corner)
;   (%y           :initarg :y           :initform 0 :accessor y)           ; y (upper left corner)
;   (%width       :initarg :width       :initform 0 :accessor width)       ; width (from upper left corner)
;   (%height      :initarg :height      :initform 0 :accessor height))     ; height (from upper left corner)
;  (:documentation "The Object Class"))
(define-class object ()
  (imageid id x y width height) 0)

;(defclass entity (object)
;  ((%dx          :initarg :dx          :initform 0 :accessor dx)          ; x direction speed
;   (%dy          :initarg :dy          :initform 0 :accessor dy)          ; y direction speed
;   (%move-cnt    :initarg :move-cnt    :initform 0 :accessor move-cnt)    ; moving counter(distance)
;   (%revival-cnt :initarg :revival-cnt :initform 0 :accessor revival-cnt) ; revival counter(wait)
;   (%explode-cnt :initarg :explode-cnt :initform 0 :accessor explode-cnt) ; explosion counter(wait)
;   (%state       :initarg :state       :initform 0 :accessor state))      ; 0:dead 1:alive 2:explosion 3:revival
;  (:documentation "The Entity Class"))
(define-class entity (object)
  (dx dy move-cnt revival-cnt explode-cnt state) 0)

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
;(defclass timing ()
; ((%scroll   :initarg :scroll   :initform 0   :accessor scroll)     ; scroll counter
;  (%interval :initarg :interval :initform 128 :accessor interval))  ; step 9 add senternce
; (:documentation "The timing Class"))
(define-class timing ()
  (scroll interval) 0)

(defgeneric Scroll-background (mapchip timing))

(defmethod Scroll-background (mapchip timing)
  "draw background"
  (setf (y mapchip) (+ -64 (mod (scroll timing) 64)))               ; scroll start from y(-64) to y(0)
  (dotimes (i 9)
    (setf (x mapchip) 0)
      (dotimes (j 8)  
        (Draw mapchip)
          (incf (x mapchip) (width mapchip)))
          (incf (y mapchip) (height mapchip))))

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
;(defclass score ()
; ((%score     :initarg :score     :initform 0    :accessor score)     ; score counter
;  (%highscore :initarg :highscore :initform 5000 :accessor highscore) ; highscore
;  (%n-ship    :initarg :n-ship    :initform 3     :accessor n-ship))   ; number of ship
; (:documentation "The Score Class"))
(define-class score ()
  (score highscore n-ship) 0)

(defgeneric Score-panel (score score-ship))

(defmethod Score-panel (score score-ship)                             ; score panel message
  "draw score and ships of rest"
  (sdl:draw-box-* 480 0 160 480 :color (sdl:color :r 64 :g 64 :b 64))
  (sdl:draw-string-solid-* (format nil "SCORE:")      496 32 :color sdl:*white* :font *menu-font*)
  (sdl:draw-string-solid-* (format nil "HIGH-SCORE:") 496 72 :color sdl:*white* :font *menu-font*)
  (sdl:draw-string-solid-* (format nil "~7,'0d" (score score))     512 52 :color sdl:*white* :font *menu-font*)
  (sdl:draw-string-solid-* (format nil "~7,'0d" (highscore score)) 512 92 :color sdl:*white* :font *menu-font*)
  (dotimes (i (- (n-ship score) 1))
    (setf (x score-ship) (+ 496 (* (+ (width score-ship) 8) i)))  ; 8 is an interval between ship
    (Draw score-ship)))

;; step6 <Key State>
;; -----------------------------------------------------------------------------------------------
;(defclass keystate ()
; ((%right :initarg :right :initform nil :accessor right)
;  (%left  :initarg :left  :initform nil :accessor left)
;  (%up    :initarg :up    :initform nil :accessor up)
;  (%down  :initarg :down  :initform nil :accessor down)
;  (%z     :initarg :z     :initform nil :accessor z))
; (:documentation "The Keystate Class"))
(define-class keystate ()
  (right left up down z) nil)

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
;(defclass game-field ()
; ((%field-x :initarg :field-x :initform 0 :reader field-x)
;  (%field-y :initarg :field-y :initform 0 :reader field-y)
;  (%width   :initarg :width :initform 480 :reader width) 
;  (%height  :initarg :height :initform 480 :reader height))
; (:documentation "The Game-field Class"))
(define-class game-field ()
  (field-x field-y width height) 0)

(defgeneric Fix-ship-position (ship game-field))

(defmethod Fix-ship-position (ship game-field)
  "ship always inside game-field"
  (when (< (x ship) (field-x game-field)) (setf (x ship) (field-x game-field)))
  (when (< (y ship) (field-y game-field)) (setf (y ship) (field-y game-field)))
  (when (> (x ship) (- (width game-field) (width ship)))   (setf (x ship) (- (width game-field) (width ship))))
  (when (> (y ship) (- (height game-field) (height ship))) (setf (y ship) (- (height game-field) (height ship)))))

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
    (setf (y shot) (- (y ship) (height shot))   ; set shot position 
          (x shot) (x ship)
          (state shot) 1)                       ; shot on
    (Play-sample *shot-sound*)                  ; shot sound
    (setf (z keystate) nil)))                   ; reset z key

;; step9 <Move Enemy>
;; -----------------------------------------------------------------------------------------------
;(defclass foe ()
; ((%enemy-list      :initarg :enemy-list      :initform nil :accessor enemy-list)
;  (%enemy-shot-list :initarg :enemy-shot-list :initform nil :accessor enemy-shot-list))
; (:documentation "The foe Class"))
(define-class foe ()
  (enemy-list enemy-shot-list) nil)

(defgeneric Move-enemy (ship foe game-field))

(defmethod Move-enemy (ship foe game-field)
  (dolist (enemy (enemy-list foe))               
    (when (= (state enemy) 1)
      (incf (x enemy) (dx enemy))
      (incf (y enemy) (dy enemy))
      (incf (move-cnt enemy) 1)
      (ecase (id enemy)
        (2 (progn ; enemy type1
             (when (> (y enemy) (- (height game-field) (height ship)))
               (if (> (x enemy) (x ship)) 
                 (setf (dx enemy) -2)
               (setf (dx enemy) 2))
               (setf (dy enemy) -2))
             (when (or (< (x enemy) (- (width enemy)))
                       (> (x enemy) (width game-field)))
               (setf (state enemy) 0))))
        (3 (progn  ; enemy type2
             (when (or (< (x enemy) (- (width enemy)))
                       (> (x enemy) (width game-field)))
               (setf (state enemy) 0))))
        (4 (progn  ; enemy type3
             (when (and (< (x enemy) (- (width enemy)))
                        (= (dx enemy) -3))
               (setf (state enemy) 0))
             (when (and (> (x enemy) (width game-field))
                        (= (dx enemy) 3))
               (setf (state enemy) 0))))))))

;; Step9 <Set Enemy>
;; -----------------------------------------------------------------------------------------------
(defparameter *enemy-max* 8)         ; enemy maximum number

(defgeneric Set-enemy (timing foe game-field))

(defmethod Set-enemy (timing foe game-field)
  "enemy appear position set"
  (when (= (mod (scroll timing) (interval timing)) 0)
    (when (< (length (enemy-list foe)) *enemy-max*)
      (let ((enemy (make-instance 'entity :id (+ (random 3) 2) :width 32 :height 32 :state 1)))            
        (ecase (id enemy)
          (2 (progn                                          ; enemy type1 appears from top
               (setf (x enemy) (random 448) 
                     (y enemy) 0
                     (dx enemy) 0
                     (dy enemy) 2)))
          (3 (progn                                          ; enemy type2 appears from bottom
               (setf (x enemy) (random 448)
                     (y enemy) (height game-field))
               (if (> (x enemy) (- (/ (width game-field) 2) 16))
                 (setf (dx enemy) -2)                        ; if enemy type2 appears right half 
                 (setf (dx enemy) 2))                        ; if enemy type3 appears left half
               (setf (dy enemy) -1)))
          (4 (progn                                          ; enemy type3 appears from left or right 
               (ecase (random 2) 
                 (0 (progn                                   ; from left  
                      (setf (x enemy) -32
                            (dx enemy) 3)))
                 (1 (progn                                   ; from right
                      (setf (x enemy) (width game-field)
                            (dx enemy) -3
                            (y enemy) (random (/ (height game-field) 2))
                            (dy enemy) 1)))))))
        (push enemy (enemy-list foe))))))

;; Step9 <Remove Dead Enemy>
;; -----------------------------------------------------------------------------------------------
(defgeneric Remove-dead-enemy (foe))

(defmethod Remove-dead-enemy (foe)
  "dead enemy remove from list"
  (setf (enemy-list foe)
	(delete-if #'(lambda (enemy) (= (state enemy) 0)) (enemy-list foe))))

;; Step10 <Move Enemy Shot>
;; -----------------------------------------------------------------------------------------------
(defgeneric Move-enemy-shot (foe game-field))

(defmethod Move-enemy-shot (foe game-field)
  "enemy shot move"
  (dolist (enemy-shot (enemy-shot-list foe))               
    (when (= (state enemy-shot) 1)           ; set enemy-shot state alive 
      (incf (x enemy-shot) (dx enemy-shot))
      (incf (y enemy-shot) (dy enemy-shot))
      (when (or (< (x enemy-shot) -16)
                (> (x enemy-shot) (width game-field))
                (< (y enemy-shot) -16)
                (> (y enemy-shot) (height game-field)))
        (setf (state enemy-shot) 0)))))

;; Step10 <Set Enemy Shot>
;; -----------------------------------------------------------------------------------------------
(defparameter *enemy-shot-max* 4)  ; enemy-shot maximum number

(defgeneric Set-enemy-shot (foe game-field))

(defmethod Set-enemy-shot (foe game-field)
  "enemy shot appear position set and move"  
  (dolist (enemy (enemy-list foe)) 
    (when (and (= (state enemy) 1)
               (> (x enemy) 0)
               (< (x enemy) (- (width game-field) (width enemy)))
               (> (y enemy) 0)
               (< (y enemy) (- (height game-field) (height enemy)))
               (= (mod (move-cnt enemy) 64) 0))
       (when (< (length (enemy-shot-list foe)) *enemy-shot-max*)                      
         (let ((enemy-shot (make-instance 'entity :imageid 2 :id 1 :state 1)))               
           (setf (x enemy-shot) (+ (x enemy) 8)         ; enemy shot position is center of enemy
                 (y enemy-shot) (+ (y enemy) 8))        ; 8 is position of enemy shot into enemy
           (ecase (id enemy)
	     (2 (progn 
		  (setf (dx enemy-shot) 0
		        (dy enemy-shot) 6)))
	     (3 (progn   
		  (setf (dx enemy-shot) 0
		        (dy enemy-shot) -6)))
	     (4 (progn
		  (if (= (dx enemy) -3)            
		      (setf (dx enemy-shot) -6)
		      (setf (dx enemy-shot) 6))
		  (setf (dy enemy-shot) 0))))
	   (push enemy-shot (enemy-shot-list foe))))))) 

;; Step10 <Remove Dead Enemy Shot>
;; -----------------------------------------------------------------------------------------------
(defgeneric Remove-dead-enemy-shot (foe))

(defmethod Remove-dead-enemy-shot (foe)
  "dead enemy shot remove from list"
  (setf (enemy-shot-list foe) 
	(delete-if #'(lambda (enemy-shot) (= (state enemy-shot) 0)) (enemy-shot-list foe))))

;; Step11 <Judge Enemy Shot Hit>
;; -----------------------------------------------------------------------------------------------
(defgeneric Ship-hit-p (ship foe))

(defmethod Ship-hit-p (ship foe)
  (when (= (state ship) 1)                              ; if ship is alive
    (let ((hit 0))
      (dolist (enemy (enemy-list foe))
        (when (and (= (state enemy) 1)                  ; enemies and ship hit 
                   (> (+ (x ship) 16) (x enemy))        ; 16 is center of ship
                   (< (+ (x ship) 16) (+ (x enemy) (width enemy)))
                   (> (+ (y ship) 16) (y enemy))
                   (< (+ (y ship) 16) (+ (y enemy) (height enemy))))
              (setf (state enemy) 0
                     hit 1)))
       (dolist (enemy-shot (enemy-shot-list foe))
         (when (and (= (state enemy-shot) 1)            ; enemies shot and ship hit
                    (> (+ (x ship) 16) (x enemy-shot))
                    (< (+ (x ship) 16) (+ (x enemy-shot) 16))
                    (> (+ (y ship) 16) (y enemy-shot))
                    (< (+ (y ship) 16) (+ (y enemy-shot) 16)))
               (setf (state enemy-shot) 0
                      hit 1)))
       (when (= hit 1) 
         (setf (state ship) 2                           ; ship is explode
         (explode-cnt ship) 0)))))                      ; ship explode count 0

;; Step11 <Explode Ship>
;; ----------------------------------------------------------------------------------------------- 
(defvar *shipbom-sound-flag* nil)
(defgeneric Explode-counter (ship score game-field))

(defmethod Explode-counter (ship score game-field)     
  (when (= (state ship) 2)                    ; ship is explode
    (when (eql *shipbom-sound-flag* nil)
      (setf *shipbom-sound-flag* t) 
      (Play-sample *shipbom-sound*))          ; shipbom sound    
    (incf (explode-cnt ship) 1)
    (when (= (explode-cnt ship) 100)
      (decf (n-ship score) 1)
      (when (> (n-ship score) 0)
        (setf (state ship) 3                  ; set ship revival
              *shipbom-sound-flag* nil
	      (revival-cnt ship) 0
	      (x ship) (/ (- (width game-field) (width ship)) 2)
	      (y ship) (- (height game-field) (* (height ship) 2)))))))
           
;; Step11 <Revive Ship>
;; ----------------------------------------------------------------------------------------------- 
(defgeneric Revive-counter (ship))

(defmethod Revive-counter (ship)
  (when (= (state ship) 3)                       ; ship is revival
    (incf (revival-cnt ship) 1)
    (when (= (revival-cnt ship) 200)
      (setf (state ship) 1))))

;; Step12 <Judge Shot Hit>
;; -----------------------------------------------------------------------------------------------
(defgeneric Enemy-hit-p (shot score foe))

(defmethod Enemy-hit-p (shot score foe)
  (when (= (state shot) 1)                       ; if shot is on                                  
    (dolist (enemy (enemy-list foe))                
      (when (and (= (state enemy) 1)             ; if enemy is alive 
                 (> (+ (x shot) (width shot)) (x enemy))
                 (< (x shot) (+ (x enemy) (width enemy)))
                 (> (+ (y shot) (height shot)) (y enemy))
                 (< (y shot) (+ (y enemy) (height enemy))))
        (setf (state enemy) 2                    ; enemy is explosion
              (explode-cnt enemy) 0
              (state shot) 0)                    ; shot is off
              (incf (score score) 100)))
  (when (> (score score) (highscore score))
        (setf (highscore score) (score score)))))

;; Step12 <Explode enemy>
;; -----------------------------------------------------------------------------------------------
(defvar *enemybom-sound-flag* nil)
(defgeneric Explode-enemy (foe))

(defmethod Explode-enemy (foe)
  "enemy explosion while 16 times loop"
  (dolist (enemy (enemy-list foe))
    (when (= (state enemy) 2)
      (when (eql *enemybom-sound-flag* nil)
        (setf *enemybom-sound-flag* t) 
        (Play-sample *enemybom-sound*))         ; enemybom sound        
      (incf (explode-cnt enemy) 1)
      (when (= (explode-cnt enemy) 16)          ; enemy explode count 16
        (setf (state enemy) 0
              *enemybom-sound-flag* nil)))))

;; Step13 <Stage Class>  <----------- step 14 correct
;; -----------------------------------------------------------------------------------------------
(defclass stage ()
 ((%stage-flag   :initarg :stage-flage  :initform t :accessor stage-flag)
  (%stage-number :initarg :stage-number :initform 0 :accessor stage-number)
  (%title-loop   :initarg :title-loop   :initform t :accessor title-loop)
  (%start        :initarg :start        :initform t :accessor start))
 (:documentation "The Stage Class"))

;; Step 4 <Scroll Counter> <--------- step 13 correct
;; -----------------------------------------------------------------------------------------------
(defgeneric Scroll-counter (stage timing))

(defmethod Scroll-counter (stage timing)
  (incf (scroll timing))
  (when (= (scroll timing) 3072)                           ; mapchip 96 pieces move 
    (when (> (interval timing) 32)
      (decf (interval timing) 32))
      (setf (stage-flag stage) t)))                        ; step 13 add sentence

;; Step13 <Start Stage Message>
;; -----------------------------------------------------------------------------------------------
(defgeneric Stage-start-message (stage timing foe))

(defmethod Stage-start-message (stage timing foe)          ; stage start message
  "Draw stage start message and set game parameters"
  (when (eql (stage-flag stage) t)
    (stop-sound)                                           ; BGM stop
    (setf (stage-flag stage) nil)
    (incf (stage-number stage) 1)
    (sdl:clear-display sdl:*black*)
    (sdl:draw-string-solid-* 
         (format nil "STAGE ~d" (stage-number stage)) 296 232 :color sdl:*white* :font *menu-font*)
    (sdl:update-display)
    (sleep 3)
    (Play-music *bg-music*)                                ; BGM start
    (setf (scroll timing) 0
	  (enemy-list foe) nil
	  (enemy-shot-list foe) nil)))

;; Step14 <Start Game Message>
;; -----------------------------------------------------------------------------------------------
(defgeneric Game-start-message (pointer stage keystate))

(defmethod Game-start-message (pointer stage keystate)             ; game start message
  "Draw game opening message"
  (sdl:draw-string-solid-* "THE SHOOTER" 80 128 :color sdl:*cyan* :font *title-font*)
  (sdl:draw-string-solid-* "START" 288 320 :color sdl:*white* :font *menu-font*)
  (sdl:draw-string-solid-* "EXIT" 288 352 :color sdl:*white* :font *menu-font* )
  (cond ((up keystate) 
          (decf (y pointer) 32)
          (setf (start stage) t)
          (when (< (y pointer) 320)                                ; y:320 is START position
            (setf (y pointer) 320)))
        ((down keystate)
          (incf (y pointer) 32)
          (setf (start stage) nil)
          (when (> (y pointer) 352)                                ; y:352 is EXIT position
            (setf (y pointer) 352))))
  (Draw pointer)
  (sdl:update-display)
  (cond ((and (z keystate) (eql (start stage) t))                  ; start position
          (setf (title-loop stage) nil
                (z keystate) nil))                                 ; z key state reset 
        ((and (z keystate) (eql (start stage) nil))                ; exit position                                
          (sdl:push-quit-event))))

;; Step11 <Game Over Message>  <------ step 14 correct
;; ----------------------------------------------------------------------------------------------- 
(defgeneric Game-over-message (score stage timing ship foe game-field))

(defmethod Game-over-message (score stage timing ship foe game-field)   ; message draw
  "Draw game over message"
  (when (= (n-ship score) 0)
    (stop-sound)                                                        ; BGM stop
    (sdl:draw-string-solid-* "GAME OVER" 208 232 :color sdl:*white* :font *menu-font*)
    (sdl:update-display)
    (sleep 5)
    (setf (title-loop stage) t
          (stage-flag stage) t
          (stage-number stage) 0
          (score score) 0
       ;   (highscore score) 5000                                       ; highscore reset
          (n-ship score) 3
          *shipbom-sound-flag* nil
          (interval timing) 128
          (state ship) 1
          (x ship) (/ (- (width game-field) (width ship)) 2)
          (y ship) (- (height game-field) (* (height ship) 2))
          (enemy-list foe) nil
          (enemy-shot-list foe) nil)))

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

      ; step 3 - 14
      (let((ship (make-instance 'entity :imageid 0 :id 0 :x 224 :y 416 :width 32 :height 32 :dx 4 :dy 4 :state 1))   
           (mapchip (make-instance 'object :imageid 1 :id 0 :width 64 :height 64))
           (timing (make-instance 'timing :interval 128))                                        ; step4 add sentence
           (score-ship (make-instance 'object :imageid 0 :id 0 :x 496 :y 128 :width 32))         ; step5 add sentence
           (score (make-instance 'score :highscore 5000 :n-ship 3))                              ; step5 add sentence
           (keystate (make-instance 'keystate))                                                  ; step6 add sentence
           (game-field (make-instance 'game-field :width 480 :height 480))                       ; step7 add sentence
           (shot (make-instance 'entity :imageid 0 :id 1 :width 32 :height 32 :dy 16 :state 0))  ; step8 add sentence
           (foe (make-instance 'foe))                                                            ; step9 add sentence 
           (explosion (make-instance 'object :imageid 1 :id 2))                                  ; step11 add sentence
           (enemy-explosion (make-instance 'object :imageid 1 :id 1))                            ; step12 add sentence
           (stage (make-instance 'stage))                                                        ; step13 add sentence
           (pointer (make-instance 'object :imageid 2 :id 0 :x 272 :y 320)))                     ; step14 add sentence

      (sdl:update-display)
      (sdl:with-events (:poll)
        (:quit-event ()

          ; step 2
          (Stop-sound)                                    ; sample and music stop
          (Close-sound)                                   ; close audio
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
          (sdl:clear-display sdl:*black*)
 
          ; step 14
	  (when (eql (title-loop stage) t)                ; title loop
            (sdl:clear-display sdl:*black*)
            (Game-start-message pointer stage keystate))

          ; step 14
          (when (eql (title-loop stage) nil)              ; game loop              
            (sdl:clear-display sdl:*black*)

	    ; step 13
	    (Stage-start-message stage timing foe)                  

	    ; step 6
	    (Move-ship ship keystate)

            ; step 7
	    (Fix-ship-position ship game-field)

	    ; step 8
	    (Move-shot shot)
	    (Set-shot shot ship keystate)

            ; step 4
	    (Scroll-background mapchip timing)             ; scroll background       

            ; step 3
	    (when (= (state ship) 1)
	      (Draw ship))                                 ; draw ship

            ; step 8
	    (when (= (state shot) 1)
	      (Draw shot))                                 ; draw shot

            ; step 9
	    (Move-enemy ship foe game-field)
	    (Set-enemy timing foe game-field)
	    (dolist (enemy (enemy-list foe))
	      (when (= (state enemy) 1)
		(Draw enemy)))                             ; draw enemy
	    (Remove-dead-enemy foe)            

            ; step 10
	    (Move-enemy-shot foe game-field)
	    (Set-enemy-shot foe game-field)
	    (dolist (enemy-shot (enemy-shot-list foe))
	      (when (= (state enemy-shot) 1)
		(Draw enemy-shot)))                        ; draw enemy shot
	    (Remove-dead-enemy-shot foe)

            ; step 11
	    (Ship-hit-p ship foe)
	    (Explode-counter ship score game-field)
	    (when (= (state ship) 2)               
	      (setf (x explosion) (- (x ship) 16)
		    (y explosion) (- (y ship) 16))
	      (Draw explosion))                            ; draw ship explosion
          
	    (Revive-counter ship)         
	    (when (and (= (state ship) 3)
		       (> 5 (mod (revival-cnt ship) 10)))
	      (Draw ship))                                 ; draw ship revival
          
            ; step 12
	    (Enemy-hit-p shot score foe)
	    (Explode-enemy foe)
	    (dolist (enemy (enemy-list foe))
	      (when (= (state enemy) 2)
		(setf (x enemy-explosion) (- (x enemy) 16)
		      (y enemy-explosion) (- (y enemy) 16))
		(Draw enemy-explosion)))                   ; draw enemy explosion
          
            ; step 5
	    (Score-panel score score-ship)                 ; draw score panel 

            ; step 4
	    (Scroll-counter stage timing)                  ; step 13 correct

            ; step 11
	    (Game-over-message score stage timing ship foe game-field)

	    (sdl:update-display)))))))

;(Common-shooter)

