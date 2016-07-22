(in-package :danmachi)

(define-class game ()
  (window-width 640)
  (window-height 480)
  (map-width 0)
  (map-height 0)
  object-list
  floor-list
  player
  (camera (list 0 0))
  (keystate (make-instance 'keystate)))

(defgeneric add-object (obj game))

;;orginal object
(define-class gameobject ()
  point-x
  point-y
  (alive t)
  (vx 0)
  (vy 0)
  width
  height
  (atk 0)
  image)

(defun kill (obj)
  (setf (alive obj) nil))

(defgeneric add-object (obj game))
(defun update-game (game)
  (mapc (lambda (obj) (update obj game)) (object-list game))
  (setf (object-list game)
	(remove-if-not #'alive (object-list game))))

(defun draw-game (game)
  (mapc (lambda (obj) (draw obj game)) (floor-list game))
  (mapc (lambda (obj) (draw obj game)) (object-list game)))

; add new object
(defmethod add-object ((obj gameobject) (game game))
  (push obj (object-list game)))

;draw object
(defgeneric draw (obj game))
(defmethod draw ((object gameobject) (game game))
  (if (image object)
      (sdl:draw-surface-at-* (image object)
			     (round (x-in-camera
				     (point-x object)
				     game))
			     (round (y-in-camera
				     (point-y object)
				     game)))
      (sdl:draw-box-* (round (x-in-camera
			      (point-x object)
			      game))
		      (round (y-in-camera
			      (point-y object)
			      game))
		      (width object)
		      (height object)
		      :color sdl:*blue*)))

(defmethod update ((object gameobject) (game game))
	(incf (point-x object) (vx object))
	(incf	(point-y object) (vy object)))

;out-gamearea detect
(defun out-of-gamearea-p (object game)
  (let* ((margin 50)
	 (left (- margin))
	 (right (+ (window-width game) margin))
	 (top (- margin))
	 (bottom (+ (window-height game) margin)))
    (not (and (<= left 
		  (x-in-camera (point-x object) game)
		  right)
	      (<= top 
		  (y-in-camera (point-y object) game)
		  bottom)))))

;;wall object
(define-class game-wall (gameobject)
  (width 32)
  (height 32)
  (image (get-image :wall)))

;;floor object
(define-class game-floor (gameobject)
  (width 32)
  (height 32)
  (image (get-image :floor)))

(defmethod add-object ((floor game-floor) (game game))
  (push floor (floor-list game)))

;bullet base class
(define-class bullet (gameobject))

(defmethod update ((b bullet) (game game))
  (call-next-method)
  (when (out-of-gamearea-p b game)
    (kill b)))
