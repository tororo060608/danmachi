(in-package :danmachi)

(define-class game ()
  (window-width 640)
  (window-height 480)
  object-list
  (keystate (make-instance 'keystate)))

(defgeneric add-object (obj game))

(defun update-game (game)
  (mapc (lambda (obj) (update obj game)) (object-list game)))

(defun draw-game (game)
  (mapc (lambda (obj) (draw obj game)) (object-list game)))

;;orginal object
(define-class gameobject ()
  point-x
  point-y
  (vx 0)
  (vy 0)
  width
  height
  image)

; add new object
(defmethod add-object ((obj gameobject) (game game))
  (push obj (object-list game)))

;draw object
(defgeneric draw (obj game))
(defmethod draw ((object gameobject) (game game))
    (sdl:draw-surface-at-* (image object)
			   (round (point-x object))
			   (round (point-y object))))


(defmethod update ((object gameobject) (game game))
  (incf (point-x object) (vx object))
  (incf (point-y object) (vy object)))

;;player object
(define-class player (gameobject)
  (player-speed 5))

(defmethod update ((p player) (game game))
  (with-accessors ((vx vx) (vy vy) (x point-x) (y point-y)
		   (speed player-speed)) p
    (with-slots (up down right left) (keystate game)
      (cond ((key-pressed-p right) (setf vx speed))
	    ((key-pressed-p left)  (setf vx (- speed)))
	    (t (setf vx 0)))
      (cond ((key-pressed-p up)   (setf vy (- speed)))
	    ((key-pressed-p down) (setf vy speed))
	    (t (setf vy 0))))
    ;; slanting move
    (when (and (/= vx 0) (/= vy 0))
      (setf vx (/ vx (sqrt 2))
	    vy (/ vy (sqrt 2)))))
  (call-next-method))

;;wall object
(define-class game-wall (gameobject))

;;floor object
(define-class game-floor (gameobject))
