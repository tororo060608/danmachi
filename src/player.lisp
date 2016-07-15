(in-package :danmachi)

;;player object
(define-class player (gamecharacter)
  (player-speed 5)
  (width 32)
  (height 32)
  (image (get-image :player)))

(defmethod update ((p player) (game game))
  (with-accessors ((vx vx) (vy vy) (x point-x) (y point-y)
		   (speed player-speed)) p
    (with-slots (up down right left z) (keystate game)
      (cond ((key-pressed-p right) (setf vx speed))
	    ((key-pressed-p left)  (setf vx (- speed)))
	    (t (setf vx 0)))
      (cond ((key-pressed-p up)   (setf vy (- speed)))
	    ((key-pressed-p down) (setf vy speed))
	    (t (setf vy 0)))
      (cond ((key-down-p z) (attack p game))))
    
    ;; slanting move
    (when (and (/= vx 0) (/= vy 0))
      (setf vx (/ vx (sqrt 2))
	    vy (/ vy (sqrt 2)))))
  (call-next-method))

(define-class player-attack (bullet)
  (width 16)
  (height 32)
  (atk 50)
  (time-limit (make-timer 10)))

(defmethod update ((patk player-attack) (game game))
  (call-next-method)
  (when (funcall (time-limit patk))
    (kill patk)))

(defmethod attack ((p player) (game game))
  (add-object (make-instance 'player-attack
			     :vx 0
			     :vy 0
			     :point-x (point-x p)
			     :point-y (+ (point-y p) (height p)))
	      game))
