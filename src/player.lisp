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

