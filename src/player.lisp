(in-package :danmachi)

;;player object
(define-class player (gamecharacter)
  (player-speed 5)
  (width 64)
  (height 64)
  (player-state nil)
  (atk-stuck (make-timer 10))
  (atk-time-limit (make-timer 10))
  (image (get-image :player_front)))

(defmethod add-object ((p player) (game game))
  (if (player game)
      (with-accessors ((x point-x) (y point-y)) (player game)
	(setf x (point-x p)
	      y (point-y p))
	(pushnew (player game) (object-list game)))
      (progn
	(setf (player game) p)
	(call-next-method))))

(defmethod update ((p player) (game game))
  (cond ((eq (player-state p) 'atk-s) (when (funcall (atk-stuck p))
					(attack p game)
					(setf (player-state p) 'atk-now)))
	((eq (player-state p) 'atk-now) (when (funcall (atk-time-limit p))
					  (setf (player-state p) 'atk-e)))
	((eq (player-state p) 'atk-e) (when (funcall (atk-stuck p))
					(setf (player-state p) nil)))
	(t (with-accessors ((vx vx) (vy vy) (x point-x) (y point-y)
			    (speed player-speed)) p
	     (with-slots (up down right left z) (keystate game)
	       (cond ((key-pressed-p right) (setf vx speed))
		     ((key-pressed-p left)  (setf vx (- speed)))
		     (t (setf vx 0)))
	       (cond ((key-pressed-p up)   (setf vy (- speed)))
		     ((key-pressed-p down) (setf vy speed))
		     (t (setf vy 0)))
	       (cond ((key-down-p z) (atk-start p game))))
	
	     ;; slanting move
	     (when (and (/= vx 0) (/= vy 0))
	       (setf vx (/ vx (sqrt 2))
		     vy (/ vy (sqrt 2)))))))
	   
  (call-next-method))
  
(defmethod attack ((p player) (game game))
  (add-object (make-instance 'player-attack) game))

(define-class player-attack (bullet)
  (width 64)
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


(defmethod atk-start ((p player) (game game))
  (setf (player-state p) 'atk-s
	(vx p) 0
	(vy p) 0))

(defmethod atk-end ((p player) (game game))
  (setf (player-state p) 'atk-e))
