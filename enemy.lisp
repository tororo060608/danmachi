(in-package :danmachi)

(define-class enemy (gameobject)
	(vx 0) (vy 0))

(define-class test-enemy (enemy)
	;(image (load-png-image "test_enemy.png"))
	(image (get-image :test-enemy))
	(state :stop)
	(move-routine (make-timer 60))
	(stop-routine (make-timer 60))
	(velocity 0.5))

(defmethod update ((e test-enemy) (game game))
	(incf (point-x e) (vx e))
	(incf	(point-y e) (vy e))
	(cond ((and (eq (state e) :stop)
							(funcall (stop-routine e)))
				 (let ((dice (mod (random 100) 4)))
					 (case dice
						 (0 (setf (vx e) 0
											(vy e) (- (velocity e))))
						 (1 (setf (vx e) (- (velocity e))
											(vy e) 0))
						 (2 (setf (vx e) 0
											(vy e) (velocity e)))
						 (3 (setf (vx e) (velocity e)
											(vy e) 0)))
					 (setf (state e) :move)))
				((and (eq (state e) :move)
							(funcall (move-routine e)))
				 (setf (vx e) 0
							 (vy e) 0
							 (state e) :stop))))
