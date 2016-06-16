(in-package :danmachi)

;bullet base class
(define-class bullet (gameobject))

(defmethod update ((b bullet) (game game))
	(call-next-method)
	(when (out-of-gamearea-p b game)
		(kill b)))

;enemy base class
(define-class enemy (gameobject))

;calc 4way random velocity
;ret (vx vy)
(defun calc-4way-randv (velocity)
	(let ((dice (mod (random 100) 4))) 
		(case dice
			(0 (list 0 (- velocity)));up
			(1 (list (- velocity) 0));left
			(2 (list 0 velocity));down
			(3 (list velocity 0)))));right

;test enemy
(define-class test-enemy (enemy)
	(image (get-image :test-enemy))
	(state :stop)
	(move-routine (make-timer 60))
	(stop-routine (make-timer 60))
	(velocity 0.5))

(defmethod update ((e test-enemy) (game game))
	(call-next-method)
	(cond ((and (eq (state e) :stop)
							(funcall (stop-routine e)))
				 (let ((v (calc-4way-randv (velocity e))))
					 (setf (vx e) (first v)
								 (vy e) (second v)
								 (state e) :move)))
				((and (eq (state e) :move)
							(funcall (move-routine e)))
				 (setf (vx e) 0 ;clear velocity
							 (vy e) 0
							 (state e) :stop))))

