(in-package :danmachi)

;enemy base class
(define-class enemy (gamecharacter)
  (width 32)
  (height 32)
  (atk 50))

;enemy bullet base class
(define-class enemy-bullet (bullet)
  (width 32)
  (height 32))

;set bullet
(defun set-bullet (obj bullet-class vx vy game)
  (add-object (make-instance bullet-class
			     :vx vx
			     :vy vy
			     :point-x (point-x obj)
			     :point-y (point-y obj))
	      game))

;change enemystate
;(change-enemy-state (<shift>*))
;<shift>:((state-from state-to timer) &body body)
(defmacro change-enemy-state (enemy &body body)
  `(cond ,@(loop for b in body
	      collect 
		`((and (eq (state ,enemy) ,(nth 0 (car b)))
		       (funcall ,(nth 2 (car b))))
		  ,@(cdr b)
		  (setf (state ,enemy) ,(nth 1 (car b)))))))

					;set 4way random velocity
(defun set-4way-randv (enemy velocity)
  (let* ((dice (mod (random 100) 4)) 
	 (v (case dice
	      (0 (list 0 (- velocity)));up
	      (1 (list (- velocity) 0));left
	      (2 (list 0 velocity));down
	      (3 (list velocity 0)))));right
    (setf (vx enemy) (first v)
	  (vy enemy) (second v))))

					;clear velocity
(defun stop-enemy (enemy)
  (setf (vx enemy) 0
	(vy enemy) 0))

					;---define enemys---

					;///test enemy///
(define-class test-enemy (enemy)
  (image (get-image :test-enemy))
  (state :stop)
  (move-timer (make-timer 60))
  (stop-timer (make-timer 60))
  (velocity 0.5))

(defmethod update ((e test-enemy) (game game))
	(call-next-method)
	(change-enemy-state e
			    ((:stop :move (stop-timer e))
			     (set-4way-randv e (velocity e)))
			    ((:move :stop (move-timer e))
			     (stop-enemy e))))

					;///test enemy2///
(define-class test-enemy2 (enemy)
  (image (get-image :test-enemy))
  (state :stop1)
  (move-timer (make-timer 60))
  (shot-timer (make-timer 45))
  (stop-timer (make-timer 20))
  (velocity 0.5)
  (bullet-v 3))

;bullet
(define-class tenemy2-bullet (enemy-bullet)
  (atk 100)
  (image (get-image :test-bullet)))

(defmethod update ((e test-enemy2) (game game))
  (call-next-method)
  (change-enemy-state e
		      ((:stop1 :move1 (stop-timer e))
		       (set-4way-randv e (velocity e)))
		      ((:move1 :stop2 (move-timer e))
		       (stop-enemy e))
		      ((:stop2 :move2 (stop-timer e))
		       (set-4way-randv e (velocity e)))
		      ((:move2 :shot (move-timer e))
		       (stop-enemy e))
		      ;;tsurai
		      ((:shot :stop1 (shot-timer e))
		       (set-bullet e 'tenemy2-bullet 
				   (bullet-v e) 0 game)
		       (set-bullet e 'tenemy2-bullet 
				   0 (bullet-v e) game)
		       (set-bullet e 'tenemy2-bullet 
				   (- (bullet-v e)) 0 game)
		       (set-bullet e 'tenemy2-bullet 
				   0 (- (bullet-v e)) game))))

