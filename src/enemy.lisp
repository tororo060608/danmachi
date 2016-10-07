(in-package :danmachi)

;enemy base class
(define-class enemy (gamecharacter)
  (width 32)
  (height 32)
  (atk 10)
  (super-arrmor nil))

(defmethod update ((e enemy) game)
  (call-next-method)
  (when (and (muteki e)
	     (not (super-arrmor e)))
    (setf (vx e) 0 (vy e) 0))
  (when (and (not (alive e))
	     (< (random 100) 30))
    (add-object (make-instance 'item-container
			       :point-x (point-x e)
			       :point-y (point-y e))
		game)))

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
  (atk 10)
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
		((:shot :stop1 (shot-timer e))
		 (set-bullet e 'tenemy2-bullet 
			     (bullet-v e) 0 game)
		 (set-bullet e 'tenemy2-bullet 
			     0 (bullet-v e) game)
		 (set-bullet e 'tenemy2-bullet 
			     (- (bullet-v e)) 0 game)
		 (set-bullet e 'tenemy2-bullet 
			     0 (- (bullet-v e)) game))))


;///test enemy react///
(define-class test-enemy-react (enemy)
	(image (get-image :test-enemy))
	(state :stop)
	(player-found-p nil)
	(react-dist 150)
	(move-timer (make-timer 60))
	(stop-timer (make-timer 60))
	(velocity 0.5))

(defmethod update ((e test-enemy-react) (game game))
  (setf (player-found-p e)
	(or (player-found-p e)
	    (around-p (player game) e 
		      (react-dist e))))
  (if (player-found-p e)
      (let ((dir (dir-univec (point-x e) (point-y e)
			     (point-x (player game)) 
			     (point-y (player game)))))
	(setf (vx e) (* (first dir) (velocity e))
	      (vy e) (* (second dir) (velocity e))))
      (change-enemy-state e
	((:stop :move (stop-timer e))
	 (set-4way-randv e (velocity e)))
	((:move :stop (move-timer e))
	 (stop-enemy e))))
  (call-next-method))

;; チョトツ
(define-class chototsu (enemy)
  (image (get-image :chototsu-front))
  (standing-images (4dir-images :chototsu-front
				:chototsu-back
				:chototsu-right
				:chototsu-left))
  (dash-images (4dir-animations :chototsu-front-dash
				:chototsu-back-dash
				:chototsu-right-dash
				:chototsu-left-dash))
  (state :search)
  (react-dist 200)
  (attack-dict 100)
  (random-turn-timer (make-timer (+ 120 (random 120))))
  (attack-charge (charge-timer 120 :charge :attack))
  (walk-speed 1)
  (dash-speed 3)
  (normal-atk 10)
  (dash-atk 15)
  (attack-frame 0))

(defmethod change-dire-image ((e chototsu) game)
  (change-image e (getf (if (= (vx e) (vy e) 0)
			    (standing-images e)
			    (dash-images e))
			(direction e))))

(defmethod update ((e chototsu) (game game))
  (case (state e)
    (:search 
     (when (around-p (player game) e (react-dist e))
       (setf (state e) :chase))
     (when (funcall (random-turn-timer e))
       (set-4way-randv e (walk-speed e))))
    (:chase 
     (destructuring-bind (vx vy) 
		(uvec e (player game) #'point-x #'point-y)
	      (setf (vx e) (* (walk-speed e) vx)
		    (vy e) (* (walk-speed e) vy)))
     (when (and (funcall (attack-charge e) :charge)
		(around-p (player game) e (attack-dict e)))
       (setf (state e) :attack)
       (funcall (attack-charge e) :attack)))
    (:attack (incf (attack-frame e))
     (cond ((< (attack-frame e) 15) (setf (vx e) 0
					  (vy e) 0))
	   ((= (attack-frame e) 15)
	    (destructuring-bind (vx vy) 
		(uvec e (player game) #'point-x #'point-y)
	      (setf (vx e) (* (dash-speed e) vx)
		    (vy e) (* (dash-speed e) vy)
		    (atk e) (dash-atk e)
		    (super-arrmor e) t)))
	   ((< (attack-frame e) 55))
	   ((< (attack-frame e) 70) (setf (vx e) 0
					  (vy e) 0
					  (atk e) (normal-atk e)
					  (super-arrmor e) nil))
	   (t (setf (attack-frame e) 0
		    (state e) :chase)))))
  (call-next-method))

;; 蜂
(define-class bee (enemy)
  (image (get-animation :bee-right))
  (standing-images (4dir-animations :bee-right
				    :bee-right
				    :bee-right
				    :bee-left))
  (state :search)
  (react-dist 400)
  (attack-dict 200)
  (random-turn-timer (make-timer (+ 120 (random 120))))
  (attack-charge (charge-timer 120 :charge :attack))
  (walk-speed 1)
  (bullet-speed 3)
  (atk 10)
  (attack-frame 0))

(defun set-nway-bullet (obj bullet-sym n game
			&key (step 30) (speed 5) ctheta)
  (let* ((v (a-to-b-vector obj (player game)
			   #'point-x #'point-y))
	 (range (* step (1- n))))
    (when (null ctheta) (setf ctheta (deg (atan-vec v))))
    (loop for i from (- (ash range -1))
                to (ash range -1) by step
       do (let ((vec-x (cos (rad (+ ctheta i))))
		(vec-y (sin (rad (+ ctheta i)))))
	    (set-bullet obj bullet-sym
			(* speed vec-x)
			(* speed vec-y)
			game)))))

(defgeneric bee-attack (bee game))
(defmethod bee-attack ((e bee) game)
  (incf (attack-frame e))
  (cond ((< (attack-frame e) 30)
	 (setf (vx e) 0 (vy e) 0))
	((= (attack-frame e) 30)
	 (set-nway-bullet e 'tenemy2-bullet 3 game
			  :speed (bullet-speed e)))
	((< (attack-frame e) 100))
	(t (setf (attack-frame e) 0
		 (state e) :chase))))
  
(defmethod update ((e bee) (game game))
  (case (state e)
    (:search 
     (when (around-p (player game) e (react-dist e))
       (setf (state e) :chase))
     (when (funcall (random-turn-timer e))
       (set-4way-randv e (walk-speed e))))
    (:chase 
     (destructuring-bind (vx vy) 
		(uvec e (player game) #'point-x #'point-y)
	      (setf (vx e) (* (walk-speed e) vx)
		    (vy e) (* (walk-speed e) vy)))
     (when (and (funcall (attack-charge e) :charge)
		(around-p (player game) e (attack-dict e)))
       (setf (state e) :attack)
       (funcall (attack-charge e) :attack)))
    (:attack (bee-attack e game)))
  (call-next-method))

(define-class red-bee (bee)
  (standing-images (4dir-animations :red-bee-right
				    :red-bee-right
				    :red-bee-right
				    :red-bee-left)))
(defmethod bee-attack ((e red-bee) game)
  (incf (attack-frame e))
  (cond ((< (attack-frame e) 30)
	 (setf (vx e) 0 (vy e) 0))
	((= (attack-frame e) 30)
	 (set-nway-bullet e 'tenemy2-bullet 3 game
			  :speed (bullet-speed e)))
	((= (attack-frame e) 80)
	 (set-nway-bullet e 'tenemy2-bullet 4 game
			  :speed (bullet-speed e)))
	((< (attack-frame e) 200))
	(t (setf (attack-frame e) 0
		 (state e) :chase))))

(define-class nuko (enemy)
  (width 128) (height 128)
  (image (get-image :nuko-stand))
  (standing-images (4dir-images :nuko-stand
				:nuko-stand
				:nuko-stand
				:nuko-stand))
  (step-images (4dir-animations :nuko-left-step
				:nuko-left-step
				:nuko-right-step
				:nuko-left-step))
  (state :stop)
  (move-dir :left)
  (stop-timer (make-timer 60))
  (step-timer (make-timer 45))
  (step-velocity 5)
  (attack-mode 0)
  (attack-frame 0))

(defmethod change-dire-image ((e nuko) game)
  (change-image e (getf (if (= (vx e) (vy e) 0)
			    (standing-images e)
			    (step-images e))
			(direction e))))

(define-class nuko-bullet (enemy-bullet)
  (atk 30)
  (image (get-image :test-bullet)))

(define-class nuko-impact (enemy-bullet)
  (width 384) (height 384)
  (image (get-image :nuko-impact))
  (atk 250)
  (life 30))

(defmethod update ((impact nuko-impact) game)
  (when (zerop (decf (life impact)))
    (kill impact)))

(defmethod update ((enemy nuko) game)
  (call-next-method)
  (case (state enemy)
    (:stop
     (when (funcall (stop-timer enemy))
       (case (move-dir enemy)
	 (:left (setf (vx enemy) (- (step-velocity enemy))
		      (move-dir enemy) :right))
	 (:right (setf (vx enemy) (step-velocity enemy)
		       (move-dir enemy) :left)))
       (setf (state enemy) :step)))
    (:step
     (when (funcall (step-timer enemy))
       (setf (vx enemy) 0) 
       (if (zerop (setf (attack-mode enemy)
			(mod (1+ (attack-mode enemy)) 3)))
	   (progn 
	     (setf (state enemy) :impact
		   (image enemy) (get-animation :nuko-attack))
	     (set-size-by-image enemy))
	   (progn
	     (setf (state enemy) :shot
		   (image enemy) (get-image :nuko-stand))
	     (set-size-by-image enemy)))))
    (:impact
     (incf (attack-frame enemy))
     (cond ((= (attack-frame enemy) 50)
	    (set-bullet enemy 'nuko-impact
			(point-x enemy) (point-y enemy) game)
	    (setf (image enemy)
		  (get-image :nuko-impact-finish))
	    (set-size-by-image enemy))
	   ((= (attack-frame enemy) 100)
	    (setf (attack-frame enemy) 0
		  (state enemy) :stop))))
    (:shot
     (incf (attack-frame enemy))
     (cond ((= (attack-frame enemy) 20)
	    (set-nway-bullet enemy 'nuko-bullet 5 game))
	   ((= (attack-frame enemy) 40)
	    (set-nway-bullet enemy 'nuko-bullet 4 game))
	   ((= (attack-frame enemy) 60)
	    (set-nway-bullet enemy 'nuko-bullet 3 game)
	    (setf (attack-frame enemy) 0
		  (state enemy) :stop)))))
  (when (not (alive enemy))
    (push-stateset '(:darkening :gameclear) game)))
