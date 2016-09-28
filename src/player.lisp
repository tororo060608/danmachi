(in-package :danmachi)

;;player object
(define-class player (gamecharacter)
  (player-speed 5)
  (width 32)
  (height 64)
  (player-state :stand)
  (atk-time-limit (make-timer 30))
  (image (get-image :mc-front))
  (mp 0)
  atk-default
  df-default
  atk
  df
  weapon protect adornment
  money
  level
  (expendables-list (list 'cider 10 'soad-pop 10))
  material-list
  weapon-list
  protect-list
  adornment-list
  (standing-images (4dir-images :mc-front :mc-back :mc-right :mc-left))
  (walk-images (4dir-animations :mc-front-walk :mc-back-walk :mc-right-walk :mc-left-walk))
  (atk-images (4dir-animations :mc-front-atk :mc-back-atk :mc-right-atk :mc-left-atk)))


(defmethod add-object ((p player) (game game))
  (if (player game)
      (with-accessors ((x point-x) (y point-y)) (player game)
	(setf x (point-x p)
	      y (point-y p))
	(pushnew (player game) (object-list game)))
      (progn
	(setf (player game) p)
	(call-next-method))))

(defmethod draw ((obj player) game)
  (call-next-method)
  (sdl:draw-box-* (round (x-in-camera (point-x obj) game))
		  (round (y-in-camera (point-y obj) game)) 4 4
		  :color sdl:*red*))

(defmethod change-state ((p player) state game)
  (setf (player-state p) state))

(defmethod change-state ((p player) (state (eql :stand)) game)
  (call-next-method)
  (change-image p (getf (standing-images p) (direction p))))

(defmethod change-state ((p player) (state (eql :walk)) game)
  (call-next-method)
  (change-image p (getf (walk-images p) (direction p))))

(defmethod change-state ((p player) (state (eql :atk)) game)
  (call-next-method)
  (attack p game)
  (change-image p (getf (atk-images p) (direction p))))

(defmethod controller-walk ((p player) game)
  (with-accessors ((vx vx) (vy vy) (x point-x) (y point-y)
		   (speed player-speed)) p
    (with-slots (up down right left z) (keystate game)
      (cond ((key-pressed-p right) (setf vx speed))
	    ((key-pressed-p left)  (setf vx (- speed)))
	    (t (setf vx 0)))
      (cond ((key-pressed-p up)   (setf vy (- speed)))
	    ((key-pressed-p down) (setf vy speed))
	    (t (setf vy 0)))
    ;; slanting move
    (when (and (/= vx 0) (/= vy 0))
      (setf vx (/ vx (sqrt 2))
	    vy (/ vy (sqrt 2)))))))

(defmethod controller-atk ((p player) game)
  (with-slots (z) (keystate game)
    (cond ((key-down-p z) (atk-start p game)))))

(defmethod controller-open-menu ((p player) game)
  (with-slots (a) (keystate game)
    (when (key-down-p a) 
      (push-state :menu-index game))))

(defmethod update ((p player) (game game))
  (case (player-state p)
    (:stand 
     (controller-open-menu p game)
     (controller-walk p game)
     (when (not (and (zerop (vx p)) (zerop (vy p))))
       (change-state p :walk game))
     (controller-atk p game))
    (:walk
     (controller-open-menu p game)
     (controller-walk p game)
     (when (and (zerop (vx p)) (zerop (vy p)))
       (change-state p :stand game))
     (controller-atk p game))
    (:atk 
     (when (funcall (atk-time-limit p))
       (change-state p :stand game))))
  (call-next-method))

(defmethod attack ((p player) (game game))
  ((lambda (arglist)
     (add-object (apply #'make-instance 'player-attack
		        :vx 0 :vy 0 arglist)
		 game))
   (case (direction p)
    (:front (list :width 64
		  :height 32
		  :point-x (point-x p)
		  :point-y (+ (point-y p) (ash (+ (height p)
						  16)
						  -1))))
    (:back (list :width 64
		 :height 32
		 :point-x (point-x p)
		 :point-y (- (point-y p) (ash (+ (height p)
						 16) -1))))
    (:right (list :width 32
		  :height 64
		  :point-x (+ (point-x p) (width p))
		  :point-y (point-y p)))
    (:left  (list :width 32
		  :height 64
		  :point-x (- (point-x p) 32)
		  :point-y (point-y p))))))

(defmethod atk-start ((p player) (game game))
  (change-state p :atk game)
  (setf (vx p) 0
	(vy p) 0))

(defmethod atk-end ((p player) (game game))
  (setf (player-state p) 'atk-e))

(defmethod change-dire-image ((p player) game)
  (print (direction p))
  (change-image p (getf (case (player-state p) 
			  (:stand (standing-images p))
			  (:walk (walk-images p))) 
			(direction p))))


(define-class player-attack (bullet)
  (width)
  (height)
  (atk 50)
  (time-limit (make-timer 10)))

(defmethod update ((patk player-attack) (game game))
  (call-next-method)
  (when (funcall (time-limit patk))
    (kill patk)))
