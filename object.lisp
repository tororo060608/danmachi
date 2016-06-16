(in-package :danmachi)

(define-class game ()
  (window-width 640)
  (window-height 480)
  object-list
  (keystate (make-instance 'keystate)))

;;orginal object
(define-class gameobject ()
  point-x
  point-y
	(vx 0)
	(vy 0)
	(alive t)
  image)

(defun kill (obj)
	(setf (alive obj) nil))

(defgeneric add-object (obj game))
(defun update-game (game)
  (mapc (lambda (obj) (update obj game)) (object-list game))
	(remove-if-not #'alive (object-list game)))
(defun draw-game (game)
  (mapc (lambda (obj) (draw obj game)) (object-list game)))


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
	(incf	(point-y object) (vy object)))

;out-gamearea detect
(defun out-of-gamearea-p (object game)
	(let* ((margin 50)
				 (left (- margin))
				 (right (+ (window-width game) margin))
				 (top (- margin))
				 (bottom (+ (window-height game) margin)))
		(not (and (<= left (point-x object) right)
							(<= top (point-y object) bottom)))))

;;player object
(define-class player (gameobject))

(defmethod update ((p player) (game game))
  (let ((x (point-x p)) ;point x
	(y (point-y p)) ;point y
	(speed 5) ;verosity
	(dx 0) (dy 0))
    (with-slots (up down right left) (keystate game)
      (cond ((key-pressed-p right) (incf dx speed))
	    ((key-pressed-p left)  (decf dx speed)))
      (cond ((key-pressed-p up)   (decf dy speed))
	    ((key-pressed-p down) (incf dy speed))))
    ;; slanting move
    (when (and (/= dx 0) (/= dy 0))
      (setf dx (/ dx (sqrt 2)) dy (/ dy (sqrt 2))))
    ;;move->my point
    (setf (point-x p) (+ x dx))
    (setf (point-y p) (+ y dy))))

;;wall object
(define-class game-wall (gameobject))

;;floor object
(define-class game-floor (gameobject))
