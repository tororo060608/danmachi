(in-package :danmachi)

(define-class game ()
  (window-width 640)
  (window-height 480)
  (map-width 0)
  (map-height 0)
  object-list
  floor-list
  player
  state-stack
  (map-id 1)
  (camera (list 0 0))
  (keystate (make-instance 'keystate)))

(defun push-state (state-node game)
  (push (if (listp state-node)
	    state-node
	    (list state-node))
	(state-stack game)))

;;状態ノードを塊でpush
(defun push-stateset (node-list game)
  (mapc (lambda (node) (push-state node game))
	(reverse node-list)))

(defun pop-state (game)
  (pop (state-stack game)))

(defgeneric add-object (obj game))

;;orginal object
(define-class gameobject ()
  point-x
  point-y
  (alive t)
  (vx 0)
  (vy 0)
  width
  height
  (atk 0)
  (image nil))

(defgeneric image-width (image))
(defmethod image-width (image))
(defmethod image-width ((surface sdl:surface))
  (sdl:width surface))
(defmethod image-width ((animation animation))
  (sdl:width (elt (sdl:cells (src animation)) 0)))

(defgeneric image-height (image))
(defmethod image-height (image))
(defmethod image-height ((surface sdl:surface))
  (sdl:height surface))
(defmethod image-height ((animation animation))
  (sdl:height (elt (sdl:cells (src animation)) 0)))

(defun set-size-by-image (obj)
  (when (and (sdl:initialized-subsystems-p) (image obj))
    (setf (width obj) (image-width (image obj))
	  (height obj) (image-height (image obj)))))

(defmethod change-image (obj image))

(defmethod change-image ((obj gameobject) (image sdl:surface))
  (setf (image obj) image)
  (set-size-by-image obj))

(defmethod change-image ((obj gameobject) (image animation))
  (setf (image obj) image)
  (set-size-by-image obj))

(defun kill (obj)
  (setf (alive obj) nil))

(defgeneric add-object (obj game))
(defun update-game (game)
  (mapc (lambda (obj) (update obj game)) (object-list game))
  (setf (object-list game)
	(remove-if-not #'alive (object-list game))))

(defun draw-game (game)
  (mapc (lambda (obj) (draw obj game)) (floor-list game))
  (mapc (lambda (obj) (draw obj game)) (object-list game)))

; add new object
(defmethod add-object ((obj gameobject) (game game))
  (push obj (object-list game)))

(defgeneric draw-image (obj point-x point-y game))
(defmethod draw-image ((s sdl:surface) point-x point-y game)
  (sdl:draw-surface-at-* s
			 (round (x-in-camera
				 point-x game))
			 (round (y-in-camera
				 point-y game))))

(defmethod draw-image ((a animation) point-x point-y game)
  (with-slots (src frame-timer frame-length now-frame) a
    (sdl:draw-surface-at-* src
			   (round (x-in-camera
				   point-x game))
			   (round (y-in-camera
				   point-y game))
			   :cell now-frame)
    (when (funcall frame-timer)
      (setf now-frame (mod (1+ now-frame) frame-length)))))



;;draw object

(defgeneric draw (obj game))

(defun draw-x (obj)
  (- (point-x obj) (ash (width obj) -1)))

(defun draw-y (obj)
  (- (point-y obj) (ash (height obj) -1)))

(defmethod draw ((object gameobject) (game game))
  (if (image object)
      (draw-image (image object)
		  (draw-x object)
		  (draw-y object)
		  game)
      (sdl:draw-box-* (round (x-in-camera
			      (draw-x object)
			      game))
		      (round (y-in-camera
			      (draw-y object)
			      game))
		      (width object)
		      (height object)
		      :color sdl:*blue*)))


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
    (not (and (<= left 
		  (x-in-camera (point-x object) game)
		  right)
	      (<= top 
		  (y-in-camera (point-y object) game)
		  bottom)))))

;;wall object
(define-class game-wall (gameobject)
  (width 32)
  (height 32)
  (image (get-image :wall)))

;;floor object
(define-class game-floor (gameobject)
  (width 32)
  (height 32)
  (image (get-image :floor)))
#|
(defmethod draw ((obj game-wall) game)
  (call-next-method)
  (sdl:draw-box-* (round (x-in-camera (point-x obj) game))
		  (round (y-in-camera (point-y obj) game)) 4 4
		  :color sdl:*red*))
|#
(defmethod add-object ((floor game-floor) (game game))
  (push floor (floor-list game)))

(define-class effect-floor (game-floor))

(define-class upstairs (effect-floor)
  (image (get-image :upstairs)))

(define-class downstairs (effect-floor)
  (image (get-image :downstairs)))

(defmethod add-object ((floor effect-floor) (game game))
  (push floor (object-list game)))

;bullet base class
(define-class bullet (gameobject))

(defmethod update ((b bullet) (game game))
  (call-next-method))

;;npc
(define-class npc (gameobject)
  (width 32)
  (height 32)
  (image (get-animation :test2))
  (textfile "mes_test.txt"))

;;animation-test
(define-class animation-test (gameobject)
  (width 24)
  (height 24)
  (image (get-animation :test)))

(define-class animation-test2 (gameobject)
  (width 32)
  (height 32)
  (image (get-animation :test2)))
