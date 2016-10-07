(in-package :danmachi)

(define-class game ()
  (window-width 960)
  (window-height 720)
  (map-width 0)
  (map-height 0)
  object-list
  floor-list
  draw-floor-list
  draw-object-list
  update-object-list
  collide-object-list
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

(defgeneric set-size-by-image (obj))
(defmethod set-size-by-image ((obj gameobject))
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
  (mapc (lambda (obj) (update obj game)) (update-object-list game))
  (setf (object-list game)
	(remove-if-not #'alive (object-list game))
	(collide-object-list game) 
	(remove-if
	 (lambda (obj) 
	   (out-of-gamearea-p obj game 100))
	 (object-list game))
	(update-object-list game)
	(remove-if 
	 (lambda (obj) 
	   (out-of-gamearea-p obj game))
	 (collide-object-list game))
	(draw-object-list game)
	(remove-if (lambda (obj)
		     (typep obj 'effect-floor))
		   (update-object-list game))
	(draw-floor-list game)
	(remove-if (lambda (obj) 
		     (out-of-gamearea-p obj game))
		   (floor-list game))))

(defun draw-game (game)
  (mapc (lambda (obj) (draw obj game)) (draw-floor-list game))
  (mapc (lambda (obj) (draw obj game)) (draw-object-list game)))

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
(defun out-of-gamearea-p (object game &optional (margin 50))
  (let* ((left (- margin))
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

(defmethod add-object ((floor game-floor) (game game))
  (push floor (floor-list game)))

(define-class effect-floor (game-floor))
(defmethod add-object ((floor effect-floor) (game game))
  (call-next-method)
  (push floor (object-list game)))

(define-class upstairs (effect-floor)
  (image (get-image :upstairs)))

(define-class downstairs (effect-floor)
  (image (get-image :downstairs)))

(define-class pop-floor (effect-floor)
  (image (get-image :floor))
  (pop-timer (make-timer 600))
  (search-dist 300)
  (around-chara 0)
  (enemy-syms '(chototsu bee red-bee test-enemy test-enemy2))
  (probability 30))


(defmethod update((floor pop-floor) game)
  (when (and (funcall (pop-timer floor))
	     (zerop (around-chara floor))
	     (< (random 1000) (* (probability floor) 10)))
    (let ((esym
	   (nth (random (length (enemy-syms floor)))
		(enemy-syms floor))))
      (add-object (make-instance esym
				 :point-x (point-x floor)
				 :point-y (point-y floor))
		  game)))
  (setf (around-chara floor) 0))

;bullet base class
(define-class bullet (gameobject))

(defmethod update ((b bullet) (game game))
  (call-next-method))

(define-class item-container (gameobject)
  (width 32)
  (height 32)
  (image (get-animation :item-container))
  (itemsym 'cider))

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
