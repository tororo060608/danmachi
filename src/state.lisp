(in-package :danmachi)

(defun init-game (game)
  (set-nil (object-list game)
	   (floor-list game)
	   (player game))
  (pop-state game))

;;新しいマップ突入時の初期化

(defun get-mapfile-name (game)
  (concatenate 'string "floor"
	       (to-s (map-id game)) ".map"))

(defun init-map (game)
  (set-nil (object-list game)
	   (floor-list game))
  (load-map (get-mapfile-name game) game)
  (init-camera game)
  (pop-state game))

(defun display-player-hp (player)
  (with-slots (hp maxhp) player
    (draw-strings (to-s hp) 10 10
		  (string-conc "/" (to-s maxhp))
		  40 40)
    (sdl:draw-box-* 80 20 200 20 :color sdl:*red*)
    (sdl:draw-box-* 80 20
		    (max 0 (round (* (/ hp maxhp) 200))) 20
		    :color sdl:*green*)))

(defun gaming-state (game)
  (sdl:clear-display sdl:*black*)
  (update-camera game)
  (update-game game)
  (round-robin (lambda (obj1 obj2)
		 (when (and (< (abs (- (point-x obj1)
				       (point-x obj2)))
			       100)
			    (< (abs (- (point-y obj1)
				       (point-y obj2))) 
			       100))
		   (collide obj1 obj2 game)))
	       (collide-object-list game))
  (unless (alive (player game))
    (pop-state game)
    (push-stateset '(:darkening :gameover) game))
  (draw-game game)
  (display-player-hp (player game)))

(defun title-state (game)
  (with-slots (z)
      (keystate game)
    (sdl:clear-display sdl:*white*)
    (sdl:draw-surface-at-* (get-image :title) 180 150)
    (sdl:draw-string-solid-* "Press Z-KEY to START" 350 450
			     :color sdl:*black*)
    (when (key-down-p z)
      (pop-state game)
      (push-stateset '(:init-game
		       :init-map
		       :game)
		     game))))

(defun gameover-state (game)
  (with-slots (up down left right z)
      (keystate game)
    (sdl:clear-display sdl:*white*)
    (sdl:draw-surface-at-* (get-image :gameover) 230 85)
    (when (key-down-p z)
      (pop-state game)
      (push-state :title game))))

(defun gameclear-state (game)
  (with-slots (up down left right z)
      (keystate game)
    (sdl:draw-surface-at-* (get-image :gameclear) 180 150)
    (sdl:draw-string-solid-* "Press Z-KEY to Title" 350 450
			     :color sdl:*white*)
    (when (key-down-p z)
      (pop-state game)
      (push-state :title game))))


(let ((frame-rest 20))
  (defun darkening-state (game)
    (sdl:draw-box-*
     0 0 (window-width game) (window-height game)
     :color (sdl:color :a 15))
    (decf frame-rest)
    (when (zerop frame-rest)
      (setf frame-rest 60)
      (pop-state game))))

(defun push-message-state (mes-list game)
  (let ((lines nil)
	(size nil))
    (setf lines (reverse mes-list)
	  size (length lines))
    (push-stateset (loop for i below size by 4 collect
			(list :display-text
			      (loop for j
				 below (min 4 (- size i))
				 collect (nth (+ i j) lines))))
		   game)))

(defun push-text-state (filename game)
  (let ((lines nil))
    (with-open-file (stream (lib-path filename))
      (iter (for l in-stream stream using #'read-line)
	    (push l lines)))
    (push-message-state lines game)))

(defun display-text-state (strlist game)
    (with-slots (z c) (keystate game)
      (sdl:draw-box-* 0 480 960 240
		      :color sdl:*black*)
      (loop for i below (length strlist) do
	   (let ((str (nth i strlist)))
	     (unless (string= str "")
	       (sdl:draw-string-solid-* str
					20 (+ 480 (* i 50))))))
      (when (or (key-down-p z) (key-down-p c))
	(pop-state game))))


(defvar *state-func-table* nil)

(defun load-state-func ()
    (setf *state-func-table*
     (list :title #'title-state
	   :gameover #'gameover-state
	   :gameclear #'gameclear-state
	   :init-game #'init-game
	   :init-map #'init-map
	   :game #'gaming-state
	   :darkening #'darkening-state
	   :menu-index #'menu-index-state
	   :select-equip #'select-equip-state
	   :select-weapon #'select-weapon-state
	   :select-protect #'select-protect-state
	   :select-adornment #'select-adornment-state
	   :item-table #'item-table-state
	   :display-text #'display-text-state)))

(defun run-state (game)
  (if (null (state-stack game))
      (error "state-stack is empty")
      (let* ((state-sym (caar (state-stack game)))
	     (state-arg (cdar (state-stack game)))
	     (state-func (getf *state-func-table*
			       state-sym)))
	(if (null state-func)
	    (error "undefined state")
	    (apply state-func (append state-arg (list game)))))))

