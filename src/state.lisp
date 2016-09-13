(in-package :danmachi)

(defun init-game (game)
  (set-nil (object-list game)
	   (floor-list game)
	   (player game))
  (pop-state game))

;新しいマップ突入時の初期化
(defun init-map (map game)
  (set-nil (object-list game)
	   (floor-list game))
  (load-map map game)
  (pop-state game))

(defun gaming-state (game)
  (sdl:clear-display sdl:*black*)
  (update-game game)
  (round-robin (lambda (obj1 obj2)
		 (collide obj1 obj2 game))
	       (object-list game))
  (update-camera game)
  ;;つらい playerが持つべき?

  (with-slots (c)
      (keystate game)
      (when (key-down-p c)
	(push-state :menu-index game)))
  (unless (alive (player game))
    (pop-state game)
    (push-state :gameover game))
  (draw-game game)
#|
  (print (camera game))
  (format t "player x : ~a, y : ~a~%"
	  (point-x (player game))
	  (point-y (player game)))
|#
  )

(defun title-state (game)
  (with-slots (up down left right z)
      (keystate game)
    (sdl:clear-display sdl:*black*)
    (sdl:draw-string-solid-* "楽しい人生"
			     30 30)
    (when (key-down-p z)
      (pop-state game)
      (push-stateset '(:init-game
		       (:init-map "large.map")
		       :game)
		     game))))

(defun gameover-state (game)
  (with-slots (up down left right z)
      (keystate game)
    (sdl:draw-string-solid-* "gameover"
			     200 150)
    (when (key-down-p z)
      (pop-state game)
      (push-state :title game))))


(let* ((cursor 0)
       (contents-table
	'("equip" "item"))
       (menu-size
	(length contents-table)))
  (defun menu-index-state (game)
    (with-slots (up down z x)
	(keystate game)
      (sdl:clear-display sdl:*black*)
      (sdl:draw-string-solid-* "menu index"
			       30 30)
      (loop for i
	 from 0 below menu-size
	 do (sdl:draw-string-solid-*
	     (nth i contents-table)
	     100
	     (+ (* i 30) 50)))
      (sdl:draw-string-solid-* "->"
	     70 (+ (* cursor 30) 50))
      (whens
	((key-down-p up)
	 (setf cursor
	       (mod (1- cursor) menu-size)))
	((key-down-p down)
	 (setf cursor
	       (mod (1- cursor) menu-size)))
	((key-down-p x) (pop-state game))
	((key-down-p z)
	 (case cursor
	   (0 (push-state :select-equip game))
	   (1 (push-state :item-table game))))))))

(defun select-equip-state (game)
  (with-slots (x) (keystate game)
    (sdl:clear-display sdl:*black*)
    (sdl:draw-string-solid-* "select equip"
			     30 30)
    (when (key-down-p x)
      (pop-state game))))

(defun item-table-state (game)
    (with-slots (x) (keystate game)
      (sdl:clear-display sdl:*black*)
      (sdl:draw-string-solid-* "item table"
			       30 30)
      (when (key-down-p x)
	(pop-state game))))

(defun push-text-state (filename game)
  (let ((lines nil)
	(size nil))
    (with-open-file (stream (lib-path filename))
      (iter (for l in-stream stream using #'read-line)
	    (push l lines)))
    (setf lines (reverse lines)
	  size (length lines))
    (push-stateset (loop for i below size by 4 collect
			(list :display-text
			      (loop for j
				 below (min 4 (- size i))
				 collect (nth (+ i j) lines))))
		   game)))

(defun display-text-state (strlist game)
    (with-slots (z) (keystate game)
      (sdl:draw-box-* 0 300 640 180
		      :color sdl:*black*)
      (loop for i below (length strlist) do
	   (sdl:draw-string-solid-* (nth i strlist)
				    20 (+ 320 (* i 40))))
      (when (key-down-p z)
	(pop-state game))))


(defvar *state-func-table* nil)

(defun load-state-func ()
    (setf *state-func-table*
     (list :title #'title-state
	   :gameover #'gameover-state
	   :init-game #'init-game
	   :init-map #'init-map
	   :game #'gaming-state
	   :menu-index #'menu-index-state
	   :select-equip #'select-equip-state
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

