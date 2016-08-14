(in-package :danmachi)

(defun gaming-state (game)
  (sdl:clear-display sdl:*black*)
  (update-game game)
  (round-robin #'interact-update (object-list game))
  (update-camera game)
  ;;つらい playerが持つべき?
  (with-slots (c)
      (keystate game)
      (when (key-down-p c)
	(push-state :menu-index game)))
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
    (sdl:draw-string-solid-* "this is title"
			     30 30)
    (when (key-pressed-p z)
      (pop-state game)
      (push-state :game game))))

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
  (with-slots (x)
      (keystate game)
    (sdl:clear-display sdl:*black*)
    (sdl:draw-string-solid-* "select equip"
			     30 30)
    (when (key-down-p x)
      (pop-state game))))

(defun item-table-state (game)
    (with-slots (x)
	(keystate game)
      (sdl:clear-display sdl:*black*)
      (sdl:draw-string-solid-* "item table"
			       30 30)
      (when (key-down-p x)
	(pop-state game))))
