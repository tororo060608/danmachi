(in-package :danmachi)

(defun gaming-state (game)
  (update-game game)
  (round-robin #'interact-update (object-list game))
  (update-camera game)
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
  (sdl:draw-string-solid-* "this is title"
			   30 30)
  (when (key-pressed-p z)
    (pop-state game)
    (push-state :game game))))
