(in-package :danmachi)

(defun main ()
  (sdl:with-init ()
    (let ((game (make-instance 'game)))
      (sdl:window (window-width game)
		  (window-height game)
		  :title-caption "danmachi")
      (setf (sdl:frame-rate) 60)
      (gameimage-load)
#|      
      (add-object (make-instance 'player
				 :point-x (* 32 5)
				 :point-y (* 32 5)
				 :width 32
				 :height 32
				 :image (get-image :player))
		  game)

      (create-wall wall-list game)
      (create-floor floor-list game)
|#
      (load-map "large.map" game)

      ;;event
      (sdl:with-events()
	(:quit-event () t)
	(:key-down-event (:key key) ; push key
			 (if (sdl:key= key :sdl-key-escape)
			     (sdl:push-quit-event)
			     (update-key-state key 3 (keystate game))))
	(:key-up-event (:key key)
		       (update-key-state key 2 (keystate game)))
	(:idle ()
	       (sdl:clear-display sdl:*black*)
	       (update-game game)
	       (round-robin #'interact-update (object-list game))
	       (update-camera game)
	       (draw-game game)
	       (sdl:update-display)
	       (print (camera game))
	       (format t "player x : ~a, y : ~a~%"
		       (point-x (player game))
		       (point-y (player game)))
	       (next-key-state (keystate game)))))))
