(in-package :danmachi)

(defun main ()
  (sdl:with-init ()
    (let ((game (make-instance 'game)))
      (sdl:window (window-width game)
		  (window-height game)
		  :title-caption "danmachi")
      (setf (sdl:frame-rate) 60)
      (add-object (make-instance 'player 
				 :point-x 0 
				 :point-y 0
				 :image (load-png-image "sample.png"))
		  game)
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
	       (draw-game game)
	       (sdl:update-display)
	       (next-key-state (keystate game)))))))










