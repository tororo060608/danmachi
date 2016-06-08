(in-package :danmachi)

(defun main ()
  (sdl:with-init ()
    (sdl:window 640 480 :title-caption "danmachi")
    (setf (sdl:frame-rate) 60)
    
    (let ((my (make-instance 'player 
			      :point-x 0 
			      :point-y 0
			      :image (load-png-image "sample.png")))
	  (current-key-state (make-instance 'key-state)))

      ;;event
      (sdl:with-events()
	(:quit-event () t)
	(:key-down-event (:key key) ; push key
			 (if (sdl:key= key :sdl-key-escape)
			     (sdl:push-quit-event)
			     (update-key-state key t current-key-state)))
	(:key-up-event (:key key)
		       (update-key-state key nil current-key-state))
	(:idle ()
	       (sdl:clear-display sdl:*black*)
	       (player-update my current-key-state)
	       (draw my)

	       (sdl:update-display))))))

