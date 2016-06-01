(in-package :danmachi)

(defgeneric update-key-state (key key-press key-state)
  (:documentation "update key state"))

(defmacro defkeystate (name &rest key-maps)
  (let ((key (gensym))
	(key-press (gensym))
	(key-state (gensym)))
    `(progn
       (defclass ,name ()
	 ,(loop for k in key-maps collect `(,(car k) :initform nil)))
    
       (defmethod update-key-state (,key ,key-press (,key-state ,name))
	 (with-slots ,(mapcar #'car key-maps) ,key-state
	   (cond ,@(loop for k in key-maps
		      collect `((sdl:key= ,key ,(cadr k))
				(setf ,(car k) ,key-press)))))))))

(defkeystate key-state
    (right :sdl-key-right)
    (left  :sdl-key-left)
    (up    :sdl-key-up)
    (down  :sdl-key-down))

(defconstant +sqrt-2+ (sqrt 2))  ;slanting move

(defun load-png-image (source-file)
  (sdl:convert-to-display-format :surface (sdl:load-image source-file)
				 :enable-alpha t
				 :pixel-alpha t))


(defun main ()
  (sdl:with-init ()
    (sdl:window 640 480 :title-caption "sample")   ;make window
    (setf (sdl:frame-rate) 60)   ;fps 60
    (let ((current-key-state (make-instance 'key-state))
	  (x 0) ;point x
	  (y 0) ;point y
	  (speed 5) ;verosity
	  ;load image
	  (player-img (load-png-image (to-s *dir-path*
					    "/sample.png"))))
          
      (sdl:update-display)  ;update

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
	       (let ((dx 0) (dy 0))
		 (with-slots (up down right left) current-key-state
		   (cond (right (incf dx speed))
			 (left  (decf dx speed)))
		   (cond (up   (decf dy speed))
			 (down (incf dy speed))))
		 ;; slanting move
		 (when (and (/= dx 0) (/= dy 0))
		   (setf dx (/ dx +sqrt-2+) dy (/ dy +sqrt-2+)))
		 ;;move->my point
		 (incf x dx)
		 (incf y dy))
	       (sdl:draw-surface-at-* player-img (round x) (round y))
	       (sdl:update-display))))))
