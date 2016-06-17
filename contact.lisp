(in-package :danmachi)

(defgeneric interact-update (obj1 obj2))
(definteract-method interact-update (obj1 player) (obj2 game-wall)
  (let ((contact-side (contact-judge obj1 obj2)))
    (loop for side in contact-side
      (case side
	(('right) (setf (point-x obj1)
			(- (point-x obj2) (width obj1) 1)))
	(('left) (setf (point-x obj1)
		       (+ (point-x obj2) (width obj2) 1)))
	(('up) (setf (point-y obj1)
		     (+ (point-y obj2) (height obj2) 1)))
	(('down) (setf (point-y obj1)
		       (- (point-y obj2) (height obj1) 1)))))))
