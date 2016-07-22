(in-package danmachi)

(defparameter *camera-move-position* 120)

(defun init-camera (game)
  (setf (camera game) (list 0 0)))

(defun camera-x (game) (first (camera game)))
(defun camera-y (game) (second (camera game)))

(defun camera-move (dx dy game)
  (setf (camera game)
	(list 
	 (clamp  (+ (camera-x game) dx)
		 0
		 (- (map-width game)
			   (window-width game)))
	 (clamp (+ (camera-y game) dy)
		0
		(- (map-height game)
		   (window-height game))))))
  
(defun coordinate-in-camera (coord game)
  (mapcar #'+ coord (camera game)))
(defun x-in-camera (x game)
  (- x (camera-x game)))
(defun y-in-camera (y game)
  (- y (camera-y game)))
	 
(defun update-camera (game)
  (let* ((player (player game))
	 (px (x-in-camera (point-x player) game))
	 (py (y-in-camera (point-y player) game)))
    ((lambda (dx dy) 
       (unless (and (zerop dx) (zerop dy))
	 (camera-move dx dy game)))
     (cond ((< px *camera-move-position*)
	    (- px *camera-move-position*))
	   ((< (- (window-width game) px)
	       *camera-move-position*)
	    (- *camera-move-position*
	       (- (window-width game) px)))
	   (t 0))
     (cond ((< py *camera-move-position*)
	    (- py *camera-move-position*))
	   ((< (- (window-height game) py) 
	       *camera-move-position*)
	    (- *camera-move-position*
	       (- (window-height game) py)))
	   (t 0)))))

