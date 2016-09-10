(in-package :danmachi)

(defmethod collidep ((obj1 gameobject) (obj2 gameobject))
  (let* ((l1 (point-x obj1))
	 (r1 (+ l1 (width obj1)))
	 (t1 (point-y obj1))
	 (b1 (+ t1 (height obj1)))
	 (l2 (point-x obj2))
	 (r2 (+ l2 (width obj2)))
	 (t2 (point-y obj2))
	 (b2 (+ t2 (height obj2))))
    (and (< l1 r2) (< l2 r1) (< t1 b2) (< t2 b1))))

#|
(defun judge-contact-side (obj1 obj2)
  ;l left
  ;r right
  ;t top
  ;b bottom
  (let* ((l1 (point-x obj1))
	 (r1 (+ l1 (width obj1)))
	 (t1 (point-y obj1))
	 (b1 (+ t1 (height obj1)))
	 (l2 (point-x obj2))
	 (r2 (+ l2 (width obj2)))
	 (t2 (point-y obj2))
	 (b2 (+ t2 (height obj2)))
	 (contact-side-list nil))
	
    (if (< l1 l2 r1 r2)
	(push 'right contact-side-list))
    (if (< l2 l1 r2 r1)
	(push 'left contact-side-list))
    (if (< t2 t1 b2 b1)
	(push 'top contact-side-list))
    (if (< t1 t2 b1 b2)
	(push 'bottom contact-side-list))
    
    contact-side-list
    ))
|#

(defun divide-half (obj1 obj2 vx vy 
		    &optional (i 5) (pos (list (- (point-x obj1) vx)
					       (- (point-y obj1) vy))))
  (setf (point-x obj1) (- (point-x obj1) (/ vx 2)))
  (setf (point-y obj1) (- (point-y obj1) (/ vy 2)))
  (if (collidep obj1 obj2)
      (if (<= i 0)
	  (progn
	    (setf (point-x obj1) (first pos))
	    (setf (point-y obj1) (second pos)))
	  (divide-half obj1 obj2 (/ vx 2) (/ vy 2) (1- i) pos))
      (unless (<= i 0)
	(divide-half obj1 obj2 (- (/ vx 2)) (- (/ vy 2))
		     (1- i) (list (point-x obj1) (point-y obj1))))))


#|
(defun correct-point (obj1 obj2)
  (let* ((l1 (point-x obj1))
	 (r1 (+ l1 (width obj1)))
	 (t1 (point-y obj1))
	 (b1 (+ t1 (height obj1)))
	 (l2 (point-x obj2))
	 (r2 (+ l2 (width obj2)))
	 (t2 (point-y obj2))
	 (b2 (+ t2 (height obj2)))
	 (vx (vx obj1))
	 (vy (vy obj1)))
    (if (<= 0 vx)
	(if (<= 0 vy)
	    (devide-half r1 t1 obj1 obj2)
	    (devide-half r1 b1 obj1 obj2))
	(if (> 0 vy)
	    (devide-half l1 t1 obj1 obj2)
	    (devide-half l1 b1 obj1 obj2))
|#  
 

(defcollide (obj1 gameobject) (obj2 gameobject))

(defcollide (character gamecharacter) (wall game-wall)
  (when (collidep character wall)
    (divide-half character wall (vx character) (vy character))))

(defcollide (player player) (enemy enemy)
  (when (collidep player enemy)
    (damage enemy player)
    (divide-half player enemy (vx player) (vy player))))

(defcollide (player player) (bullet enemy-bullet)
  (when (collidep player bullet)
    (damage bullet player)))


(defcollide (weapon player-attack) (enemy enemy)
  (when (collidep weapon enemy)
    (damage weapon enemy)))

(defcollide (player player) (enemy test-enemy-react)
  (call-next-method)
  (whens ((and (not (player-found-p enemy))
	       (< (distance player enemy #'point-x #'point-y)
		  (react-dist enemy)))
	  (setf (player-found-p enemy) t))
	 ((player-found-p enemy)
	  (let ((dir (dir-univec (point-x enemy) (point-y enemy)
				 (point-x player) (point-y player))))
	    (setf (vx enemy) (* (first dir) (velocity enemy))
		  (vy enemy) (* (second dir) (velocity enemy)))))))

(defcollide (player player) (upstairs upstairs)
  (when (collidep player upstairs)
    (setf (move-floor player) :up)))

(defcollide (player player) (downstairs downstairs)
  (when (collidep player downstairs)
    (setf (move-floor player) :down)))

