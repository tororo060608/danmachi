(in-package :danmachi)

(defmethod collidep ((obj1 gameobject) (obj2 gameobject))
  (with-accessors ((x1 point-x) (y1 point-y)
		   (w1 width) (h1 height)) obj1
    (with-accessors ((x2 point-x) (y2 point-y)
		     (w2 width) (h2 height)) obj2
      (let* ((hw1 (ash w1 -1))
	     (hh1 (ash h1 -1))
	     (hw2 (ash w2 -1))
	     (hh2 (ash h2 -1))
	     (l1 (- x1 hw1))
	     (r1 (+ x1 hw1))
	     (t1 (- y1 hh1))
	     (b1 (+ y1 hh1))
	     (l2 (- x2 hw2))
	     (r2 (+ x2 hw2))
	     (t2 (- y2 hh2))
	     (b2 (+ y2 hh2)))
	(and (< l1 r2) (< l2 r1) (< t1 b2) (< t2 b1))))))

(defun around-p (obj1 obj2 dist)
  (< (distance obj1 obj2 #'point-x #'point-y)
     dist))

(defun faced-p (player obj)
  (let* ((dir (a-to-b-vector player obj
			     #'point-x #'point-y))
	 (vecx (first dir))
	 (vecy (second dir)))
    (case (direction player)
      (:back (and (> vecx vecy) (> (- vecx) vecy)))
      (:front (and (< vecx vecy) (< (- vecx) vecy)))
      (:left (and (< vecx vecy) (> (- vecx) vecy)))
      (:right (and (> vecx vecy) (< (- vecx) vecy))))))

(defmacro defcollide (arg1 arg2 &body body)
  (if (some (lambda (x) (eq (car body) x)) 
	    '(:before :after :around))
      `(progn
	 (defmethod collide ,(car body) (,arg1 ,arg2 game)
	   ,@(cdr body))
	 (defmethod collide ,(car body) (,arg2 ,arg1 game)
	   ,@(cdr body)))
      `(progn
	 (defmethod collide (,arg1 ,arg2 game)
	   ,@body)
	 (defmethod collide (,arg2 ,arg1 game)
	   ,@body))))


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

(defcollide (obj1 gameobject) (obj2 gameobject))

(defcollide (character gamecharacter) (wall game-wall)
  (when (collidep character wall)
  #|  (format t "p : ~a,~a     w : ~a,~a~%" (round (point-x character))
	    (round (point-y character)) (round (point-x wall)) (round (point-y wall)))|#
    (divide-half character wall (vx character) (vy character))))

(defcollide (player player) (enemy enemy)
  (when (collidep player enemy)
    (damage enemy player)))

(defcollide (wall game-wall) (bullet bullet)
  (when (collidep wall bullet)
    (kill bullet)))
(defcollide (player player) (bullet enemy-bullet)
  (when (collidep player bullet)
    (damage bullet player)))


(defcollide (weapon player-attack) (enemy enemy)
  (when (collidep weapon enemy)
    (damage weapon enemy)))

(defcollide (player player) (upstairs upstairs)
  (with-slots (c) (keystate game)
    (when (and (collidep player upstairs)
	       (key-down-p c))
      (decf (map-id game))
      (push-stateset '(:darkening :init-map) game))))

(defcollide (player player) (downstairs downstairs)
  (with-slots (c) (keystate game)
    (when (and (collidep player downstairs)
	       (key-down-p c))
      (incf (map-id game))
      (push-stateset '(:darkening :init-map) game))))

(defparameter *npc-react-dist* 60)
(defcollide (player player) (npc npc)
  (with-slots (c) (keystate game)
    (when (collidep player npc)
      (divide-half player npc (vx player) (vy player)))
    (when (and (around-p player npc *npc-react-dist*)
	       (faced-p player npc)
	       (key-down-p c))
      (push-text-state (textfile npc) game))))

