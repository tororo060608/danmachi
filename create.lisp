(in-package danmachi)

(defun create-wall (wall-list game)
  ;;wall-list -> wall's point list
  (mapcar (lambda (p) (add-object (make-instance 'game-wall
						 :point-x (* 32 (car p))
						 :point-y (* 32 (cadr p))
						 :width 32
						 :height 32
						 :image (load-png-image "wall.png"))
				  game))
	  wall-list))

(defun create-floor (floor-list game)
  ;;floor-list -> floor's point list
  (mapcar (lambda (point) (add-object (make-instance 'game-floor
						 :point-x (* 32 (car point))
						 :point-y (* 32 (cadr point))
						 :width 32
						 :height 32
						 :image (load-png-image "floor.png"))
				  game))
	  floor-list))


(defparameter wall-list '((0 0)
			  (0 1)
			  (0 2)
			  (0 3)
			  (0 4)
			  (1 0)
			  (2 0)
			  (3 0)
			  (4 0)
			  (4 1)
			  (4 2)
			  (4 3)
			  (4 4)
			  (1 4)
			  (2 4)
			  (3 4)))


(defparameter  floor-list '((1 1)
			    (1 2)
			    (1 3)
			    (2 1)
			    (2 2)
			    (2 3)
			    (3 1)
			    (3 2)
			    (3 3)))
