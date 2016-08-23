(in-package :danmachi)

(defun load-lib-images (&rest args)
  (apply #'load-images (mapcar (lambda (arg)
				 (if (stringp arg)
				     (lib-path arg)
				     arg)) args)))

(defun gameimage-load ()
  (load-lib-images :player-front "chara_front.png"
		   :player-back "chara_back.png"
		   :player-right "chara_right.png"
		   :player-left "chara_left.png"
		   :test-enemy "test_enemy.png"
		   :test-bullet "test_bullet.png"
		   :wall "wall.png"
		   :floor "floor.png"
		   :upstairs "upstairs.png"
		   :downstairs "downstairs.png"
		   :sword-front1 "sword-front1.png"
		   :sword-front2 "sword-front2.png"
		   :sword-back1 "sword-back1.png"
		   :sword-back2 "sword-back2.png"
		   :sword-right1 "sword-right1.png"
		   :sword-right2 "sword-right2.png"
		   :sword-left1 "sword-left1.png"
		   :sword-left2 "sword-left2.png"
		   ))

