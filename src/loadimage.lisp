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
		   :downstairs "downstairs.png"))
