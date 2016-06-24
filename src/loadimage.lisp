(in-package :danmachi)

(defun load-lib-images (&rest args)
  (apply #'load-images (mapcar (lambda (arg)
				 (if (stringp arg)
				     (lib-path arg)
				     arg)) args)))

(defun gameimage-load ()
  (load-lib-images :player "chara1.png"
		   :test-enemy "test_enemy.png"
		   :test-bullet "test_bullet.png"
		   :wall "wall.png"
		   :floor "floor.png"
		   ))
