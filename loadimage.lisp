(in-package :danmachi)

(defun gameimage-load ()
	(load-images '(:player "sample.png")
							 '(:test-enemy "test_enemy.png")
							 '(:test-bullet "test_bullet.png")))
