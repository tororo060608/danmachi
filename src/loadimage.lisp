(in-package :danmachi)

(defvar *animation-table* nil)

(define-class animation ()
  src
  frame-timer
  frame-length
  (now-frame 0))

(defun make-animation (indicator path
		       w h source-w source-h
		       &optional (frame-time 3))
  (load-animation indicator (lib-path path) w h source-w source-h)
  (let ((srcimg (get-image indicator)))
    (setf (getf *animation-table* indicator)
	  (make-instance 'animation
			 :src srcimg
			 :frame-timer (make-timer frame-time)
			 :frame-length (length (sdl:cells srcimg))))))

(defun get-animation (indicator)
  (if-let (anim (getf *animation-table* indicator))
    anim
    (warn "animation ~a is not found" indicator)))

(defun get-animation-list (&rest keys)
  (mapcar #'get-animation keys))

(defun load-lib-images (&rest args)
  (apply #'load-images (mapcar (lambda (arg)
				 (if (stringp arg)
				     (lib-path arg)
				     arg)) args)))

(defun make-animations (&rest args)
  (mapc #'(lambda (arg) (apply #'make-animation arg)) args))


(defun gameimage-load ()
  (load-lib-images :mc-front "mc-front.png"
		   :mc-back "mc-back.png"
		   :mc-right "mc-right.png"
		   :mc-left "mc-left.png"
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


(defun gameanimation-load ()
  (make-animations '(:test "bomb-r_ani.png"
		     24 24 96 24)
		   '(:test2 "explosion2.png"
		     32 32 128 64 3)
		   '(:mc-left-walk "mc-left-walk.png"
		     32 64 64 64 20)
		   '(:mc-right-walk "mc-right-walk.png"
		     32 64 64 64 20)
		   '(:mc-front-walk "mc-front-walk.png"
		     32 64 64 64 20)
		   '(:mc-back-walk "mc-back-walk.png"
		     32 64 64 64 20)
		   '(:mc-left-atk "mc-left-atk.png"
		     96 64 288 64 10)
		   '(:mc-right-atk "mc-right-atk.png"
		     96 64 288 64 10)
		   '(:mc-back-atk "mc-back-atk.png"
		     96 96 288 96 10)
		   '(:mc-front-atk "mc-front-atk.png"
		     96 96 288 96 10)
		   ))
