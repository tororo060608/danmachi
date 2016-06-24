(in-package :danmachi)

(defun main ()
  (sdl:with-init ()
    (sdl:window 640 480 :title-caption "danmachi")
    (setf (sdl:frame-rate) 60)
    
    (let (())

      (sdl:update-display))

