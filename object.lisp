(in-package :danmachi)

;;orginal object
(defclass gameobject ()
  ((point-x :accessor point-x
	    :initform nil
	    :initarg :point-x)
   (point-y :accessor point-y
	    :initform nil
	    :initarg :point-y)
   (image   :accessor image
	    :initform nil
	    :initarg :image)))

;;load image
(defun load-png-image (source-file)
  (sdl:convert-to-display-format :surface (sdl:load-image source-file)
				 :enable-alpha t
				 :pixel-alpha t))

;draw object
(defgeneric draw (obj))
(defmethod draw ((object gameobject))
    (sdl:draw-surface-at-* (image object)
			   (round (point-x object))
			   (round (point-y object))))

(defmethod object-update (object gameobject) ())


;;player object
(defclass player (gameobject) ())

(defmethod player-update ((x player) key-state)
  (player-move x key-state))


;;wall object
(defclass game-wall (gameobject) ())

;;floor object
(defclass game-floor (gameobject) ())
