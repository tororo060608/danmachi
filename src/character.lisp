(in-package :danmachi)

(define-class gamecharacter (gameobject)
  (hp 100)
  (muteki nil)
  (muteki-count 0)
  (muteki-time 10))

(defmethod alive-detect ((char gamecharacter) game)
  (when (<= (hp char) 0)
    (kill char)))

(defmethod dec-muteki-frame ((chr gamecharacter))
  (if (and (muteki chr) (zerop  (muteki-count chr)))
      (setf (muteki chr) nil)
      (decf (muteki-count chr))))

(defmethod update ((chr gamecharacter) game)
	(call-next-method)
  (dec-muteki-frame chr)
  (alive-detect chr game)
  (when (out-of-gamearea-p chr game)
    (kill chr)))

(defmethod damage ((obj gameobject) (char gamecharacter))
  (when (not (muteki char))
    (decf (hp char) (atk obj))
    (setf (muteki char) t
	  (muteki-count char) (muteki-time char))))

