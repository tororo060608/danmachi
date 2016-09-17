(in-package :danmachi)

(define-class gamecharacter (gameobject)
  (hp 100)
  (muteki nil)
  (muteki-timer (make-timer 10))
  (direction :front)
  ; 立っている時の絵
  ; plistで:front, :back, :left, :rightの4つの要素を持つ
  (standing-images nil)
  ; 歩く時の絵(アニメーション)
  ; plistでfront, back, left, rightの4つの要素を持つ
  (walk-images nil))

(defmethod alive-detect ((char gamecharacter) game)
  (when (<= (hp char) 0)
    (kill char)))

(defmethod dec-muteki-frame ((chr gamecharacter))
  (when (and (muteki chr) (funcall (muteki-timer chr)))
    (setf (muteki chr) nil)))

(defmethod update ((chr gamecharacter) game)
  (call-next-method)
  (dec-muteki-frame chr)
  (alive-detect chr game)
  (update-image chr game))

(defmethod damage ((obj gameobject) (char gamecharacter))
  (when (not (muteki char))
    (decf (hp char) (atk obj))
    (setf (muteki char) t)))

(defmethod change-direction ((char gamecharacter) game)
  (if (< (abs (vy char)) (abs (vx char)))
      (if (< 0 (vx char))
	  (setf (direction char) :right)
	  (setf (direction char) :left))
      (if (< 0 (vy char))
	  (setf (direction char) :front)
	  (setf (direction char) :back))))

(defmethod direction-change-p ((char gamecharacter))
  (with-accessors ((vx vx) (vy vy)) char
    (and (some (compose #'not #'zerop) (list vx vy))
	 (not (case (direction char)
		(:front (plusp vy))
		(:back (minusp vy))
		(:right (plusp vx))
		(:left (minusp vx)))))))

(defun 4dir-images (front back right left)
  (list :front (get-image front)
	:back (get-image back)
	:right (get-image right)
	:left (get-image left)))

(defun 4dir-animations (front back right left)
  (list :front (get-animation front)
	:back (get-animation back)
	:right (get-animation right)
	:left (get-animation left)))

(defmethod change-dire-image ((char gamecharacter) game)
  (change-image char (getf (standing-images char) (direction char))))

(defmethod update-image ((char gamecharacter) game)
  (when (direction-change-p char)
    (change-direction char game)
    (change-dire-image char game)))
