(in-package :danmachi)

(defgeneric update-key-state (key key-press key-state)
  (:documentation "update key state"))

(defmacro defkeystate (name &rest key-maps)
  (let ((key (gensym))
	(key-press (gensym))
	(key-state (gensym)))
    `(progn
       (defclass ,name ()
	 ,(loop for k in key-maps collect `(,(car k) :initform nil)))
    
       (defmethod update-key-state (,key ,key-press (,key-state ,name))
	 (with-slots ,(mapcar #'car key-maps) ,key-state
	   (cond ,@(loop for k in key-maps
		      collect `((sdl:key= ,key ,(cadr k))
				(setf ,(car k) ,key-press)))))))))

(defkeystate key-state
    (right :sdl-key-right)
    (left  :sdl-key-left)
    (up    :sdl-key-up)
    (down  :sdl-key-down))

(defconstant +sqrt-2+ (sqrt 2))  ;slanting move

(defun player-move (playerobject key-state)
  (let ((x (point-x playerobject)) ;point x
	(y (point-y playerobject)) ;point y
	(speed 5) ;verosity
	(dx 0) (dy 0))
    (with-slots (up down right left) key-state
      (cond (right (incf dx speed))
	    (left  (decf dx speed)))
      (cond (up   (decf dy speed))
	    (down (incf dy speed))))
    ;; slanting move
    (when (and (/= dx 0) (/= dy 0))
      (setf dx (/ dx +sqrt-2+) dy (/ dy +sqrt-2+)))
    ;;move->my point
    (setf (point-x playerobject) (+ x dx))
    (setf (point-y playerobject) (+ y dy))))
