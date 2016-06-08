(in-package sdlutils)

;; key
(defgeneric update-key-state (key keypress keystate))

(defun key-pressed-p (key)
  (oddp key))
(defun key-down-p (key)
  (= key 3))
(defun key-up-p (key)
  (= key 2))


(defmacro defkeystate (name &rest keymaps)
  (with-gensyms (key key-press key-state)
    `(progn
       (defclass ,name ()
	  ,(mapcar (lambda (x) `(,(car x) :initform 0)) keymaps))
       (defmethod update-key-state (,key ,key-press (,key-state ,name))
	  (with-slots ,(mapcar #'car keymaps) ,key-state
	    (cond ,@(mapcar (lambda (keys)
			     `((sdl:key= ,key ,(cadr keys))
			       (setf ,(car keys) ,key-press)))
			   keymaps))))
       (defmethod next-key-state ((,key-state ,name))
	 (nmapslot (lambda (x) (mod x 2)) ,key-state)))))

;; joystick

(defmacro defjoystick (name &rest keymaps)
  (with-gensyms (gtype gkey-num gvalue gjoystate)
    `(progn
       (define-class ,name ()
	 ,@(mapcar (lambda (x) `(,(car x) 0)) keymaps))
       (defmethod update-joy-state (,gtype ,gkey-num ,gvalue 
				    (,gjoystate ,name))
	 (with-slots ,(mapcar #'car keymaps) ,gjoystate
	   (whens ,@(mapcar 
		    (lambda (keys)
		      (dbind (slot (type key-num . pmsym)) 
			  keys
			`((and (eq ,gtype ,type)
			       (= ,gkey-num ,key-num))
			  (setf ,slot  
			   ,(ccase type 
				   (:button `(+ ,gvalue 2))
				   (:axis `(cond ((,(if (eq (car pmsym) :plus)
							'axis-value-plus-p
							'axis-value-minus-p) ,gvalue)
						  (if (key-pressed-p ,slot)
						      ,slot 3))
						 ((axis-value-middle-p ,gvalue)
						  (if (key-pressed-p ,slot)
						      2 ,slot))
						 (t ,slot))))))))
		    keymaps))))
       (defmethod next-key-state ((,gjoystate ,name))
	 (nmapslot (lambda (x) (mod x 2)) ,gjoystate)))))

(defun axis-value-minus-p (value)
  (< value -20000))
(defun axis-value-plus-p (value)
  (> value 20000))
(defun axis-value-middle-p (value)
  (< -10000 value 10000))

(defun new-joystick ()
  (let ((fp (sdl-cffi::sdl-joystick-open 0)))
    (if (sdl:is-valid-ptr fp)
	fp
	nil)))

;; input

(defmacro definput (name (&body device-classes) &body keymaps)
  (with-gensyms (ginput)
    `(progn
       (define-class ,name ()
	 ,@(mapcar (lambda (dc) `(,(symbolicate (first dc)) 
				   (make-instance ',(second dc))))
		   device-classes)
	 ,@(mapcar (lambda (key) 
		     `(,(if (consp key) (car key) key) 0))
		   keymaps))
       (defmethod update-input ((,ginput ,name))
	 (with-slots ,(mapcar (lambda (keys) (if (consp keys) (car keys) keys))
			      keymaps) ,ginput
	   ,@(mapcar (lambda (keys)
		(if (atom keys)
		    `(setf ,keys
			   (max ,@(mapcar (lambda (key-pair)
					    `(slot-value (slot-value ,ginput ',(ensure-symbol (car key-pair)))
							 ',keys))
					  device-classes)))
		    `(setf ,(car keys)
			   (max ,@(mapcar 
				   (lambda (key-pair)
				     (let ((device-keys (getf (cdr keys) (car key-pair))))
				       (if (atom device-keys)
					   `(slot-value (slot-value ,ginput ',(ensure-symbol (car key-pair)))
							',device-keys)
					   `(max ,@(mapcar (lambda (dk)
							     `(slot-value 
							       (slot-value ,ginput ',(ensure-symbol (car key-pair)))
							       ',dk))
							   device-keys)))))
				   device-classes)))))
		     keymaps)))
       (defmethod next-key-state ((,ginput ,name))
	 ,@(mapcar (lambda (key-pair)
		     `(next-key-state (slot-value ,ginput ',(ensure-symbol (car key-pair)))))
		   device-classes)))))

;; example

(defkeystate titechfes-key
    (right :sdl-key-right)
  (left :sdl-key-left)
  (jump :sdl-key-c)
  (down :sdl-key-down)
  (up :sdl-key-up)
  (dash :sdl-key-lshift)
  (shot :sdl-key-x)
  (weapon :sdl-key-z)
  (start :sdl-key-return))

(defjoystick titechfes-joystick
  (left (:axis 0 :minus))
  (right (:axis 0 :plus))
  (up (:axis 1 :minus))
  (down (:axis 1 :plus))
  (start (:button 7))
  (a (:button 0))
  (b (:button 1))
  (x (:button 2))
  (y (:button 3))
  (lb (:button 4))
  (rb (:button 5))
  (lt (:axis 2 :plus))
  (rt (:axis 5 :plus)))

(definput game-input ((:key titechfes-key) (:joystick titechfes-joystick))
  left
  right
  up 
  down 
  start
  (dash :key dash :joystick (x lt))
  (jump :key jump :joystick a)
  (shot :key shot :joystick (b rt))
  (weapon :key weapon :joystick (y rb)))
