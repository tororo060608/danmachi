(in-package :danmachi)

(define-class menu ()
  contents-list
  size
  (topindex 0)
  (cursor 0))
(defun make-menu (menusym lst)
  (make-instance menusym
		 :contents-list lst
		 :size (length lst)))

(defun get-cursor (menu)
  (nth (cursor menu) (contents-list menu)))

(defun move-next (menu)
  (with-slots (cursor topindex size) menu
    (when (plusp size)
      (setf cursor (mod (1+ cursor) (size menu))
	    topindex (clamp topindex (- cursor 9) cursor)))))
(defun move-prev (menu)
  (with-slots (cursor topindex size) menu
    (when (plusp size)
      (setf cursor (mod (1- cursor) (size menu))
	    topindex (clamp topindex (- cursor 9) cursor)))))

(defgeneric display-menu (menu left top step))
(defmethod display-menu ((menu menu) left top step)
  (with-slots (contents-list size cursor) menu
    (loop for i
       from 0 below size
       do (sdl:draw-string-solid-*
	   (to-s (nth i contents-list))
	   (+ left 30)
	   (+ (* i step) top)))
    (sdl:draw-string-solid-* "->"
			     left (+ (* cursor step) top))))

(define-class plist-menu (menu)
  contents-plist)
(defun make-plist-menu (menusym plist)
  (make-instance menusym
		 :contents-list (plist-keys plist)
		 :contents-plist plist
		 :size (length (plist-keys plist))))

(defun update-menu (menu new-plist)
  (with-slots (contents-list contents-plist
	       size cursor topindex) menu
    (setf contents-list  (plist-keys new-plist)
	  contents-plist  new-plist
	  size (length contents-list)
	  cursor (clamp cursor 0 (max (1- size) 0))
	  topindex (clamp topindex (- cursor 9) cursor))))


;;item
(define-class item-menu (plist-menu))
(defmethod display-menu ((menu item-menu) left top step)
  (with-slots (contents-list contents-plist size cursor) menu
    (loop for i
       from 0 below size
       do (let ((y (+ (* i step) top)))
	    (sdl:draw-string-solid-*
	     (name (get-item (nth i contents-list)))
	     (+ left 30) y)
	    (sdl:draw-string-solid-*
	     (to-s (getf contents-plist (nth i contents-list)))
	     (+ left 200) y)))
    (sdl:draw-string-solid-*
     "->" left (+ (* cursor step) top))
    (when (get-item (nth cursor contents-list))
      (sdl:draw-string-solid-*
       (description (get-item (nth cursor contents-list)))
       0 450))))


;;equip
(define-class equip-menu (plist-menu))
(defun equip-info (itemsym)
  (aif (get-item itemsym)
       (name it) "-"))

(defmethod display-menu ((menu equip-menu) left top step)
  (with-slots (contents-list contents-plist size cursor) menu
    (loop for i
       from 0 below size
       do (let ((y (+ (* i step) top)))
	    (sdl:draw-string-solid-*
	     (to-s (nth i contents-list))
	     (+ left 30) y)
	    (sdl:draw-string-solid-*
	     (equip-info (getf contents-plist
			       (nth i contents-list)))
	     (+ left 200) y)))
    (sdl:draw-string-solid-*
     "->" left (+ (* cursor step) top))))

(defmacro define-equip-state (name partsym equip-list)
  (with-gensyms (gkeysym)
    `(let* ((,gkeysym
	     (intern (symbol-name ,partsym) "KEYWORD")))
       (defun ,name (game)
	 (with-slots (,equip-list equip) (player game)
	   (with-slots (z x down up) (keystate game)
	     (sdl:clear-display sdl:*black*)
	     (when (null (getf menu ,gkeysym))
	       (setf (getf menu ,gkeysym)
		     (make-plist-menu 'item-menu ,equip-list)))
	     (let ((index-menu (getf menu :index))
		   (equip-menu (getf menu :equip))
		   (item-menu (getf menu ,gkeysym)))
	       (display-menu index-menu 30 30 30)
	       (display-menu equip-menu 200 30 30)
	       (display-menu item-menu 200 200 30)
	       (cond ((key-down-p down) (move-next item-menu))
		     ((key-down-p up) (move-prev item-menu))
		     ((key-down-p x) (left-menu ,gkeysym))
		     ((and (key-down-p z)
			   (get-cursor item-menu))
		      (awhen (getf equip ,partsym)
			  (push-item it (player game)))
		      (let ((itemsym (get-cursor item-menu)))
			(setf (getf equip ,partsym) itemsym)
			(update-menu equip-menu equip)
			(equip-effect (get-item itemsym) game)
			(delete-item itemsym (player game))
			(update-menu item-menu ,equip-list)))))))))))

(defmacro left-menu (menusym)
  `(progn (setf (getf menu ,menusym) nil)
	  (pop-state game)))

;;status
(defun display-status-menu (left top player)
  (with-slots (hp maxhp
	       atk atk-default
	       df df-default) player
    (draw-strings (string-conc "HP: " (to-s hp) "/" (to-s maxhp))
		  left top
		  (string-conc "ちから: " (to-s atk-default))
		  left (+ top 40)
		  (string-conc "こうげき: " (to-s atk))
		  (+ left 150) (+ top 40)
		  (string-conc "まもり: " (to-s df-default))
		  left (+ top 80)
		  (string-conc "ぼうぎょ: " (to-s df))
		  (+ left 150) (+ top 80))))

(let ((menu nil))  
  (defun menu-index-state (game)
    (with-slots (up down z x) (keystate game)
      (unless (getf menu :index)
	(setf (getf menu :index) (make-menu 'menu '(:equip :item))))
      (sdl:clear-display sdl:*black*)
      (let ((index-menu (getf menu :index)))
	(display-menu index-menu 30 30 40)
	(display-status-menu 200 30 (player game))
	(cond ((key-down-p up) (move-next index-menu))
	      ((key-down-p down) (move-prev index-menu))
	      ((key-down-p x) (left-menu :index))
	      ((key-down-p z)
	       (case (get-cursor index-menu)
		 (:equip (push-state :select-equip game))
		 (:item (push-state :item-table game))))))))
  
  (defun item-table-state (game)
    (with-slots (expendables-list) (player game)
      (with-slots (z x down up) (keystate game)
	(sdl:clear-display sdl:*black*)
	(when (null (getf menu :item))
	  (setf (getf menu :item)
		(make-plist-menu 'item-menu expendables-list)))
	(let ((index-menu (getf menu :index))
	      (item-menu (getf menu :item)))
	  (display-menu index-menu 30 30 40)
	  (display-menu item-menu 200 30 40)
	  (cond ((key-down-p down) (move-next item-menu))
		((key-down-p up) (move-prev item-menu))
		((key-down-p x) (left-menu :item))
		((and (key-down-p z)
		      (get-cursor item-menu))
		 (use-expendables (get-cursor item-menu) game)
		 (update-menu item-menu expendables-list)))))))
        
  (defun select-equip-state (game)
    (with-slots (equip) (player game)
      (with-slots (z x down up) (keystate game)
	(sdl:clear-display sdl:*black*)
	(when (null (getf menu :equip))
	  (setf (getf menu :equip)
		(make-plist-menu 'equip-menu equip)))
	(let ((index-menu (getf menu :index))
	      (equip-menu (getf menu :equip)))
	  (display-menu index-menu 30 30 40)
	  (display-menu equip-menu 200 30 40)
	  (cond ((key-down-p down) (move-next equip-menu))
		((key-down-p up) (move-prev equip-menu))
		((key-down-p x) (left-menu :equip))
		((and (key-down-p z)
		      (get-cursor equip-menu))
		 (case (get-cursor equip-menu)
		   (weapon (push-state :select-weapon game))
		   (protect (push-state :select-protect game))
		   (adornment (push-state :select-adornment game)))))))))
  
  (define-equip-state select-weapon-state 'weapon weapon-list)
  (define-equip-state select-protect-state 'protect protect-list)
  (define-equip-state select-adornment-state 'adornment adornment-list))
