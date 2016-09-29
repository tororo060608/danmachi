(in-package :danmachi)

(defun init-game (game)
  (set-nil (object-list game)
	   (floor-list game)
	   (player game))
  (pop-state game))

;;新しいマップ突入時の初期化

(defun get-mapfile-name (game)
  (concatenate 'string "floor"
	       (to-s (map-id game)) ".map"))

(defun init-map (game)
  (set-nil (object-list game)
	   (floor-list game))
  (load-map (get-mapfile-name game) game)
  (pop-state game))

(defun gaming-state (game)
  (sdl:clear-display sdl:*black*)
  (update-camera game)  
  (update-game game)
  (round-robin (lambda (obj1 obj2)
		 (collide obj1 obj2 game))
	       (object-list game))
  (unless (alive (player game))
    (pop-state game)
    (push-state :gameover game))
  (draw-game game))

(defun title-state (game)
  (with-slots (up down left right z)
      (keystate game)
    (sdl:clear-display sdl:*black*)
    (sdl:draw-string-solid-* "楽しい人生"
			     30 30)
    (when (key-down-p z)
      (pop-state game)
      (push-stateset '(:init-game
		       :init-map
		       :game)
		     game))))

(defun gameover-state (game)
  (with-slots (up down left right z)
      (keystate game)
    (sdl:draw-string-solid-* "gameover"
			     200 150)
    (when (key-down-p z)
      (pop-state game)
      (push-state :title game))))


(define-class menu ()
  contents-list
  size
  (topindex 0)
  (cursor 0))

(define-class plist-menu (menu)
  contents-plist)

(define-class item-menu (plist-menu))
(define-class equip-menu (plist-menu))

(defun make-menu (menusym lst)
  (make-instance menusym
		 :contents-list lst
		 :size (length lst)))
(defun make-plist-menu (menusym plist)
  (make-instance menusym
		 :contents-list (plist-keys plist)
		 :contents-plist plist
		 :size (length (plist-keys plist))))

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

(defun update-menu (menu new-plist)
  (with-slots (contents-list contents-plist
	       size cursor topindex) menu
    (setf contents-list  (plist-keys new-plist)
	  contents-plist  new-plist
	  size (length contents-list)
	  cursor (clamp cursor 0 (max (1- size) 0))
	  topindex (clamp topindex (- cursor 9) cursor))))


(defmacro left-menu (menusym)
  `(progn (setf ,menusym nil)
	  (pop-state game)))

(let ((menu nil))
  (defun menu-index-state (game)
    (with-slots (up down z x) (keystate game)
      (unless menu
	(setf menu (make-menu 'menu '(:equip :item))))
      (sdl:clear-display sdl:*black*)
      (sdl:draw-string-solid-* "menu index"
			       30 30)
      (display-menu menu 70 70 30)
      (cond ((key-down-p up) (move-next menu))
	    ((key-down-p down) (move-prev menu))
	    ((key-down-p x) (left-menu menu))
	    ((key-down-p z)
	     (case (get-cursor menu)
	       (:equip (push-state :select-equip game))
	       (:item (push-state :item-table game))))))))

(let ((menu nil))
  (defun item-table-state (game)
    (with-slots (expendables-list) (player game)
      (with-slots (z x down up) (keystate game)
	(sdl:clear-display sdl:*black*)
	(when (null menu)
	  (setf menu
		(make-plist-menu 'item-menu expendables-list)))
	  (display-menu menu 70 70 30)
	  (cond ((key-down-p down) (move-next menu))
		((key-down-p up) (move-prev menu))
		((key-down-p x) (left-menu menu))
		((and (key-down-p z)
		      (get-cursor menu))
		 (use-expendables (get-cursor menu) game)
		 (update-menu menu expendables-list)))))))

(defmacro define-equip-state (name part-sym equip-list)
  `(defun ,name (game)
    (with-slots (,equip-list equip) (player game)
      (with-slots (z x down up) (keystate game)
	(sdl:clear-display sdl:*black*)
	(when (null item-menu)
	  (setf item-menu
		(make-plist-menu 'item-menu ,equip-list)))
	(display-menu equip-menu 0 70 30)
	(display-menu item-menu 0 270 30)
	(cond ((key-down-p down) (move-next item-menu))
	      ((key-down-p up) (move-prev item-menu))
	      ((key-down-p x) (left-menu item-menu))
	      ((and (key-down-p z)
		    (get-cursor item-menu))
	       (awhen (getf equip ,part-sym)
		 (push-item it (player game)))
	       (let ((itemsym (get-cursor item-menu)))
		 (setf (getf equip ,part-sym) itemsym)
		 (equip-effect (get-item itemsym) game)
		 (delete-item itemsym (player game)))
	       (update-menu item-menu ,equip-list)))))))

(let ((equip-menu nil)
      (item-menu nil))
  (defun select-equip-state (game)
    (with-slots (equip) (player game)
      (with-slots (z x down up) (keystate game)
	(sdl:clear-display sdl:*black*)
	(when (null equip-menu)
	  (setf equip-menu
		(make-plist-menu 'equip-menu equip)))
	  (display-menu equip-menu 0 70 30)
	  (cond ((key-down-p down) (move-next equip-menu))
		((key-down-p up) (move-prev equip-menu))
		((key-down-p x) (left-menu equip-menu))
		((and (key-down-p z)
		      (get-cursor equip-menu))
		 (case (get-cursor equip-menu)
		   (weapon (push-state :select-weapon game))
		   (protect (push-state :select-protect game))
		   (adornment (push-state :select-adornment game))))))))
  (define-equip-state select-weapon-state 'weapon weapon-list)
  (define-equip-state select-protect-state 'protect protect-list)
  (define-equip-state select-adornment-state 'adornment adornment-list))


(defun push-text-state (filename game)
  (let ((lines nil)
	(size nil))
    (with-open-file (stream (lib-path filename))
      (iter (for l in-stream stream using #'read-line)
	    (push l lines)))
    (setf lines (reverse lines)
	  size (length lines))
    (push-stateset (loop for i below size by 4 collect
			(list :display-text
			      (loop for j
				 below (min 4 (- size i))
				 collect (nth (+ i j) lines))))
		   game)))

(defun display-text-state (strlist game)
    (with-slots (c) (keystate game)
      (sdl:draw-box-* 0 300 640 180
		      :color sdl:*black*)
      (loop for i below (length strlist) do
	   (let ((str (nth i strlist)))
	     (unless (string= str "")
	       (sdl:draw-string-solid-* str
					20 (+ 320 (* i 40))))))
      (when (key-down-p c)
	(pop-state game))))


(defvar *state-func-table* nil)

(defun load-state-func ()
    (setf *state-func-table*
     (list :title #'title-state
	   :gameover #'gameover-state
	   :init-game #'init-game
	   :init-map #'init-map
	   :game #'gaming-state
	   :menu-index #'menu-index-state
	   :select-equip #'select-equip-state
	   :select-weapon #'select-weapon-state
	   :select-protect #'select-protect-state
	   :select-adornment #'select-adornment-state
	   :item-table #'item-table-state
	   :display-text #'display-text-state)))

(defun run-state (game)
  (if (null (state-stack game))
      (error "state-stack is empty")
      (let* ((state-sym (caar (state-stack game)))
	     (state-arg (cdar (state-stack game)))
	     (state-func (getf *state-func-table*
			       state-sym)))
	(if (null state-func)
	    (error "undefined state")
	    (apply state-func (append state-arg (list game)))))))

