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
  (init-camera game)
  (pop-state game))

(defun gaming-state (game)
  (sdl:clear-display sdl:*black*)
  (update-camera game)
  (update-game game)
  (round-robin (lambda (obj1 obj2)
		 (when (and (< (abs (- (point-x obj1)
				       (point-x obj2)))
			       100)
			    (< (abs (- (point-y obj1)
				       (point-y obj2))) 
			       100))
		   (collide obj1 obj2 game)))
	       (collide-object-list game))
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
(define-class item-menu (menu)
  item-plist)

(defun make-menu (lst)
  (make-instance 'menu
		 :contents-list lst
		 :size (length lst)))
(defun make-item-menu (plist)
  (make-instance 'item-menu
		 :contents-list (plist-keys plist)
		 :item-plist plist
		 :size (ash (length plist) -1))) 

(defun get-cursor (menu)
  (nth (cursor menu) (contents-list menu)))

(defun move-next (menu)
  (with-slots (cursor topindex size) menu
    (setf cursor (mod (1+ cursor) (size menu))
	  topindex (clamp topindex (- cursor 9) cursor))))
(defun move-prev (menu)
  (with-slots (cursor topindex size) menu
    (setf cursor (mod (1- cursor) (size menu))
	  topindex (clamp topindex (- cursor 9) cursor))))

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
  (with-slots (contents-list item-plist size cursor) menu
    (loop for i
       from 0 below size
       do (let ((y (+ (* i step) top)))
	    (sdl:draw-string-solid-*
	     (name (get-item (nth i contents-list)))
	     (+ left 30) y)
	    (sdl:draw-string-solid-*
	     (to-s (getf item-plist (nth i contents-list)))
	     (+ left 200) y)))
    (sdl:draw-string-solid-*
     "->" left (+ (* cursor step) top))))

(defun update-menu (item-menu new-plist)
  (with-slots (contents-list item-plist size) item-menu
    (setf contents-list  (plist-keys new-plist)
	  item-plist  new-plist
	  size (length contents-list))))

(defmacro left-menu (menusym)
  `(progn (setf ,menusym nil)
	  (pop-state game)))

(let ((menu nil))
  (defun menu-index-state (game)
    (with-slots (up down z x) (keystate game)
      (unless menu
	(setf menu (make-menu '(:equip :item))))
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
	(when (and (null menu)
		   (not (null expendables-list)))
	  (setf menu
		(make-item-menu expendables-list)))
	(unless (null menu)
	  (with-slots (size cursor) menu
	    (display-menu menu 70 70 30)
	    (cond ((key-down-p up) (move-next menu))
		  ((key-down-p down) (move-prev menu))
		  ((key-down-p z)
		   (use-expendables (get-cursor menu) game)
		   (update-menu menu expendables-list)
		   (if (null expendables-list)
		       (setf menu nil))
		   (setf cursor (clamp 0 cursor (1- size)))))))
	(when (key-down-p x)
	  (left-menu menu))))))

(defun select-equip-state (game)
  (pop-state game))

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

