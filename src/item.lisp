(in-package :danmachi)

(define-class item ()
  name
  description)

(define-class equipment (item)
  (atk 0)
  (df 0))

(define-class weapon (equipment)
  (atk-area (list 0 0)))

(define-class protect (equipment))

(define-class adornment (equipment))

(define-class expendables (item)
  (effect (lambda (game) (declare (ignore game)) nil)))

(define-class material (item))

(defmethod equip-effect ((equip equipment) (game game))
  (setf (atk (player game)) (+ (atk-default (player game)) (atk equip))
	(df (player game)) (+ (df-default (player game)) (df equip))))

(defparameter *item-table* (make-hash-table))

(defun get-item (namesym)
  (gethash namesym *item-table*))

(defmacro defitem (sym type name description &rest args)
  `(setf (gethash ',sym *item-table*)
	 (make-instance ',type
			:name ,name
			:description ,description
			,@args)))
(defmacro defitems (&rest definition)
  `(progn ,@(mapcar (lambda (def) (cons 'defitem def))
		   definition)))

(defun item-type (itemsym)
  (let ((item (get-item itemsym)))
    (cond ((typep item 'equipment)
	   (cons :equipment (equip-type itemsym)))
	   ((typep item 'expendables)
	    (list :expendables))
	   ((typep item 'material)
	    (list :material)))))
(defun equip-type (itemsym)
  (let ((item (get-item itemsym)))
    (cond ((typep item 'weapon) (list :weapon))
	  ((typep item 'protect) (list :protect))
	  ((typep item 'adornment) (list :adornment)))))
(defun item-typep (typesym itemsym)
  (if (find (intern (symbol-name typesym) "KEYWORD")
	    (item-type itemsym))
      t nil))

(defmacro itemtype-case (itemsym &body cases)
  `(cond ,@(loop for c in cases
	      collect `((item-typep ,(car c) ,itemsym)
			,@(cdr c)))))

(defun add-itemlist (plist itemsym)
  (if (null (getf plist itemsym))
      (setf (getf plist itemsym) 1)
      (incf (getf plist itemsym)))
  plist)
(defmacro push-itemlist (itemsym item-plist)
  `(setf ,item-plist
	 (add-itemlist ,item-plist ,itemsym)))
(defun push-item (itemsym player)
  (itemtype-case itemsym
    ('weapon (push-itemlist itemsym (weapon-list player)))
    ('protect (push-itemlist itemsym (protect-list player)))
    ('adornment (push-itemlist itemsym (adornment-list player)))
    ('expendables (push-itemlist itemsym (expendables-list player)))
    ('material (push-itemlist itemsym (material-list player)))))


(defun rem-itemlist (plist itemsym)
  (if (null (getf plist itemsym))
      (error "remove item ~A failed" itemsym)
      (progn 
	(decf (getf plist itemsym))
	(when (zerop (getf plist itemsym))
	  (remf plist itemsym))
	plist)))
(defmacro delete-itemlist (itemsym item-plist)
  `(setf ,item-plist
	 (rem-itemlist ,item-plist ,itemsym)))
(defun delete-item (itemsym player)
    (itemtype-case itemsym
      ('weapon (delete-itemlist itemsym (weapon-list player)))
      ('protect (delete-itemlist itemsym (protect-list player)))
      ('adornment (delete-itemlist itemsym (adornment-list player)))
      ('expendables (delete-itemlist itemsym (expendables-list player)))
      ('material (delete-itemlist itemsym (material-list player)))))

(defun use-expendables (itemsym game)
  (funcall (effect (get-item itemsym)) game)
  (delete-item itemsym (player game)))
