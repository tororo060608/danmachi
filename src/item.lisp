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
