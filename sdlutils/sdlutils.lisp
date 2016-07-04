;;;; sdlutils.lisp

(in-package #:sdlutils)

;;; "utils" goes here. Hacks and glory await!

(defun take-names (filepath)
  (iter (for x in-file filepath)
	(when (member (car x) '(defun defmacro))
	  (collect (second x)))))

(defun all-lib-name (libpath &rest filepaths)
  (apply #'append
	 (mapcar (lambda (f)
		   (take-names (concatenate 'string libpath f))) 
		 filepaths)))

(defun round-robin (fn lis)
  (mapl (lambda (xs) (mapcar (lambda (x) (funcall fn (car xs) x))
			     (cdr xs)))
	lis))

(defun slot-list (instance)
  (mapcar #'c2mop:slot-definition-name
	  (c2mop:class-slots (class-of instance))))

(defun nmapslot (fn instance)
  (dolist (slot (slot-list instance))
    (setf (slot-value instance slot)
	  (funcall fn (slot-value instance slot)))))

(defun to-s (&rest obj)
  (format nil "~{~a~}" obj))

(defun make-timer (num)
  (let ((i 0))
    (lambda ()
      (if (<= num i)
	  (progn (setf i 1) t)
	  (progn (incf i) nil)))))

(defun charge-timer (num)
  (let ((charge num))
    (lambda (message)
      (case message
	(:charge (progn (setf charge (clamp (1+ charge) 0 num))
			(<= num charge)))
	(:shot (and (<= num charge)
		    (setf charge 0)
		    t))))))

(defun print-if (test exp)
  (if test (print exp) exp))

(defun make-pairs (list)
  (labels ((rec (lis acc)
	     (if (null (cdr lis))
		 (nreverse acc)
		 (destructuring-bind (a b . c) lis
		   (rec c (cons (list a b) acc))))))
    (rec list nil)))
