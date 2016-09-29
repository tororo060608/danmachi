(in-package :danmachi)

(defun take-nth (n seq)
  (do ((i 0 (+ i n))
       (res nil (cons (elt seq i) res))
       (len (length seq)))
      ((<= len i) (nreverse res))))

(defmacro set-nil (&rest objs)
  `(progn
     ,@(mapcar (lambda (x) `(setf ,x nil))
	       objs)))

(let ((before nil)
      (timer (make-timer 20)))
  (defun show-framerate ()
    (if (funcall timer)
	(let ((time (sdl:sdl-get-ticks)))
	  (if (and before (< before time))
	      (print (/ 1000.0 (/ (- time before) 20))))
	  (setf before time)))))

(defun plist-keys (plist)
  (loop for i below (length plist) by 2
       collect (nth i plist)))

(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

(defmacro awhen (test-form &body body)
  `(aif ,test-form
        (progn ,@body)))
