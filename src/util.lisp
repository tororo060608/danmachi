(in-package :danmachi)

(defun half (a)
  (ash a -1))

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

(defmacro draw-strings (&body info-list)
  `(progn
     ,@(loop for i below (length info-list) by 3
	  collect `(sdl:draw-string-solid-* ,(nth i info-list)
					    ,(nth (+ i 1) info-list)
					    ,(nth (+ i 2) info-list)))))

(defun string-conc (&rest strings)
  (apply #'concatenate 'string strings))

