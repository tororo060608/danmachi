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

