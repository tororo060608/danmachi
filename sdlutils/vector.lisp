(in-package sdlutils)

(defun rad (degree) (* pi (/ degree 180.0)))

(defun vec-abs (x y)
  (sqrt (+ (* x x) (* y y))))

(defun euc-dist (x1 y1 x2 y2)
  (vec-abs (- x1 x2) (- y1 y2)))

(defun univec (x y)
  (let ((dist (vec-abs x y)))
    (if (zerop dist)
	(list 0.0 0.0)
	(list (/ x dist) (/ y dist)))))

(defun dir-univec (sx sy tx ty)
  (let ((vec (univec (- tx sx) (- ty sy))))
    (list (first vec) (second vec))))

(defun a-to-b-vector (a b &optional (xfun #'car) (yfun #'cdr))
  (list (- (funcall xfun b) (funcall xfun a))
	(- (funcall yfun b) (funcall yfun a))))

(defun distance (a b &optional (xfun #'car) (yfun #'cdr))
  (sqrt (+ (expt (- (funcall xfun a) (funcall xfun b)) 2)
	   (expt (- (funcall yfun a) (funcall yfun b)) 2))))

(defun uvec (a b &optional (xfun #'car) (yfun #'cdr))
  (let ((dis (distance a b xfun yfun)))
    (mapcar (lambda (x) (float (/ x dis)))
	    (a-to-b-vector a b xfun yfun))))
