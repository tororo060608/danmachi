(in-package danmachi)

(defvar *block-size* 32)

(defun load-map (map game)
  (with-open-file (s (lib-path map))
    (let* ((config (read s))
	   (size (getf config :size))
	   (table (load-table config)))
      (setf (map-width game) (* *block-size* (first size))
	    (map-height game) ( * *block-size* (second size)))
      (dotimes (y (second size) game)
	(let ((line (or (take-nth 2  (read-line s)) (take-nth 2 (read-line s)))))
	  (iter (for x to (first size))
	    (for s in line)
	    (when-let (objs (getf table s))
	      (mapc (lambda (obj)
		      ((lambda (o) (add-object o game))
		       (make-instance obj
				      :point-x (+ (half *block-size*) (* *block-size* x))
				      :point-y (+ (half *block-size*) (* *block-size* y)))))
		    (ensure-list objs)))))))))


(defun load-map-size (config)
  (getf config :size))

(defun load-table (config)
  (let ((raw (getf config :table)))
    (mapplist (lambda (key val)
		((lambda (newkey) (values newkey val))
		 (let ((strkey (string-downcase 
				(string key))))
		   (if (= (length strkey) 1)
		       (char strkey 0)
		       (case key
			 (space #\space)
			 (t (error "Loading map table is fail. ~a can't represent charactor" key)))))))
	      raw)))


(defun mapplist (func plist &optional (acc nil))
  (cond ((null plist) (nreverse acc))
	((not (listp (cdr plist)))
	 (error "This is not plist"))
	(t (destructuring-bind (key value . rest) plist
	     (multiple-value-bind (nkey nvalue) 
		 (funcall func key value)
	       (mapplist func rest (cons nvalue
					 (cons nkey acc))))))))
