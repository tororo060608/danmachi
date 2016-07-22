(in-package danmachi)

(defun load-map (map game)
  (with-open-file (s (lib-path map))
    (let* ((config (read s))
	   (size (getf config :size))
	   (table (load-table config)))
      (setf (map-width game) (* 32 (first size))
	    (map-height game) ( * 32 (second size)))
      (dotimes (y (second size) game)
	(let ((line (take-nth 2 (read-line s))))
	  (iter (for x to (first size))
	    (for s in line)
	    (when-let (objs (getf table s))
	      (mapc (lambda (obj)
		      ((lambda (o) (add-object o game))
		       (make-instance obj
				      :point-x (* 32 x)
				      :point-y (* 32 y))))
		    (ensure-list objs)))))))))

(defun take-nth (n seq)
  (do ((i 0 (+ i n))
       (res nil (cons (elt seq i) res))
       (len (length seq)))
      ((<= len i) (nreverse res))))

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
