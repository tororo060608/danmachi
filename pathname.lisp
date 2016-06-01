(in-package :danmachi)

(defvar *current-path* (load-time-value
			(or #.*compile-file-pathname* 
			    *load-pathname*)))
(defvar *dir-path* (directory-namestring *current-path*))



