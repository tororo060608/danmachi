(in-package :danmachi)

(defun parent-directory (path)
  (let ((pos (position #\/ path :from-end t :end (1- (length path)))))
    (if pos (subseq path 0 (1+ pos)))))

(defvar *current-path* (load-time-value
			(or #.*compile-file-pathname* 
			    *load-pathname*)))
(defvar *dir-path* (directory-namestring *current-path*))
(defvar *lib-path* (to-s (parent-directory *dir-path*) "lib/"))

(defun lib-path (path)
  (to-s *lib-path* path))









