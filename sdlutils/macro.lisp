(in-package sdlutils)

(defmacro define-class (class-name parent  &rest res)
  `(defclass ,class-name ,parent
     ,(mapcar (lambda (lis)
		(if (listp lis)
		    (apply(lambda (x &optional (y nil) (z x))
			    `(,x :initarg 
				 ,(intern (symbol-name x) "KEYWORD") 
				 :initform ,y :accessor ,z))
			  lis)
		    ((lambda (x) 
		       `(,x :initarg 
			    ,(intern (symbol-name x) "KEYWORD") 
			    :initform nil :accessor ,x))
		     lis)))
	      res)))

(defmacro whens (&body body)
  `(progn
     ,@(loop for b in body collect `(when ,@b))))

(defmacro definteract-method (method-name arg1 arg2 &body body)
  `(progn
     (defmethod ,method-name (,arg1 ,arg2)
       ,@body)
     (defmethod ,method-name (,arg2 ,arg1)
       ,@body)))


(defmacro defcollide (arg1 arg2 &body body)
  (if (some (lambda (x) (eq (car body) x)) 
	    '(:before :after :around))
      `(progn
	 (defmethod collide ,(car body) (,arg1 ,arg2 game)
	   ,@(cdr body))
	 (defmethod collide ,(car body) (,arg2 ,arg1 game)
	   ,@(cdr body)))
      `(progn
	 (defmethod collide (,arg1 ,arg2 game)
	   ,@body)
	 (defmethod collide (,arg2 ,arg1 game)
	   ,@body))))

(defmacro pmif (test num)
  (with-gensyms (gnum)
    `(let ((,gnum ,num))
       (if ,test ,gnum (- ,gnum)))))

(defmacro alambda (vars &body body)
  `(labels ((self ,vars ,@body))
     #'self))

(defmacro letrec (vars &body body)
  `(labels ((rec ,(mapcar #'first vars) ,@body))
     (rec ,@(mapcar #'second vars))))
(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))

(defmacro dbind (a b &body body)
  `(destructuring-bind ,a ,b ,@body))
