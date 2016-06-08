;;;; sdlutils.asd

(asdf:defsystem #:sdlutils
  :description "Describe utils here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :serial t
  :components ((:file "package")
	       (:file "sdlutils")
	       (:file "macro")
	       (:file "input")
	       (:file "pathname")
	       (:file "vector")
               (:file "image"))
  :depends-on (:alexandria :closer-mop :lispbuilder-sdl :iterate))

