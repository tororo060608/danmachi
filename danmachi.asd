;;;; danmachi.asd

;; To load eval (require :danmachi)
;; To reload eval (asdf:operate 'asdf:load-op :danmachi)

(asdf:defsystem #:danmachi
  :description "Describe danmachi here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :serial t
  :components ((:module "src"
		:serial t
		:components
		((:file "package")
		 (:file "util")
		 (:file "pathname")
		 (:file "loadimage")
		 (:file "key")
		 (:file "object")
		 (:file "character")
		 (:file "player")
		 (:file "enemy")
		 (:file "item")
		 (:file "itemlist")
		 (:file "camera")
		 (:file "create")
		 (:file "contact")
		 (:file "menu")
		 (:file "state")
		 (:file "font")
		 (:file "main"))))
  :depends-on (:lispbuilder-sdl :lispbuilder-sdl-ttf :alexandria :closer-mop :split-sequence :iterate :sdlutils))


