;;;; rpd-boardgame-sdl.asd

(asdf:defsystem #:rpd-boardgame-sdl
  :serial t
  :depends-on (#:rpd-boardgame #:lispbuilder-sdl #:iterate #:bordeaux-threads)
  :components ((:module
		:sdl
		:serial t
		:components ((:file "package")
			     (:file "display")))))
