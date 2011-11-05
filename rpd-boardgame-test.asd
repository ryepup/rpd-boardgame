;;;; rpd-boardgame.asd

(asdf:defsystem #:rpd-boardgame-test
  :serial t
  :depends-on (#:rpd-boardgame #:lisp-unit #:cl-log #:vecto)
  :components ((:module
		:test
		:serial t
		:components ((:file "package")
			     (:file "board")
			     (:file "hex-board")
			     (:file "drawing")))))
