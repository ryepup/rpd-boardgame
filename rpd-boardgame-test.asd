;;;; rpd-boardgame.asd

(asdf:defsystem #:rpd-boardgame-test
  :serial t
  :depends-on (#:rpd-boardgame #:lisp-unit #:cl-log)
  :components ((:module
		:test
		:serial t
		:components ((:file "package")
			     (:file "board")))))