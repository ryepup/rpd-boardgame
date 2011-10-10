;;;; rpd-boardgame.asd

(asdf:defsystem #:rpd-boardgame
  :serial t
  :depends-on (#:iterate
	       #:cl-log
               #:alexandria)
  :components ((:file "package")
               (:file "rpd-boardgame")))

(asdf:defsystem #:rpd-boardgame-test
  :serial t
  :depends-on (#:rpd-boardgame #:lisp-unit #:cl-log)
  :components ((:module
		:test
		:serial t
		:components ((:file "package")))))