;;;; rpd-boardgame.asd

(asdf:defsystem #:rpd-boardgame
  :serial t
  :depends-on (#:iterate
	       #:cl-log
               #:alexandria)
  :components ((:file "package")
               (:file "rpd-boardgame")))

