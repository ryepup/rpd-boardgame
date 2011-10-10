;;;; rpd-boardgame.asd

(asdf:defsystem #:rpd-boardgame
  :serial t
  :depends-on (#:iterate
	       #:cl-log
               #:alexandria)
  :components
  ((:module :src
    :serial T
    :components
    ((:file "package")
     (:file "rpd-boardgame")))))