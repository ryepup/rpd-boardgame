;;;; package.lisp

(defpackage #:rpd-boardgame
  (:use #:cl #:iterate)
  (:shadowing-import-from #:cl-log
			  #:log-message))

