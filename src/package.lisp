;;;; package.lisp

(defpackage #:rpd-boardgame
  (:use #:cl #:iterate)
  (:shadowing-import-from #:cl-log
			  #:log-message)
  (:shadowing-import-from #:alexandria
			  #:flatten #:clamp)
  (:export
   :make-board
   :rows
   :columns
   :cell-at
   :row
   :column
   :board
   :coordinates
   :neighbors
   :vertices
   :screen-vertices
   :make-screen
   :do-cells
   :screen
   :cell))

