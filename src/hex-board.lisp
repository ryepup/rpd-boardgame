(in-package #:rpd-boardgame)

(defclass hex-board (board) ())

(defmethod neighbor-cells ((self hex-board) row column)
  `((,(1+ row) ,column)
    (,row ,(1+ column))
    (,(1- row) ,column)
    (,row ,(1- column))
    (,(1+ row) ,(1- column))
    (,(1- row) ,(1+ column))))
