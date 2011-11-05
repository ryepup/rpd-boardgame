(in-package #:rpd-boardgame)

(defclass hex-cell (cell) ())

(defmethod neighbor-cells ((self hex-cell))
  (with-coords (row column self)
    `((,(1+ row) ,column)
      (,row ,(1+ column))
      (,(1- row) ,column)
      (,row ,(1- column))
      (,(1+ row) ,(1- column))
      (,(1- row) ,(1+ column)))))

(defparameter +hex-vertex-transform+
  '((0 0 :l)
    (0 0 :r)
    (1 0 :l)
    (1 -1 :l)
    (-1 0 :r)
    (-1 1 :r)))

(defmethod vertices ((cell hex-cell))
  (with-coords (row column cell)
    (iter (for (r c dir) in +hex-vertex-transform+)
      (collect (vector (+ r row)
		       (+ c column)
		       dir)))))
