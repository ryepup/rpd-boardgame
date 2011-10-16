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

(defmethod screen-vertices ((cell hex-cell) width height)
  (iter
    (with side-length = (/ height (sqrt 3)))
    (with half-width = (round (/ width 2)))
    (with half-height = (round (/ height 2)))
    (with x-offset = (round
		      (sqrt (- (* side-length side-length)
			       (* half-height half-height)))))
    (with narrow-width = (- width x-offset))
    (with face-x = (* narrow-width (column cell)))
    (with face-y = (* height (+ (round (* 0.5 (column cell)))
				(row cell))))
    (for (r c dir) in +hex-vertex-transform+)
    (for x = (+ face-x
		(if (zerop r)
		    (ecase dir
		      (:l (- half-width))
		      (:r half-width))
		    (if (zerop c)
			(ecase dir
			  (:r (- x-offset))
			  (:l x-offset))
			(* c x-offset)))))
    (for y = (+ face-y (* half-height r)))
    (collect (vector x y))))
