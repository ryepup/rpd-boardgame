(in-package #:rpd-boardgame)

(defclass screen ()
  ((width :reader width :initarg :width)
   (height :reader height :initarg :height)
   (board :reader board :initarg :board)))

(defun make-screen (board width height)
  (make-instance 'screen
		 :width width :height height
		 :board board))

(defmethod screen-vertices ((cell cell) (screen screen)) 
  (iter
    (with cell-w = (round (/ (width screen) (columns (board screen)))))
    (with cell-h = (round (/ (height screen) (rows (board screen)))))
    ;;TODO: cache things north of here
    (for coord in (vertices cell))
    (collect (vector (* (aref coord 0) cell-w)
		     (* (aref coord 1) cell-h)))))

(defun screen-vertices/cell-metrics (rows columns
				     screen-w screen-h)
  (let* (
	 ;;counting the northward creep from the hexes,
	 ;; what is the largest value that will fit in
	 ;; the screen?
	 ;; H = (* cell-height (+ rows (/ (1- columns) 2)))
	 ;; we know H, solve for cell-height:
	 (max-cell-height (/ screen-h
			     (+ rows
				(/ (1- columns)
				   2))))
	 ;;using 30-60-90 triangle math, calc the cell width
	 ;; from the height
	 (cell-width-for-max-height
	   (/ (* 2 max-cell-height)
	      (sqrt 3)))
	 ;;what is the largest width value that will fit?
	 (max-cell-width (/ screen-w
			    (+
			     (floor (/ (1+ columns) 2))
			     (/ (floor (/ columns 2))
				(sqrt 3)))))
	 ;;which direction is the limiting factor?
	 (constrained-axis
	   (if (< max-cell-width cell-width-for-max-height)
	       :width
	       :height))
	 ;;depending on the constraint, calculate best fits
	 (cell-width (ecase constrained-axis
		       (:width max-cell-width)
		       (:height cell-width-for-max-height)))
	 #+nil(cell-height (ecase constrained-axis
			(:width (* (sqrt 3)
				   (/ max-cell-width 2)))
			(:height max-cell-height)))
	 (offset (/ cell-width 4)))
    (values offset)))

(defun screen-vertices/face-metrics (screen)
  ;;TODO: cache everything here on the screen
  (let* ((x-offset (screen-vertices/cell-metrics
		    (rows (board screen))
		    (columns (board screen))
		    (width screen) (height screen)))
	 (half-width (* x-offset 2))
	 (half-height (* x-offset (sqrt 3)))
	 (height (* half-height 2))
	 (narrow-width (* 3 x-offset)))
    (values x-offset half-width half-height height
	    narrow-width)))

(defun screen-vertices/face (row column screen)
  (multiple-value-bind
	(x-offset half-width half-height height narrow-width)
      (screen-vertices/face-metrics screen)
    (let ((face-x (+ half-width (* narrow-width column)))
	  (face-y (+ half-height
		     (* height (+ (/ column 2) row)))))
      (values face-x face-y half-width half-height x-offset))))

(defmethod screen-vertices ((cell hex-cell) (screen screen))
  (multiple-value-bind
	(face-x face-y half-width half-height x-offset)
      (screen-vertices/face (row cell) (column cell) screen) 
    (iter    
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
      (collect (vector (round x) (round y))))))

(defmethod cell-at ((screen screen) y x)
  (etypecase (cell-at (board screen) 0 0)
    (hex-cell
     (multiple-value-bind
	   (x-offset half-width half-height height narrow-width)
	 (screen-vertices/face-metrics screen)
       (declare (ignore x-offset half-width half-height))
       (flet ((dist (cell)
		(multiple-value-bind (face-x face-y) (screen-vertices/face (row cell) (column cell) screen)
		  (+ (expt (- x face-x) 2)
		     (expt (- y face-y) 2)))
		))
	 (let* ((col (/ x narrow-width))
		(row (- (/ y height) (/ col 2)))
		(c (ignore-errors (cell-at (board screen) (floor row) (floor col)))))
	   (when c
	     ;;find the closest neighbor
	     (iter
	       (with winner = c)
	       (with min-dist = (dist c))
	       (for cell in (neighbors c))
	       (for d = (dist cell))
	       (when (< d min-dist)
		 (setf min-dist d
		       winner cell))
	       (finally (return winner))
	       ))))))
    (cell
     (let ((cell-w (round (/ (width screen) (columns (board screen)))))
	   (cell-h (round (/ (height screen) (rows (board screen))))))
       (cell-at (board screen)
		(floor (/ x cell-w))
		(floor (/ y cell-h)))))
    )
  )