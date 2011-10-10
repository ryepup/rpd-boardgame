(in-package #:rpd-boardgame)

(defgeneric cell-at (board row column)
  (:documentation "returns the cell at the given row/column, or nil if out of bounds"))
(defgeneric neighbor-cells (board row column))

(defclass board ()
  ((rows :accessor rows :initarg :rows)
   (columns :accessor columns :initarg :columns)
   (cells :accessor cells)))

(defclass cell ()
  ((row :accessor row :initarg :row)
   (column :accessor column :initarg :column)
   (board :reader board :initarg :board)))

(defmethod coordinates ((self cell))
  "returns a #(row col) vector for the cell"
  (vector (row self) (column self)))

(defmethod initialize-instance :after ((self board) &rest args)
  (declare (ignore args))
  (setf (cells self)
	(make-array (list (rows self) (columns self))
		    :element-type 'cell))
  (dotimes (row (rows self))
    (dotimes (col (columns self))
      (setf (aref (cells self) row col)
	    (make-instance 'cell :board self
				 :row row :column col)))))

(defmethod cell-at ((self board) row column)
  (when (and
	 (<= 0 column (columns self))
	 (<= 0 row (rows self)))
    (aref (cells self) row column)))

(defmethod neighbor-cells ((self board) row column)
  `((,(1+ row) ,column)
    (,row ,(1+ column))
    (,(1- row) ,column)
    (,row ,(1- column))))

(defmethod neighbors ((self cell))
  (with-accessors ((row row)
		   (column column)
		   (board board)) self
    (iter
      (for (r c) in (neighbor-cells board row column))
      (for cell = (cell-at board r c))
      (when cell
	(collect cell)))))

(defun make-board (rows columns &key (type :square))
  (make-instance
   (ecase type
     (:square 'board)
     (:hex 'hex-board))
   :rows rows :columns columns))
