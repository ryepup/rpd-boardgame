(in-package #:rpd-boardgame)

(defgeneric cell-at (board row column)
  (:documentation "returns the cell at the given row/column, or nil if out of bounds"))
(defgeneric neighbor-cells (cell))
(defgeneric vertices (cell))

(defclass board ()
  ((rows :accessor rows :initarg :rows)
   (columns :accessor columns :initarg :columns)
   (cells :accessor cells)))

(defmethod initialize-instance :after ((self board) &key (cell-class 'cell) &allow-other-keys)
  (setf (cells self)
	(make-array (list (rows self) (columns self))
		    :element-type cell-class))
  (dotimes (row (rows self))
    (dotimes (col (columns self))
      (setf (aref (cells self) row col)
	    (make-instance cell-class :board self
				      :row row :column col)))))

(defmethod cell-at ((self board) row column)
  (when (and
	 (<= 0 column (columns self))
	 (<= 0 row (rows self)))
    (aref (cells self) row column)))

(defun make-board (rows columns &key (type :square))
  (make-instance 'board
   :rows rows :columns columns
   :cell-class
   (ecase type
     (:square 'cell)
     (:hex 'hex-cell))))

(defclass cell ()
  ((row :accessor row :initarg :row)
   (column :accessor column :initarg :column)
   (board :reader board :initarg :board)))

(defmacro with-coords ((row-var col-var cell) &body body)
  `(with-accessors ((,row-var row) (,col-var column)) ,cell
     ,@body))

(defmethod coordinates ((self cell))
  "returns a #(row col) vector for the cell"
  (vector (row self) (column self)))

(defmethod neighbor-cells ((cell cell))
  (with-coords (row column cell)
    `((,(1+ row) ,column)
      (,row ,(1+ column))
      (,(1- row) ,column)
      (,row ,(1- column)))))

(defmethod neighbors ((self cell)) 
  (iter
    (for (r c) in (neighbor-cells self))
    (for cell = (cell-at (board self) r c))
    (when cell
      (collect cell))))

(defmethod vertices ((cell cell))
  (with-coords (row column cell)
    `(#(,row ,column)
       #(,(1+ row) ,column)
       #(,row ,(1+ column))
       #(,(1+ row) ,(1+ column)))))

(defmethod screen-vertices ((cell cell) width height)
  (iter
    (for coord in (vertices cell))
    (collect (vector (* (aref coord 0) width)
		     (* (aref coord 1) height)))))
