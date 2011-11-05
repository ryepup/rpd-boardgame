(in-package #:rpd-boardgame-tests)

(define-test make-board
  (let ((b (make-board 10 15)))
    (assert-eq 10 (rows b))
    (assert-eq 15 (columns b))))

(define-test cells
  (let* ((b (make-board 10 15))
	 (c (cell-at b 1 0)))
    (assert-true (not (null c)) )
    (assert-eq b (board c))
    (assert-eq 1 (row c))
    (assert-eq 0 (column c))
    (assert-equalp #(1 0) (coordinates c))))

(defmacro assert-set-equalp (l1 l2 &rest extras)
  `(assert-true
    (set-equal ,l1 ,l2 :test #'equalp)
    ,@extras))

(define-test neighbors
  (let* ((b (make-board 10 15))
	 (c (cell-at b 0 0))
	 (neighbors (mapcar #'coordinates
			    (neighbors c)))
	 (full-set (mapcar #'coordinates
			   (neighbors (cell-at b 3 3)))))
    (assert-eq 2 (length neighbors))
    (assert-set-equalp (list #(0 1) #(1 0))
		       neighbors)
    (assert-eq 4 (length full-set))
    (assert-set-equalp (list #(2 3) #(4 3) #(3 2) #(3 4))
		       full-set)))

(define-test vertices
  (let* ((b (make-board 10 15))
	 (v (vertices (cell-at b 0 0))))
    (assert-eq 4 (length v))
    (assert-set-equalp (list #(0 0) #(1 0)
			     #(0 1) #(1 1))
		       v)))

(define-test screen-vertices
  (let ((b (make-board 10 15)))
    (assert-set-equalp (list #(0 0) #(10 0)
			     #(0 10) #(10 10))
		       (screen-vertices (cell-at b 0 0) (make-screen b 150 100)))
    (assert-set-equalp (list #(0 0) #(10 0)
			     #(0 5) #(10 5))
		       (screen-vertices (cell-at b 0 0) (make-screen b 150 50)))))
