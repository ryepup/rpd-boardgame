(in-package #:rpd-boardgame-tests)

(define-test make-board/hex
  (let ((b (make-board 10 15 :type :hex)))
    (assert-eq 10 (rows b))
    (assert-eq 15 (columns b))))

(define-test cells/hex
  (let* ((b (make-board 10 15 :type :hex))
	 (c (cell-at b 1 0)))
    (assert-true (not (null c)) )
    (assert-eq b (board c))
    (assert-eq 1 (row c))
    (assert-eq 0 (column c))
    (assert-equalp #(1 0) (coordinates c))))


(define-test neighbors/hex
  (let* ((b (make-board 10 15 :type :hex))
	 (c (cell-at b 0 0))
	 (neighbors (mapcar #'coordinates
			    (neighbors c)))
	 (full-set (mapcar #'coordinates
			   (neighbors (cell-at b 3 3)))))
    (assert-eq 2 (length neighbors))
    (assert-set-equalp (list #(0 1) #(1 0))
		       neighbors)
    (assert-eq 6 (length full-set))
    (assert-set-equalp (list
			#(3 4) #(4 3)
			#(4 2) #(2 4)
			#(2 3) #(3 2))
		       full-set)))

(define-test vertices/hex
  (let* ((b (make-board 10 15 :type :hex))
	 (v (vertices (cell-at b 0 0))))
    (assert-eq 6 (length v)) 
    (assert-set-equalp (list #(0 0 :l)
			     #(0 0 :r)
			     #(1 0 :l)
			     #(1 -1 :l)
			     #(-1 0 :r)
			     #(-1 1 :r))
		       v)))