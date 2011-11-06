(in-package :rpd-boardgame-sdl)

(defmethod render ((coords simple-vector)
		   &key
		     ((:surface sdl:*default-surface*) sdl:*default-surface*)
		     ((:color sdl:*default-color*) sdl:*default-color*)
		     fill
		     (board *current-board*)
		     &allow-other-keys)
  (assert (not (null board)) nil "Must have a current board.")
  (render (cell-at board (aref coords 1) (aref coords 0))
	  :fill fill
	  :screen (make-screen board (sdl:width sdl:*default-surface*)
			       (sdl:height sdl:*default-surface*))
	  )
  )

(defmethod render ((board board)
		   &key
		     ((:surface sdl:*default-surface*) sdl:*default-surface*)
		     ((:color sdl:*default-color*) sdl:*default-color*)
		     &allow-other-keys)
  ;;create a screen based on the surface size
  (render (make-screen board (sdl:width sdl:*default-surface*)
		       (sdl:height sdl:*default-surface*))))

(defmethod render ((screen screen)
		   &key
		     ((:surface sdl:*default-surface*) sdl:*default-surface*)
		     ((:color sdl:*default-color*) sdl:*default-color*) &allow-other-keys)
  (do-cells (c) screen
    (render c :screen screen))
  (sdl:update-display))

(defmethod render ((cell cell)
		   &key
		     ((:surface sdl:*default-surface*) sdl:*default-surface*)
		     ((:color sdl:*default-color*) sdl:*default-color*)
		     fill
		     screen &allow-other-keys)
  (assert (not (null screen)) screen "must pass in the screen")
  (let ((pts (iter (for v in (screen-vertices cell screen))
	       (collect (sdl:point :x (svref v 0) :y (svref v 1))))))
    (when fill
	(sdl:draw-filled-polygon pts :color fill))
    (sdl:draw-polygon pts)))


(defvar *test-window-thread* nil "the thread running the test window")

(defun test-window (&key (h 480) (w 640) (r 20) (c 20) (type :hex))
  (let* ((b (make-board r c :type type))
	 (s (make-screen b w h))
	 highlighted-cell)
    (sdl:with-init ()
      (sdl:window w h :title-caption "boardgame test window"
		  :resizable T)
      (setf (sdl:frame-rate) 30)
      (render s :color sdl:*white*)
      
      (sdl:with-events ()
	(:VIDEO-RESIZE-EVENT (:W W :H H)  
			     (setf s (make-screen b w h))
			     (sdl:clear-display sdl:*black*)
			     (render s :color sdl:*white*)) 
	(:quit-event () T)
	(:MOUSE-MOTION-EVENT (:STATE STATE :X X :Y Y :X-REL X-REL :Y-REL Y-REL)
			     (declare (ignore state x-rel y-rel))
			     ;;highlight the current cell
			     (alexandria:when-let ((c (cell-at s y x)))
			       (when (and highlighted-cell
					  (not (eq c highlighted-cell)))
				 (render highlighted-cell :screen s :fill sdl:*black*)
				 (dolist (c (neighbors highlighted-cell))
				   (render c :screen s :fill sdl:*black*)))
			       
			       (render c :screen s :fill sdl:*green*)
			       (dolist (c (neighbors c))
				 (render c :screen s :fill sdl:*cyan*))
			       (setf highlighted-cell c)
			       )

			     ) 
	(:video-expose-event () (sdl:update-display))
	(:idle () (sdl:update-display))))))

(defun start-test-window (&rest args)
  (setf *test-window-thread*
	(sb-thread:make-thread #'(lambda () (apply #'test-window args))
			       :name "sdl test window"))

  )