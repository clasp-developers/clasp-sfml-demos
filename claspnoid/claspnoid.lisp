;; this should be the path where the sfml libraries are located.
(setf *DEFAULT-PATHNAME-DEFAULTS* #P"~/dev/sfml/clasp-sfml-build/lib/boehm/release/")
(ext:chdir #P"~/dev/clasp-sfml-demos/claspnoid/resources/")

(defvar *main-window* nil "The main window")
(defvar *movable-objects* nil "A list of all movable objects in the game.")
(defvar *static-objects* nil "A list of all static objects in the game.")
(defvar *bounce-sound* nil)

(defvar *texture-atlas-more-pieces*
  '(:small-ball (0 138 8 8)
    :large-ball (13 128 16 16)
    :paddle (0 176 80 16)
    :solid-brick (208 32 48 16)
    :checkered-brick (208 64 48 16)
    :striped-brick (208 96 48 16)))

(defun load-sfml ()
  "Load all available SFML libraries."
  (load "libclasp-sfml-window.so")
  (load "libclasp-sfml-graphics.so")
  (load "libclasp-sfml-audio.so"))

(load-sfml)

(defgeneric draw (draw-target to-draw)
  (:documentation "Draw the GAME-OBJECT onto the screen"))

(defgeneric collide (go1 go2)
  (:documentation "Check whether GO1 and GO2 collide and perform the appropriate action."))

(defgeneric move (to-move)
  (:documentation "Move TO-MOVE by the specified MOVE-VECTOR"))

(defclass drawable-object ()
    ((drawable :initarg drawable
	       :accessor drawable)))

(defclass bounding-box-collider () ())

(defclass game-object (drawable-object bounding-box-collider) ())

(defclass movable-object (game-object)
  ((move-vector :initarg move-vector
		:accessor move-vector
		:initform '(0.0 0.0))))

(defclass paddle (movable-object)
  ((drawable
    :initform
    (sf:make-rectangle-shape '(80.0 10.0)))))

(defclass ball (movable-object)
  ((drawable
    :initform
    (sf:make-circle-shape 6 30))
   (move-vector :initform '(5.0 -5.0))))

(defclass brick (movable-object)
  ((drawable
    :initform
    (sf:make-rectangle-shape '(90.0 20.0)))))

(defclass border (movable-object) ())

(defclass horizontal-border (border)
  ((drawable
     :initform
     (sf:make-rectangle-shape '(1024.0 5.0)))))

(defclass vertical-border (border)
  ((drawable
     :initform
     (sf:make-rectangle-shape '(5.0 768.0)))))

(defun about-equal (n1 n2)
  (< (abs (- n1 n2)) 0.001))

(defmethod draw (draw-target (to-draw drawable-object))
  (sf:draw draw-target (slot-value to-draw 'drawable)))

(defun draw-to-main-window (to-draw)
  (draw *main-window* to-draw))

(defmethod move ((to-move movable-object))
  (sf:move (slot-value to-move 'drawable) (slot-value to-move 'move-vector)))

(defmethod move :after ((to-move paddle))
  (setf (move-vector to-move) '(0.0 0.0)))


;; (defmethod collide ((go1 game-object) (go2 game-object))
;;   (sf:float-rect-intersects
;;    (sf:get-global-bounds (drawable go1))
;;    (sf:get-global-bounds (drawable go2))))


(defmethod collide ((go1 game-object) (go2 game-object))
  (let* ((intersection-area (sf:make-float-rect 0.0 0.0 0.0 0.0))
	 (intersects 
	  (sf:float-rect-intersects-area
	   (sf:get-global-bounds (drawable go1))
	   (sf:get-global-bounds (drawable go2))
	   intersection-area)))
    (values intersects intersection-area)))

(defun change-y-move (go1 go2)
  (setf (cadr (move-vector go1)) (* -1 (cadr (move-vector go1))))
  (setf (cadr (move-vector go2)) (* -1 (cadr (move-vector go2)))))

(defun change-x-move (go1 go2)
  (setf (car (move-vector go1)) (* -1 (car (move-vector go1))))
  (setf (car (move-vector go2)) (* -1 (car (move-vector go2)))))

(defun apply-collision-effect (go1 go2 intersection-area)
  (let* ((go1rect (sf:get-global-bounds (drawable go1)))
	 (go2rect (sf:get-global-bounds (drawable go2)))
	 (int-width (sf:float-rect-width intersection-area))
	 (int-height (sf:float-rect-height intersection-area))
	 (go1-width (sf:float-rect-width go1rect))
	 (go1-height (sf:float-rect-height go1rect))
	 (go2-width (sf:float-rect-width go2rect))
	 (go2-height (sf:float-rect-height go2rect)))
    (cond ((or (= int-width go1-width)
	       (= int-width go2-width))
	   (change-y-move go1 go2))
	  ((or (= int-height go1-height)
	       (= int-height go2-height))
	   (change-x-move go1 go2))
	  ((and (< (int-width go1-width)) (< (int-width go2-width))
		(< (int-height go1-height)) (< (int-height go2-height)))
	   (change-x-move go1 go2)
	   (change-y-move go1 go2)))))

(defmethod collide :around ((go1 movable-object) (go2 movable-object))
  (let* ((intersects nil)
	 (intersection-area nil))
    (multiple-value-bind (intersects intersection-area) (call-next-method)
      (when intersects
	(apply-collision-effect go1 go2 intersection-area)))))

(defmethod collide :around ((go1 ball) (go2 brick))
  (when (call-next-method)
    (delete go2 *static-objects*)
    ;(sf:play *bounce-sound*)
    ))

;; (defmethod collide :around ((go1 ball) (go2 paddle))
;;   (when (call-next-method)
;;     (setf  (cadr (move-vector go1)) (* -1 (cadr (move-vector go1))))))

;; (defmethod collide :around ((go1 ball) (go2 vertical-border))
;;   (when (call-next-method)
;;     (setf  (car (move-vector go1)) (* -1 (car (move-vector go1))))))

;; (defmethod collide :around ((go1 ball) (go2 horizontal-border))
;;   (when (call-next-method)
;;     (setf  (cadr (move-vector go1)) (* -1 (cadr (move-vector go1))))))

(defun make-bricks (texture-pack)
  (let ((bricks nil))
    (loop for y from 0 to 2 do
	 (loop for x from 0 to 12 do
	      (setf bricks (cons (make-instance 'brick) bricks))
	      (setf (drawable (car bricks))
		    (sf:make-sprite-from-texture-rect
		     texture-pack
		     (sf:make-int-rect
		      (mod (* x 70) 279) 0 64 32)))
	      (format t "drawable of brick: ~a" (drawable (car bricks)))
	      (sf:set-position
	       (drawable (car bricks))
	       (list (+ 15.0 (* x 80.0))
		     (+ 20.0 (* y 50.0))))))
    bricks))

(defun get-sprite-rect (texture-atlas sprite-kind &optional color)
  (let ((x-offset 0)
	(y-offset 32)
	(rect (getf texture-atlas sprite-kind)))
    (when (or (eq color :yellow) (eq color :dark-blue) (eq color :brown))
      (setf x-offset 384))
    (when (or (eq color :pink) (eq color :beige) (eq color :gray))
      (setf x-offset 768))
    (when (or (eq color :light-blue) (eq color :dark-blue) (eq color :beige))
      (setf y-offset 205))
    (when (or (eq color :green) (eq color :brown) (eq color :gray))
      (setf x-offset 481))
    (+ (car rect) x-offset)
    (+ (cadr rect) y-offset)
    rect))

(defun create-sprite (texture texture-atlas sprite-kind &optional color)
  "Create a sprite from TEXTURE. SPRITE-KIND defines what kind of sprite
should be created. COLOR defines the color."
  (let ((rect (get-sprite-rect texture-atlas sprite-kind color)))
    (sf:make-sprite-from-texture-rect
     texture
     (apply #'sf:make-int-rect rect))))


(defun make-level (brick-list
		   &optional (x-offset 100) (y-offset 16)
		     (x-start 32.0) (y-start 32.0))
  (loop for inner-brick-list in brick-list
     counting t into y-count
     with object-list = nil
     finally (return (nreverse object-list)) do
       (loop for brick-type in inner-brick-list
	  counting t into x-count do
	    (let ((brick-instance (make-instance brick-type)))
	      (sf:set-position
	       (drawable brick-instance)
	       (list (+ x-start (* x-count x-offset))
		     (+ y-start (* y-count y-offset))))
	      (setf object-list (cons brick-instance object-list))))))

(defvar *level1*
  '((brick brick brick brick brick)))

(defun run-game ()
  "Run the game"
  (setf *main-window* (sf:make-render-window-video-mode '(1024 768 32) "Claspnoid"))
  ;(setf *bounce-sound* (sf:make-sound))
  (sf:set-vertical-sync-enabled *main-window* t)
  (let ((basic-pack (sf:make-texture))
	(more-pieces (sf:make-texture))
	(event (sf:make-event))
	(event-type nil)
	(ball (make-instance 'ball))
	(paddle (make-instance 'paddle))
	;(bounce-sound-buffer (sf:make-sound-buffer))
	(top-border (make-instance 'horizontal-border))
	(bottom-border (make-instance 'horizontal-border))
	(left-border (make-instance 'vertical-border))
	(right-border (make-instance 'vertical-border)))
    (sf:load-from-file/texture basic-pack "BasicArkanoidPack.png" (sf:make-int-rect 0 0 0 0))
    (sf:load-from-file/texture more-pieces "more_breakout_pieces.png" (sf:make-int-rect 0 0 0 0))
    ;(sf:load-from-file bounce-sound-buffer "Select.wav")
    ;(sf:set-buffer *bounce-sound* bounce-sound-buffer)
    ;; (setf (drawable paddle) 
    ;; 	  (sf:make-sprite-from-texture-rect
    ;; 	   basic-pack
    ;; 	   (sf:make-int-rect 0 110 162 26)))
    (setf (drawable paddle)
	  (create-sprite  more-pieces *texture-atlas-more-pieces* :paddle :yellow))
    (setf (drawable ball)
	  (create-sprite more-pieces *texture-atlas-more-pieces* :large-ball :yellow))
    (sf:set-position (slot-value bottom-border 'drawable) '(0.0 768.0))
    (sf:set-position (slot-value right-border 'drawable) '(1024.0 0.0))
    (sf:set-position (slot-value top-border 'drawable) '(0.0 -5.0))
    (sf:set-position (slot-value left-border 'drawable) '(-5.0 0.0))
    (sf:set-origin (slot-value ball 'drawable) '(3.0 3.0))
    ;(sf:set-origin (slot-value paddle 'drawable) '(40.0 5.0))
    (sf:set-position (slot-value ball 'drawable) '(512.0 700.0))
    (sf:set-position (slot-value  paddle 'drawable) '(512.0 740.0))
    (setf *movable-objects*
	  (list ball paddle))
    (setf *static-objects*
	  (append
	   (list top-border bottom-border
		 left-border right-border)
	   (make-level *level1* )))
    (loop while (sf:is-open *main-window*) do
    	 (loop while (sf:poll-event *main-window* event) do
    	      (setf event-type (sf:type event))
    	      (cond
    		((eq event-type 'CLOSED) 
    		 (sf:close *main-window*))))
    	 (when (sf::keyboard/is-key-pressed 'keyboard-key/left)
    	   (setf (move-vector paddle) '(-10.0 0.0)))
    	 (when (sf::keyboard/is-key-pressed 'keyboard-key/right)
    	   (setf (move-vector paddle) '(10.0 0.0)))
	 (mapcar #'move *movable-objects*)
	 (loop for (o1 . orest) on *movable-objects* do
	      (loop for o2 in orest do
		   (collide o1 o2)))
	 (loop for o1 in *movable-objects* do
	      (loop for o2 in *static-objects* do
		   (collide o1 o2)))
    	 (sf:clear *main-window* '(0 0 0 255))
	 (mapcar #'draw-to-main-window *movable-objects*)
	 (mapcar #'draw-to-main-window *static-objects*)
	 (sf:display *main-window*))))
