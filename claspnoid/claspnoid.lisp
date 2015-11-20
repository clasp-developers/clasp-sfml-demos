;; this should be the path where the sfml libraries are located.
(setf *DEFAULT-PATHNAME-DEFAULTS* #P"~/dev/sfml/clasp-sfml-build/lib/boehm/release/")
(ext:chdir #P"~/dev/clasp-sfml-demos/claspnoid/resources/")

(defvar *main-window* nil "The main window")
(defvar *game-objects* nil "A list of all objects in the game.")
(defvar *bounce-sound* nil)

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

(defclass block (movable-object)
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

(defmethod collide :around ((go1 ball) (go2 block))
  (when (call-next-method)
    (delete go2 *game-objects*)
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

(defun make-blocks (texture-pack)
  (let ((blocks nil))
    (loop for y from 0 to 2 do
	 (loop for x from 0 to 12 do
	      (setf blocks (cons (make-instance 'block) blocks))
	      (setf (drawable (car blocks))
		    (sf:make-sprite-from-texture-rect
		     texture-pack
		     (sf:make-int-rect
		      (mod (* x 70) 279) 0 64 32)))
	      (sf:set-position
	       (drawable (car blocks))
	       (list (+ 15.0 (* x 80))
		     (+ 20.0 (* y 50))))))
    blocks))

(defun run-game ()
  "Run the game"
  (setf *main-window* (sf:make-render-window-vs '(1024 768 32) "Claspnoid"))
  ;(setf *bounce-sound* (sf:make-sound))
  (sf:set-vertical-sync-enabled *main-window* t)
  (let ((basic-pack (sf:make-texture))
	(event (sf:make-event))
	(event-type nil)
	(ball (make-instance 'ball))
	(paddle (make-instance 'paddle))
	;(bounce-sound-buffer (sf:make-sound-buffer))
	(top-border (make-instance 'horizontal-border))
	(bottom-border (make-instance 'horizontal-border))
	(left-border (make-instance 'vertical-border))
	(right-border (make-instance 'vertical-border)))
    (sf:load-from-file basic-pack "BasicArkanoidPack.png" (sf:make-int-rect 0 0 0 0))
    ;(sf:load-from-file bounce-sound-buffer "Select.wav")
    ;(sf:set-buffer *bounce-sound* bounce-sound-buffer)
    (setf (drawable paddle) 
    	  (sf:make-sprite-from-texture-rect
    	   basic-pack
    	   (sf:make-int-rect 0 110 162 26)))
    (sf:set-position (slot-value bottom-border 'drawable) '(0.0 768.0))
    (sf:set-position (slot-value right-border 'drawable) '(1024.0 0.0))
    (sf:set-position (slot-value top-border 'drawable) '(0.0 -5.0))
    (sf:set-position (slot-value left-border 'drawable) '(-5.0 0.0))
    (sf:set-origin (slot-value ball 'drawable) '(3.0 3.0))
    ;(sf:set-origin (slot-value paddle 'drawable) '(40.0 5.0))
    (sf:set-position (slot-value ball 'drawable) '(512.0 700.0))
    (sf:set-position (slot-value  paddle 'drawable) '(512.0 740.0))
    (format t "loaded")
    (setf *game-objects*
	  (append
	   (list ball paddle
		 top-border bottom-border
		 left-border right-border)
	   (make-blocks basic-pack)))
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
	 (mapcar #'move *game-objects*)
	 (loop for (o1 . orest) on *game-objects* do
	      (loop for o2 in orest do
		   (collide o1 o2)))
    	 (sf:clear *main-window* '(0 0 0 255))
	 (mapcar (lambda (obj) (draw *main-window* obj)) *game-objects*)
	 (sf:display *main-window*))))
