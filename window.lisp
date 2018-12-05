;;;; window.lisp
(in-package #:mkvid)
(in-readtable :qtools)

(defun rslot-value (object &rest slots)
  "Retrieve the slots repeatedly from OBJECT.

For a list of slot names ('one 'two 'three ...),
first retrieve slot ONE of OBJECT,
then retrieve slot TWO of the result thereof,
then retrieve slot THREE of that result thereof,
and so on until the end of SLOTS is reached."
  (loop for i in slots
        for acc = (slot-value object i) then (slot-value acc i)
        finally (return acc)))

;;; Main canvas type that everything hangs on, plus the main window
(define-widget main-window (QMainWindow) ())
(define-widget main-widget (QWidget) ())
(define-subwidget (main-window central-widget) (make-instance 'main-widget)
  (setf (q+:central-widget main-window) central-widget))

(define-widget canvas (QWidget)
  ((progression :initarg :progression :accessor progression)
   (scene :initform (make-instance 'flare:scene) :reader scene)
   (background :initform *background-colour* :accessor background)
   (main-window :initarg :main-window :reader main-window)))

(define-subwidget (main-widget stage)
    ;; Temporary widget to have something hold on to the slot
    ;; before the real one takes over
    (make-instance 'canvas :main-window main-widget
                           :width 1024
                           :height 576
                           :progression nil))

(defmethod initialize-instance :after ((canvas canvas) &key width height)
  (setf (q+:fixed-size canvas) (values width height)))

;;; Animation
(defclass presentation (flare:progression-definition)
  ((width :initarg :width :reader width)
   (height :initarg :height :reader height))
  (:documentation "A progression-definition embedded with height and width
to allow relative measurements to take place.")
  (:default-initargs
   :width *width*
   :height *height*))

(define-subwidget (canvas timer) (q+:make-qtimer canvas)
  (setf (q+:single-shot timer) NIL)
  (q+:start timer (round 1000/30)))

(define-slot (canvas update) ()
  (declare (connected timer (timeout)))
  (with-finalizing ((canvas-size (q+:size canvas)))
    (let ((*width* (q+:width canvas-size))
          (*height* (q+:height canvas-size)))
      (flare:update (scene canvas)))
    (q+:repaint canvas)))

(define-override (canvas paint-event) (ev)
  (declare (ignore ev))
  (with-finalizing ((painter (q+:make-qpainter canvas))
                    (back (brush (background canvas))))
    (setf (q+:background painter) back)
    (q+:erase-rect painter (q+:rect canvas))
    (q+:save painter)
    (flare:paint scene painter)
    (q+:restore painter))
  (stop-overriding))

(defmethod flare:call-with-translation (func (target qobject) vec)
  (q+:save target)
  (unwind-protect
       (with-finalizing ((point (q+:make-qpointf (vx vec) (vy vec))))
         (q+:translate target point)
         (funcall func))
    (q+:restore target)))

(defmacro define-presentation (name (width height &rest initargs) &body intervals)
  `(let ((*width* ,width)
         (*height* ,height))
     (flare:define-progression ,name
       ,@intervals)
     (change-class (flare:progression-definition ',name) 'presentation
                   :height ,height
                   :width ,width
                   ,@initargs)))

;;; More buttons
(define-subwidget (main-widget start-stop-button)
    (q+:make-qpushbutton "Start/Stop" main-widget))

(define-subwidget (main-widget restart-button)
    (q+:make-qpushbutton "Restart" main-widget))

(define-subwidget (main-widget load-button)
    (q+:make-qpushbutton "Load" main-widget))

(define-subwidget (main-widget render-button)
    (q+:make-qpushbutton "Render" main-widget))

(define-subwidget (main-widget quit-button)
    (q+:make-qpushbutton "Quit" main-widget))

(define-subwidget (main-widget information)
    (q+:make-qtextedit main-widget)
  (q+:set-plain-text information "This is a placeholder text."))

(define-subwidget (main-widget controls-layout)
    (q+:make-qgridlayout main-widget)
  ;;                                               t l h w
  (q+:add-widget controls-layout start-stop-button 0 0 1 2)
  (q+:add-widget controls-layout restart-button    1 0)
  (q+:add-widget controls-layout load-button       1 1)
  (q+:add-widget controls-layout render-button     2 0)
  (q+:add-widget controls-layout quit-button       2 1)
  (q+:add-widget controls-layout information       3 0 5 2))

(define-subwidget (main-widget controls-group-box)
    (q+:make-qgroupbox main-widget)
  (setf (q+:layout controls-group-box) controls-layout))

;;; Controls
(define-signal (main-window play/pause) ())
(define-signal (main-window play) ())
(define-signal (main-window pause) ())
(define-signal (main-window restart) ())
(define-signal (main-window seek) (float)) ; float = seek time delta
(define-signal (main-window load-presentation) (string)) ; string = name of symbol of presentation

(define-slot (canvas play/pause) ()
  (declare (connected main-window (play/pause)))
  (print "Play/pause received.")
  (let ((scene (scene canvas)))
    (if (flare:running scene)
        (signal! main-window (pause))
        (signal! main-window (play)))))

(define-slot (canvas play) ()
  (declare (connected main-window (play)))
  (flare:start (scene canvas)))

(define-slot (canvas pause) ()
  (declare (connected main-window (pause)))
  (flare:stop (scene canvas)))

(define-slot (canvas restart) ()
  (declare (connected main-window (restart)))
  (flare:reset (scene canvas)))

(defun load-presentation (presentation-symbol canvas)
  (let* ((presentation (flare:progression-definition presentation-symbol))
         (instance (flare:progression-instance presentation-symbol))
         (*width* (width presentation))
         (*height* (height presentation)))
    (setf (progression canvas) presentation
          (q+:fixed-size canvas) (values (width presentation)
                                         (height presentation)))
    (flare:start (scene canvas))
    (flare:start (print (flare:enter instance (scene canvas))))))

(define-slot (canvas load-presentation) ((presentation-name string))
  (declare (connected main-window (load-presentation string)))
  ;; "Put the progression named PROGRESSION into a canvas, but don't play it."
  (load-presentation
   (find-symbol (string-upcase presentation-name))
   canvas))

;;; Keybinds
;; Currently there is no need to do anything elaborate or serious,
;; just respond to the key presses to the canvas that correspond to some action.
;; we'll figure something more complex later if we actually need it.
(define-override (main-window key-press-event) (event)
  (let ((key (q+:key event)))
    (cond
      ((eql key (q+:qt.Key_Q))
       (q+:quit *qapplication*))
      ((eql key (q+:qt.Key_Space))
       (print "Space pressed.")
       (signal! main-window (play/pause))
       (print "Signal sent."))
      ;; ((and (not (flare:running scene))
      ;;       (eql key (q+:qt.Key_Comma)))
      ;;  (flare:synchronize scene (- (print (flare:clock scene)) 1/10))
      ;;  (flare:update scene)
      ;;  (q+:repaint canvas))
      ((eql key (q+:qt.Key_R))
       (signal! main-window (restart)))
      (t (stop-overriding)))))

;;; (Future) other widgets and layout
;; status bar
(define-subwidget (main-window status) (q+:make-qstatusbar main-window)
  (setf (q+:size-grip-enabled status) nil
        (q+:fixed-height status) 20))

(define-slot (main-window paused) ()
  (declare (connected main-window (pause)))
  (q+:show-message (q+:status-bar main-window) "Paused."))

(define-slot (main-window playing) ()
  (declare (connected main-window (play)))
  (q+:show-message (q+:status-bar main-window) "Playing."))

(define-slot (main-window restart) ()
  (declare (connected main-window (pause))
           (connected (rslot-value main-window 'central-widget 'restart-button)
                      (released)))
  (q+:show-message (q+:status-bar main-window) "Restarted." 2000)
  (q+:show-message (q+:status-bar main-window) "Paused."))

;; Layout
(define-subwidget (main-widget layout) (q+:make-qhboxlayout main-widget)
  (q+:add-widget layout stage)
  (q+:add-widget layout controls-group-box))

;;; Main
(defun present (name)
  (with-main-window (w 'main-window)
    (load-presentation name (rslot-value w 'central-widget 'stage))
    (q+:show-message (q+:status-bar w) "Ready.")
    (setf (q+:window-title w) (format nil "Presenting: ~a" (symbol-name name)))))
