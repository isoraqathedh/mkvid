;;;; window.lisp
(in-package #:mkvid)
(in-readtable :qtools)

;;; Main canvas type that everything hangs on, plus the main window
(define-widget main-window (QWidget) ())

(define-widget canvas (QWidget)
  ((progression :initarg :progression :accessor progression)
   (scene :initform (make-instance 'flare:scene) :reader scene)
   (background :initform *background-colour* :accessor background)
   (main-window :initarg :main-window :reader main-window)))

(defmethod initialize-instance :after ((canvas canvas) &key width height)
  (setf (q+:fixed-size canvas) (values width height))
  (flare:start (scene canvas))
  (flare:start (flare:enter (flare:progression-instance (progression canvas)) (scene canvas))))

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

;;; Controls
(define-signal (main-window play/pause) ())
(define-signal (main-window play) ())
(define-signal (main-window pause) ())
(define-signal (main-window restart) ())
;; Also maybe: seek ±0.1s, reload another animation

(define-slot (canvas play/pause) ()
  (declare (connected main-window (play/pause)))
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
       (signal! main-window (play/pause)))
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
  (declare (connected main-window (pause)))
  (q+:show-message (q+:status-bar main-window) "Restarted." 2000)
  (q+:show-message (q+:status-bar main-window) "Paused."))

;; Layout
(define-subwidget (main-window layout) (q+:make-qvboxlayout main-window)
  (q+:add-widget layout stage)
  (q+:add-widget layout status))

;;; Main
(defun present (name)
  (with-main-window (w 'main-window)
    (load-presentation name (slot-value (q+:central-widget w) 'stage))
    (q+:show-message (q+:status-bar w) "Ready.")
    (setf (q+:window-title w) (format nil "Presenting: ~a" (symbol-name name)))))
