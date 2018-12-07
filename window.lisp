;;;; window.lisp
(in-package #:mkvid)
(in-readtable :qtools)

;;; Main canvas type that everything hangs on, plus the main window
;;; and any other widgets that needs to be defined.
(define-widget main-window (QMainWindow)
  ((widget-registry :accessor widgets :initform (make-hash-table))))
(define-widget main-widget (QWidget)
  ((main-window :reader main-window :initarg :main-window)))

(define-subwidget (main-window central-widget)
    (make-instance 'main-widget :main-window main-window)
  (setf (q+:central-widget main-window) central-widget))

(define-widget canvas (QWidget)
  ((progression :initarg :progression :accessor progression)
   (scene :initform (make-instance 'flare:scene) :reader scene)
   (background :initform *background-colour* :accessor background)
   (main-window :initarg :main-window :reader main-window)))

(defmethod initialize-instance :after ((canvas canvas) &key width height)
  (setf (q+:fixed-size canvas) (values width height)))

(define-subwidget (main-widget stage)
    ;; Temporary widget to have something hold on to the slot
    ;; before the real one takes over
    (make-instance 'canvas :main-window main-widget
                           :width 1024
                           :height 576
                           :progression nil))

(define-subwidget (main-window status) (q+:make-qstatusbar main-window)
  (setf (q+:size-grip-enabled status) nil
        (q+:fixed-height status) 20))

(define-subwidget (main-window clock-display)
    (q+:make-qlabel (format-clock nil) (q+:status-bar main-window))
  (setf (q+:alignment clock-display) (q+:qt.align-right)
        (q+:text-format clock-display) (q+:qt.plain-text)
        (q+:frame-style clock-display) (q+:qframe.vline)
        (q+:fixed-width clock-display) 200)
  (q+:add-permanent-widget (q+:status-bar main-window) clock-display))

(define-subwidget (main-window clock-font)
    (q+:make-qfont "Ubuntu Mono")
  (setf (q+:font clock-display) clock-font))

;; Side buttons
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

(define-subwidget (main-widget layout) (q+:make-qhboxlayout main-widget)
  (q+:add-widget layout stage)
  (q+:add-widget layout controls-group-box))

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

(define-slot (canvas play) ()
  (flare:start (scene canvas)))

(define-slot (canvas pause) ()
  (flare:stop (scene canvas)))

(define-slot (canvas restart) ()
  (flare:reset (scene canvas)))

(defun load-presentation (presentation-symbol canvas)
  (let* ((presentation (flare:progression-definition presentation-symbol))
         (instance (flare:progression-instance presentation-symbol))
         (w (width presentation))
         (h (height presentation))
         (*width* w)
         (*height* h))
    (setf (progression canvas) presentation
          (q+:fixed-size canvas) (values w h))
    (flare:start (scene canvas))
    (flare:start (flare:enter instance (scene canvas)))))

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

;;; Main
(define-initializer (main-window final-setup -2)
  (q+:show-message (q+:status-bar main-window) "Ready."))

(defun present (name)
  (with-main-window (w 'main-window)
    (load-presentation name (rslot-value w 'central-widget 'stage))
    (setf (q+:window-title w) (format nil "Presenting: ~a" (symbol-name name)))))
