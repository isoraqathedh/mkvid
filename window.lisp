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

(defgeneric progression-of (canvas)
  (:documentation "Get the progression (of the progression instance) of the canvas.")
  (:method ((canvas canvas))
    (first (flare:progressions (scene canvas)))))

(defmethod initialize-instance :after ((canvas canvas) &key width height)
  (setf (q+:fixed-size canvas) (values width height)))

(define-subwidget (main-widget stage)
    ;; Temporary widget to have something hold on to the slot
    ;; before the real one takes over
    (make-instance 'canvas :main-window (slot-value main-widget 'main-window)
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
        (q+:frame-style clock-display) (q+:qframe.vline))
  (q+:add-permanent-widget (q+:status-bar main-window) clock-display))

(defun format-clock (seconds-or-nil)
  (format nil "t: ~:[---.--~;~:*~6,2f~] s" seconds-or-nil))

(define-subwidget (main-window clock-font)
    (q+:make-qfont "Ubuntu Mono" 15)
  (with-finalizing ((metric (q+:make-qfontmetrics clock-font main-window)))
    (setf (q+:fixed-width clock-display)
          (* 3                          ; × 3 because the width
                                        ; is wrong in a way I can't fix.
             (q+:width metric (format-clock nil)))
          (q+:font clock-display) clock-font)))

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

(define-subwidget (main-widget playback-input)
    (q+:make-qlineedit "1" main-widget))

(define-subwidget (main-widget playback-label)
    (q+:make-qlabel "Speed" main-widget)
  (setf (q+:buddy playback-label) playback-input))

(define-subwidget (main-widget playback-reset)
    (q+:make-qpushbutton "Reset speed" main-widget))

(define-subwidget (main-widget information)
    (q+:make-qtextedit main-widget)
  (q+:set-plain-text information "This is a placeholder text."))

(define-subwidget (main-widget controls-layout)
    (q+:make-qgridlayout main-widget)
  ;;                                               t l h w
  (q+:add-widget controls-layout start-stop-button 0 0 1 4)
  (q+:add-widget controls-layout restart-button    1 0 1 2)
  (q+:add-widget controls-layout load-button       1 2 1 2)
  (q+:add-widget controls-layout render-button     2 0 1 2)
  (q+:add-widget controls-layout quit-button       2 2 1 2)
  (q+:add-widget controls-layout playback-label    3 0 1 1)
  (q+:add-widget controls-layout playback-input    3 1 1 2)
  (q+:add-widget controls-layout playback-reset    3 3 1 1)
  (q+:add-widget controls-layout information       4 0 5 4))

(define-subwidget (main-widget controls-group-box)
    (q+:make-qgroupbox main-widget)
  (setf (q+:layout controls-group-box) controls-layout))

(define-subwidget (main-widget layout) (q+:make-qhboxlayout main-widget)
  (q+:add-widget layout controls-group-box)
  (q+:add-widget layout stage))

;;; Animation
(defclass presentation (flare:progression-definition)
  ((width :initarg :width :reader width)
   (height :initarg :height :reader height))
  (:documentation "A progression-definition embedded with height and width
to allow relative measurements to take place.")
  (:default-initargs
   :width *width*
   :height *height*))

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
(defparameter *step-interval* 1/30
  "Interval for which the comma and full stop keys will jump the animation for.")

(define-override (main-window key-press-event) (event)
  (let* ((key (q+:key event))
         (canvas (rslot-value main-window 'central-widget 'stage)))
    (cond
      ((eql key (q+:qt.Key_Q))
       (q+:quit *qapplication*))
      ((eql key (q+:qt.Key_Space))
       (signal! main-window (play/pause)))
      ((eql key (q+:qt.Key_Comma))
       (signal! canvas (seek-by float) (float (- *step-interval*) 0.0)))
      ((eql key (q+:qt.Key_Period))
       (signal! canvas (seek-by float) (float (+ *step-interval*) 0.0)))
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
