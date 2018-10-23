;;;; window.lisp

(in-package #:mkvid)
(in-readtable :qtools)

(defparameter *default-fps* 60
  "The rate at which the animation should be run at.")

(define-widget main-window (QWidget)
  ((counter :accessor counter
            :initform 0
            :documentation "A counter for a particular timer.")
   (fps :accessor fps
        :initform *default-fps*
        :initarg :fps)
   (timer-running :accessor timer-running
                  :initform nil)))

;;; Stage
;;; a stage is a place where all the action is held and recorded.
;;; It has a size that is the same as in the final video.
(define-widget qstage (QWidget)
  ((stage-width :initform 1280
                :initarg :stage-width
                :accessor stage-width)
   (stage-height :initform 720
                 :initarg :stage-height
                 :accessor stage-height)
   (progression :initarg :progression
                :reader progression)
   (scene :initform (make-instance 'flare:scene)
          :reader scene)
   (background :initform (vec 0 0 0)
               :accessor background))
  (:documentation "A stage where all activity will be recorded."))

(defmethod initialize-instance :after ((qstage qstage) &key &allow-other-keys)
  (setf (q+:fixed-size qstage) (values (stage-width qstage)
                                       (stage-height qstage)))
  ;; (flare:start (scene qstage))
  ;; (flare:start (flare:enter
  ;;               (flare:progression-instance
  ;;                (progression qstage))
  ;;               (scene qstage)))
  )

(define-subwidget (main-window the-stage) (make-instance 'qstage))

(define-override (qstage paint-event) (ev)
  (declare (ignore ev))
  (with-finalizing ((painter (q+:make-qpainter qstage))
                    (back (brush (background qstage))))
    (setf (q+:background painter) back)
    (q+:erase-rect painter (q+:rect qstage))
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

;;; Controls
;;; Here are some buttons used for controls.
;;; As of yet they are not used but they help constrain the stage a bit.
(define-subwidget (main-window start-stop-button)
    (q+:make-qpushbutton "Start/Stop" main-window))

(define-subwidget (main-window restart-button)
    (q+:make-qpushbutton "Restart" main-window))

(define-subwidget (main-window load-button)
    (q+:make-qpushbutton "Load" main-window))

(define-subwidget (main-window render-button)
    (q+:make-qpushbutton "Render" main-window))

(define-subwidget (main-window information)
    (q+:make-qtextedit main-window)
  (q+:set-plain-text information "This is a placeholder text."))

(define-subwidget (main-window controls-layout)
    (q+:make-qgridlayout main-window)
  (q+:add-widget controls-layout start-stop-button 0 0 1 2)
  (q+:add-widget controls-layout restart-button    1 0)
  (q+:add-widget controls-layout load-button       1 1)
  (q+:add-widget controls-layout render-button     2 0 1 2)
  (q+:add-widget controls-layout information       3 0 6 2))

(define-subwidget (main-window controls-group-box)
    (q+:make-qgroupbox "Controls" main-window)
  (q+:set-layout controls-group-box controls-layout))

(define-subwidget (main-window controls-and-stage)
    (q+:make-qhboxlayout main-window)
  (q+:add-widget controls-and-stage the-stage)
  (q+:add-widget controls-and-stage controls-group-box))

(define-subwidget (main-window controls-and-stage-group-box)
    (q+:make-qgroupbox main-window)
  (q+:set-layout controls-and-stage-group-box controls-and-stage))

(define-slot (main-window toggle-start-stop) ()
  (declare (connected start-stop-button (released)))
  (setf timer-running (not timer-running)))

(define-slot (main-window reset-timer) ()
  (declare (connected restart-button (released)))
  (setf counter 0 timer-running nil))

;;; Clocks
;;; This is the animation clock that motions would be timed against.
;;; It also includes a display.
(define-subwidget (main-window timer-display) (q+:make-qlcdnumber main-window)
  (q+:set-digit-count timer-display 10)
  (q+:display timer-display "----"))
(define-subwidget (main-window timer-label) (q+:make-qlabel "Seconds - Frames"))

(define-subwidget (main-window clock) (q+:make-qgridlayout main-window)
  (q+:add-widget clock timer-label 0 0)
  (q+:add-widget clock timer-display 1 0 3 1))

(define-subwidget (main-window clock-group-box) (q+:make-qgroupbox "Clock" main-window)
  (q+:set-layout clock-group-box clock))

(define-subwidget (main-window timer) (q+:make-qtimer main-window)
  (setf (q+:single-shot timer) nil)
  (q+:start timer (round (/ 1000 fps))))

(define-slot (main-window update) ()
  (declare (connected timer (timeout)))
  (when timer-running
    (incf counter))
  (multiple-value-bind (seconds frames) (floor counter fps)
    (q+:display timer-display (format nil "~d-~2,'0d" seconds frames))))

(define-subwidget (main-window layout) (q+:make-qvboxlayout main-window)
  (q+:add-widget layout controls-and-stage-group-box)
  (q+:add-widget layout clock-group-box))

;; (define-slot (viewer update) ()
;;   (declare (connected timer (timeout)))
;;   (update scene)
;;   (q+:repaint viewer))

;;; Main function.
(defun main ()
  (with-main-window (w 'main-window)
    (setf (q+ window-title w) "mkvid")))

(defun present (name)
  (with-main-window (w name)))
