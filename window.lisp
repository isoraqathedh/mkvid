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
        :initarg :fps)))

;;; Stage
;;; a stage is a place where all the action is held and recorded.
;;; It has a size that is the same as in the final video.
(define-widget qstage (QWidget)
  ((stage-width :initform 1280
                :initarg :stage-width
                :accessor stage-width)
   (stage-height :initform 720
                 :initarg :stage-height
                 :accessor stage-height))
  (:documentation "A stage where all activity will be recorded."))

(define-override (qstage size-hint) ()
  (q+:make-qsize stage-width stage-height))

(define-override (qstage minimum-size-hint) ()
  (q+:make-qsize stage-width stage-height))

(define-subwidget (main-window the-stage) (make-instance 'qstage))

;;; Clocks
;;; This is the animation clock that motions would be timed against.
;;; It also includes a display.
(define-subwidget (main-window seconds-display) (q+:make-qlcdnumber main-window)
  (q+:set-digit-count seconds-display 10)
  (q+:display seconds-display "--"))
(define-subwidget (main-window seconds-label) (q+:make-qlabel "Seconds - Frames"))

(define-subwidget (main-window clock) (q+:make-qgridlayout main-window)
  (q+:add-widget clock seconds-label 0 0)
  (q+:add-widget clock seconds-display 1 0 3 1))

(define-subwidget (main-window clock-group-box) (q+:make-qgroupbox "Clock" main-window)
  (q+:set-layout clock-group-box clock))

(define-subwidget (main-window timer) (q+:make-qtimer main-window)
  (setf (q+:single-shot timer) nil)
  (q+:start timer (round (/ 1000 fps))))

(define-slot (main-window update) ()
  (declare (connected timer (timeout)))
  (multiple-value-bind (seconds frames) (floor (incf counter) fps)
    (q+:display seconds-display (format nil "~d-~2,'0d" seconds frames))))

(define-subwidget (main-window layout) (q+:make-qvboxlayout main-window)
  (q+:add-widget layout the-stage)
  (q+:add-widget layout clock-group-box))

;;; Main function.
(defun main ()
  (with-main-window (w 'main-window)
    (setf (q+ window-title w) "mkvid")))
