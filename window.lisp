;;;; window.lisp

(in-package #:mkvid)
(in-readtable :qtools)

(define-widget main-window (QWidget)
  ())

;;; Stage
;;; a stage is a place where all the action is held and recorded.
(define-widget qstage (QWidget)
  ((stage-width :initform 1280
                :initarg :stage-width
                :accessor stage-width)
   (stage-height :initform 720
                 :initarg :stage-height
                 :accessor stage-height))
  (:documentation "A stage where all activity will be recorded.

(In qwidget format, experimental)"))

(define-override (qstage size-hint) ()
  (q+:make-qsize stage-width stage-height))

(define-override (qstage minimum-size-hint) ()
  (q+:make-qsize stage-width stage-height))

(define-subwidget (main-window the-stage) (make-instance 'qstage))

(define-subwidget (main-window visible-test) (q+:make-qlcdnumber main-window)
  (q+:set-digit-count visible-test 5))

(define-subwidget (main-window layout) (q+:make-qvboxlayout main-window)
  (q+:add-widget layout the-stage)
  (q+:add-widget layout visible-test))


;; (define-subwidget (viewer timer) (q+:make-qtimer viewer)
;;   (setf (q+:single-shot timer) nil)
;;   (q+:start timer (round 1000/60)))

;; (define-slot (viewer update) ()
;;   (declare (connected timer (timeout)))
;;   (update scene)
;;   (q+:repaint viewer))

(defun main ()
  (with-main-window (w 'main-window)
    (setf (q+ window-title w) "mkvid")))
