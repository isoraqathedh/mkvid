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

(defparameter *background-colour* (q+:make-qcolor 0 10 25)
  "The background colour for the stage.")
(defparameter *text-colour* (q+:make-qcolor 240 240 15)
  "The foreground colour for the stage.")

;; (define-subwidget (viewer timer) (q+:make-qtimer viewer)
;;   (setf (q+:single-shot timer) nil)
;;   (q+:start timer (round 1000/60)))

;; (define-slot (viewer update) ()
;;   (declare (connected timer (timeout)))
;;   (update scene)
;;   (q+:repaint viewer))

;; (defclass style-combination ()
;;   ((font-face :initarg :font-face
;;               :accessor font-face
;;               :initform nil)
;;    (font-size :initarg :font-size
;;               :accessor font-size
;;               :initform nil)
;;    (fg-colour :initarg :fg-colour
;;               :accessor fg-colour
;;               :initform nil)
;;    (bg-colour :initarg :bg-colour
;;               :accessor bg-colour
;;               :initform nil))
;;   (:documentation "A combination of styles."))

;; (defgeneric make-font (style-combination)
;;   (:documentation "Make a font object based on the style combination.")
;;   (:method ((style-combination style-combination))
;;     (q+:make-qfont (font-face style-combination)
;;                    (font-size style-combination))))

(defun main ()
  (with-main-window (w 'main-window)
    (setf (q+ window-title w) "mkvid")))
