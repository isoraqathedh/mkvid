;;;; window.lisp

(in-package #:mkvid)
(in-readtable :qtools)

(define-widget main-window (QWidget)
  ((stage-tl-corner-x :initform 60
                      :initarg :stage-topleft-x
                      :accessor stage-topleft-x)
   (stage-tl-corner-y :initform 60
                      :initarg :stage-topleft-y
                      :accessor stage-topleft-y)
   (stage-width :initform 1280
                :initarg :stage-width
                :accessor stage-width)
   (stage-height :initform 720
                 :initarg :stage-height
                 :accessor stage-height)))

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
