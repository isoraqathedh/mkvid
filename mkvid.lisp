;;;; mkvid.lisp

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

(defgeneric absolute-stage-coordinates (window dimension offset)
  (:documentation "Calculate the coordinates of a particular point in a stage,
with the offset given as the number of absolute pixels. ")
  (:method ((window main-window) (dimension (eql :x)) (offset number))
    (+ (stage-topleft-x window) offset))
  (:method ((window main-window) (dimension (eql :y)) (offset number))
    (+ (stage-topleft-y window) offset))
  (:method ((window main-window) (dimension (eql :point)) (offset list))
    (destructuring-bind (x y) offset
      (q+:make-qpoint (absolute-stage-coordinates window :x x)
                      (absolute-stage-coordinates window :y y)))))

(defgeneric relative-stage-coordinates* (window dimension offset)
  (:documentation "Calculate the number of pixels that span some fraction OFFSET
of the stage.")
  (:method ((window main-window) (dimension (eql :x)) (offset number))
    (* offset (stage-width window)))
  (:method ((window main-window) (dimension (eql :y)) (offset number))
    (* offset (stage-height window))))

(defgeneric relative-stage-coordinates (window dimension offset)
  (:documentation "Calculate the coordinates that is some fraction OFFSET
from the top or left side.")
  (:method ((window main-window) (dimension (eql :x)) (offset number))
    (absolute-stage-coordinates window :x (* offset (stage-width window))))
  (:method ((window main-window) (dimension (eql :y)) (offset number))
    (absolute-stage-coordinates window :y (* offset (stage-height window))))
  (:method ((window main-window) (dimension (eql :point)) (offset list))
    (destructuring-bind (x y) offset
      (q+:make-qpointf (relative-stage-coordinates window :x x)
                       (relative-stage-coordinates window :y y)))))

(defgeneric relative-rectangle (window left top width height)
  (:documentation "Create a rectangle with the listed coordinates.")
  (:method ((window main-window) left top width height)
   (q+:make-qrectf
    (relative-stage-coordinates window :x left)
    (relative-stage-coordinates window :y top)
    (relative-stage-coordinates window :x width)
    (relative-stage-coordinates window :y height))))

(define-override (main-window paint-event) (ev)
  (declare (ignore ev))
  (with-finalizing ((painter (q+:make-qpainter main-window)))
    (let ((main-box (relative-rectangle main-window 1/5 2/5 3/5 1/5)))
     (q+:fill-rect painter
                   (relative-rectangle main-window 0 0 1 1)
                   (q+:make-qbrush *background-colour*))
      (q+:set-font painter (q+:make-qfont "Inziu Iosevka TC" 30))
      (q+:set-pen painter *text-colour*)
      (q+:draw-rect painter main-box)
      (q+:draw-text painter main-box (q+:qt.align-center) "Test string"))))

(defun main ()
  (with-main-window (w 'main-window)
    (setf (q+ window-title w) "mkvid")))
