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

(defgeneric centred-relative-rectangle (window width height)
  (:documentation "Create a rectangle centred with the stage
with the provided width and height.")
  (:method ((window main-window) width height)
    (let ((top (/ (- 1 height) 2))
          (left (/ (- 1 width) 2)))
      (relative-rectangle window left top width height))))

(defgeneric centred-relative-rectangle+offset (window width height dx dy)
  (:documentation "Create a rectangle centred with the stage
with the provided width and height, then shift it by some amount.")
  (:method ((window main-window) width height dx dy)
    (let ((box (centred-relative-rectangle window width height)))
      (q+:translate box
                    (relative-stage-coordinates* window :x dx)
                    (relative-stage-coordinates* window :y dy))
      box)))

;; (define-subwidget (viewer timer) (q+:make-qtimer viewer)
;;   (setf (q+:single-shot timer) nil)
;;   (q+:start timer (round 1000/60)))

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

;; (defgeneric )

;; (define-slot (viewer update) ()
;;   (declare (connected timer (timeout)))
;;   (update scene)
;;   (q+:repaint viewer))

(defgeneric text-actor (painter position text
                        &key font size fg-colour bg-colour)
  (:method ((painter painter) position text
            &key font size fg-colour bg-colour)
    ))
(defgeneric circle-actor (painter position size &optional font ))

(define-override (main-window paint-event) (ev)
  (declare (ignore ev))
  (with-finalizing ((painter (q+:make-qpainter main-window)))
    (let ((main-box (centred-relative-rectangle+offset main-window 1/2 2/5 1/6 0))
          (code-box (relative-rectangle main-window 1/20 37/40 9/10 1/40))
          (line-box (relative-rectangle main-window 1/20 1/20 9/10 1/40)))
      (q+:fill-rect painter
                    (relative-rectangle main-window 0 0 1 1)
                    (q+:make-qbrush *background-colour*))
      (q+:fill-rect painter
                    (relative-rectangle main-window 9/30 0 1/40 1)
                    (q+:make-qbrush (q+:make-qcolor 200 15 15)))
      (setf (q+ brush painter) (q+:make-qbrush *text-colour*)
            (q+ pen painter) (q+:make-qpen
                              (q+:make-qbrush
                               (q+:make-qcolor 255 255 255))
                              3))
      (q+:draw-ellipse
       painter
       (absolute-stage-coordinates
        main-window
        :point
        (list (+ 30 (* (stage-width main-window) (+ 9/30 1/80)))
              (+ 30 (* (stage-height main-window) 1/2))))
       30
       30)
      (setf (q+ pen painter) *text-colour*
            (q+ brush painter) (q+:make-qbrush *background-colour*)
            (q+ font painter) (q+:make-qfont "Inziu Iosevka TC" 45))
      (q+:draw-rect painter main-box)
      (q+:draw-text painter main-box (logior (q+:qt.align-vcenter)
                                             (q+:qt.align-left))
                    "London King's Cross")
      (setf (q+ font painter) (q+:make-qfont "Inziu Iosevka TC" 20))
      (q+:draw-rect painter code-box)
      (q+:draw-text painter code-box (logior (q+:qt.align-vcenter)
                                             (q+:qt.align-left))
                    "LKX")
      (q+:draw-rect painter line-box)
      (q+:draw-text painter line-box (logior (q+:qt.align-vcenter)
                                             (q+:qt.align-left))
                    "East Coast Main Line")
      (print (q+:pen painter)))))

(defun main ()
  (with-main-window (w 'main-window)
    (setf (q+ window-title w) "mkvid")))
