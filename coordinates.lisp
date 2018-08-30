(in-package #:mkvid)

(defstruct component
  (magnitude 0)
  (unit :absolute))

(defstruct coordinates
  (x :type component)
  (y :type component))

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
