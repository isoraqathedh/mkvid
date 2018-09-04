;;;; coordinates.lisp
;;;; Handles stage coordinates.

(in-package #:mkvid)
(in-readtable :qtools)

;;; Commentary:
;;; Coordinates have two components, one for the x axis and one for the y axis.

;;; There are two systems in use.
;;; - one where the bottom and right edge receive the value 1
;;;   for both x and y axes (the "relative" system)
;;; - one where 1 unit is 1 pixel in both x and y coordinates
;;; In both systems the top of the stage is the x-axis,
;;; and the left the stage is the y-axis.
;;; One can mix and match these,
;;; but remember that the "ultimate" representation is the latter.

;;; There are functions available that turn them both into absolute coordinates,
;;; as well as adjusting them by addition and subtraction.

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

;;; Coordinates

(defstruct coordinates
  (rel-x 0)
  (rel-y 0)
  (abs-x 0)
  (abs-y 0))

(defun origin ()
  (make-coordinates))

(defun apply-all-components (function coordinates)
  "Create a new coordinate object
where each component in COORDINATES has been applied to FUNCTION.

FUNCTION must be a function that takes in two items:
- ACCESSOR, which will take the four functions that extract portions
  out of each coordinates.
- COORDINATE(S), which can either be a single coordinate or a list thereof.

It is up to the user to provide the correct form of FUNCTION
to accept "
  (make-coordinates :rel-x (funcall function #'coordinates-rel-x coordinates)
                    :rel-y (funcall function #'coordinates-rel-y coordinates)
                    :abs-x (funcall function #'coordinates-abs-x coordinates)
                    :abs-y (funcall function #'coordinates-abs-y coordinates)))

(defun absolute-coordinate-p (coordinate)
  "Is the relative coordinates zero?"
  (= 0 (coordinates-rel-x coordinate) (coordinates-rel-y coordinate)))

(defun relative-coordinate-p (coordinate)
  "Is the absolute coordinates zero?"
  (= 0 (coordinates-abs-x coordinate) (coordinates-abs-y coordinate)))

(defun coordinate+ (coordinate &rest coordinates)
  "Add coordinates together."
  (let ((all-coordinates (cons coordinate coordinates)))
    (apply-all-components (lambda (accessor coordinates)
                            (reduce #'+ coordinates
                                    :key accessor
                                    :initial-value 0))
                          all-coordinates)))

(defun coordinate- (coordinate &rest coordinates)
  "Subtract COORDINATES from COORDINATE.

When COORDINATES is nil, negate COORDINATE."
  (if coordinates
      (let ((all-coordinates (cons
                              coordinate
                              (mapcar #'coordinate- coordinates))))
        (apply #'coordinate+ all-coordinates))
      (apply-all-components (lambda (accessor coordinate)
                              (- (funcall accessor coordinate)))
                            coordinate)))

(defgeneric ->absolute (window coordinates)
  (:documentation "Convert coordinates to all-absolute.")
  (:method ((window main-window) (coordinates coordinates))
    (make-coordinates :abs-x (+ (* (coordinates-rel-x coordinates)
                                   (stage-width window))
                                (coordinates-abs-x coordinates))
                      :abs-y (+ (* (coordinates-rel-y coordinates)
                                   (stage-width window))
                                (coordinates-abs-y coordinates)))))

(defgeneric ->relative (window coordinates)
  (:documentation "Convert coordinates to all-relative.")
  (:method ((window main-window) (coordinates coordinates))
    (make-coordinates :rel-x (+ (/ (coordinates-abs-x coordinates)
                                   (stage-width window))
                                (coordinates-rel-x coordinates))
                      :rel-y (+ (/ (coordinates-abs-y coordinates)
                                   (stage-width window))
                                (coordinates-rel-y coordinates)))))

(defun coordinate*-absolute% (coordinate scalar &optional (centre (origin)))
  "Multiply SCALAR to COORDINATES, with an optional CENTRE.

Only the absolute portions of the coordinates are affected.
Relative parts would be zeroed out."
  (unless (and (absolute-coordinate-p centre)
               (absolute-coordinate-p coordinate))
    (warn "Relative proportions not zero. They will be discarded."))
  (let ((px (coordinates-abs-x coordinate))
        (py (coordinates-abs-y coordinate))
        (cx (coordinates-abs-x centre))
        (cy (coordinates-abs-y centre)))
    (make-coordinates :abs-x (+ cx (* scalar (- px cx)))
                      :abs-y (+ cy (* scalar (- py cy))))))

(defun coordinate*-relative% (coordinate scalar &optional (centre (origin)))
  "Multiply SCALAR to COORDINATES, with an optional CENTRE.

Only the relative portions of the coordinates are affected.
Absolute parts would be zeroed out."
  (unless (and (relative-coordinate-p centre)
               (relative-coordinate-p coordinate))
    (warn "Absolute proportions not zero. They will be discarded."))
  (let ((px (coordinates-rel-x coordinate))
        (py (coordinates-rel-y coordinate))
        (cx (coordinates-rel-x centre))
        (cy (coordinates-rel-y centre)))
    (make-coordinates :rel-x (+ cx (* scalar (- px cx)))
                      :rel-y (+ cy (* scalar (- py cy))))))

(defgeneric ->qpointf (window coordinates)
  (:documentation "Convert coordinates to a qpointf.")
  (:method ((window main-window) (coordinates coordinates))
    (let ((absoluted (->absolute window coordinates)))
      (q+:make-qpointf (+ (stage-topleft-x window)
                          (coordinates-abs-x absoluted))
                       (+ (stage-topleft-y window)
                          (coordinates-abs-y absoluted))))))

(defun rectangle (alignment variant &key top-left bottom-right width height)
  (ecase alignment
    (:anchored (centred-rectangle width height))
    (:free (free-rectangle ))))

(defgeneric centred-rectangle (width height)
  (:documentation "Create a rectangle centred on the "))

(defgeneric ->qrect (top-left bottom-right)
  (:documentation "Create a rectangle with the listed coordinates.")
  (:method ((top-left qpointf) (bottom-right qpointf))
    (q+:make-qrectf top-left bottom-right))
  (:method ((top-left coordinates) (bottom-right coordinates))
    ))

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
