;;;; actor.lisp
(in-package #:mkvid)
(in-readtable :qtools)

;;; ==========
;;; The basics
(defclass entity (flare:entity)
  ((flare:location :accessor location)))

(defgeneric paint (thing painter)
  (:method-combination progn))

(defmethod paint progn ((entity entity) painter))

(defmethod flare:paint ((entity entity) target)
  (flare:with-translation ((location entity) target)
    (paint entity target)))

(defclass sized-entity (entity)
  ((size :initarg :size :accessor size))
  (:default-initargs
   :size (vec 50 50)))

;;; Also some basic brush, pen and font stuff.
(defgeneric brush (colour)
  (:method ((colour colour))
    (q+:make-qbrush (->qcolor colour))))

(defgeneric pen (colour)
  (:method ((colour colour))
    (q+:make-qpen (->qcolor colour))))

(defun fill-rect (painter brush rect)
  (with-finalizing ((rect (rect rect)))
    (q+:fill-rect painter rect brush)))

;;; =========================
;;; Different types of actors

;;; Rectangle actor
(defclass box (sized-entity)
  ((background :initarg :background :accessor background)
   (border :initarg :border :accessor border))
  (:default-initargs
   :background (->colour 0 0 0 0)
   :border (cons (vec 0 0 0 0) (->colour 0 0 0))))

(defgeneric rect (rectangle)
  (:method ((rect vec4))
    (q+:make-qrectf (coerce (vx rect) 'single-float)
                    (coerce (vy rect) 'single-float)
                    (coerce (vz rect) 'single-float)
                    (coerce (vw rect) 'single-float)))

  (:method ((rect vec2))
    (q+:make-qrectf 0.0 0.0
                    (coerce (vx rect) 'single-float)
                    (coerce (vy rect) 'single-float))))

(defmethod paint progn ((box box) painter)
  (let ((size (size box)))
    (destructuring-bind (offset . color) (border box)
      (with-finalizing ((brush (brush color)))
        (fill-rect painter brush (vec (- (vw offset))
                                      (- (vx offset))
                                      (+ (vx size) (vw offset) (vy offset))
                                      (vx offset)))
        (fill-rect painter brush (vec (vx size)
                                      (- (vx offset))
                                      (vy offset)
                                      (+ (vy size) (vx offset) (vz offset))))
        (fill-rect painter brush (vec (- (vw offset))
                                      (vy size)
                                      (+ (vx size) (vw offset) (vy offset))
                                      (vz offset)))
        (fill-rect painter brush (vec (- (vw offset))
                                      (- (vx offset))
                                      (vw offset)
                                      (+ (vy size) (vx offset) (vz offset))))))
    (with-finalizing ((brush (brush (background box))))
      (fill-rect painter brush (vec 0 0 (vx size) (vy size))))))

;;; Ellipse actor
(defclass oval (sized-entity)
  ((background :initarg :background :accessor background)
   (border :initarg :border :accessor border))
  (:default-initargs
   :background (->colour 0 0 0 0)
   :border (cons 0 (->colour 0 0 0))))

(defmethod paint progn ((oval oval) painter)
  (q+:save painter)
  (unwind-protect
       (with-finalizing ((pen (q+:make-qpen))
                         (origin (q+:make-qpointf 0.0 0.0))
                         (brush (brush (background oval))))
         (setf (q+:color pen) (->qcolor (cdr (border oval)))
               (q+:width pen) (car (border oval))
               (q+:brush painter) brush
               (q+:pen painter) pen)
         (q+:draw-ellipse painter
                          origin
                          (coerce (/ (vx (size oval)) 2) 'single-float)
                          (coerce (/ (vy (size oval)) 2) 'single-float)))
    (q+:restore painter)))

;;; Text-based actor
(defclass text (sized-entity)
  ((text :initarg :text :accessor text)
   (color :initarg :font-color :accessor font-color)
   (font-size :initarg :font-size :accessor font-size)
   (font :initarg :font :accessor font)
   (alignment :initarg :align :accessor alignment))
  (:default-initargs
   :text "< >"
   :font "Inziu Iosevka TC"
   :font-size -1
   :font-color (->colour 240 240 15)
   :align (cons :center :center)))

(defmethod paint progn ((text text) painter)
  (with-finalizing ((brush (brush (background text)))
                    (pen (pen (font-color text)))
                    (rect (rect (size text)))
                    (font (q+:make-qfont (font text)
                                         (font-size text)))
                    (option (q+:make-qtextoption
                             (logior (ecase (car (alignment text))
                                       (:left (q+:qt.align-left))
                                       (:right (q+:qt.align-right))
                                       (:center (q+:qt.align-hcenter))
                                       (:justify (q+:qt.align-justify)))
                                     (ecase (cdr (alignment text))
                                       (:top (q+:qt.align-top))
                                       (:bottom (q+:qt.align-bottom))
                                       (:center (q+:qt.align-vcenter)))))))
    (setf (q+:brush painter) brush
          (q+:pen painter) pen
          (q+:font painter) font)
    (q+:draw-text painter rect (text text) option)))

;;; Text + rectangle
(defclass textbox (box text)
  ())
