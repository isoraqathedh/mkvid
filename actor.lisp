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
                                         (round (font-size text))))
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

;;; Image actor
(defclass image-actor (box)
  ((image :initarg :qpixmap
          :accessor qpixmap-of)
   (sprite-location :initarg :sprite-location
                    :accessor sprite-location))
  (:default-initargs
   :sprite-location (vec 0 0)))

(defmethod initialize-instance :after ((object image-actor) &key image-file)
  ;; Load the image.
  (setf (qpixmap-of object)
        (q+:make-qpixmap (namestring image-file))))

(defmethod paint progn ((image-actor image-actor) painter)
  (let ((sprite-width (floor (vx (size image-actor))))
        (sprite-height (floor (vy (size image-actor)))))
    (q+:draw-pixmap
     painter
     0 0                                ; location
     sprite-width sprite-height         ; drawing rectangle size
     (qpixmap-of image-actor)           ; image
     ;; The location of the sprite
     ;; We'll take the size of the sprite as the size of the actor,
     ;; and then take the vxth column of the vyth row
     ;; of the image divided into rectangles of size that.
     (floor (* (vx (sprite-location image-actor))
               (vx (size image-actor))))
     (floor (* (vy (sprite-location image-actor))
               (vy (size image-actor))))
     sprite-width sprite-height)))  ; sprite rectangle size

(defmethod flare:leave :after ((unit image-actor) scene-graph)
  (declare (ignore scene-graph))
  ;; Need to clean up the qpixmap after the actor leaves
  ;; as it's basically impossible to access the actor after leaving
  ;; we'll declare it dead and just finalize the thing.
  (finalize (qpixmap-of unit)))

;;; Group actor
;;; a formation that join two or more actors.
(defclass group-actor (flare:container-unit)
  ((locations :accessor locations :initarg :locations)
   (flare:location :accessor location :initarg :location))
  (:default-initargs
   :locations nil))

(defgeneric adjust-subactor-positions (group-actor)
  (:method ((group group-actor))
    (for:for ((actor flare-queue:in-queue (objects group))
              (actor-offset in (locations group)))
      (setf (location actor) (v+ actor-offset (location group))))))

(defmethod flare:enter :after (unit (container group-actor))
  (adjust-subactor-positions container))

(defmethod paint progn ((group-actor group-actor) painter)
  (flare:do-container-tree (actor group-actor)
    (paint actor painter)))

(defmethod (setf location) :after (value (group-actor group-actor))
  (declare (ignore value))
  (adjust-subactor-positions group-actor))

;;; Shifter actor
;;; a group actor that allows its members to shift position one after another
;;; in a "musical chairs" kind of way.
(defclass shifter-actor (group-actor) ())

(defgeneric shift-actors (shifter-actor)
  (:method ((shifter-actor shifter-actor))
    ;; placeholder until later.
    (let ((top (pop (locations shifter-actor))))
      (setf (locations shifter-actor)
            (nconc (locations shifter-actor)
                   (list top))))))

;;; Indicator bar actor
(defclass abstract-bar-actor (box)
  ((bar-maximum :initarg :maximum :accessor bar-maximum)
   (bar-current :initarg :current :accessor bar-current)
   (label)
   (label-position)))

(defclass bar-actor (abstract-bar-actor)
  ())
