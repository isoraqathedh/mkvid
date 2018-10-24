;;;; actor.lisp

(in-package #:mkvid)
(in-readtable :qtools)

(defclass colour () ()
  (:documentation "A colour."))

(defclass 3-colour (colour)
  ((r :initarg :r)
   (g :initarg :g)
   (b :initarg :b))
  (:default-initargs
   :r 0 :g 0 :b 0))

(defclass 4-colour (3-colour)
  ((a :initarg :a))
  (:default-initargs
   :a 255))

(defmethod print-object ((thing 3-colour) stream)
  (print-unreadable-object (thing stream :type t)
    (with-slots (r g b) thing
      (format stream "#~2,'0x~2,'0x~2,'0x" r g b))))

(defmethod print-object ((thing 4-colour) stream)
  (print-unreadable-object (thing stream :type t)
    (with-slots (r g b a) thing
      (format stream "#~2,'0x~2,'0x~2,'0x~2,'0x" r g b a))))

(defgeneric ->colour (thing &optional g b a)
  (:documentation "Make an intermediate colour representation.

The input representation can either be a hex code
with either 6 (#ABCDEF) or 8 (#ABCDEF89) digits for the alpha channel,
a vec3 or vec4 with numbers scaled from 0 (no input) to 1 (full input),
or provided directly with four integers either separately or in a list
for values in the red, green, blue and optionally alpha sequence.")
  (:method ((thing vec3) &optional i1 i2 i3)
    (declare (ignore i1 i2 i3))
    (make-instance '3-colour :r (floor (* 255 (vx thing)))
                             :g (floor (* 255 (vy thing)))
                             :b (floor (* 255 (vz thing)))))
  (:method ((thing vec4) &optional i1 i2 i3)
    (declare (ignore i1 i2 i3))
    (make-instance '4-colour :r (floor (* 255 (vx thing)))
                             :g (floor (* 255 (vy thing)))
                             :b (floor (* 255 (vz thing)))
                             :a (floor (* 255 (vw thing)))))
  (:method ((thing list) &optional i1 i2 i3)
    (declare (ignore i1 i2 i3))
    (destructuring-bind (r g b &optional (a nil a-supplied-p)) thing
      (if a-supplied-p
          (make-instance '4-colour :r r :g g :b b :a a)
          (make-instance '3-colour :r r :g g :b b))))
  (:method ((r number) &optional (g 0) (b 0) (a 255 alpha-supplied-p))
    (if alpha-supplied-p
        (make-instance '4-colour :r r :g g :b b :a a)
        (make-instance '3-colour :r r :g g :b b)))
  (:method ((colour-code string) &optional i1 i2 i3)
    (declare (ignore i1 i2 i3))
    (setf colour-code (delete-if-not #'alphanumericp colour-code))
    (ecase (length colour-code)
      (6 (make-instance
          '3-colour
          :r (parse-integer colour-code :start 0 :end 2 :radix 16)
          :g (parse-integer colour-code :start 2 :end 4 :radix 16)
          :b (parse-integer colour-code :start 4 :end 6 :radix 16)))
      (8 (make-instance
          '4-colour
          :r (parse-integer colour-code :start 0 :end 2 :radix 16)
          :g (parse-integer colour-code :start 2 :end 4 :radix 16)
          :b (parse-integer colour-code :start 4 :end 6 :radix 16)
          :a (parse-integer colour-code :start 6 :end 8 :radix 16))))))

(defgeneric ->qcolor (thing)
  (:documentation "Create a qcolor from the intermediate colour representation.")
  (:method ((thing 3-colour))
    (q+:make-qcolor (floor (slot-value thing 'r))
                    (floor (slot-value thing 'g))
                    (floor (slot-value thing 'b))))
  (:method ((thing 4-colour))
    (q+:make-qcolor (floor (slot-value thing 'r))
                    (floor (slot-value thing 'g))
                    (floor (slot-value thing 'b))
                    (floor (slot-value thing 'a)))))

(defparameter *background-colour* (->colour 0 10 25)
  "The background colour for the stage.")
(defparameter *text-colour* (->colour 240 240 15)
  "The foreground colour for the stage.")

(defclass point-actor (flare:particle)
  ((flare:location :accessor location))
  (:documentation "A single object that can move around in the stage."))

(defclass actor (point-actor)
  ((size :initarg :size :accessor size))
  (:default-initargs
   :size (vec 50 50)))

(defclass rectangle-actor (actor)
  ((background-colour :accessor background-colour :initarg :background)
   (foreground-colour :accessor foreground-colour :initarg :foreground)
   (border-width :accessor border-width :initarg :border-width))
  (:documentation "A rectangle.")
  (:default-initargs
   :background *background-colour*
   :foreground *text-colour*
   :border-width nil
   :size (vec2 50 50)))

(defclass text-actor (rectangle-actor)
  ((text :accessor text :initarg :text)
   (alignment :accessor alignment :initarg :alignment)
   (font :accessor font :initarg :font)
   (font-size :accessor font-size :initarg :font-size)
   (include-box :accessor include-box :initarg :include-box))
  (:documentation "A rectangle with text.")
  (:default-initargs
   :size nil
   :text "(...)"
   :alignment 0
   :font "sans-serif"
   :font-size 15
   :include-box nil))

(defclass ellipse-actor (rectangle-actor)
  ())

(defclass group-actor (actor)
  ((contents :accessor contents
             :initform nil
             :initarg :contents)))

;;; Casts are a list of actors.
;;; but maybe Flare's `scene' is what we need.
(defclass cast ()
  ((actors :accessor actors
           :initform (make-hash-table)))
  (:documentation "Container for a list of actors in a scene."))

(defgeneric %define-actor (cast-list name)
  (:documentation "Ensure the actor NAME exists and has an actor associated with it.")
  (:method ((cast-list cast) (name symbol))
    (setf (gethash name (actors cast)) (make-instance 'actor))))

;;; Painting
(defgeneric rect (rectangle))

(defmethod rect ((rect vec4))
  (q+:make-qrectf (coerce (vx rect) 'single-float)
                  (coerce (vy rect) 'single-float)
                  (coerce (vz rect) 'single-float)
                  (coerce (vw rect) 'single-float)))

(defmethod rect ((rect vec2))
  (q+:make-qrectf 0.0 0.0
                  (coerce (vx rect) 'single-float)
                  (coerce (vy rect) 'single-float)))

(defgeneric brush (colour)
  (:method ((colour colour))
    (q+:make-qbrush (->qcolor colour))))

(defmethod rect ((rect vec4))
  (q+:make-qrectf (coerce (vx rect) 'single-float)
                  (coerce (vy rect) 'single-float)
                  (coerce (vz rect) 'single-float)
                  (coerce (vw rect) 'single-float)))

(defmethod rect ((rect vec2))
  (q+:make-qrectf 0.0 0.0
                  (coerce (vx rect) 'single-float)
                  (coerce (vy rect) 'single-float)))

(defgeneric paint (thing painter)
  (:method-combination progn))

(defmethod paint progn ((entity actor) painter))

(defmethod flare:paint ((entity actor) target)
  (flare:with-translation ((location entity) target)
    (paint entity target)))

;;; Specific types of actor
(defclass rectangle-actor (actor)
  ((background-colour :accessor background-colour :initarg :background)
   (foreground-colour :accessor foreground-colour :initarg :foreground)
   (border-width :accessor border-width :initarg :border-width))
  (:documentation "A rectangle.")
  (:default-initargs
   :background *background-colour*
   :foreground *text-colour*
   :border-width nil
   :size (vec2 50 50)))

(defmethod paint progn ((box rectangle-actor) painter)
  (let ((size (size box)))
    (destructuring-bind (offset . color) (border box)
      (with-finalizing ((brush (brush color)))
        (fill painter brush (vec (- (vw offset))
                                 (- (vx offset))
                                 (+ (vx size) (vw offset) (vy offset))
                                 (vx offset)))
        (fill painter brush (vec (vx size)
                                 (- (vx offset))
                                 (vy offset)
                                 (+ (vy size) (vx offset) (vz offset))))
        (fill painter brush (vec (- (vw offset))
                                 (vy size)
                                 (+ (vx size) (vw offset) (vy offset))
                                 (vz offset)))
        (fill painter brush (vec (- (vw offset))
                                 (- (vx offset))
                                 (vw offset)
                                 (+ (vy size) (vx offset) (vz offset))))))
    (with-finalizing ((brush (brush (background box))))
      (fill painter brush (vec 0 0 (vx size) (vy size))))))

(defclass text (sized-entity)
  ((text :initarg :text :accessor text)
   (color :initarg :color :accessor color)
   (font :initarg :font :accessor font)
   (alignment :initarg :align :accessor alignment))
  (:default-initargs
   :text "< >"
   :color (->colour 0 0 0)
   :align (cons :center :center)))

(defmethod paint progn ((text text) painter)
  (with-finalizing ((brush (brush (color text)))
                    (rect (rect (size text)))
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
    (setf (q+:brush painter) brush)
    (q+:draw-text painter rect (text text) option)))

(defclass ellipse-actor (rectangle-actor)
  ())

(defclass textbox (box text)
  ())

(defclass group-actor (actor)
  ((contents :accessor contents
             :initform nil
             :initarg :contents)))

(defmacro define-presentation (name (width height &rest initargs) &body intervals)
  `(progn
     (define-widget ,name (QWidget qstage)
       ()
       (:default-initargs
         :progression ',name
         :stage-width ,width
         :stage-height ,height
         ,@initargs))
     (flare:define-progression ,name
       ,@intervals)))

(define-presentation test (800 600)
  1 1 (T (flare:enter textbox :border (cons (vec 3 3 3 3) (->colour (floor ) 0 0))
                              :location (vec 50 50)
                              :text "Hi iso"
                              :size (vec 100 30)
                              :name :box))
  0 T (:box (flare:calc location :to (vec (+ 400 (* 400 (sin flare:clock))) 50))))
