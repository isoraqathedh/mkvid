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

(defgeneric ->qcolour (thing)
  (:method ((thing 3-colour))
    (q+:make-qcolor (slot-value thing 'r)
                    (slot-value thing 'g)
                    (slot-value thing 'b)))
  (:method ((thing 4-colour))
    (q+:make-qcolor (slot-value thing 'r)
                    (slot-value thing 'g)
                    (slot-value thing 'b)
                    (slot-value thing 'a))))

(defparameter *background-colour* (vec3 0 10 25)
  "The background colour for the stage.")
(defparameter *text-colour* (vec3 240 240 15)
  "The foreground colour for the stage.")

(defclass actor (flare:particle)
  ()
  (:documentation "A single object that can move around in the stage. "))

(defclass rectangle-actor (actor flare:sized-entity)
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
