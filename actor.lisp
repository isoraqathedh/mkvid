;;;; actor.lisp

(in-package #:mkvid)
(in-readtable :qtools)

(defclass actor ()
  ()
  (:documentation "A single object that can move around in the stage. "))

(defclass rectangle-actor (actor)
  ((x :accessor actor-width
      :initarg :actor-width
      :initform (error "Must specify width."))
   (y :accessor actor-height
      :initarg :actor-height
      :initform (error "Must specify height."))))

(defclass text-actor (rectangle-actor)
  ((text :accessor text
         :initarg :text)
   (x :accessor actor-width
      :initarg :actor-width
      :initform nil)
   (y :accessor actor-height
      :initarg :actor-height
      :initform nil))
  (:documentation "A rectangle with text."))

(defclass ellipse-actor (rectangle-actor)
  ())

(defclass group-actor (actor)
  ())

(defgeneric draw-figure (actor window))
