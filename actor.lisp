;;;; actor.lisp

(in-package :mkvid)
(in-readtable :qtools)

(defclass actor (flare:particle)
  ((name :accessor actor-name
         :initarg :actor-name
         :initform nil))
  (:documentation "A single object in the wholse "))

(defclass text-actor (actor)
  ())

(defclass circle-actor (actor)
  ())

(defclass rectangle-actor (actor)
  ())

(defclass group-actor (actor)
  ())

(defgeneric draw-figure (actor window))
