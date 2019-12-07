;;; last-this-next.lisp

;;; Last/this/next actor
;;; A special form of shifter actor that has three positions:
;;; a specific "this" position and the associated actor,
;;; with everything that comes before it being in "last" positions
;;; and everything that comes after it is in the "future" positions.
;;; Each position has a specific style that will stay on the positions,
;;; not the object (i.e. fixed values for some slots.)
;;;
;;; It is required that all the actors have to have the same slots,
;;; as it makes no sense to convert one actor type to another.

(in-package #:mkvid)
(in-readtable :qtools)

(defclass last/this/next-actor (group-actor)
  ((this-position :accessor this-position)))

(defgeneric tween-to-last (group-actor subactor))
(defgeneric tween-to-this (group-actor subactor))
(defgeneric tween-to-next (group-actor subactor))

;; (defmethod flare:enter ())
