;;;; actor.lisp
;;; Contains most of the basics of an actor, including the paint methods.
(in-package #:mkvid)
(in-readtable :qtools)

;;; ==========
;;; The basics
(defclass entity (flare:entity)
  ((flare:location :accessor location)))

(defgeneric paint (thing painter)
  (:method-combination progn))

(defmethod paint progn ((entity entity) painter))

(defmethod flare:paint ((entity entity) (target qt:QObject))
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
