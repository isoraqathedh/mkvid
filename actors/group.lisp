;;;; group.lisp

;;; Group actor
;;; a formation that join two or more actors.
;;; Their positions are, instead of having their own,
;;; completely determined by the group-actor.
;;; The actor should be placed at (0, 0) to begin with to minimise surprises.

(in-package #:mkvid)
(in-readtable :qtools)

(defclass group-actor (flare:container-unit)
  ((locations :accessor locations :initarg :locations)
   (flare:location :accessor location :initarg :location))
  (:default-initargs
   :locations nil
   :location (vec 0 0)))

(defgeneric adjust-subactor-positions (group-actor)
  (:method ((group group-actor))
    (for:for ((actor flare-queue:in-queue (objects group))
              (actor-offset in (locations group)))
      (setf (location actor) (v+ actor-offset (location group))))))

(defgeneric register-subactor-positions (group-actor)
  (:method ((group group-actor))
    (for:for ((actor flare-queue:in-queue (objects group)))
      (push (location actor) (locations group)))))

(defmethod flare:enter :after (unit (container group-actor))
  (adjust-subactor-positions container))

(defmethod paint progn ((group-actor group-actor) painter)
  (flare:do-container-tree (actor group-actor)
    (paint actor painter)))

(defmethod (setf location) :after (value (group-actor group-actor))
  (declare (ignore value))
  (adjust-subactor-positions group-actor))
