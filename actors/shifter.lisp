;;;; shifter.lisp
;;; Shifter actor
;;; a group actor that allows its members to shift position one after another
;;; in a "musical chairs" kind of way.
(in-package #:mkvid)
(in-readtable :qtools)

(defclass shifter-actor (group-actor) ())

(defgeneric shift-actors (shifter-actor)
  (:method ((shifter-actor shifter-actor))
    ;; placeholder until later.
    (let ((top (pop (locations shifter-actor))))
      (setf (locations shifter-actor)
            (nconc (locations shifter-actor)
                   (list top))))))

(defmethod flare:ease-object ((from list) (to list) x by)
  (let ((len (length from)))
    (mapcar #'flare:ease-object from to
            (make-list len :initial-element x)
            (make-list len :initial-element by))))
