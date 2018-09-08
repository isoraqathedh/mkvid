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
      :initform (error "Must specify height.")))
  (:documentation "A rectangle."))

(defclass text-actor (rectangle-actor)
  ((text :accessor text
         :initarg :text)
   (x :initform nil)
   (y :initform nil))
  (:documentation "A rectangle with text."))

(defclass ellipse-actor (rectangle-actor)
  ())

(defclass group-actor (actor)
  ((contents :accessor contents
             :initform nil
             :initarg :contents)))

(defgeneric draw-figure (painter actor)
  (:documentation "Draw the figure onto the stage.")
  (:method ((painter painter) (actor rectangle-actor))
    ()))

;; (defmethod flare:call-with-translation ((func function) (painter painter) vec)
;;   (q+:save painter)
;;   (q+:translate painter
;;                 (3d-vectors:vx vec)
;;                 (3d-vectors:vy vec))
;;   (funcall func)
;;   (q+:restore painter))
