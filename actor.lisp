;;;; actor.lisp

(in-package #:mkvid)
(in-readtable :qtools)

(defclass actor (flare:particle)
  ()
  (:documentation "A single object that can move around in the stage. "))

(defclass rectangle-actor (actor)
  ((size :accessor size-of
         :initarg :size
         :initform (error "Must provide size."))
   (brush :accessor brush
          :initarg :brush
          :initform nil)
   (pen :accessor pen
        :initarg :pen
        :initform nil))
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

;; (defmethod flare:call-with-translation ((func function) (painter painter) vec)
;;   (q+:save painter)
;;   (q+:translate painter
;;                 (3d-vectors:vx vec)
;;                 (3d-vectors:vy vec))
;;   (funcall func)
;;   (q+:restore painter))

(defmacro with-brush-pen-font ((painter brush pen font) &body body)
  `(progn
     (q+:save ,painter)
     ;; Let new setup
     (when ,brush
       (setf (q+ brush ,painter) ,brush))
     (when ,pen
       (setf (q+ pen ,painter) ,pen))
     (when ,font
       (setf (q+ font ,painter) ,font))
     ;; Main stuff
     ,@body
     ;; Restore old setup
     (q+:restore ,painter)))

(defun text-actor (painter position text
                   &key brush pen font include-box alignment)
  (with-brush-pen-font (painter brush pen font)
    (when include-box
      (q+:draw-rect painter position))
    (q+:draw-text painter position alignment text)))

(defun circle-actor (painter position size &key brush pen)
  (with-brush-pen-font (painter brush pen nil)
    (q+:draw-ellipse painter position size size)))

(defun rectangle-actor (painter rectangle &key brush)
  (with-brush-pen-font (painter brush nil nil)
    (q+:fill-rect painter rectangle brush)))

(defgeneric draw-figure (painter actor)
  (:documentation "Draw the figure onto the stage.")
  (:method (painter (actor rectangle-actor))
    (with-brush-pen-font (painter (brush actor) nil nil)
      (q+:fill-rect painter (rectangle-of actor) brush)))
  (:method (painter (actor text-actor))
    (with-brush-pen-font (painter
                          (brush actor)
                          (pen actor)
                          (font actor))
      (when (include-box actor)
        (q+:draw-rect painter (rectangle-of actor)))
      (q+:draw-text painter
                    (rectangle-of actor)
                    (alignment-of actor)
                    (text-of actor))))
  (:method (painter (actor ellipse-actor))))

(defclass cast ()
  ((actors :accessor actors
           :initform (make-hash-table)))
  (:documentation "Container for a list of actors in a scene."))

(defgeneric %define-actor (cast-list name)
  (:documentation "Ensure the actor NAME exists and has an actor associated with it.")
  (:method ((cast-list cast) (name symbol))
    (setf (gethash name (actors cast)) (make-instance 'actor))))

(defmethod paint ((paintable actor) target)
  ;; Do something with the thing here
  )
