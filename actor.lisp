;;;; actor.lisp

(in-package #:mkvid)
(in-readtable :qtools)

(defclass actor (flare:particle)
  ()
  (:documentation "A single object that can move around in the stage. "))

(defclass rectangle-actor (actor)
  ((x :accessor actor-width
      :initarg :actor-width
      :initform (error "Must specify width."))
   (y :accessor actor-height
      :initarg :actor-height
      :initform (error "Must specify height."))
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

;; (defclass scene (flare:scene)
;;   ((actors :accessor actors
;;            :initarg :actors)))

(defun draw-current-scene (window scene)
  "Draw the current scene into the window."
  (loop for actor in (actors scene)
        do (draw-figure paintor actor)))

(defun %define-actor (name ))
