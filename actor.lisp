;;;; actor.lisp

(in-package #:mkvid)
(in-readtable :qtools)

(defclass actor (flare:particle)
  ()
  (:documentation "A single object that can move around in the stage. "))

(defclass rectangle-actor (actor)
  ((size :accessor size-of
         :initarg :size)
   (brush :accessor brush
          :initarg :brush)
   (pen :accessor pen
        :initarg :pen))
  (:documentation "A rectangle.")
  (:default-initargs
   :size (error "Must provide size.")
   :brush nil
   :pen nil))

(defclass text-actor (rectangle-actor)
  ((text :accessor text
         :initarg :text))
  (:documentation "A rectangle with text.")
  (:default-initargs
   :size nil
   :text "(...)"))

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

(defmacro with-saved-painter-state (painter &body body)
  "Save the painter state, execute BODY, then restore it."
  `(progn
     (q+:save ,painter)
     ,@body
     (q+:restore ,painter)))

(defmacro with-brush-pen-font ((painter brush pen font) &body body)
  `(with-saved-painter-state ,painter
     ;; Let new setup
     (when ,brush
       (setf (q+ brush ,painter) ,brush))
     (when ,pen
       (setf (q+ pen ,painter) ,pen))
     (when ,font
       (setf (q+ font ,painter) ,font))
     ;; Main stuff
     ,@body))

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

;;; We're painting on `qpainter's, but they don't actually exist in lisp,
;;; so we're specialising on `qobject's instead.
(defmethod paint ((paintable rectangle-actor) (target qobject))
  (with-brush-pen-font (target (brush actor) nil nil)
    (q+:fill-rect target
                  (q+:make-qrectf (coordinates :output #'q+:make-qpointf)
                                  (size paintable)))))

(defmethod paint ((paintable ellipse-actor) (target qobject))
  (with-brush-pen-font (target (brush paintable) (pen paintable) nil)
    (q+:draw-ellipse target
                     (coordinates* :output #'q+:make-qpointf)
                     (size paintable))))

(defmethod paint ((paintable text-actor) (target qobject))
  (with-brush-pen-font (target (brush paintable) (pen paintable) (font paintable))
    (when (include-box paintable)
      (q+:draw-rect target (origin)))
    (q+:draw-text target position (alignment paintable) (text paintable))))
