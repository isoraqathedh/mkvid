;;;; actor.lisp

(in-package #:mkvid)
(in-readtable :qtools)

(defparameter *background-colour* (vec3 0 10 25)
  "The background colour for the stage.")
(defparameter *text-colour* (vec3 240 240 15)
  "The foreground colour for the stage.")

(defclass actor (flare:particle)
  ()
  (:documentation "A single object that can move around in the stage. "))

(defclass rectangle-actor (actor flare:sized-entity)
  ((background-colour :accessor background-colour :initarg :background)
   (foreground-colour :accessor foreground-colour :initarg :foreground)
   (border-width :accessor border-width :initarg :border-width))
  (:documentation "A rectangle.")
  (:default-initargs
   :background-colour nil
   :foreground-colour *text-colour*))

(defclass text-actor (rectangle-actor)
  ((text :accessor text :initarg :text)
   (alignment :accessor alignment :initarg :alignment)
   (font :accessor font :initarg :font)
   (font-size :accessor font-size :initarg :font-size)
   (include-box :accessor include-box :initarg :include-box))
  (:documentation "A rectangle with text.")
  (:default-initargs
   :size nil
   :text "(...)"
   :alignment 0
   :font "sans-serif"
   :font-size 15
   :include-box nil))

(defclass ellipse-actor (rectangle-actor)
  ())

(defclass group-actor (actor)
  ((contents :accessor contents
             :initform nil
             :initarg :contents)))

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
                  (q+:make-qrectf *origin*
                                  (size-of paintable)))))

(defmethod paint ((paintable ellipse-actor) (target qobject))
  (with-finalizing ((brush (q+:make-qbrush (brush paintable)))
                    (pen (q+:make-qpen (pen paintable))))
    (with-brush-pen-font (target brush pen nil)
      (q+:draw-ellipse target *origin* (size paintable)))))

(defmethod paint ((paintable text-actor) (target qobject))
  (with-finalizing ((brush (apply #'q+:make-qbrush (brush paintable)))
                    (pen (apply #'q+:make-qpen (pen paintable)))
                    (font (apply #'q+:make-qfont (font paintable))))
    (with-brush-pen-font (target brush pen font)
      (when (include-box paintable)
        (q+:draw-rect target *origin* (size paintable)))
      (q+:draw-text target
                    *origin*
                    (alignment paintable)
                    (text paintable)))))

;;; Flare
(defmethod flare:call-with-translation ((func function) (painter painter) vec)
  (with-saved-painter-state painter
    (q+:translate painter (vx vec) (vy vec))
    (funcall func)))
