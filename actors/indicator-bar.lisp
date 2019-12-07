;;;; indicator-bar.lisp

(in-package #:mkvid)
(in-readtable :qtools)

(defclass abstract-bar-actor (box)
  ((bar-maximum :initarg :maximum :accessor bar-maximum)
   (bar-current :initarg :current :accessor bar-current)
   (bar-direction :initarg :direction :accessor bar-direction))
  (:default-initargs
   :maximum 100
   :current 0
   :direction :horizontal))

(defmethod initialize-instance :after ((bar abstract-bar-actor)
                                       &key direction &allow-other-keys)
  (assert (member direction '(:horizontal :vertical))))

(defclass bar-actor (abstract-bar-actor)
  ((colour :initarg :colour :accessor colour))
  (:default-initargs
   :colour (->colour 190 190 190)))

(defmethod paint progn ((actor abstract-bar-actor) painter)
  (with-accessors ((bar-maximum bar-maximum)
                   (bar-current bar-current)
                   (bar-direction bar-direction)
                   (size size)) actor
    (with-finalizing ((brush (brush (colour actor))))
      (fill-rect painter
                 brush
                 (case bar-direction
                   (:horizontal
                    (vec2
                     (* (/ bar-current bar-maximum) (vx2 size))
                     (vx2 size)))
                   (:vertical
                    (let ((height (* (/ bar-current bar-maximum) (vy2 size))))
                      (vec4
                       0.0
                       (- (vy2 size) height)
                       (vx2 size)
                       height))))))))
