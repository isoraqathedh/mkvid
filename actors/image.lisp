;;; image.lisp
;;; Image actor

(in-package #:mkvid)
(in-readtable :qtools)

(defclass image-actor (box)
  ((image :initarg :qpixmap
          :accessor qpixmap-of)
   (sprite-location :initarg :sprite-location
                    :accessor sprite-location))
  (:default-initargs
   :sprite-location (vec 0 0)))

(defmethod initialize-instance :after ((object image-actor) &key image-file)
  ;; Load the image.
  (setf (qpixmap-of object)
        (q+:make-qpixmap (namestring image-file))))

(defmethod paint progn ((image-actor image-actor) painter)
  (let ((sprite-width (floor (vx (size image-actor))))
        (sprite-height (floor (vy (size image-actor)))))
    (q+:draw-pixmap
     painter
     ;; Painting the sprite
     ;; we want the image to be centred at the location mark.
     ;; This is more expected behaviour.
     (round sprite-width -2) (round sprite-height -2) ; location
     sprite-width sprite-height                       ; drawing rectangle size
     (qpixmap-of image-actor)                         ; image
     ;; The location of the sprite
     ;; We'll take the size of the sprite as the size of the actor,
     ;; and then take the vxth column of the vyth row
     ;; of the image divided into rectangles of size that.
     (floor (* (vx (sprite-location image-actor))
               (vx (size image-actor))))
     (floor (* (vy (sprite-location image-actor))
               (vy (size image-actor))))
     sprite-width sprite-height)))  ; sprite rectangle size

(defmethod flare:leave :after ((unit image-actor) scene-graph)
  (declare (ignore scene-graph))
  ;; Need to clean up the qpixmap after the actor leaves
  ;; as it's basically impossible to access the actor after leaving
  ;; we'll declare it dead and just finalize the thing.
  (finalize (qpixmap-of unit)))
