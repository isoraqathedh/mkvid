(in-package #:flare)
;;;; Temporary fix for old versions until the new ones kick in.
(defun parse-animation (beginning duration expression)
  (destructuring-bind (selector &rest changes) expression
    (let ((animation (gensym "ANIMATION")))
      `(let ((,animation (make-instance 'animation :beginning ,beginning
                                                   :duration ,duration
                                                   :selector ',selector
                                                   :defindex (incf *animation-defindex*))))
         ,@(loop for change in changes
                 collect `(push (compile-change ,@change) (changes ,animation)))
         (setf (changes ,animation) (nreverse (changes ,animation)))
         ,animation))))

(defmethod flare:ease-object ((from vec2) (to vec2) x by)
  (vec2 (flare:ease x by (vx2 from) (vx2 to))
        (flare:ease x by (vy2 from) (vy2 to))))

(defmethod flare:ease-object ((from vec4) (to vec4) x by)
  (vec4 (flare:ease x by (vx4 from) (vx4 to))
        (flare:ease x by (vy4 from) (vy4 to))
        (flare:ease x by (vz4 from) (vz4 to))
        (flare:ease x by (vw4 from) (vw4 to))))
