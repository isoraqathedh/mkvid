;;;; extra-changes.lisp

;;;; This file contains more changes from Flare
;;;; that are more useful for mkvid.

(in-package #:mkvid)
(in-readtable :qtools)

(defclass range-tweenf (flare:range-tween)
  ((adding-function
    :accessor adding-func
    :initarg :adding-func)
   (by
    :accessor by
    :initarg :by))
  (:default-initargs
   :to nil ; Not needed in range-tweenf
   :by 1
   :adding-func '+))

(defmethod flare::copy ((tween range-tweenf))
  (let ((c (call-next-method)))
    (setf (adding-func c) (adding-func tween)
          (by c) (by tween))
    c))

(defclass range-accessor-tweenf (range-tweenf flare::accessor-tween) ())

(defmethod flare::tween-value ((tween range-tweenf) object clock step)
  (let ((old-value (or (flare::from tween)
                       (flare:original-value object tween))))
    (flare:ease-object old-value
                       (funcall (symbol-function (adding-func tween))
                                old-value
                                (by tween))
                       step
                       (flare::ease-func tween))))

(define-change-parser increasef (accessor &key from (by (vec 0 1))
                                          (ease 'flare:linear)
                                          (adding-func 'v+))
  `(make-instance 'range-accessor-tweenf
                  :ease ',ease
                  :from ,from
                  :by ,by
                  :adding-func ',adding-func
                  :accessor ',accessor))
