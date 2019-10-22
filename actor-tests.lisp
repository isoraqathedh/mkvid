;;;; actor-tests.lisp

;;;; This file contains tests for each of the actor types.
;;;; They should demonstrate how the group works
;;;; either using the demonstration actor defined here or something similar.

(in-package #:mkvid)
(in-readtable :qtools)

(defclass actor-test-textbox (aux-data-textbox)
  ()
  (:default-initargs
   :font-size 16
   :size (vec 250 60)
   :background *background-colour*))

(defclass range-tween* (flare:range-tween)
  ((adding-function
    :accessor adding-func
    :initarg :adding-func)
   (by
    :accessor by
    :initarg :by))
  (:default-initargs
   :to nil ; Not needed in range-tween*
   :by 1
   :adding-func '+))

(defclass range-accessor-tween* (range-tween* flare::accessor-tween) ())

(defmethod flare::tween-value ((tween range-tween*) object clock step)
  (print tween)
  (let ((old-value (or (flare::from tween)
                       (flare:original-value object tween))))
    (flare:ease-object old-value
                       (funcall (print (symbol-function (adding-func tween)))
                                old-value
                                (by tween))
                       step
                       (flare::ease-func tween))))

(define-change-parser vincrease (accessor &key from (by (vec 0 1))
                                          (ease 'linear)
                                          (adding-func 'v+))
  `(print (make-instance 'range-accessor-tween*
                         :ease ',ease
                         :from ,from
                         :by ,by
                         :adding-func ',adding-func
                         :accessor ',accessor)))

(define-presentation group-actor-test (1024 576)
  1 (t (enter group-actor :name :group :location (vec 400 0)))
  2 (:group (enter actor-test-textbox :location (vec 100 100)
                                      :text "[A]"
                                      :name :text-A))
  3 (:group (enter actor-test-textbox :location (vec 300 300)
                                      :text "[B]"
                                      :name :text-B))
  4 4.5 ((:group >) (vincrease location :by (vec 250 100)
                                        :adding-func 'v+))
  5 (:text-A (leave)))
