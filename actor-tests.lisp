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

(define-change-parser increase* (accessor &key from by (ease 'flare:linear)
                                          (with #'+))
  `(make-instance 'range-accessor-tween :ease ',ease
                                        :from ,from
                                        :to (funcall ,with ,from ,by)
                                        :accessor ',accessor))

(define-presentation group-actor-test (1024 576)
  1 (t (enter group-actor :name :group :location (vec 400 0)))
  2 (:group (enter actor-test-textbox :location (vec 100 100)
                                      :text "[A]"
                                      :name :text-A))
  3 (:group (enter actor-test-textbox :location (vec 300 300)
                                      :text "[B]"
                                      :name :text-B))
  ;; Doesn't work -- need to figure out if actor positions are altered.
  ;; The main issue here is that `increase' does not take vectors.
  ;; we'll have to figure out how to do it.
  4 4.5 ((:group >) (set location :to (vec 250 100))))
