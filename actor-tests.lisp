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

(define-presentation group-actor-test (1024 576)
  1 (t (enter group-actor :name :group :location (vec 400 0)))
  2 (:group (enter actor-test-textbox :location (vec 100 100)
                                      :text "[A]"
                                      :name :text-A))
  3 (:group (enter actor-test-textbox :location (vec 300 300)
                                      :text "[B]"
                                      :name :text-B))
  4 4.5 ((:group >) (increasef location :by (vec 250 100)
                                        :adding-func v+))
  5 (:text-A (leave)))
