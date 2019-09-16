;;;; actor-tests.lisp
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
  ;; Doesn't work -- need to figure out if actor positions are altered.
  4 4.5 (:group (set location :to (vec 250 0) :ease flare:quad-out)))
