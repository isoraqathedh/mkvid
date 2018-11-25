;;;; main test file (copied from shin)
(in-package #:mkvid)
(in-readtable :qtools)

;;; presentation files
(defclass aux-data-textbox (textbox)
  ()
  (:default-initargs
   :border (cons (vec 1 1 1 1) *text-colour*)
   :align (cons :left :center)
   :font-size 30
   :background *background-colour*))

(define-presentation test (1024 576)
  0.5
  (T (enter box
            :location (vec (rx 1/8) 0)
            :size (vec 30 (ry 1))
            :name :band
            :background (->colour 60 120 15))
     ;; station label, contents defined later.
     (enter group-actor
            :location (vec 0 0)
            :locations (list (vec (rx 1/4) (ry 4/10))
                             (vec (+ (rx 1/8)
                                     (* 30 1/2))
                                  (ry 1/2)))
            :name :station-label)
     (enter aux-data-textbox
            :location (vec (rx 1/20) (ry 17/20))
            :size (vec (rx 9/10) (ry 1/10))
            :text "LKX"
            :name :station-code-box)
     (enter aux-data-textbox
            :location (vec (rx 1/20) (ry 1/20))
            :size (vec (rx 9/10) (ry 1/10))
            :text "East Coast Main Line"
            :name :line-box)
     (enter textbox
            :border (cons (vec 3 3 3 3) (->colour 240 0 0))
            :location (vec (rx 1/5) (ry 1/5))
            :text "Test"
            :font-color *text-colour*
            :font-size 20
            :size (vec 150 40)
            :name :box)
     (enter image-actor
            :image-file (asdf:system-relative-pathname 'mkvid "sprite-test" :type "png")
            :size (vec 100 100)
            :location (vec (rx 1/2) (ry 1/2))
            :name :coin))
  (:station-label (enter textbox
                         :border (cons (vec 0 0 0 0) (->colour 0 0 0 0))
                         :size (vec (rx 5/8) (ry 2/10))
                         :align (cons :left :center)
                         :text "London King's Cross"
                         :font-size 50
                         :background *background-colour*
                         :name :station-code-box)
                  (enter oval
                         :size (vec 80 80)
                         :border (cons 15 *background-colour*)
                         :background *text-colour*
                         :name :station-oval))

  0 T (:box (calc location :to (let ((clk clock))
                                 (vec (+ 400 (* 200 (sin clk)))
                                      (+ 300 (* 200 (sin (* 3/5 clk)))))))
            (calc text :to (format nil "~,2,,'#,'0f s" clock)))

  0 40 (:coin (calc sprite-location :to (vec (floor (mod (* clock 30) 10)) 0)))

  5 5.5
  (:station-label (set location :to (vec 0 -100) :ease flare:quad-out))
  (:station-code-box (increase font-size :by -15))

  10 10.5
  (:station-label (set location :to (vec 0 -200) :ease flare:quad-out))

  40 (:coin (leave)))
