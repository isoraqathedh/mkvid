;;;; main test file (copied from shin)
(in-package #:mkvid)
(in-readtable :qtools)

;;; presentation files
(define-presentation test (1024 576)
  1 1 (T (flare:enter textbox :border (cons (vec 3 3 3 3) (->colour 240 0 0))
                              :location (vec (rx 1/5) (ry 1/5))
                              :text "Test"
                              :font-color *text-colour*
                              :font-size 20
                              :size (vec 150 40)
                              :name :box)
         (flare:enter textbox :border (cons (vec 0 0 0 0) (->colour 0 0 0 0))
                              :location (vec (rx 1/4) (ry 4/10))
                              :size (vec (rx 5/8) (ry 2/10))
                              :align (cons :left :center)
                              :text "London King's Cross"
                              :font-size 50
                              :background *background-colour*
                              :name :station-code-box)
         (flare:enter textbox :border (cons (vec 1 1 1 1) *text-colour*)
                              :location (vec (rx 1/20) (ry 1/20))
                              :size (vec (rx 9/10) (ry 1/10))
                              :align (cons :left :center)
                              :text "East Coast Main Line"
                              :font-size 30
                              :background *background-colour*
                              :name :line-box)
         (flare:enter textbox :border (cons (vec 1 1 1 1) *text-colour*)
                              :location (vec (rx 1/20) (ry 17/20))
                              :size (vec (rx 9/10) (ry 1/10))
                              :align (cons :left :center)
                              :text "LKX"
                              :font-size 30
                              :background *background-colour*
                              :name :station-code-box)
         (flare:enter oval :location (vec (+ (rx 1/8)
                                             (* 30 1/2))
                                          (ry 1/2))
                           :size (vec 80 80)
                           :border (cons 15 *background-colour*)
                           :background *text-colour*
                           :name :station-oval)
         (flare:enter box :location (vec (rx 1/8) 0)
                          :size (vec 30 (ry 1))
                          :name :band
                          :background (->colour 60 120 15)))
  0 T (:box (flare:calc location :to (let ((clock flare:clock))
                                       (vec (+ 400 (* 200 (sin clock)))
                                            (+ 300 (* 200 (sin (* 3/5 clock)))))))
            (flare:calc text :to (format nil "t = ~d" (floor flare:clock)))))
