;;;; mkvid.lisp

(in-package #:mkvid)
(in-readtable :qtools)

(defgeneric relative (window &key rx ry)
  (:documentation "Convert relative coordinates to an absolute one.

Relative coordinates are those that set 1 to the far edge of the stage
\(bottom and right\).
As such 1 unit in the x direction is not necessarily
the same as 1 unit in the y direction.

This function can only convert one direction at any one time.
An error will be signalled if both RX and RY are nonzero.")
  (:method ((window qstage) &key (rx 0) (ry 0))
    (cond ((and (/= 0 rx) (/= 0 ry))
           (error "Cannot convert both rx and ry at once."))
          ((/= 0 rx)
           (* (stage-width window) rx))
          ((/= 0 ry)
           (* (stage-height window) ry))
          (t
           0))))

;; Helper functions for `%coordinates'
(defun arg-x (x y)
  (declare (ignore y))
  x)

(defun arg-y (x y)
  (declare (ignore x))
  y)

(defgeneric %coordinates (window x y rx ry output)
  (:method ((window qstage) x y rx ry output)
    (let ((final-x (+ x (relative window :rx rx)))
          (final-y (+ y (relative window :ry ry))))
     (etypecase output
       ((eql :x) final-x)
       ((eql :y) final-y)
       (function (funcall output final-x final-y))))))

(defgeneric coordinates (window &key x y rx ry output)
  (:documentation "Create a point.

The point is defined by a combination of relative and absolute coordinates.
Relative coordinates set 1 to be the far edge of the stage (bottom and right),
whereas absolute coordinates set 1 to 1 pixel in size.
The argument OUTPUT indicates what output value is desired:
use the keyword :X to output the X coordinate,
the keyword :Y to output the Y coordinate,
or use a function to call that function with those two coordinates.")
  (:method ((window qstage) &key (x 0) (y 0) (rx 0) (ry 0) (output #'cons))
    (%coordinates window x y rx ry output)))

(defun origin (stage &optional (function #'cons))
  "Return the coordinates corresponding to the top-left of the stage."
  (coordinates stage :output function))

(defgeneric offset-box (rectangle coordinates-as-cons)
  (:method (rectangle (coordinates cons))
    (q+:translate rectangle (car coordinates) (cdr coordinates))
    rectangle))

(defun rectangle (stage type
                  &key (left-a     0 lap)  (top-a      0 tap)
                       (width-a    0 wap)  (height-a   0 hap)
                       (bottom-a   0 bap)  (right-a    0 rap)
                       (left-r     0 lrp)  (top-r      0 trp)
                       (width-r    0 wrp)  (height-r   0 hrp)
                       (bottom-r   0 brp)  (right-r    0 rrp)
                       (anchor-x-a 0 axap) (anchor-y-a 0 ayap)
                       (anchor-x-r 0 axrp) (anchor-y-r 0 ayrp)
                       (prop-x-a   0 pxap) (prop-y-a   0 pyap)
                       (prop-x-r   0 pxrp) (prop-y-r   0 pyrp))
  "Create a rectangle.

TYPE indicates the type of vertices that are provided:

- :FREE --
  Here, the user provides a top-left (via top and left)
  and a either a size (height and width)
  or a bottom-right corner (via bottom and right).
- :CENTRED --
  Here, provide a size (height and width).
  The rectangle will be centred with the stage.
- :ANCHORED --
  Provide a size (height and width), and also an anchor point (anchor)
  and a proportion (prop).
  The rectangle would be the size provided,
  and the point given in anchor would be the given proportion of the way
  across the rectangle.
  For instance, this option can specify a (size =) 1/3 by 1/3 rectangle
  where the centre of the stage (anchor = 1/2, 1/2)
  is 40% of the way across the x-axis
  and 60% of the way across the y-axis (prop = 2/5, 3/5).

Any item provided but not required will be ignored."
  (declare (ignore tap rap lrp trp rrp))
  (let ((size
          (apply #'coordinates stage
                 :output #'q+:make-qsizef
                 (cond ((or wrp hrp wap hap)
                        (list :x width-a
                              :y height-a
                              :rx width-r
                              :ry height-r))
                       ((or brp lap bap lap)
                        (list :x (- right-a left-a)
                              :y (- bottom-a top-a)
                              :rx (- right-r left-r)
                              :ry (- bottom-r top-r)))
                       (t nil)))))
    (q+:make-qrectf
     (case type
       (:free
        (coordinates stage :x left-a :y top-a
                            :rx left-r :ry top-r
                            :output #'q+:make-qpointf))
       (:centred
        ;; (assuming relative coordinates)
        ;; A centred rectangle has the top left corner at:
        ;; (1/2 - w/2, 1/2 - h/2).
        (coordinates stage
                     :rx (- 1/2 (* width-r 1/2)) :ry (- 1/2 (* height-r 1/2))
                     :x (- (* width-a 1/2)) :y (- (* height-a 1/2))
                     :output #'q+:make-qpointf))
       (:anchored
        ;; In general, with an anchor at (a, b)
        ;; and the required anchor at (p, q)
        ;; (where p = 0 means the left edge of the rectangle
        ;; and p = 1 means the right edge of the rectangle)
        ;; the top-left corner is at (a - wp, b - hq),
        ;; and the bottom-right corner is at (a + (1-w)p, b + (1-h)q).

        ;; To make things simple, we require the provided values
        ;; to be all-relative or all-absolute.
        (cond ((or axap ayap pxap pyap)
               (coordinates stage
                            :rx (- anchor-x-r prop-x-a)
                            :ry (- anchor-y-r prop-y-a)
                            :x  (- anchor-x-a prop-x-a)
                            :y  (- anchor-y-a prop-y-a)
                            :output #'q+:make-qpointf))
              ((or axrp ayrp pxrp pyrp)
               (coordinates stage
                            :rx (- anchor-x-r (* width-r  prop-x-r))
                            :ry (- anchor-y-r (* height-r prop-y-r))
                            :x  (- anchor-x-a (* width-a  prop-x-r))
                            :y  (- anchor-y-a (* height-a prop-y-r))
                            :output #'q+:make-qpointf))
              (t (error "Anchors and sizes must be both ~
relative or both absolute.")))))
     size)))

(defparameter *counter* 9876)

(define-override (qstage paint-event) (ev)
  (declare (ignore ev))
  (with-finalizing ((painter (q+:make-qpainter qstage))
                    (main-box (offset-box
                               (rectangle qstage :centred
                                          :width-r 1/2 :height-r 2/5)
                               (coordinates qstage :rx 1/6 :ry 0)))
                    (code-box (rectangle qstage :free
                                         :left-r 1/20 :top-r (- 1 1/20 3/40)
                                         :width-r 9/10 :height-r 3/40))
                    (line-box (rectangle qstage :free
                                         :left-r 1/20 :top-r 1/20
                                         :width-r 9/10 :height-r 3/40))
                    (background-brush (q+:make-qbrush *background-colour*)))
    (block background
      (rectangle-actor painter (rectangle qstage :free
                                          :left-r 0 :top-r 0
                                          :width-r 1 :height-r 1)
                       :brush background-brush))

    (block station-line-band
      (rectangle-actor painter (rectangle qstage :free
                                          :left-r 9/30 :top-r 0
                                          :width-r 1/15 :height-r 1)
                       :brush (q+:make-qbrush (q+:make-qcolor 200 15 15))))

    (block station-mark
      (circle-actor painter
                    (coordinates qstage
                                 :rx (+ 9/30 1/30) :ry 1/2
                                 :output #'q+:make-qpointf)
                    30
                    :brush (q+:make-qbrush *text-colour*)
                    :pen (q+:make-qpen
                          (q+:make-qbrush
                           (q+:make-qcolor 255 255 255))
                          3)))

    (block station-name-box
      (text-actor painter main-box "London King's Cross"
                  :brush (q+:make-qbrush *background-colour*)
                  :pen *text-colour*
                  :font (q+:make-qfont "Inziu Iosevka TC" 35)
                  :include-box t
                  :alignment (logior (q+:qt.align-vcenter)
                                     (q+:qt.align-left))))

    (block station-code-box
      (text-actor painter code-box "LKX"
                  :brush background-brush
                  :font (q+:make-qfont "Inziu Iosevka TC" 20)
                  :pen *text-colour*
                  :include-box t
                  :alignment (logior (q+:qt.align-vcenter)
                                     (q+:qt.align-left))))

    (block station-line-name-box
      (text-actor painter line-box "East Coast Main Line"
                  :brush background-brush
                  :font (q+:make-qfont "Inziu Iosevka TC" 20)
                  :pen *text-colour*
                  :include-box t
                  :alignment (logior (q+:qt.align-vcenter)
                                     (q+:qt.align-left))))))

(define-override (main-window paint-event) (ev)
  (declare (ignore ev))
  (q+:display visible-test *counter*))
