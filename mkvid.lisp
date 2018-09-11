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
  (:method ((window main-window) &key (rx 0) (ry 0))
    (cond ((and (/= 0 rx) (/= 0 ry))
           (error "Cannot convert both rx and ry at once."))
          ((/= 0 rx)
           (* (stage-width window) rx))
          ((/= 0 ry)
           (* (stage-height window) ry))
          (t
           0))))

(defgeneric coordinates (window &key x y rx ry output suppress-stage)
  (:documentation "Create a point.

The point is defined by a combination of relative and absolute coordinates.
Relative coordinates set 1 to be the far edge of the stage (bottom and right),
whereas absolute coordinates set 1 to 1 pixel in size.
The argument OUTPUT indicates what output value is desired:
use the keyword :X to output the X coordinate,
the keyword :Y to output the Y coordinate,
or use a function to call that function with those two coordinates.
SUPPRESS-STAGE cancels the offset created by the stage.")
  (:method ((window main-window)
            &key (x 0) (y 0) (rx 0) (ry 0)
                 (output #'cons)
                 suppress-stage)
    (let ((final-x (+ (if suppress-stage
                          0
                          (stage-topleft-x window))
                      x
                      (relative window :rx rx)))
          (final-y (+ (if suppress-stage
                          0
                          (stage-topleft-y window))
                      y
                      (relative window :ry ry))))
     (etypecase output
       ((eql :x) final-x)
       ((eql :y) final-y)
       (function (funcall output final-x final-y))))))

(defgeneric relative-rectangle (window left top width height)
  (:documentation "Create a rectangle with the listed coordinates.")
  (:method ((window main-window) left top width height)
    (q+:make-qrectf
     (coordinates window :rx left :ry top :output #'q+:make-qpointf)
     (coordinates window :rx width :ry height :output #'q+:make-qsizef
                         :suppress-stage t))))

(defgeneric centred-relative-rectangle (window width height)
  (:documentation "Create a rectangle centred with the stage
with the provided width and height.")
  (:method ((window main-window) width height)
    (let ((top (/ (- 1 height) 2))
          (left (/ (- 1 width) 2)))
      (relative-rectangle window left top width height))))

(defgeneric centred-relative-rectangle+offset (window width height dx dy)
  (:documentation "Create a rectangle centred with the stage
with the provided width and height, then shift it by some amount.")
  (:method ((window main-window) width height dx dy)
    (let ((box (centred-relative-rectangle window width height)))
      (q+:translate box
                    (relative-stage-coordinates* window :x dx)
                    (relative-stage-coordinates* window :y dy))
      box)))

(defmacro with-brush-pen-font ((painter brush pen font) &body body)
  (alexandria:with-gensyms (old-brush old-pen old-font)
    `(let (,old-brush ,old-pen ,old-font)
       ;; Let new setup
       (when ,brush
         (setf  ,old-brush (q+:brush ,painter)
                (q+ brush ,painter) ,brush))
       (when ,pen
         (setf  ,old-pen (q+:pen ,painter)
                (q+ pen ,painter) ,pen))
       (when ,font
         (setf  ,old-font (q+:font ,painter)
                (q+ font ,painter) ,font))
       ;; Main stuff
       ,@body
       ;; Restore old setup
       (when ,old-brush
         (setf (q+ brush ,painter) ,old-brush))
       (when ,old-pen
         (setf (q+ pen ,painter) ,old-pen))
       (when ,old-font
         (setf (q+ font ,painter) ,old-font)))))

(defun text-actor (painter position text
                        &key brush pen font include-box alignment)
  (with-brush-pen-font (painter brush pen font)
    (when include-box
      (q+:draw-rect painter position))
    (q+:draw-text painter position alignment text)))

(defun circle-actor (painter position size &key brush pen)
  (with-brush-pen-font (painter brush pen nil)
    (q+:draw-ellipse painter position size size)))

(defun rectangle-actor (painter rectangle &key brush)
  (with-brush-pen-font (painter brush nil nil)
    (q+:fill-rect painter rectangle brush)))

(define-override (main-window paint-event) (ev)
  (declare (ignore ev))
  (with-finalizing ((painter (q+:make-qpainter main-window)))
    (let ((main-box (centred-relative-rectangle+offset main-window 1/2 2/5 1/6 0))
          (code-box (relative-rectangle main-window 1/20 37/40 9/10 1/40))
          (line-box (relative-rectangle main-window 1/20 1/20 9/10 1/40))
          (background-brush (q+:make-qbrush *background-colour*)))
      (block background
        (rectangle-actor painter (relative-rectangle main-window 0 0 1 1)
                         :brush background-brush))

      (block station-line-band
        (rectangle-actor painter (relative-rectangle main-window 9/30 0 1/40 1)
                         :brush (q+:make-qbrush (q+:make-qcolor 200 15 15))))

      (block station-mark
        (circle-actor painter
                      (absolute-stage-coordinates
                       main-window
                       :point
                       (list (+ 30 (* (stage-width main-window) (+ 9/30 1/80)))
                             (+ 30 (* (stage-height main-window) 1/2))))
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
                                       (q+:qt.align-left)))))))

(defun main ()
  (with-main-window (w 'main-window)
    (setf (q+ window-title w) "mkvid")))
