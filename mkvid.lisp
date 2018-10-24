;;;; main test file (copied from shin)
(in-package #:mkvid)
(in-readtable :qtools)

(define-widget canvas (QWidget)
  ((progression :initarg :progression :reader progression)
   (scene :initform (make-instance 'flare:scene) :reader scene)
   (background :initform *background-colour* :accessor background)))

(defmethod initialize-instance :after ((canvas canvas) &key width height)
  (setf (q+:fixed-size canvas) (values width height))
  (flare:start (scene canvas))
  (flare:start (flare:enter (flare:progression-instance (progression canvas)) (scene canvas))))

(define-subwidget (canvas timer) (q+:make-qtimer canvas)
  (setf (q+:single-shot timer) NIL)
  (q+:start timer (round 1000/30)))

(define-slot (canvas update) ()
  (declare (connected timer (timeout)))
  (flare:update (scene canvas))
  (q+:repaint canvas))

(define-override (canvas paint-event) (ev)
  (declare (ignore ev))
  (with-finalizing ((painter (q+:make-qpainter canvas))
                    (back (brush (background canvas))))
    (setf (q+:background painter) back)
    (q+:erase-rect painter (q+:rect canvas))
    (q+:save painter)
    (flare:paint scene painter)
    (q+:restore painter))
  (stop-overriding))

(defmethod flare:call-with-translation (func (target qobject) vec)
  (q+:save target)
  (unwind-protect
       (with-finalizing ((point (q+:make-qpointf (vx vec) (vy vec))))
         (q+:translate target point)
         (funcall func))
    (q+:restore target)))

(defmacro define-presentation (name (width height &rest initargs) &body intervals)
  `(progn
     (define-widget ,name (QWidget canvas)
       ()
       (:default-initargs
         :progression ',name
         :width ,width
         :height ,height
         ,@initargs))
     (flare:define-progression ,name
       ,@intervals)))

(defun present (name)
  (with-main-window (w name)))

;;; actors and painting
(defclass entity (flare:entity)
  ((flare:location :accessor location)))

(defgeneric paint (thing painter)
  (:method-combination progn))

(defgeneric rect (rectangle))

(defmethod paint progn ((entity entity) painter))

(defmethod flare:paint ((entity entity) target)
  (flare:with-translation ((location entity) target)
    (paint entity target)))

(defclass sized-entity (entity)
  ((size :initarg :size :accessor size))
  (:default-initargs
   :size (vec 50 50)))

(defclass box (sized-entity)
  ((background :initarg :background :accessor background)
   (border :initarg :border :accessor border))
  (:default-initargs
   :background (->colour 0 0 0 0)
   :border (cons (vec 0 0 0 0) (->colour 0 0 0))))

(defmethod rect ((rect vec4))
  (q+:make-qrectf (coerce (vx rect) 'single-float)
                  (coerce (vy rect) 'single-float)
                  (coerce (vz rect) 'single-float)
                  (coerce (vw rect) 'single-float)))

(defmethod rect ((rect vec2))
  (q+:make-qrectf 0.0 0.0
                  (coerce (vx rect) 'single-float)
                  (coerce (vy rect) 'single-float)))

(defgeneric brush (colour)
  (:method ((colour colour))
    (q+:make-qbrush (->qcolor colour))))

(defun fill-rect (painter brush rect)
  (with-finalizing ((rect (rect rect)))
    (q+:fill-rect painter rect brush)))

(defmethod paint progn ((box box) painter)
  (let ((size (size box)))
    (destructuring-bind (offset . color) (border box)
      (with-finalizing ((brush (brush color)))
        (fill-rect painter brush (vec (- (vw offset))
                                      (- (vx offset))
                                      (+ (vx size) (vw offset) (vy offset))
                                      (vx offset)))
        (fill-rect painter brush (vec (vx size)
                                      (- (vx offset))
                                      (vy offset)
                                      (+ (vy size) (vx offset) (vz offset))))
        (fill-rect painter brush (vec (- (vw offset))
                                      (vy size)
                                      (+ (vx size) (vw offset) (vy offset))
                                      (vz offset)))
        (fill-rect painter brush (vec (- (vw offset))
                                      (- (vx offset))
                                      (vw offset)
                                      (+ (vy size) (vx offset) (vz offset))))))
    (with-finalizing ((brush (brush (background box))))
      (fill-rect painter brush (vec 0 0 (vx size) (vy size))))))

(defclass text (sized-entity)
  ((text :initarg :text :accessor text)
   (color :initarg :font-color :accessor font-color)
   (font-size :initarg :font-size :accessor font-size)
   (font :initarg :font :accessor font)
   (alignment :initarg :align :accessor alignment))
  (:default-initargs
   :text "< >"
   :font "Inziu Iosevka TC"
   :font-size -1
   :font-color (->colour 240 240 0 255)
   :align (cons :center :center)))

(defmethod paint progn ((text text) painter)
  (with-finalizing ((brush (brush (font-color text)))
                    (rect (rect (size text)))
                    (font (q+:make-qfont (font text)
                                         (font-size text)))
                    (option (q+:make-qtextoption
                             (logior (ecase (car (alignment text))
                                       (:left (q+:qt.align-left))
                                       (:right (q+:qt.align-right))
                                       (:center (q+:qt.align-hcenter))
                                       (:justify (q+:qt.align-justify)))
                                     (ecase (cdr (alignment text))
                                       (:top (q+:qt.align-top))
                                       (:bottom (q+:qt.align-bottom))
                                       (:center (q+:qt.align-vcenter)))))))
    (setf (q+:brush painter) brush
          (q+:font painter) font)
    (q+:draw-text painter rect (text text) option)))

(defclass textbox (box text)
  ())

(define-presentation test (800 600)
  1 1 (T (flare:enter textbox :border (cons (vec 3 3 3 3) (->colour 240 0 0))
                              :location (vec 400 300)
                              :text "Test"
                              :font-color *text-colour*
                              :font-size 10
                              :size (vec 100 30)
                              :name :box))
  0 T (:box (flare:calc location :to (vec (+ 400 (* 200 (sin flare:clock)))
                                          (+ 300 (* 200 (sin (* 3/5 flare:clock))))))))
