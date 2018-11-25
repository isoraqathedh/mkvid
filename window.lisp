;;;; window.lisp
(in-package #:mkvid)
(in-readtable :qtools)

;;; Main canvas type that everything hangs on
(define-widget canvas (QWidget)
  ((progression :initarg :progression :reader progression)
   (scene :initform (make-instance 'flare:scene) :reader scene)
   (background :initform *background-colour* :accessor background)))

(defmethod initialize-instance :after ((canvas canvas) &key width height)
  (setf (q+:fixed-size canvas) (values width height))
  (flare:start (scene canvas))
  (flare:start (flare:enter (flare:progression-instance (progression canvas)) (scene canvas))))

;;; (Future) other widgets and layout

;;; Animation
(define-subwidget (canvas timer) (q+:make-qtimer canvas)
  (setf (q+:single-shot timer) NIL)
  (q+:start timer (round 1000/30)))

(define-slot (canvas update) ()
  (declare (connected timer (timeout)))
  (with-finalizing ((canvas-size (q+:size canvas)))
    (let ((*width* (q+:width canvas-size))
          (*height* (q+:height canvas-size)))
      (flare:update (scene canvas)))
    (q+:repaint canvas)))

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
  (let ((*width* width)
        (*height* height))
    `(progn
       (define-widget ,name (QWidget canvas)
         ()
         (:default-initargs
           :progression ',name
           :width ,*width*
           :height ,*height*
           ,@initargs))
       (let ((*width* ,*width*)
             (*height* ,*height*))
         (flare:define-progression ,name
           ,@intervals)))))

;;; Keybinds
;; Currently there is no need to do anything elaborate or serious,
;; just respond to the key presses to the canvas that correspond to some action.
;; we'll figure something more complex later if we actually need it.
(define-override (canvas key-press-event) (event)
  (let ((key (q+:key event))
        (scene (scene canvas)))
    (cond
      ((eql key (q+:qt.Key_Q))
       (q+:quit qt:*qapplication*))
      ((eql key (q+:qt.Key_S))
       (if (flare:running scene)
           (flare:stop scene)
           (flare:start scene)))
      ((eql key (q+:qt.Key_R))
       (flare:stop scene)
       (flare:reset scene))
      (t (stop-overriding)))))

;;; Main
(defun present (name)
  (with-main-window (w name)
    (setf (q+:window-title w)
          (format nil "Presenting: ~a" (symbol-name name)))))
