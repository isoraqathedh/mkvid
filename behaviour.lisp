;;;; behaviour.lisp
(in-package #:mkvid)
(in-readtable :qtools)

(defun rslot-value (object &rest slots)
  "Retrieve the slots repeatedly from OBJECT.

For a list of slot names ('one 'two 'three ...),
first retrieve slot ONE of OBJECT,
then retrieve slot TWO of the result thereof,
then retrieve slot THREE of that result thereof,
and so on until the end of SLOTS is reached."
  (loop for i in slots
        for acc = (slot-value object i) then (slot-value acc i)
        finally (return acc)))

;;; Playback control
(define-signal (main-window play/pause) ())
(define-slot (main-window play/pause-receiver) ()
  (let ((scene (scene (rslot-value main-window 'central-widget 'stage))))
    (if (flare:running scene)
        (signal! main-window (pause))
        (signal! main-window (play)))))

(define-signal (main-window play) ())
(define-slot (main-window play-receiver) ()
  (signal! main-window (play)))

(define-signal (main-window pause) ())
(define-slot (main-window pause-receiver) ()
  (signal! main-window (pause)))

(define-signal (main-window restart) ())
(define-slot (main-window restart-receiver) ()
  (signal! main-window (restart)))

(define-signal (main-window seek) (float))
(define-slot (main-window seek-receiver) ((delta float))
  (signal! main-window (seek float) delta))

(define-signal (main-window load-presentation) (string))
(define-slot (main-window load-presentation-receiver) ((presentation string))
  (signal! main-window (load-presentation string) presentation))

(define-slot (canvas load-presentation) ((presentation-name string))
  ;; "Put the progression named PROGRESSION into a canvas, but don't play it."
  (load-presentation
   (find-symbol (string-upcase presentation-name))
   canvas))

;;; Status bar display.
(define-slot (main-window paused) ()
  (declare (connected main-window (pause)))
  (q+:show-message (q+:status-bar main-window) "Paused."))

(define-slot (main-window playing) ()
  (declare (connected main-window (play)))
  (q+:show-message (q+:status-bar main-window) "Playing."))

(define-slot (main-window restart) ()
  (declare (connected main-window (restart)))
  (q+:show-message (q+:status-bar main-window) "Restarted." 2000))

;;; Clock display.
(define-signal (main-window set-clock-time) (float))
(define-slot (main-window set-time) ((clock-time float))
  (declare (connected main-window (set-clock-time float)))
  (setf (q+:text clock-display) (format-clock clock-time)))
(define-slot (main-window reset-time) ()
  (setf (q+:text clock-display) (format-clock nil)))

;;; Canvas
(define-subwidget (canvas timer) (q+:make-qtimer canvas)
  (setf (q+:single-shot timer) NIL)
  (q+:start timer (round 1000/30)))

(define-slot (canvas update) ()
  (declare (connected timer (timeout)))
  (with-finalizing ((canvas-size (q+:size canvas)))
    (let ((*width* (q+:width canvas-size))
          (*height* (q+:height canvas-size)))
      (flare:update (scene canvas)))
    (signal! main-window (set-clock-time float) (clock (scene canvas)))
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

(define-slot (canvas play) ()
  (flare:start (scene canvas)))

(define-slot (canvas pause) ()
  (flare:stop (scene canvas)))

(define-slot (canvas restart) ()
  (flare:reset (scene canvas)))

(defun load-presentation (presentation-symbol canvas)
  (let* ((presentation (flare:progression-definition presentation-symbol))
         (instance (flare:progression-instance presentation-symbol))
         (w (width presentation))
         (h (height presentation))
         (*width* w)
         (*height* h))
    (setf (progression canvas) presentation
          (q+:fixed-size canvas) (values w h))
    (flare:start (scene canvas))
    (flare:start (flare:enter instance (scene canvas)))))

;;; Connecting everything up
(define-initializer (main-window connect-signals -1)
  "Connect all the inter-widget slots together."
  ;; Senders
  (connect! (rslot-value central-widget 'start-stop-button) (pressed)
            main-window (play/pause-receiver))
  (connect! (rslot-value central-widget 'restart-button) (released)
            main-window (restart-receiver))

  ;; Receivers
  (connect! main-window (play)
            (rslot-value central-widget 'stage) (play))
  (connect! main-window (pause)
            (rslot-value central-widget 'stage) (pause))
  (connect! main-window (restart)
            (rslot-value central-widget 'stage) (restart))

  ;; other
  (connect! (rslot-value central-widget 'playback-reset) (pressed)
            (rslot-value central-widget 'stage) (restore-speed))
  ;; (connect! main-window (play) main-window (playing))
  ;; (connect! main-window (pause) main-window (paused))
  t)
