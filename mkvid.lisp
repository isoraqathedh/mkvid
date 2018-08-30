;;;; mkvid.lisp

(in-package #:mkvid)
(in-readtable :qtools)

(defparameter *background-colour* (q+:make-qcolor 0 10 25)
  "The background colour for the stage.")
(defparameter *text-colour* (q+:make-qcolor 240 240 15)
  "The foreground colour for the stage.")

;; (define-subwidget (viewer timer) (q+:make-qtimer viewer)
;;   (setf (q+:single-shot timer) nil)
;;   (q+:start timer (round 1000/60)))

;; (defclass style-combination ()
;;   ((font-face :initarg :font-face
;;               :accessor font-face
;;               :initform nil)
;;    (font-size :initarg :font-size
;;               :accessor font-size
;;               :initform nil)
;;    (fg-colour :initarg :fg-colour
;;               :accessor fg-colour
;;               :initform nil)
;;    (bg-colour :initarg :bg-colour
;;               :accessor bg-colour
;;               :initform nil))
;;   (:documentation "A combination of styles."))

;; (defgeneric make-font (style-combination)
;;   (:documentation "Make a font object based on the style combination.")
;;   (:method ((style-combination style-combination))
;;     (q+:make-qfont (font-face style-combination)
;;                    (font-size style-combination))))

;; (defgeneric )

;; (define-slot (viewer update) ()
;;   (declare (connected timer (timeout)))
;;   (update scene)
;;   (q+:repaint viewer))
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
