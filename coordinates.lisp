;;;; coordinates.lisp
;;;; handles coordinates and sizing.

(in-package #:mkvid)
(in-readtable :qtools)

(defvar *width*)
(defvar *height*)

(defun rx (proportion)
  (* proportion *width*))

(defun ry (proportion)
  (* proportion *height*))

(defun make-size (&key (x 0) (y 0) (rx 0) (ry 0))
  (vec (+ x (rx rx))
       (+ y (ry ry))))
