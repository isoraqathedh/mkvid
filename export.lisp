;;;; export.lisp
;;;; Create frames and stitch into video.

(in-package #:mkvid) (in-readtable :qtools)

(defparameter *frames-folder*
  (merge-pathnames
   #p"frames/"
   (asdf:system-relative-pathname 'mkvid nil))
  "Output folder for frames.")
