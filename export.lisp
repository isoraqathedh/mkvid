;;;; export.lisp
;;;; Create frames and stitch into video.

(in-package #:mkvid) (in-readtable :qtools)

(defparameter *frames-folder*
  (merge-pathnames
   #p"frames/"
   (asdf:system-relative-pathname 'mkvid nil))
  "Output folder for frames.")

(define-slot (canvas dump-image) ()
  (with-finalizing* ((canvas-rect (q+:rect canvas))
                     (region (q+:make-qregion canvas-rect))
                     (origin (q+:make-qpoint))
                     (img (q+:make-qpixmap
                           (q+:width size)
                           (q+:height size))))
    ;; widget->render(&pixmap, QPoint(), QRegion(rectangle));
    (q+:render canvas img origin region)
    (q+:save img (namestring (merge-pathnames
                              (make-pathname :name "test" :type "png")
                              *frames-folder*)))))

(defun frames->video (dir)
  "Create a video with the frames in DIR.")

;;; To do: bypass the filesystem and find some way to use `clave' directly.
