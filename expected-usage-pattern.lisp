;;;; expected-usage-pattern.lisp

;;;; This is an example of a script that MKVID is expected to parse
;;;; and turn into a video.
;;;; It includes a list of features that is desired to be in the program,
;;;; as well as all the actor types that should be included by default.
;;;; The goal of this file is to guide the creation of the program
;;;; such that its contents can be read without errors
;;;; and the video correctly defined and created.

;;;; This expected usage file is not intended to be the final scripting language
;;;; -- which is in essence a normal Common Lisp file
;;;; that defaults to loading the `mkvid' package and the `qtools' read table --
;;;; but it should expand to this at least,
;;;; along with some additional `eval-when'-like code
;;;; that allows the entire file to be read independently
;;;; (perhaps using an external script).

;;;; This is not a formal part of mkvid and contains no essential code for it.

(in-package #:mkvid)
(named-readtables:in-readtable :qtools)

(define-presentation example-video (1024 768)
  ;; examples here...
  )

(mkvid example-video)
(mksubtitles example-video)
