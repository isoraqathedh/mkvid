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

;;; Preamble
(in-package #:mkvid)
(named-readtables:in-readtable :qtools)

;;; Definitions go here

;;; Custom actors go here, if there are any

;;; The actual video
(define-presentation example-video (1024 576)
  ;; Title screen
  0.1 (t (enter subtitles :name :subtitles))
  0.1 1 (t (enter title-textbox :name :title-text
                                :text "The Title"
                                :ease flare:quad-in))
  1 7 (:subtitles (display "A narration of the title."))
  5 6 (:title-text (leave))
  ;; A bunch of text scrolling by.
  8 (t (enter textbox :name :lyrics-box
                      :align (cons :left :bottom)
                      :size (vec (rx 3/8) (ry 4/5))
                      :size (vec (rx 1/8) (ry 1/10))
                      :text ""
                      :background *background-colour*))
  10 14 (:subtitles (display "A new line | Another new line"))
  10 (:lyrics-box (append-text "A new line"))
  12 (:lyrics-box (append-text "Another new line"))
  ;; More examples later...

  16 (:subtitles (display))
  16 (:lyrics-box (leave))

  20 (t (enter text :text "A word"
               ;; alignment arguments omitted
                    :name :word))
  20 23 (:subtitles (display "So we start here"))
  24 (t (enter image-actor
               :name :card-1
               :image-file #P"card-1.png"
               :location (vec (rx 1) (ry 1/2))
               :size (vec 100 100)))
  24.5 25.3 (:card-1 (set location :to (vec (rx 1/2) (ry 1/2))
                                   :ease flare:quad-in))
  25.3 (t (enter image-actor :name :explosion
                             :location (vec (rx 1/2) (ry 1/2))
                             :image-file #P"explosion.png"))
  25.5
  ((:card-1 :explosion) (leave))
  (:word (set text :to "B word"))
  23 26 (:subtitles (display "and if then fire this card,")))

(mkvid example-video #P"example-video.mp4")
(mksubtitles example-video :subtitles)
