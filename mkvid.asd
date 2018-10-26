;;;; mkvid.asd

(asdf:defsystem #:mkvid
  :description "Describe mkvid here"
  :author "Isoraķatheð Zorethan <isoraqathedh.zorethan@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:qtools #:qtcore #:qtgui #:flare #:3d-vectors)
  :components ((:file "package")
               (:file "colours")
               (:file "coordinates")
               (:file "window")
               (:file "actor")
               (:file "mkvid")))
