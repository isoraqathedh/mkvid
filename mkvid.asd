;;;; mkvid.asd

(asdf:defsystem #:mkvid
  :description "Describe mkvid here"
  :author "Isoraķatheð Zorethan <isoraqathedh.zorethan@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:qtools #:qtcore #:qtgui #:flare)
  :components ((:file "package")
               (:file "window")
               (:file "actor")
               (:file "mkvid")))
