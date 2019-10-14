;;;; mkvid.asd

(asdf:defsystem #:mkvid
  :description "An animation engine based on Flare."
  :author "Isoraķatheð Zorethan <isoraqathedh.zorethan@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:qtools #:qtcore #:qtgui #:flare #:3d-vectors #:parse-float)
  :components ((:file "package")
               (:file "colours")
               (:file "coordinates")
               (:file "window")
               (:file "behaviour")
               (:file "export")
               (:file "actor")
               (:file "mkvid")))

(asdf:defsystem #:mkvid/additional-tests
  :description "An system with additional tests for mkvid."
  :author "Isoraķatheð Zorethan <isoraqathedh.zorethan@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:mkvid)
  :components ((:file "actor-tests")))
