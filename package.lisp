;;;; package.lisp

(defpackage #:info.isoraqathedh.mkvid
  (:nicknames #:mkvid)
  (:use #:cl+qt #:3d-vectors)
  (:import-from #:flare
   #:define-change-parser
   #:change
   #:call-change
   #:call
   #:clock
   #:objects
   #:creator
   #:enter
   #:create
   #:leave-operation
   #:leave
   #:objects
   #:tween
   #:slot-tween
   #:originals
   #:original-value
   #:range-tween
   #:range-slot-tween
   #:set
   #:constant-tween
   #:increase-slot-tween
   #:increase
   #:call-slot-tween
   #:call-accessor-tween
   #:calc)
  (:export #:main
           #:present
           #:define-presentation))
