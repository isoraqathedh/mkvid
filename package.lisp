;;;; package.lisp

(defpackage #:info.isoraqathedh.mkvid
  (:nicknames #:mkvid)
  (:use #:cl+qt #:3d-vectors)
  (:import-from #:flare
                #:particle #:scene #:define-progression #:enter #:start #:calc
                #:paint)
  (:export #:main))
