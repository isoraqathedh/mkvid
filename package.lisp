;;;; package.lisp

(defpackage #:info.isoraqathedh.mkvid
  (:nicknames #:mkvid)
  (:use #:cl+qt)
  (:import-from #:flare
                #:particle #:scene #:define-progression #:enter #:start #:calc
                #:paint)
  (:export #:main))
