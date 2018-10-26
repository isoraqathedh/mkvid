(in-package #:mkvid)
(in-readtable :qtools)

(defclass colour () ()
  (:documentation "A colour."))

(defclass 3-colour (colour)
  ((r :initarg :r)
   (g :initarg :g)
   (b :initarg :b))
  (:default-initargs
   :r 0 :g 0 :b 0))

(defclass 4-colour (3-colour)
  ((a :initarg :a))
  (:default-initargs
   :a 255))

(defmethod print-object ((thing 3-colour) stream)
  (print-unreadable-object (thing stream :type t)
    (with-slots (r g b) thing
      (format stream "#~2,'0x~2,'0x~2,'0x" r g b))))

(defmethod print-object ((thing 4-colour) stream)
  (print-unreadable-object (thing stream :type t)
    (with-slots (r g b a) thing
      (format stream "#~2,'0x~2,'0x~2,'0x~2,'0x" r g b a))))

(defgeneric ->colour (thing &optional g b a)
  (:documentation "Make an intermediate colour representation.

The input representation can either be a hex code
with either 6 (#ABCDEF) or 8 (#ABCDEF89) digits for the alpha channel,
a vec3 or vec4 with numbers scaled from 0 (no input) to 1 (full input),
or provided directly with four integers either separately or in a list
for values in the red, green, blue and optionally alpha sequence.")
  (:method ((thing vec3) &optional i1 i2 i3)
    (declare (ignore i1 i2 i3))
    (make-instance '3-colour :r (floor (* 255 (vx thing)))
                             :g (floor (* 255 (vy thing)))
                             :b (floor (* 255 (vz thing)))))
  (:method ((thing vec4) &optional i1 i2 i3)
    (declare (ignore i1 i2 i3))
    (make-instance '4-colour :r (floor (* 255 (vx thing)))
                             :g (floor (* 255 (vy thing)))
                             :b (floor (* 255 (vz thing)))
                             :a (floor (* 255 (vw thing)))))
  (:method ((thing list) &optional i1 i2 i3)
    (declare (ignore i1 i2 i3))
    (destructuring-bind (r g b &optional (a nil a-supplied-p)) thing
      (if a-supplied-p
          (make-instance '4-colour :r r :g g :b b :a a)
          (make-instance '3-colour :r r :g g :b b))))
  (:method ((r number) &optional (g 0) (b 0) (a 255 alpha-supplied-p))
    (if alpha-supplied-p
        (make-instance '4-colour :r r :g g :b b :a a)
        (make-instance '3-colour :r r :g g :b b)))
  (:method ((colour-code string) &optional i1 i2 i3)
    (declare (ignore i1 i2 i3))
    (setf colour-code (delete-if-not #'alphanumericp colour-code))
    (ecase (length colour-code)
      (6 (make-instance
          '3-colour
          :r (parse-integer colour-code :start 0 :end 2 :radix 16)
          :g (parse-integer colour-code :start 2 :end 4 :radix 16)
          :b (parse-integer colour-code :start 4 :end 6 :radix 16)))
      (8 (make-instance
          '4-colour
          :r (parse-integer colour-code :start 0 :end 2 :radix 16)
          :g (parse-integer colour-code :start 2 :end 4 :radix 16)
          :b (parse-integer colour-code :start 4 :end 6 :radix 16)
          :a (parse-integer colour-code :start 6 :end 8 :radix 16))))))

(defgeneric ->qcolor (thing)
  (:documentation "Create a qcolor from the intermediate colour representation.")
  (:method ((thing 3-colour))
    (q+:make-qcolor (floor (slot-value thing 'r))
                    (floor (slot-value thing 'g))
                    (floor (slot-value thing 'b))))
  (:method ((thing 4-colour))
    (q+:make-qcolor (floor (slot-value thing 'r))
                    (floor (slot-value thing 'g))
                    (floor (slot-value thing 'b))
                    (floor (slot-value thing 'a)))))

;;; colour palette stuff
(defclass colour-palette ()
  ((palette :initform (make-hash-table))
   (default :initform (->colour 255 0 255)
            :accessor default-colour))
  (:documentation "A colour palette for named colours."))

(defmethod print-object ((object colour-palette) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream ":LENGTH ~d :DEFAULT ~a"
            (hash-table-count (slot-value object 'palette))
            (default-colour object))))

(defgeneric get-colour (palette name)
  (:method ((palette colour-palette) name)
    (gethash name (slot-value palette 'palette)
             (default-colour palette))))

(defgeneric (setf get-colour) (value palette name)
  (:method (value (palette colour-palette) name)
    (setf (gethash name (slot-value palette 'palette)) value)))

(defgeneric delete-colour (palette name)
  (:method ((palette colour-palette) name)
    (remhash name (gethash name (slot-value palette 'palette)))))

(defmacro define-palette (&body args)
  (let ((palette (gensym "PALETTE")))
   `(let ((,palette (make-instance 'colour-palette)))
      (prog1 ,palette
        ,@(loop for (name . colour-arguments) in args
                collect (case name
                          ((t)
                           `(setf (default-colour ,palette)
                                      (->colour ,@colour-arguments)))
                          ((nil)
                           `(setf (default-colour ,palette) nil))
                          (t
                           `(setf (get-colour ,palette ',name)
                                    (->colour ,@colour-arguments)))))))))

(defvar *palette*
  (define-palette
    (:foreground 240 240 15)
    (:background 0 10 25))
  "The current palette. Can be rebound to ")

(defparameter *background-colour* (->colour 0 10 25)
  "The background colour for the stage.")
(defparameter *text-colour* (->colour 240 240 15)
  "The foreground colour for the stage.")
