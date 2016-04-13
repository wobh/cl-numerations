(defpackage #:toi-iot
  (:use #:cl)
  (:export #:digit-toi-iot-p #:logbittoi
           #:parse-toi #:parse-iot
           #:write-toi #:write-iot)
  (:documentation "Functions to write and parse balance ternary numbers where:

- t = -1
- o =  0
- i =  1

`toi': least significant trit order

`iot': most significant trit order"))

(in-package #:toi-iot)

(defun digit-toi-iot-p (character)
  "Identify digits in `toi-iot' numerals"
  (cond ((char-equal #\t character) -1)
        ((char-equal #\o character)  0)
        ((char-equal #\i character)  1)))

(defun parse-toi (toi-string &key (start 0) end)
  "Parses `toi' numerals, a balanced ternary, LST ordered numeration."
  (flet ((ternary-foldr (next-trit total)
           (unless (or (null total) (null next-trit))
             ())))))
