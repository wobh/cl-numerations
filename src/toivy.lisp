(defpackage #:toivy
  (:use #:cl)
  (:export #:digit-toivy-p #:logbittoivy
           #:parse-toi #:parse-iot
           #:write-toi #:write-iot)
  (:documentation "Functions to write and parse ternary numerations where:

- t = -1
- o =  0
- i =  1
- v =  2
- y =  3

Numerations include:

`toi': balanced ternary, least significant trit order

`iot': balanced ternary, most significant trit order

`oiv': ternary, least significant trit order

`vio': ternary, most significant trit order

`ivy': bijective ternary, least significant trit order

`yvi': bijective ternary, most significant trit order"))

(in-package #:toivy)

(defun digit-toi-p (character &optional)
  "Identify digits in `toi' or `iot' numerations"
  (let ((index (position character "toi" :test #'char-equal)))
    (when index
      (1- index))))

(setf (fdefinition 'digit-iot-p) #'digit-toi-p)

(defun parse-toi (toi-string &key (start 0) end)
  "Parses `toi' numerals, a balanced ternary, LST ordered numeration."
  (flet ((ternary-foldr (next-trit total)
           (unless (or (null total) (null next-trit))
             (+ next-trit total total total))))
    (reduce #'ternary-foldr toi-string
            :key #'digit-toi-p
            :initial-value 0
            :start start
            :end end
            :from-end t)))

(defun parse-iot (iot-string &key (start 0) end)
  "Parses `iot' numerals, a balanced ternary, MST ordered numeration."
  (flet ((ternary-foldl (total next-trit)
           (unless (or (null total) (null next-trit))
             (+ next-trit total total total))))
    (reduce #'ternary-foldl iot-string
            :key #'digit-iot-p
            :initial-value 0
            :start start
            :end end)))


(assert (= -1 (digit-toi-p #\t) (digit-iot-p #\T)))
(assert (=  0 (digit-toi-p #\o) (digit-iot-p #\O)))
(assert (=  1 (digit-toi-p #\i) (digit-iot-p #\I)))

(assert (null (digit-toi-p #\r)))
(assert (null (digit-toi-p #\3)))

(assert (=  0 (parse-toi "o") (parse-iot "o")))
(assert (=  1 (parse-toi "i") (parse-iot "i")))
(assert (= -1 (parse-toi "t") (parse-iot "t")))

(assert (=  0 (parse-toi "oo") (parse-iot "oo")))
(assert (=  1 (parse-toi "io") (parse-iot "oi")))
(assert (= -1 (parse-toi "to") (parse-iot "ot")))
(assert (=  2 (parse-toi "ti") (parse-iot "it")))
(assert (= -2 (parse-toi "it") (parse-iot "ti")))
(assert (=  3 (parse-toi "oi") (parse-iot "io")))
(assert (= -3 (parse-toi "ot") (parse-iot "to")))
(assert (=  4 (parse-toi "ii") (parse-iot "ii")))
(assert (= -4 (parse-toi "tt") (parse-iot "tt")))

(assert (=   0 (parse-toi "ooo") (parse-iot "ooo")))
(assert (=   1 (parse-toi "ioo") (parse-iot "ooi")))
(assert (=  -1 (parse-toi "too") (parse-iot "oot")))
(assert (=   2 (parse-toi "tio") (parse-iot "oit")))
(assert (=  -2 (parse-toi "ito") (parse-iot "oti")))
(assert (=   3 (parse-toi "oio") (parse-iot "oio")))
(assert (=  -3 (parse-toi "oto") (parse-iot "oto")))
(assert (=   4 (parse-toi "iio") (parse-iot "oii")))
(assert (=  -4 (parse-toi "tto") (parse-iot "ott")))
(assert (=   5 (parse-toi "tti") (parse-iot "itt")))
(assert (=  -5 (parse-toi "iit") (parse-iot "tii")))
(assert (=   6 (parse-toi "oti") (parse-iot "ito")))
(assert (=  -6 (parse-toi "oit") (parse-iot "tio")))
(assert (=   7 (parse-toi "iti") (parse-iot "iti")))
(assert (=  -7 (parse-toi "tit") (parse-iot "tit")))
(assert (=   8 (parse-toi "toi") (parse-iot "iot")))
(assert (=  -8 (parse-toi "iot") (parse-iot "toi")))
(assert (=   9 (parse-toi "ooi") (parse-iot "ioo")))
(assert (=  -9 (parse-toi "oot") (parse-iot "too")))
(assert (=  10 (parse-toi "ioi") (parse-iot "ioi")))
(assert (= -10 (parse-toi "tot") (parse-iot "tot")))
(assert (=  11 (parse-toi "tii") (parse-iot "iit")))
(assert (= -11 (parse-toi "itt") (parse-iot "tti")))
(assert (=  12 (parse-toi "oii") (parse-iot "iio")))
(assert (= -12 (parse-toi "ott") (parse-iot "tto")))
(assert (=  13 (parse-toi "iii") (parse-iot "iii")))
(assert (= -13 (parse-toi "ttt") (parse-iot "ttt")))

(defun digit-oiv-p (character &optional)
  "Identify digits in `oiv' or `vio' numerations"
  (position character "oiv" :test #'char-equal))

(setf (fdefinition 'digit-vio-p) #'digit-voi-p)

(defun parse-oiv (vio-string &key (start 0) end)
  "Parses `oiv' numerals, a balanced ternary, LST ordered numeration."
  (flet ((ternary-foldr (next-trit total)
           (unless (or (null total) (null next-trit))
             (+ next-trit total total total))))
    (reduce #'ternary-foldr oiv-string
            :key #'digit-oiv-p
            :initial-value 0
            :start start
            :end end
            :from-end t)))

(defun parse-vio (vio-string &key (start 0) end)
  "Parses `vio' numerals, a balanced ternary, MST ordered numeration."
  (flet ((ternary-foldl (next-trit total)
           (unless (or (null total) (null next-trit))
             (+ next-trit total total total))))
    (reduce #'ternary-foldl vio-string
            :key #'digit-vio-p
            :initial-value 0
            :start start
            :end end)))
