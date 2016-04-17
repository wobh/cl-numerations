(defpackage #:toivy
  (:use #:cl)
  (:export #:digit-toi-p #:digit-iot-p
           #:parse-toi #:parse-iot
           #:logtrit-toi #:logtrit-iot
           #:write-toi #:write-iot)
  (:documentation "Functions to write and parse ternary numerations where:

- t = -1
- o =  0
- i =  1
- v =  2
- y =  3

Numerations include:

`oiv': ternary, least significant trit order

`vio': ternary, most significant trit order

`toi': balanced ternary, least significant trit order

`iot': balanced ternary, most significant trit order

`ivy': bijective ternary, least significant trit order

`yvi': bijective ternary, most significant trit order"))

(in-package #:toivy)


;;; Ternary

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

(defun logtrit-oiv (index integer)
  "Return `oiv' or `vio' trit char at index position of integer."
  (char "oiv" (mod (floor integer (expt 3 index)) 3)))

(setf (fdefinition 'logtrit-vio) #'logtrit-oiv)

(defun ternary-length (integer)
  "Return length of integer in ternary."
  (if (zerop integer)
      1
      (1+ (truncate (log integer 3)))))

(defun write-oiv (pos-integer &key (out-stream *standard-output*))
  "Writes number as `oiv', a ternary LST ordered numeration: o = 0, i = 1, v = 2."
  (declare (type (integer 0) pos-integer))
  (with-output-to-string (ret-stream)
    (with-open-stream (bcast (make-broadcast-stream ret-stream out-stream))
      (dotimes (index (ternary-length pos-integer))
        (princ (logtrit-oiv index pos-integer) bcast)))))

(defun write-vio (pos-integer &key (out-stream *standard-output*))
  "Writes number as `vio', a ternary MST ordered numeration: o = 0, i = 1, v = 2."
  (declare (type (integer 0) pos-integer))
  (with-output-to-string (ret-stream)
    (with-open-stream (bcast (make-broadcast-stream ret-stream out-stream))
      (loop
         for index from (1- (ternary-length pos-integer)) downto 0
         do (princ (logtrit-vio index pos-integer) bcast)))))

;;; Ternary tests

(assert (= 0 (digit-oiv-p #\o) (digit-vio-p #\O)))
(assert (= 1 (digit-oiv-p #\i) (digit-vio-p #\I)))
(assert (= 2 (digit-oiv-p #\v) (digit-vio-p #\V)))

(assert (null (digit-oiv-p #\r)))
(assert (null (digit-vio-p #\3)))

(assert (= 0 (parse-oiv "o") (parse-vio "o")))
(assert (= 1 (parse-oiv "i") (parse-vio "i")))
(assert (= 2 (parse-oiv "v") (parse-vio "v")))

(assert (= 0 (parse-oiv "oo") (parse-vio "oo")))
(assert (= 1 (parse-oiv "io") (parse-vio "oi")))
(assert (= 2 (parse-oiv "vo") (parse-vio "ov")))
(assert (= 3 (parse-oiv "oi") (parse-vio "io")))
(assert (= 4 (parse-oiv "ii") (Parse-vio "Ii")))
(assert (= 5 (parse-oiv "vi") (parse-vio "iv")))
(assert (= 6 (parse-oiv "ov") (parse-vio "vo")))
(assert (= 7 (parse-oiv "iv") (parse-vio "vi")))
(assert (= 8 (parse-oiv "vv") (parse-vio "vv")))

(assert (=  0 (parse-oiv "ooo") (parse-vio "ooo")))
(assert (=  1 (parse-oiv "ioo") (parse-vio "ooi")))
(assert (=  2 (parse-oiv "too") (parse-vio "oot")))
(assert (=  3 (parse-oiv "tio") (parse-vio "oit")))
(assert (=  4 (parse-oiv "ito") (parse-vio "oti")))
(assert (=  5 (parse-oiv "oio") (parse-vio "oio")))
(assert (=  6 (parse-oiv "oto") (parse-vio "oto")))
(assert (=  7 (parse-oiv "iio") (parse-vio "oii")))
(assert (=  8 (parse-oiv "tto") (parse-vio "ott")))
(assert (=  9 (parse-oiv "tti") (parse-vio "itt")))
(assert (= 10 (parse-oiv "iit") (parse-vio "tii")))
(assert (= 11 (parse-oiv "oti") (parse-vio "ito")))
(assert (= 12 (parse-oiv "oit") (parse-vio "tio")))
(assert (= 13 (parse-oiv "iti") (parse-vio "iti")))
(assert (= 14 (parse-oiv "tit") (parse-vio "tit")))
(assert (= 15 (parse-oiv "toi") (parse-vio "iot")))
(assert (= 16 (parse-oiv "iot") (parse-vio "toi")))
(assert (= 17 (parse-oiv "ooi") (parse-vio "ioo")))
(assert (= 18 (parse-oiv "oot") (parse-vio "too")))
(assert (= 19 (parse-oiv "ioi") (parse-vio "ioi")))
(assert (= 20 (parse-oiv "tot") (parse-vio "tot")))
(assert (= 21 (parse-oiv "tii") (parse-vio "iit")))
(assert (= 22 (parse-oiv "itt") (parse-vio "tti")))
(assert (= 23 (parse-oiv "oii") (parse-vio "iio")))
(assert (= 24 (parse-oiv "ott") (parse-vio "tto")))
(assert (= 25 (parse-oiv "iii") (parse-vio "iii")))
(assert (= 26 (parse-oiv "ttt") (parse-vio "ttt")))


;;; Balanced Ternary (WIP)

(defun digit-toi-p (character)
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

(defun logtrit-toi (index integer)
  "Return `toi' or `iot' trit char at index position of integer."
  (char "toi" (mod (floor integer (expt 3 index)) 3)))


(defun write-toi (pos-integer &key (out-stream *standard-output*))
  "Writes number as `toi', a ternary LST ordered numeration: o = 0, i = 1, t = -1."
  (declare (type (integer 0) pos-integer))
  (with-output-to-stream (ret-stream)
    (with-open-stream (bcast (make-broadcast-stream ret-stream out-stream))
      (dotimes (index (ternary-length pos-integer))))))

;;; Balanced ternary tests

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


;;; Bijective ternary (TODO)

(defun digit-ivy-p ())
(defun digit-yvi-p ())
(defun parse-ivy ())
(defun parse-yvi ())
(defun write-ivy ())
(defun write-yvi ())

;;; Bijective ternary tests

(assert (= 1 (digit-ivy-p #\i) (digit-yvi-p #\I)))
(assert (= 2 (digit-ivy-p #\v) (digit-yvi-p #\V)))
(assert (= 0 (digit-ivy-p #\y) (digit-yvi-p #\Y)))

(assert (null (digit-ivy-p #\r)))
(assert (null (digit-yvi-p #\3)))

(assert (=  1 (parse-ivy "i") (parse-yvi "i")))
(assert (=  2 (parse-ivy "v") (parse-yvi "v")))
(assert (=  3 (parse-ivy "y") (parse-yvi "y")))

(assert (=  4 (parse-ivy "ii") (parse-yvi "ii")))
(assert (=  5 (parse-ivy "vi") (parse-yvi "iv")))
(assert (=  6 (parse-ivy "yi") (parse-yvi "iy")))
(assert (=  7 (parse-ivy "iv") (parse-yvi "vi")))
(assert (=  8 (parse-ivy "vv") (parse-yvi "vv")))
(assert (=  9 (parse-ivy "yv") (parse-yvi "vy")))
(assert (= 10 (parse-ivy "iy") (parse-yvi "yi")))
(assert (= 11 (parse-ivy "vy") (parse-yvi "yv")))
(assert (= 12 (parse-ivy "yy") (parse-yvi "yy")))

(assert (= 13 (parse-ivy "iii") (parse-yvi "iii")))
(assert (= 14 (parse-ivy "vii") (parse-yvi "iiv")))
(assert (= 15 (parse-ivy "yii") (parse-yvi "iiy")))
(assert (= 16 (parse-ivy "ivi") (parse-yvi "ivi")))
(assert (= 17 (parse-ivy "vvi") (parse-yvi "ivv")))
(assert (= 18 (parse-ivy "yvi") (parse-yvi "ivy")))
(assert (= 19 (parse-ivy "iyi") (parse-yvi "iyi")))
(assert (= 20 (parse-ivy "vyi") (parse-yvi "iyv")))
(assert (= 21 (parse-ivy "yyi") (parse-yvi "iyy")))
(assert (= 22 (parse-ivy "iiv") (parse-yvi "vii")))
(assert (= 23 (parse-ivy "viv") (parse-yvi "viv")))
(assert (= 24 (parse-ivy "yiv") (parse-yvi "viy")))
(assert (= 25 (parse-ivy "ivv") (parse-yvi "vvi")))
(assert (= 26 (parse-ivy "vvv") (parse-yvi "vvv")))
(assert (= 27 (parse-ivy "yvv") (parse-yvi "vvy")))
(assert (= 28 (parse-ivy "iyv") (parse-yvi "vyi")))
(assert (= 29 (parse-ivy "vyv") (parse-yvi "vyv")))
(assert (= 30 (parse-ivy "yyv") (parse-yvi "vyy")))
(assert (= 31 (parse-ivy "iiy") (parse-yvi "yii")))
(assert (= 32 (parse-ivy "viy") (parse-yvi "yiv")))
(assert (= 33 (parse-ivy "yiy") (parse-yvi "yiy")))
(assert (= 34 (parse-ivy "ivy") (parse-yvi "yvi")))
(assert (= 35 (parse-ivy "vvy") (parse-yvi "yvv")))
(assert (= 36 (parse-ivy "yvy") (parse-yvi "yvy")))
(assert (= 37 (parse-ivy "iyy") (parse-yvi "yyi")))
(assert (= 38 (parse-ivy "vyy") (parse-yvi "yyv")))
(assert (= 39 (parse-ivy "yyy") (parse-yvi "yyy")))
