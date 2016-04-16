(defpackage #:oxxo
  (:use #:cl)
  (:export #:digit-oxxo-p #:logbitox
           #:parse-ox #:parse-xo
           #:write-ox #:write-xo)
  (:documentation "Functions to write and parse binary numbers where o = 0, x = 1.

`ox': least-significant-bit order 

`xo': most-significant-bit order"))

(in-package #:oxxo)

(defun digit-oxxo-p (character)
  "Identify digits in binary numeration: o = 0, x = 1."
  (position character "ox" :test #'char-equal))

(defun parse-ox (ox-string &key (start 0) end)
  "Parses `ox' numerals, a binary, LSB ordered numeration: o = 0, x = 1."
  (declare (type string ox-string))
  ;; TODO: Mimic behavior of `parse-integer' as much as possible.
  (flet ((binary-foldr (next-bit total)
           (unless (or (null total) (null next-bit))
             (+ total total next-bit))))
    (reduce #'binary-foldr ox-string
            :key #'digit-oxxo-p
            :initial-value 0
            :start start
            :end end
            :from-end t)))

(defun parse-xo (xo-string &key (start 0) end)
  "Parses `xo' numerals, a binary, MSB ordered numeration: o = 0, x = 1."
  (declare (type string xo-string))
  ;; TODO: Mimic behavior of `parse-integer' as much as possible.
  (flet ((binary-foldl (total next-bit)
           (unless (or (null total) (null next-bit))
             (+ total total next-bit))))
    (reduce #'binary-foldl xo-string
            :key #'digit-oxxo-p
            :initial-value 0
            :start start
            :end end)))

(defun logbitox (index integer)
  "Returns `ox' bit char at index position of integer."
  (char "ox" (ldb (byte 1 index) integer)))

(defun write-ox (pos-integer &key (out-stream *standard-output*))
  "Writes number as `ox', a binary LSB ordered numeration: o = 0, x = 1."
  (declare (type (integer 0) pos-integer))
  (with-output-to-string (ret-stream)
    (with-open-stream (bcast (make-broadcast-stream ret-stream out-stream))
      (dotimes (index (integer-length pos-integer))
        (princ (logbitox index pos-integer) bcast)))))

(defun write-xo (pos-integer &key (out-stream *standard-output*))
  "Writes number as `xo', a binary MSB ordered numeration: o = 0, x = 1."
  (declare (type (integer 0) pos-integer))
  (with-output-to-string (ret-stream)
    (with-open-stream (bcast (make-broadcast-stream ret-stream out-stream))
      (loop
         for index from (1- (integer-length pos-integer)) downto 0
         do (princ (logbitox index pos-integer) bcast)))))

#+oxxo-test
(labels ((string-maybe= (string1 string2)
           (unless (null string1)
             (string= string1 string2)))
         (strings= (string1 string2 &rest strings)
           (reduce #'string-maybe= (list* string1 string2 strings))))
  (assert (zerop (digit-oxxo-p #\o)))
  (assert (zerop (digit-oxxo-p #\O)))

  (assert (= 1 (digit-oxxo-p #\x)))
  (assert (= 1 (digit-oxxo-p #\X)))

  (assert (null (digit-oxxo-p #\r)))
  (assert (null (digit-oxxo-p #\R)))

  (assert (null (parse-ox "oxen")))
  (assert (null (parse-ox "epoxy")))
  (assert (null (parse-ox "orthodox")))

  (assert (null (parse-xo "oxen")))
  (assert (null (parse-xo "epoxy")))
  (assert (null (parse-xo "orthodox")))

  (assert (= 0 (parse-ox "o") (parse-xo "o")))
  (assert (= 1 (parse-ox "x") (parse-xo "x")))

  (assert (= 0 (parse-ox "oo") (parse-xo "oo")))
  (assert (= 1 (parse-ox "xo") (parse-xo "ox")))
  (assert (= 2 (parse-ox "ox") (parse-xo "xo")))
  (assert (= 3 (parse-ox "xx") (parse-xo "xx")))

  (assert (= 0 (parse-ox "ooo") (parse-xo "ooo")))
  (assert (= 1 (parse-ox "xoo") (parse-xo "oox")))
  (assert (= 2 (parse-ox "oxo") (parse-xo "oxo")))
  (assert (= 3 (parse-ox "xxo") (parse-xo "oxx")))
  (assert (= 4 (parse-ox "oox") (parse-xo "xoo")))
  (assert (= 5 (parse-ox "xox") (parse-xo "xox")))
  (assert (= 6 (parse-ox "oxx") (parse-xo "xxo")))
  (assert (= 7 (parse-ox "xxx") (parse-xo "xxx")))

  (assert (= 9 (parse-ox "xoox") (parse-xo "xoox")))
  (assert (= 26 (parse-ox "oxoxx") (parse-xo "xxoxo")))
  (assert (= 1128 (parse-ox "oooxoxxooox") (parse-xo "xoooxxoxooo")))

  (assert (strings= #\o (oxbitp 0 2)))
  (assert (strings= #\x (oxbitp 1 2)))
  (assert (strings= #\o (oxbitp 2 2)))

  (assert (strings= #\x (oxbitp 0 3)))
  (assert (strings= #\x (oxbitp 1 3)))
  (assert (strings= #\o (oxbitp 2 3)))

  (assert (strings= "o" (write-xo 0) (write-ox 0)))
  (assert (strings= "x" (write-xo 1) (write-ox 1)))

  (assert (strings= "oo" (write-xo 0) (write-ox 0)))
  (assert (strings= "ox" (write-xo 1) (write-ox 2)))
  (assert (strings= "xo" (write-xo 2) (write-ox 1)))
  (assert (strings= "xx" (write-xo 3) (write-ox 3)))

  (assert (strings= "ooo" (write-xo 0) (write-ox 0)))
  (assert (strings= "oox" (write-xo 1) (write-ox 4)))
  (assert (strings= "oxo" (write-xo 2) (write-ox 2)))
  (assert (strings= "oxx" (write-xo 3) (write-ox 6)))
  (assert (strings= "xoo" (write-xo 4) (write-ox 1)))
  (assert (strings= "xox" (write-xo 5) (write-ox 5)))
  (assert (strings= "xxo" (write-xo 6) (write-ox 3)))
  (assert (strings= "xxx" (write-xo 7) (write-ox 7)))

  (assert (strings= "xoox" (write-xo 9) (write-ox 9)))

  (assert (strings= "xxoxo" (write-xo 26)))
  (assert (strings= "oxoxx" (write-ox 26)))

  (assert (strings= "xoooxxoxooo" (write-xo 1128)))
  (assert (strings= "oooxoxxooox" (write-ox 1128))))
