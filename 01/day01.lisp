(defpackage :aoc-2020-day-1
  (:use :cl))

(in-package :aoc-2020-day-1)

(defun read-input (filename)
  (with-open-file (stream filename :direction :input)
    (loop for line = (read-line stream nil nil)
          while (and line (find-if #'digit-char-p line))
          collect
          (parse-integer line))))


(defun find-2020 (numbers length)
  (alexandria:map-combinations
   (lambda (x) (when (= 2020 (apply #'+ x)) (return-from find-2020 x)))
   numbers
   :length length))


(defun solve1 ()
  (apply #'* (find-2020 (read-input "input01") 2)))

(defun solve2 ()
  (apply #'* (find-2020 (read-input "input01") 3)))
