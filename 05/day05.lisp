(defpackage :aoc-2020-day-5
  (:use :cl))

(in-package :aoc-2020-day-5)


(defun bisect (low high lower)
  (if lower
      (values low (- high (/ (- high low) 2)))
      (values (+ (/ (- high low) 2) low) high)))


(defun read-input (filename)
  (with-open-file (stream filename :direction :input)
    (loop for line = (read-line stream nil nil)
          with seats = (make-array '(128 8) :element-type 'boolean :initial-element nil)
          with min-row = 0
          with max-row = (first (array-dimensions seats))
          with min-column = 0
          with max-column = (second (array-dimensions seats))
          while (not (null line))
          do
             (progn
               (dotimes (i 7)
                 (setf (values min-row max-row)
                       (bisect min-row max-row (char= #\F (char line i)))))
               (dotimes (i 3)
                 (setf (values min-column max-column)
                       (bisect min-column max-column (char= #\L (char line (+ 7 i))))))
               (setf (aref seats min-row min-column) t)
               (setf min-row 0
                     max-row (first (array-dimensions seats))
                     min-column 0
                     max-column (second (array-dimensions seats))))
          finally (return seats))))


(defun solve1 ()
  (let ((seats (read-input "input")))
    (loop for i from (1- (array-total-size seats)) downto 0
          when (row-major-aref seats i)
            return i)))


(defun solve2 ()
  (let ((seats (read-input "input")))
    (print seats)
    (loop for i from 1 upto (array-total-size seats)
          when (and (row-major-aref seats (1- i))
                    (row-major-aref seats (1+ i))
                    (null (row-major-aref seats i)))
            return i)))
