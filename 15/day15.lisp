(defpackage :aoc-2020-day-15
  (:use :cl))

(in-package :aoc-2020-day-15)

(defun game (start turns)
  (let ((map (make-hash-table)))
    (loop for n in start
          for i from 1 below (length start)
          do
             (setf (gethash n map) i))
    (loop for turn from (+ 2 (hash-table-count map))
          with number = (car (last start))
          for pos = (gethash number map)
          for last-number = number
          if (null pos)
            do
               (setf number 0)
          else
            do
               (setf number (- (1- turn) pos))
          end
          do
             (setf (gethash last-number map) (1- turn)
                   last-number number)
          when (>= turn turns)
            do
               (return last-number))))

(defun solve1 ()
  (game '(6 19 0 5 7 13 1) 2020))

(defun solve2 ()
  (game '(6 19 0 5 7 13 1) 30000000))
