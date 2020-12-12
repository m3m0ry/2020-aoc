(defpackage :aoc-2020-day-10
  (:use :cl))

(in-package :aoc-2020-day-10)

(defun read-input (filename)
  (with-open-file (stream filename :direction :input)
    (loop for line = (read-line stream nil nil)
          while (not (null line))
          collect (parse-integer line))))

(defun count-all-adapters (input)
  (let ((jolts (mapcar #'- (cdr input) input)))
    (* (count 3 jolts) (count 1 jolts))))

(defun solve1 ()
  (let ((input (sort (read-input "input") #'<)))
  (count-all-adapters
   (append '(0)
           input
           (list (+ 3 (car (last input))))))))

(defun count-all-possibilities (numbers)
  (let ((paths (make-array (length numbers) :initial-element 0)))
    (setf (aref paths 0) 1)
    (loop for number in numbers
          for i from 0
          do
             (loop for delta in '(1 2 3)
                   for prev = (- i delta)
                   when (and (>= prev 0)
                             (<= (- number (nth prev numbers)) 3))
                     do
                        (incf (aref paths i) (aref paths prev))))
    (aref paths (1- (length numbers)))))

(defun solve2 ()
  (let ((input (sort (read-input "input") #'<)))
    (setf input
          (append '(0)
                  input
                  (list (+ 3 (car (last input))))))
    (count-all-possibilities input)))

