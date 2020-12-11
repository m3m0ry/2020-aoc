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

(defun count-possibility (a b c)
  (if (>= 3 (- b a))
      (if (>= 3 (- c a))
          1
          2)
      0))

(defun count-all-possibilities (input)
  (count-if (lambda (x) (not (null x))) (mapcar #'count-possibility input (cdr input) (cddr input))))

(defun solve2 ()
  (let ((input (sort (read-input "test-input") #'<)))
    (count-all-possibilities
     (append '(0)
             input
             (list (+ 3 (car (last input))))))))

