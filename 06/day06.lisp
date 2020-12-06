(defpackage :aoc-2020-day-6
  (:use :cl))

(in-package :aoc-2020-day-6)

(defun string-to-set (string)
  (remove-duplicates (coerce string 'list))


(defun read-input1 (filename)
  (with-open-file (stream filename :direction :input)
    (mapcar
      (lambda (x) (reduce #'union x))
      (split-sequence:split-sequence
      '()
      (loop for line = (read-line stream nil nil)
            while (not (null line))
            collect
            (string-to-set line))))))


(defun solve1 ()
  (reduce #'+ (mapcar #'length (read-input1 "input"))))


(defun read-input2 (filename)
  (with-open-file (stream filename :direction :input)
    (loop for line = (read-line stream nil nil)
          with current-group = '()
          while (not (null line))
          if (string= "" line)
            collect
            (prog1
                (reduce #'intersection current-group)
              (setf current-group '()))
          else
            do (push (string-to-set line) current-group))))

(defun solve2 ()
  (reduce #'+ (mapcar #'length (read-input2 "input"))))
