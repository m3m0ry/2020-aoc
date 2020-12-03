(defpackage :aoc-2020-day-3
  (:use :cl))

(in-package :aoc-2020-day-3)

(defun read-input (filename)
  (with-open-file (stream filename :direction :input)
    (loop :for line = (read-line stream nil nil)
          :while (not (null line))
          :collect
          (loop :for c :across line
                :collect
                (char= c #\#)))))


(defun list-to-array (input)
  (make-array (list (length input)
                    (length (first input)))
              :initial-contents input))


(defun count-trees (trees &optional (i-multiplier 1) (j-multiplier 3))
  (loop :for i :from 0 :below (array-dimension trees 0) :by i-multiplier
        :for j :from 0 :by j-multiplier
        :counting (aref trees i (mod j (array-dimension trees 1)))))


(defun solve01 ()
  (count-trees (list-to-array (read-input "input"))))


(defun solve02 ()
  (let ((slopes '((1 1) (3 1) (5 1) (7 1) (1 2)))
        (trees (list-to-array (read-input "input"))))
    (apply #'* (loop :for slope :in slopes
                     collect
                     (count-trees trees (second slope) (first slope))))))
