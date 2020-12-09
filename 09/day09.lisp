(defpackage :aoc-2020-day-9
  (:use :cl))

(in-package :aoc-2020-day-9)

(defun read-input (filename)
  (with-open-file (stream filename :direction :input)
    (loop for line = (read-line stream nil nil)
          while (not (null line))
          collect (parse-integer line))))


(defun get-preamble (numbers length &optional (preamble '()))
  (dotimes (i (1+ (- (length preamble) length)))
    (pop preamble))
  (setf preamble (nreverse preamble))
  (dotimes (i (- length (length preamble)))
    (push (pop numbers) preamble))
  (setf preamble (nreverse preamble))
  (values preamble numbers))


(defun get-factor (number preamble)
  (alexandria:map-combinations
   (lambda (x) (when (= (+ (first x) (second x)) number)
                 (return-from get-factor t)))
   preamble
   :length 2)
  nil)

(defun find-number (numbers length)
  (multiple-value-bind (preamble numbers)
      (get-preamble numbers length)
    (loop for number in numbers
          unless (get-factor number preamble)
            return number
          do
            (setf (values preamble numbers) (get-preamble numbers length preamble)))))

(defun solve1 ()
  (find-number (read-input "input") 25))

(defun find-sublist (number input)
  (loop for i from 0 below (length input)
        do
           (loop for j upfrom (1+ i) below (length input)
                 for sub = (subseq input i j)
                 when (> (apply #'+ sub) number)
                   return nil
                 when (= (apply #'+ sub) number)
                   do
                      (return-from find-sublist sub))))

(defun solve2 ()
  (let* ((input (read-input "input"))
         ( sublist (find-sublist (find-number input 25) input)))
    (+ (apply #'max sublist) (apply #'min sublist))))

