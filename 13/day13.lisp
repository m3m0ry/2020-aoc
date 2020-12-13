(defpackage :aoc-2020-day-13
  (:use #:cl))

(in-package :aoc-2020-day-13)


(defun read-input (filename)
  (with-open-file (stream filename :direction :input)
    (let ((timestamp (parse-integer (read-line stream nil nil))))
     (loop for id in (split-sequence:split-sequence #\, (read-line stream nil nil))
           for i from 0
           when (string/= id "x")
             collect (parse-integer id) into ids
             and
               collect i into offsets
           finally (return (values timestamp ids offsets))))))

(defun find-timestamp (timestamp ids)
  (loop for time from timestamp
        for i from 0
        do
           (loop for id in ids
                 when (= 0 (mod time id))
                   do (return-from find-timestamp (* i id)))))

(defun solve1 ()
  (multiple-value-bind (timestamp ids offsets)
      (read-input "input")
    (find-timestamp timestamp ids)))

(defun egcd (a b)
  (if (= 0 a)
      (values b 0 1)
      (multiple-value-bind (g x y)
          (egcd (mod b a) a)
        (values g (- y (* (floor (/ b a)) x)) x))))

(defun invmod (a m)
  (multiple-value-bind (r s k) (egcd a m)
    (unless (= 1 r) (error "invmod: Values ~a and ~a are not coprimes." a m))
    s))

(defun find-singularity (nums rems)
  (loop for num in nums
        for rem in rems
        with prod = (apply #'* nums)
        for pp = (/ prod num)
        sum
          (* (- num rem) (invmod pp num) pp) into result
        finally (return (mod result prod))))


(defun solve2 ()
  (multiple-value-bind (timestamp ids offsets)
      (read-input "input")
    (find-singularity ids offsets)))
      (chinese-remainder offsets ids)))
