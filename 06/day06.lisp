(defpackage :aoc-2020-day-6
  (:use :cl))

(in-package :aoc-2020-day-6)

(defun string-to-set (string)
  (let ((set '()))
    (map 'list (lambda (x) (setf set (adjoin x set))) string)
    set))


(defun read-input (filename)
  (with-open-file (stream filename :direction :input)
    (loop for line = (read-line stream nil nil)
          with current-group = ""
          while (not (null line))
          if (string= "" line)
            collect
            (prog1
                (string-to-set current-group)
              (setf current-group ""))
          else
            do (setf current-group (concatenate 'string current-group line)))))


(defun solve1 ()
  (reduce #'+ (mapcar #'length (read-input "input"))))


(defun group-to-set (persons)
  (loop for person in persons
        with group = (first persons)
        do
           (setf group (intersection group person))
        finally (return group)))


(defun read-input2 (filename)
  (with-open-file (stream filename :direction :input)
    (loop for line = (read-line stream nil nil)
          with current-group = '()
          while (not (null line))
          if (string= "" line)
            collect
            (prog1
                (group-to-set current-group)
              (setf current-group '()))
          else
            do (push (string-to-set line) current-group))))

(defun solve2 ()
  (reduce #'+ (mapcar #'length (read-input2 "input"))))
