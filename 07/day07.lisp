(defpackage :aoc-2020-day-7
  (:use :cl))

(in-package :aoc-2020-day-7)



(defun read-input (filename)
  (with-open-file (stream filename :direction :input)
    (loop for line = (read-line stream nil nil)
          while (not (null line))
          ;Stupid cl-ppcre can't handle this regex :(
          ;with regex = (ppcre:create-scanner "([a-z]+ [a-z]+) bags contain (no other|\\d [a-z]+ [a-z]+) bags?(?|, (\\d [a-z]+ [a-z]+) bags?)?(?|, (\\d [a-z]+ [a-z]+) bags?)?(?|, (\\d [a-z]+ [a-z]+) bags?)?.")
          with regex-line = (ppcre:create-scanner "([a-z]+ [a-z]+) bags contain (.+)")
          with regex-bag = (ppcre:create-scanner "(\\d) ([a-z]+ [a-z]+) bags?,?.?")
          collect
          (ppcre:register-groups-bind (bag contains)
              (regex-line line)
            (list bag
            (loop for s in (split-sequence:split-sequence #\, contains :remove-empty-subseqs t)
                  collect
                  (ppcre:register-groups-bind (number type)
                      (regex-bag s)
                    (list (parse-integer number) type))))))))

(defun create-hash1 (input)
  (let ((hash (make-hash-table :test 'equal)))
    (loop for (source target) in input
          do
             (loop for bag in target
                   do
                      (push source (gethash (second bag) hash))))
    hash))

(defun find-possibilities (hash target)
  (loop for bag = target then (pop targets)
        while (not (null bag))
        with targets = '()
        with options = '()
        do
           (loop for option in (gethash bag hash)
                 unless (member option options :test #'equal)
                   do
                      (pushnew option options)
                   and do
                     (push option targets)
                 end)
        finally (return options)))

(defun solve1 ()
  (length (find-possibilities (create-hash1 (read-input "input")) "shiny gold")))


(defun create-hash2 (input)
  (let ((hash (make-hash-table :test 'equal)))
    (loop for (source target) in input
          do
             (mapc (lambda (x) (push x (gethash source hash)))
                   target))
    hash))

(defun count-bags (hash target)
  (reduce #'+
          (mapcar
           (lambda (x)
             (if (null x)
                 0
                 (+ (* (first x) (count-bags hash (second x)))
                    (first x))))
           (gethash target hash))))


(defun solve2 ()
  (count-bags (create-hash2 (read-input "input")) "shiny gold"))

