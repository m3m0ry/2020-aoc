
(defpackage :aoc-2020-day-2
  (:use :cl))

(in-package :aoc-2020-day-2)

(defstruct password
  min
  max
  letter
  password)

(defun read-input (filename)
  (with-open-file (stream filename :direction :input)
    (loop :for line = (read-line stream nil nil)
          :with regex = (ppcre:create-scanner "(\\d+)-(\\d+)\\s+(\\w):\\s+(\\w+)")
          :while (not (null line))
          :collect
          (ppcre:register-groups-bind (min max letter password)
              (regex line )
            (make-password :min (parse-integer min)
                           :max (parse-integer max)
                           :letter (char letter 0)
                           :password password)))))

(defun valid-password (password)
  (let ((count (loop :for c :across (password-password password)
                     :counting (char= c (password-letter password)))))
    (and (<= (password-min password) count)
         (>= (password-max password) count))))

(defun xor (a b)
  (or (and a (not b)) (and (not a) b)))

(defun valid-password2 (password)
  (let ((min (password-min password))
        (max (password-max password))
        (pass (password-password password))
        (letter (password-letter password)))
  (xor (char= letter (char pass (1- min)))
       (char= letter (char pass (1- max))))))

(defun amount-valid (passwords &optional (predicate #'valid-password))
  (loop :for password :in passwords
        :counting (funcall predicate password)))

(defun solve01 ()
  (amount-valid (read-input "input")))

(defun solve02 ()
  (amount-valid (read-input "input") #'valid-password2))
