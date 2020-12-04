(defpackage :aoc-2020-day-4
  (:use :cl))

(in-package :aoc-2020-day-4)


(defstruct (passport
  (:conc-name pass-))
  byr
  iyr
  eyr hgt
  hcl
  ecl
  pid
  cid)


(defun string-to-password (string)
  (let ((values
          (mapcar (lambda (x) (split-sequence:split-sequence #\: x))
                  (split-sequence:split-sequence #\Space string :remove-empty-subseqs t))))
    (apply #'make-passport
           (mapcan (lambda (x) (list (intern (string-upcase (first x)) :keyword) (second x)))
                   values))))


(defun read-input (filename)
  (with-open-file (stream filename :direction :input)
    (loop for line = (read-line stream nil nil)
          with current-passport = ""
          while (not (null line))
          if (string= "" line)
            collect (prog1
                        (string-to-password current-passport)
                        (setf current-passport ""))
          else
            do (setf current-passport (concatenate 'string current-passport " " line)))))


(defun valid-passport1 (pass)
  (and (pass-byr pass)
       (pass-iyr pass)
       (pass-eyr pass)
       (pass-hgt pass)
       (pass-hcl pass)
       (pass-ecl pass)
       (pass-pid pass)))


(defun valid-passport2 (pass)
  (and (valid-passport1 pass)
       (>= (parse-integer (pass-byr pass)) 1920)
       (<= (parse-integer (pass-byr pass)) 2002)
       (>= (parse-integer (pass-iyr pass)) 2010)
       (<= (parse-integer (pass-iyr pass)) 2020)
       (>= (parse-integer (pass-eyr pass)) 2020)
       (<= (parse-integer (pass-eyr pass)) 2030)
       (or (and (string= "cm" (reverse (subseq (reverse (pass-hgt pass)) 0 2)))
                (>= (parse-integer (reverse (subseq (reverse (pass-hgt pass)) 2))) 150)
                (<= (parse-integer (reverse (subseq (reverse (pass-hgt pass)) 2))) 193))
           (and (string= "in" (reverse (subseq (reverse (pass-hgt pass)) 0 2)))
                (>= (parse-integer (reverse (subseq (reverse (pass-hgt pass)) 2))) 59)
                (<= (parse-integer (reverse (subseq (reverse (pass-hgt pass)) 2))) 76)))
       (and (string= "#" (subseq (pass-hcl pass) 0 1))
            (every #'alphanumericp (subseq (pass-hcl pass) 1)))
       (member (pass-ecl pass) '("amb" "blu" "brn" "gry" "grn" "hzl" "oth") :test #'string=)
       (and (= (length (pass-pid pass)) 9)
            (every #'digit-char-p (pass-pid pass)))))


(defun solve1 ()
  (count-if #'valid-passport1 (read-input "input")))


(defun solve2 ()
  (count-if #'valid-passport2 (read-input "input")))
