(defpackage :aoc-2020-day-18-2
  (:use :cl :esrap))

(in-package :aoc-2020-day-18-2)

(defun lexer (s)
  (remove-if-not (lambda (c) (or (alphanumericp c) (find c "*+()"))) s))

(defrule expression (or '1 '2))

(defrule (intern "1") (or "a" "b"))

(defrule mul (and expression "*" term)
  (:destructure (a op b)
                (list (intern op) a b)))

(defrule term (or add brackets number))

(defrule add (and term "+" term)
  (:destructure (a op b)
    (list (intern op) a b)))

(defrule brackets (and "(" expression ")")
  (:destructure (b1 e b2)
                (declare (ignore b1 b2))
                e))

(defrule nums (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))

(defrule number (and (+ nums))
  (:lambda (list)
    (parse-integer (text list) :radix 10)))

(defun print-grammar ()
  (describe-grammar 'expression))

(defun read-input (filename)
  (with-open-file (stream filename :direction :input)
    (loop for line = (read-line stream nil nil)
          while (not (null line))
          collect (parse 'expression (lexer line)))))

(defun solve2 ()
  (reduce #'+ (mapcar #'eval (read-input "input"))))
