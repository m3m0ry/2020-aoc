(defpackage :aoc-2020-day-19
  (:use :cl :esrap))

(in-package :aoc-2020-day-19)

(defun terminal (rule target)
  (eval `(defrule ,(intern rule) ,target)))

(defun direct (rule target)
  (eval `(defrule ,(intern rule) ,(intern target))))

(defun double (rule target1 target2)
  (eval `(defrule ,(intern rule) (and ,(intern target1) ,(intern target2)))))

(defun double-or (rule targeta1 targeta2 targetb1 targetb2)
  (eval `(defrule ,(intern rule) (or (and ,(intern targeta1) ,(intern targeta2))
                                     (and ,(intern targetb1) ,(intern targetb2))))))

(defun g-or (rule targeta targetb)
  (eval `(defrule ,(intern rule) (or ,(intern targeta) ,(intern targetb)))))

(defparameter *terminal* "(\\d+): \"(\\w+)\"$")
(defparameter *direct* "(\\d+): (\\d+)$")
(defparameter *double* "(\\d+): (\\d+) (\\d+)$")
(defparameter *or* "(\\d+): (\\d+) \\| (\\d+)$")
(defparameter *double-or* "(\\d+): (\\d+) (\\d+) \\| (\\d+) (\\d+)$")

(defun read-input (filename)
  (let ((rules '())
        (targets '()))
  (with-open-file (stream filename :direction :input)
    (loop for line = (read-line stream nil nil)
          while (and (not (null line)) (> (length line) 0))
          do
             (cond
               ((ppcre:scan *terminal* line)
                (ppcre:register-groups-bind (rule target)
                    (*terminal* line)
                  (terminal rule target)
                  (pushnew rule rules :test #'string=)
                  (pushnew target targets :test #'string=)))
               ((ppcre:scan *direct* line)
                (ppcre:register-groups-bind (rule target)
                    (*direct* line)
                  (direct rule target)
                  (pushnew rule rules :test #'string=)
                  (pushnew target targets :test #'string=)))
               ((ppcre:scan *double* line)
                (ppcre:register-groups-bind (rule target1 target2)
                    (*double* line)
                  (double rule target1 target2)
                  (pushnew rule rules :test #'string=)
                  (pushnew target1 targets :test #'string=)
                  (pushnew target2 targets :test #'string=)))
               ((ppcre:scan *double-or* line)
                (ppcre:register-groups-bind (rule targeta1 targeta2 targetb1 targetb2)
                    (*double-or* line)
                  (double-or rule targeta1 targeta2 targetb1 targetb2)
                  (pushnew rule rules :test #'string=)
                  (pushnew targeta1 targets :test #'string=)
                  (pushnew targeta2 targets :test #'string=)
                  (pushnew targetb1 targets :test #'string=)
                  (pushnew targetb2 targets :test #'string=)))
               ((ppcre:scan *or* line)
                (ppcre:register-groups-bind (rule targeta targetb)
                    (*or* line)
                  (g-or rule targeta targetb)
                  (pushnew rule rules :test #'string=)
                  (pushnew targeta targets :test #'string=)
                  (pushnew targetb targets :test #'string=)))
               (t (print line))))
    (values
     (first (set-difference rules targets :test #'string=))
     (loop for line = (read-line stream nil nil)
           while (not (null line))
           collect line)))))


(defun solve1 ()
  (multiple-value-bind (start messages)
      (read-input "input")
    (flet ((parser (x)
             (handler-case
                 (parse (intern start) x)
               (esrap-parse-error () nil))))
      (count-if-not #'null
                      (mapcar #'parser
                                messages)))))


(defun solve2 ()
  (multiple-value-bind (start messages)
      (read-input "test-input")
    (eval `(defrule ,(intern "8") (or ,(intern "42")
                                      (and ,(intern "42") ,(intern "8"))))); this line is buggy
    (eval `(defrule ,(intern "11") (or (and ,(intern "42") ,(intern "11") ,(intern "31"))
                                       (and ,(intern "42") ,(intern "31")))))
    (flet ((parser (x)
             (handler-case
                 (parse (intern start) x)
               (esrap-parse-error (wtf) (print wtf) nil))))
      (count-if-not #'null
                      (mapcar #'parser
                                messages)))))
