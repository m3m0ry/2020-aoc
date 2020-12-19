(defpackage :aoc-2020-day-16
  (:use :cl))

(in-package :aoc-2020-day-16)

(defun parse-ticket (string)
  (loop for n in (split-sequence:split-sequence #\, string)
        collect (parse-integer n)))

(defun read-input (filename)
  (with-open-file (stream filename :direction :input)
    (let ((rules
            (loop for line = (read-line stream nil nil)
                  while (and (not (null line)) (> (length line) 0))
                  with map = (make-hash-table)
                  with regex = "(.+): (\\d+)-(\\d+) or (\\d+)-(\\d+)"
                    do
                       (ppcre:register-groups-bind
                        (rule fl fh sl sh)
                        (regex line)
                        (setf (gethash rule map)
                              (list (parse-integer fl)
                                    (parse-integer fh)
                                    (parse-integer sl)
                                    (parse-integer sh))))
                  finally (return map)))
          (your-ticket
            (prog2
                (read-line stream nil nil)
                (parse-ticket (read-line stream nil nil))
              (read-line stream nil nil)))
          (nearby-tickets
           (progn
             (read-line stream nil nil)
             (loop for line = (read-line stream nil nil)
                   while (not (null line))
                   collect (parse-ticket line)))))
      (values rules your-ticket nearby-tickets))))


(defun check-all (rules tickets)
  (loop for ticket in tickets
        sum (check-ticket rules ticket)))

(defun check-ticket (rules ticket)
  (loop for number in ticket
        sum (check-number rules number)))

(defun check-number (rules number)
  (loop for v being the hash-values in rules using (hash-key k)
        when (or (and (<= (first v) number)
                      (>= (second v) number))
                 (and (<= (third v) number)
                      (>= (fourth v) number)))
          do
             (return-from check-number 0))
  number)

(defun solve1 ()
  (multiple-value-bind (rules your nearby)
      (read-input "input")
    (check-all rules nearby)))

(defun check-ticket2 (rules ticket)
  (notany #'null
         (loop for number in ticket
               collect (check-number2 rules number))))

(defun check-number2 (rules number)
  (loop for v being the hash-values in rules using (hash-key k)
        when (or (and (<= (first v) number)
                      (>= (second v) number))
                 (and (<= (third v) number)
                      (>= (fourth v) number)))
          do
             (return-from check-number2 t))
  nil)

(defun discard (rules tickets)
  (remove-if-not (lambda (x) (check-ticket2 rules x)) tickets))

(defun all-rules (rules tickets)
  (loop for ticket in tickets
        for j from 0
        with available = (loop repeat (hash-table-count rules)
                               collect
                               (loop for k being the hash-keys in rules
                                     collect k))
        do
           (loop for n in ticket
                 for i from 0
                 do
                    (setf (nth i available)
                          (let ((test
                          (loop for a in (nth i available)
                                for v = (gethash a rules)
                                when (or (and (<= (first v) n)
                                              (>= (second v) n))
                                         (and (<= (third v) n)
                                              (>= (fourth v) n)))
                                  collect a)))
                            test)))
        finally
           (return available)))


(defun find-possible (possible)
  (loop repeat 21
        with l = (length possible)
        do
           (loop for i from 0 below l
                 with to-remove = '()
                 when (= 1 (length (nth i possible)))
                   do
                      (push (first (nth i possible)) to-remove)
                 finally
                    (loop for j from 0 below l
                          unless (= 1 (length (nth j possible)))
                            do
                               (setf (nth j possible)
                                     (set-difference (nth j possible) to-remove  :test #'string=))))
        finally
           (return (loop for p in possible
                         collect (first p)))))


(defun solve2 ()
  (multiple-value-bind (rules your nearby)
      (read-input "input")
    (setf nearby (discard rules nearby))
    (let* ((possible (all-rules rules nearby))
           (ticket (find-possible possible)))
      (reduce #'* (loop for type in ticket
            for i from 0
            when (and (< 9 (length type)) (string= (subseq type 0 9) "departure"))
              collect (nth i your))))))

