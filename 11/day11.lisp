(defpackage :aoc-2020-day-11
  (:use #:cl #:petalisp))

(in-package :aoc-2020-day-11)


(defun parse-seat (char)
  (cond ((char= char #\L) 'empty)
        ((char= char #\#) 'occupied)
        ((char= char #\.) 'floor)
          (t (error "~A is not a valid argument" char))))

(defun print-seat (seat)
  (ecase seat
    (empty #\L)
    (occupied #\#)
    (floor #\.)))


(defun list-to-2d-array (list)
  (make-array (list (length list)
                    (length (first list)))
              :initial-contents list))

(defun fill-floor (input)
  (list (loop repeat (length (first input))
        collect 'floor)))

(defun read-input (filename)
  (with-open-file (stream filename :direction :input)
    (let ((input
            (loop for line = (read-line stream nil nil)
                  while (not (null line))
                  collect (append '(floor)
                                  (loop for char across line
                                        collect (parse-seat char))
                                  '(floor)))))
      (list-to-2d-array
       (append (fill-floor input)
               input
               (fill-floor input))))))

(defun next-state (self &rest neighbors)
  (when (and (eql self 'empty) (= 0 (count 'occupied neighbors)))
    (return-from next-state 'occupied))
  (when (and (eql self 'occupied) (<= 4 (count 'occupied neighbors)))
    (return-from next-state 'empty))
  self)

(defun seats-of-life (a &optional (iterations 1))
  (let* ((a (lazy-array a))
         (interior (array-interior a)))
    (loop repeat iterations do
      (setf a (fuse* a (α #'next-state
                          (reshape (reshape a (τ (i j) (i j))) interior)
                          (reshape (reshape a (τ (i j) ((1+ i) j))) interior)
                          (reshape (reshape a (τ (i j) ((1- i) j))) interior)
                          (reshape (reshape a (τ (i j) (i (1+ j)))) interior)
                          (reshape (reshape a (τ (i j) (i (1- j)))) interior)
                          (reshape (reshape a (τ (i j) ((1+ i) (1+ j)))) interior)
                          (reshape (reshape a (τ (i j) ((1+ i) (1- j)))) interior)
                          (reshape (reshape a (τ (i j) ((1- i) (1+ j)))) interior)
                          (reshape (reshape a (τ (i j) ((1- i) (1- j)))) interior)))))
    a))

(defun print-seats (seats)
  (loop for i from 1 below (1- (array-dimension seats 0))
        do
           (loop for j from 1 below (1- (array-dimension seats 1))
                 do
                    (format t "~a" (print-seat (aref seats i j ))))
       do
          (format t "~%"))
  seats)

(defun count-empty-seats (seats)
  (loop for i from 1 below (1- (array-dimension seats 0))
        sum
        (loop for j from 1 below (1- (array-dimension seats 1))
              count (eql 'occupied (aref seats i j)))))

(defun solve1 ()
  (let ((input (read-input "input")))
    (count-empty-seats
           (print-seats
            (compute
             (seats-of-life input 100)))))) ;; The value 100 is hard-coded
