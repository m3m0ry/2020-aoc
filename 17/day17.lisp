(defpackage :aoc-2020-day-17
  (:use #:cl #:petalisp))

(in-package :aoc-2020-day-17)


(defun parse-cube (char)
  (cond ((char= char #\.) 'inactive)
        ((char= char #\#) 'active)
        (t (error "~A is not a valid argument" char))))


(defun print-cube (cube)
  (ecase cube
    (inactive #\.)
    (active #\#)))

(defun print-cubes (cubes)
  (loop for k from 0 below (array-dimension cubes 2)
        do
           (loop for i from 0 below (array-dimension cubes 0)
                 do
                    (loop for j from 0 below (array-dimension cubes 1)
                          do
                             (format t "~a" (print-cube (aref cubes i j k))))
                 do
                    (format t "~%"))
        do
        (format t "~%")))

(defun fill-cubes (input cycles)
  (loop repeat cycles
        collect
        (loop repeat (length (first input))
              collect 'inactive)))

(defun fill-row (cycles)
  (loop repeat cycles
        collect 'inactive))

(defun list-to-2d-array (list)
  (make-array (list (length list)
                    (length (first list)))
              :initial-contents list))

(defun read-input (filename cycles)
  (with-open-file (stream filename :direction :input)
    (let ((input
            (loop for line = (read-line stream nil nil)
                  while (not (null line))
                  collect (append (fill-row cycles)
                                  (loop for char across line
                                        collect (parse-cube char))
                                  (fill-row cycles)))))
      (list-to-2d-array
       (append (fill-cubes input cycles)
               input
               (fill-cubes input cycles))))))

(defun 2d-to-3d (input cycles)
  (let ((array (make-array
                (append (array-dimensions input) (list (* 2 cycles)))
                :initial-element 'inactive)))
    (loop for i from 0 below (array-dimension input 0) do
      (loop for j from 0 below (array-dimension input 1) do
        (setf (aref array i j cycles)
              (aref input i j))))
    array))



(defun next-state (self &rest neighbors)
  (when (and
         (eql self 'active)
         (or
          (= 2 (count 'active neighbors))
          (= 3 (count 'active neighbors))))
    (return-from next-state 'active))
  (when (and (eql self 'inactive)
             (= 3 (count 'active neighbors)))
    (return-from next-state 'active))
  'inactive)

(defun ntimes (n f x)
  (loop repeat n do (setf x (funcall f x))))

(defun cubes-of-life (a &optional (iterations 1))
  (let* ((a (lazy-array a))
         (interior (array-interior a)))
    (loop repeat iterations do
      (setf a
            (funcall #'fuse*
                   a
                   (apply #'α #'next-state
                      (loop for di in '(0 -1 1) append
                        (loop for dj in '(0 -1 1) append
                          (loop for dk in '(0 -1 1) collect
                                (reshape a (τ (i j k) ((+ i di) (+ j dj) (+ k dk))) interior))))))))
    a))

(defun count-active-cubes (cubes)
  (loop for i from 0 below (array-total-size cubes)
        count (eql 'active (row-major-aref cubes i))))


(defun cubes-of-life2 (a &optional (iterations 1))
  (let* ((a (lazy-array a))
         (interior (array-interior a)))
    (loop repeat iterations do
      (setf a
            (funcall #'fuse*
                     a
                     (apply #'α #'next-state
                            (loop for di in '(0 -1 1) append
                              (loop for dj in '(0 -1 1) append
                                (loop for dk in '(0 -1 1) append
                                  (loop for dl in '(0 -1 1) collect
                                    (reshape a (τ (i j k l)
                                                  ((+ i di) (+ j dj) (+ k dk) (+ l dl)))
                                             interior)))))))))
          a))


(defun 2d-to-4d (input cycles)
  (let ((array (make-array
                (append (array-dimensions input) (list (* 2 cycles) (* 2 cycles)))
                :initial-element 'inactive)))
    (loop for i from 0 below (array-dimension input 0) do
      (loop for j from 0 below (array-dimension input 1) do
        (setf (aref array i j cycles cycles)
              (aref input i j))))
    array))


  (defun solve1 ()
  (let* ((cycles 6)
         (halo (1+ cycles))
         (input (read-input "input" halo))
         (cubes (2d-to-3d input halo)))
    (count-active-cubes
     (compute
      (cubes-of-life cubes cycles)))))



  (defun solve2 ()
    (let* ((cycles 6)
           (halo (* 2 cycles))
           (input (read-input "input" halo))
           (cubes (2d-to-4d input halo)))
      (count-active-cubes
       (compute
        (cubes-of-life2 cubes cycles)))))
