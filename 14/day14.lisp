(defpackage :aoc-2020-day-14
  (:use :cl))

(in-package :aoc-2020-day-14)

(defstruct code
  address
  number
  mask)

(defun read-input (filename)
  (with-open-file (stream filename :direction :input)
    (loop for line = (read-line stream nil nil)
          with mask-regex = (ppcre:create-scanner "mask\\s=\\s([10X]+)")
          with mem-regex = (ppcre:create-scanner "mem\\[(\\d+)\\]\\s=\\s(\\d+)")
          with current-mask = nil
          while (not (null line))
          if (string= "mask" (subseq line 0 4))
            do
               (ppcre:register-groups-bind (mask)
                   (mask-regex line)
                 (setf current-mask mask))
          else
            collect
               (ppcre:register-groups-bind (mem n)
                   (mem-regex line)
                       (make-code :address (parse-integer mem)
                                  :number (parse-integer n)
                                  :mask current-mask)))))

(defun string->bit-vector (input x-target)
  (map 'bit-vector
       (lambda (x) (if (char= x #\X) x-target (digit-char-p x)))
       input))

(defun bit-vector->integer (bit-vector)
  (reduce #'(lambda (first-bit second-bit)
              (+ (* first-bit 2) second-bit))
          bit-vector))

(defun get-mask (mask target)
  (bit-vector->integer (string->bit-vector mask target)))

(defun run-port (input)
  (loop for code in input
        for address = (code-address code)
        for number = (code-number code)
        for mask = (code-mask code)
        with memory = (make-hash-table)
        do
           (setf (gethash address memory)
                 (logior (logand number (get-mask mask 1)) (get-mask mask 0)))
        finally
           (return memory)))


(defun count-memory (memory)
  (loop for v being the hash-values in memory using (hash-key k)
        sum v))

(defun solve1 ()
  (count-memory (run-port (read-input "input"))))

(defun integer->bit-list (integer)
  (labels ((integer->bit-list (int &optional accum)
             (cond ((> int 0)
                    (multiple-value-bind (i r) (truncate int 2)
                      (integer->bit-list i (push r accum))))
                   ((null accum) (push 0 accum))
                   (t accum))))
    (coerce (integer->bit-list integer) 'list)))

(defun get-mask2 (input x)
  (let ((x-long (append
                 (reverse (integer->bit-list x))
                 (loop repeat 128
                       collect 0)))
        (pos 0)
        (mask (reverse input)))
    (reverse (map 'bit-vector
                  (lambda (x) (if (char= x #\X)
                                  (prog1
                                      (nth pos x-long)
                                    (incf pos))
                                  (digit-char-p x)))
                  mask))))

(defun run-port2 (input)
  (loop for code in input
        for address = (code-address code)
        for number = (code-number code)
        for mask = (code-mask code)
        with memory = (make-hash-table)
        do
           (loop for i from 0 below (expt 2 (count #\X mask))
                 for m = (bit-vector->integer (get-mask2 mask i))
                 do
                    (setf
                     (gethash
                      (logior
                       m
                       (logand
                        (lognot
                         (logxor (get-mask mask 0)
                                 (get-mask mask 1)))
                        address))
                      memory)
                     number))
        finally
           (return memory)))

(defun solve2 ()
  (count-memory (run-port2 (read-input "input"))))
