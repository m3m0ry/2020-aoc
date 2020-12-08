(defpackage :aoc-2020-day-8
  (:use :cl))

(in-package :aoc-2020-day-8)

(defstruct (instruction (:conc-name instr-))
  (type 'nop :type symbol)
  (value 0 :type integer))

(defun instruction-to-symbol (string)
  (alexandria:eswitch (string :test #'string=)
    ("nop" 'nop)
    ("jmp" 'jmp)
    ("acc" 'acc)))

(defun read-input (filename)
  (with-open-file (stream filename :direction :input)
    (loop for line = (read-line stream nil nil)
          for i from 0
          while (not (null line))
          with code = (make-hash-table)
          for in = (split-sequence:split-sequence #\Space line)
          do
             (setf (gethash i code)
                   (make-instruction :type (instruction-to-symbol (first in))
                                     :value (parse-integer (second in))))
          finally (return code))))

(defun interpret (program accumulator ip visited &optional (executor #'execute))
  (if (gethash ip program)
      (let ((value (instr-value (gethash ip program))))
        (case (instr-type (gethash ip program))
          (nop (funcall executor program accumulator (1+ ip) visited))
          (jmp (funcall executor program accumulator (+ ip value) visited))
          (acc (funcall executor program (+ accumulator value) (1+ ip) visited))))
        accumulator))

(defun execute (program accumulator ip visited)
  (if (member ip visited)
      accumulator
      (progn
        (pushnew ip visited)
        (interpret program accumulator ip visited))))

(defun solve1 ()
  (execute (read-input "input") 0 0 '()))

(defun execute2 (program accumulator ip visited)
  (unless (member ip visited)
    (pushnew ip visited)
    (interpret program accumulator ip visited #'execute2)))

(defun swap-instruction (program i)
  (ecase (instr-type (gethash i program))
    (nop (setf (instr-type (gethash i program)) 'jmp))
    (jmp (setf (instr-type (gethash i program)) 'nop))))

(defun try (program i)
  (when (or (eql 'nop (instr-type (gethash i program)))
            (eql 'jmp (instr-type (gethash i program))))
    (let ((hacked-program (alexandria:copy-hash-table program)))
      (prog2
          (swap-instruction hacked-program i)
          (execute2 hacked-program 0 0 '())
        (swap-instruction hacked-program i)))))

(defun hack (program)
  (loop for i from 0 below (hash-table-count program)
        collect (try program i)))

(defun solve2 ()
  (first (remove-if #'null (hack (read-input "input")))))



