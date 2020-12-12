(defpackage :aoc-2020-day-12
  (:use #:cl #:petalisp))

(in-package :aoc-2020-day-12)

(defstruct ship
  (ew 0 :type integer)
  (ns 0 :type integer)
  (dir 'E :type symbol))

(defstruct nav
  (dir 'F :type symbol)
  (amount 0 :type integer))

(defun parse-nav (line)
  (make-nav :dir (intern (subseq line 0 1))
            :amount (parse-integer (subseq line 1))))

(defun read-input (filename)
  (with-open-file (stream filename :direction :input)
    (loop for line = (read-line stream nil nil)
          while (not (null line))
          collect (parse-nav line))))

(defun go-go (direction amount ship)
  (ecase direction
    (N (make-ship
        :ew (ship-ew ship)
        :ns (+ (ship-ns ship) amount)
        :dir (ship-dir ship)))
    (S (make-ship
        :ew (ship-ew ship)
        :ns (- (ship-ns ship) amount)
        :dir (ship-dir ship)))
    (E (make-ship
        :ew (+ (ship-ew ship) amount)
        :ns (ship-ns ship)
        :dir (ship-dir ship)))
    (W (make-ship
        :ew (- (ship-ew ship) amount)
        :ns (ship-ns ship)
        :dir (ship-dir ship)))))

(defun new-direction (turn degree ship)
  (let ((direction (ship-dir ship))
         (turns (/ (if (eql turn 'R) degree (- degree))
                  90))
         (directions (list 'N 'E 'S 'W)))
    ;;(setf (cdr (last directions)) directions)
    (make-ship :ew (ship-ew ship)
               :ns (ship-ns ship)
               :dir (nth (mod (+ turns
                                 (position direction directions))
                              4)
                         directions))))

(defun go-ship (step ship)
  (ecase (nav-dir step)
    (F (go-go (ship-dir ship) (nav-amount step) ship))
    ((N S E W) (go-go (nav-dir step) (nav-amount step) ship))
    ((L R) (new-direction (nav-dir step) (nav-amount step) ship))))

(defun move-ship (steps ship)
  (if (null steps)
      ship
      (move-ship (cdr steps) (go-ship (pop steps) ship))))

(defun solve1()
  (let ((ship (move-ship (read-input "input") (make-ship))))
    (+ (abs (ship-ew ship)) (abs (ship-ns ship)))))

(defstruct waypoint
  (ew 10 :type integer)
  (ns 1 :type integer))


(defun go-go-waypoint (direction amount waypoint)
  (ecase direction
    (N (make-waypoint
        :ew (waypoint-ew waypoint)
        :ns (+ (waypoint-ns waypoint) amount)))
    (S (make-waypoint
        :ew (waypoint-ew waypoint)
        :ns (- (waypoint-ns waypoint) amount)))
    (E (make-waypoint
        :ew (+ (waypoint-ew waypoint) amount)
        :ns (waypoint-ns waypoint)))
    (W (make-waypoint
        :ew (- (waypoint-ew waypoint) amount)
        :ns (waypoint-ns waypoint)))))

(defun go-go-ship (amount waypoint ship)
  (make-ship :dir (ship-dir ship)
             :ew (+ (ship-ew ship) (* amount (waypoint-ew waypoint)))
             :ns (+ (ship-ns ship) (* amount (waypoint-ns waypoint)))))


(defun turn-waypoint (turn degree waypoint)
  (let ((turns (/ (if (eql turn 'R) degree (- degree))
                  90))
        (wp (copy-waypoint waypoint)))
    (loop repeat (abs turns)
          do
             (setf wp (make-waypoint :ew (* (signum turns) (waypoint-ns wp))
                                     :ns (* (- (signum turns)) (waypoint-ew wp)))))
    wp))

(defun go-waypoint (step ship waypoint)
  (print (nav-dir step))
  (ecase (nav-dir step)
    (F (values (go-go-ship (nav-amount step) waypoint ship) waypoint))
    ((N S E W) (values ship (go-go-waypoint (nav-dir step) (nav-amount step) waypoint)))
    ((L R) (values ship (turn-waypoint (nav-dir step) (nav-amount step) waypoint)))))

(defun move-waypoint (steps ship waypoint)
  (if (null steps)
      ship
      (multiple-value-bind (s w)
          (go-waypoint (pop steps) ship waypoint)
        (print s)
        (print w)
        (move-waypoint steps s w))))

(defun solve2()
  (let ((ship (move-waypoint (read-input "input") (make-ship) (make-waypoint))))
    (+ (abs (ship-ew ship)) (abs (ship-ns ship)))))
