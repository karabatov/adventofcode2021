                                        ; Advent of Code 2021 day 6: https://adventofcode.com/2021/day/6

(defun load-numbers (filename)
  (with-open-file (stream filename :direction :input)
    (let* ((line (read-line stream nil))
           (cleaned (substitute #\Space #\, line))
           (as-vec (read-from-string (concatenate 'string "#(" cleaned ")")))
           (vec (make-array (length as-vec) :element-type 'fixnum :initial-contents as-vec :adjustable t :fill-pointer t)))
      vec)))

(defun process-fish (timer)
  (cond ((= timer 0) (values 6 8))
        (t (values (- timer 1) nil))))

(defun simulate (school)
  (loop for idx from 0 below (fill-pointer school)
        do (multiple-value-bind (next new) (process-fish (aref school idx))
             (setf (aref school idx) next)
             (when new (vector-push-extend new school)))))

(defun evolve (school days)
  (loop repeat days
        do (simulate school)))

(defun day6-1 ()
  (let ((school (load-numbers "input.txt")))
    (evolve school 80)
    (fill-pointer school)))

(defun evolve-one (start days)
  (let ((school (make-array 1 :element-type 'fixnum :initial-element start :adjustable t :fill-pointer t)))
    (evolve school days)
    (fill-pointer school)))

(defun day6-2 ()
  (let* ((school (load-numbers "input2.txt"))
        (days 18)
        (reference (evolve-one 0 days))
        (total 0))
    (loop for idx from 0 below (fill-pointer school)
          do (setf total (+ total (aref school idx) reference)))
    total))
