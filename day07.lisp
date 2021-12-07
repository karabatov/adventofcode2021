                                        ; Advent of Code 2021 day 7: https://adventofcode.com/2021/day/7

(defun load-numbers (filename)
  (with-open-file (stream filename :direction :input)
    (let* ((line (read-line stream nil))
           (cleaned (substitute #\Space #\, line))
           (vec (read-from-string (concatenate 'string "#(" cleaned ")"))))
      vec)))

(defun simple-fuel (from to)
  (abs (- from to)))

(defun fuel-to-index (target crabs fuel-fun)
  (loop for idx from 0 below (length crabs)
        sum (funcall fuel-fun target (aref crabs idx)) into total
        finally (return total)))

(defun day7-1 ()
  (let ((crabs (load-numbers "input.txt")))
    (loop for idx from 0 below (length crabs)
          minimize (fuel-to-index (aref crabs idx) crabs 'simple-fuel))))

(defun progressive-fuel (from to)
  (let ((steps (simple-fuel from to)))
    (floor (* steps (1+ steps)) 2)))

(defun day7-2 ()
  (let* ((crabs (load-numbers "input.txt"))
         (pos-min (loop for idx from 0 below (length crabs) minimize (aref crabs idx)))
         (pos-max (loop for idx from 0 below (length crabs) maximize (aref crabs idx))))
    (loop for pos from pos-min to pos-max
          minimize (fuel-to-index pos crabs 'progressive-fuel))))
