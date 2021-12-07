                                        ; Advent of Code 2021 day 7: https://adventofcode.com/2021/day/7

(defun load-numbers (filename)
  (with-open-file (stream filename :direction :input)
    (let* ((line (read-line stream nil))
           (cleaned (substitute #\Space #\, line))
           (vec (read-from-string (concatenate 'string "#(" cleaned ")"))))
      vec)))

(defun fuel-to-index (index crabs)
  (let ((val (aref crabs index)))
    (loop for idx from 0 below (length crabs)
          sum (abs (- val (aref crabs idx))) into total
          finally (return total))))

(defun day7-1 ()
  (let ((crabs (load-numbers "input.txt")))
    (loop for idx from 0 below (length crabs)
          minimize (fuel-to-index idx crabs))))
