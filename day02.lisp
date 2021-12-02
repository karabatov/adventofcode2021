                                        ; Advent of Code 2021 day 2: https://adventofcode.com/2021/day/2

(defun load-lines (filename apply-fun)
  (with-open-file (in filename :direction :input)
    (loop for line = (read-line in nil)
          while line
          collect (funcall apply-fun line))))

(defun command-from-string (string)
  (read-from-string (concatenate 'string "(" string ")")))

(defun move-forward (from amount)
  (cons (+ amount (car from)) (cdr from)))

(defun move-up (from amount)
  (cons (car from) (- (cdr from) amount)))

(defun move-down (from amount)
  (move-up from (- amount)))

(defun move (from command)
  (let ((amount (cadr command)))
    (case (car command) ('forward (move-forward from amount))
          ('up (move-up from amount))
          ('down (move-down from amount))
          (otherwise from))))

(defun perform-commands (start commands)
  (reduce 'move commands :initial-value start))

(defun day2-1 ()
  (perform-commands (cons 0 0) (load-lines "input.txt" 'command-from-string)))
