                                        ; Advent of Code 2021 day 2: https://adventofcode.com/2021/day/2

(defun load-lines (filename apply-fun)
  (with-open-file (in filename :direction :input)
    (loop for line = (read-line in nil)
          while line
          collect (funcall apply-fun line))))

(defun command-from-string (string)
  (read-from-string (concatenate 'string "(" string ")")))

(defun pos (from)
  (car from))

(defun depth (from)
  (cadr from))

(defun move-forward (from amount)
  (list (+ amount (pos from)) (depth from)))

(defun move-up (from amount)
  (list (pos from) (- (depth from) amount)))

(defun move-down (from amount)
  (move-up from (- amount)))

(defun move (from command)
  (let ((amount (cadr command)))
    (case (car command) ('forward (move-forward from amount))
          ('up (move-up from amount))
          ('down (move-down from amount))
          (otherwise from))))

(defun perform-commands (move-func start commands)
  (reduce move-func commands :initial-value start))

(defun day2-1 ()
  (perform-commands 'move '(0 0) (load-lines "input.txt" 'command-from-string)))

(defun aim (from)
  (caddr from))

(defun move-forward2 (from amount)
  (list (+ amount (pos from)) (+ (depth from) (* amount (aim from))) (aim from)))

(defun move-up2 (from amount)
  (list (pos from) (depth from) (- (aim from) amount)))

(defun move-down2 (from amount)
  (move-up2 from (- amount)))

(defun move2 (from command)
  (let ((amount (cadr command)))
    (case (car command) ('forward (move-forward2 from amount))
          ('up (move-up2 from amount))
          ('down (move-down2 from amount))
          (otherwise from))))

(defun day2-2 ()
  (perform-commands 'move2 '(0 0 0) (load-lines "input.txt" 'command-from-string)))
