                                        ; Advent of Code 2021 day 8: https://adventofcode.com/2021/day/8

(defun read-digits (stream)
  (let ((line (read-line stream nil)))
    (when line (read-from-string
                (concatenate 'string "#(" (substitute "\" \"" " " line)) ")"))))

(defun read-multiple (stream read-fun)
  (loop for item = (funcall read-fun stream)
        while item
        collect item))
  
(defun load-numbers (filename)
  (with-open-file (in filename :direction :input)
    (read-multiple in 'read-digits)))

(defun digits-destructure (digits)
  (values (subseq digits 0 9)
          (subseq digits 11)))

(defun simple-digit-p (digit)
  (case (length digit)
    (2 t)
    (3 t)
    (4 t)
    (7 t)
    (t nil)))

(defun sum-simple-digits (digits)
  (multiple-value-bind (num disp) (digits-destructure digits)
    (reduce #'(lambda (prev next) (+ prev (length (remove-if-not 'simple-digit-p next))))
            disp)))

(defun day8-1 ()
  (let ((lines (load-numbers "input2.txt")))
    (reduce '+ (mapcar 'sum-simple-digits lines))))
