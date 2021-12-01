                                        ; Advent of Code 2021 day 1: https://adventofcode.com/2021/day/1

(defun load-ints (filename)
  (with-open-file (in filename :direction :input)
    (loop for line = (read-line in nil)
          until (null line)
          collect (parse-integer line))))

(defun reduce-fun (prev next)
  (if (> next (cdr prev))
      (cons (1+ (car prev)) next)
      (cons (car prev) next)))

(defun count-increases (list)
  (car (reduce #'reduce-fun (cdr list) :initial-value (cons 0 (car list)))))

(defun day1-1 ()
  (count-increases (load-ints "input.txt")))
