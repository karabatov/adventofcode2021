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

(defun sliding-window (list num-elems)
  (loop for idx from 0 to (- (length list) num-elems)
        collect (subseq list idx (+ idx num-elems))))

(defun sliding-sum (list)
  (map 'list #'(lambda (elem) (reduce #'+ elem)) list))

(defun day1-2 ()
  (count-increases (sliding-sum (sliding-window (load-ints "input.txt") 3))))

(defun day1-1-alt ()
  (count-increases (sliding-sum (sliding-window (load-ints "input.txt") 1))))
