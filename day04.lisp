                                        ; Advent of Code 2021 day 4: https://adventofcode.com/2021/day/4

(defun numbers-from-string (string separator-char)
  (read-from-string
   (concatenate 'string "(" (substitute #\Space separator-char string) ")")))

(defun read-callouts (stream)
  (numbers-from-string (read-line stream nil) #\,))

(defun read-bingo-card (stream)
  (if (read-line stream nil)
      (loop repeat 5
            collect (numbers-from-string (read-line stream nil) #\Space))))

(defun read-multiple (stream read-fun)
  (loop for item = (funcall read-fun stream)
        while item
        collect item))

(defun read-bingo-cards (stream)
  (read-multiple stream 'read-bingo-card))

(defun load-lines (filename callouts-fun cards-fun)
  (with-open-file (in filename :direction :input)
    (values (funcall callouts-fun in)
            (funcall cards-fun in))))




