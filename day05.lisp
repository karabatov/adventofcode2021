                                        ; Advent of Code 2021 day 5: https://adventofcode.com/2021/day/5

(defun number-filter (char)
  (if (digit-char-p char) char #\Space))

(defun numbers-from-string (string)
  (read-from-string
   (concatenate 'string "(" (map 'string 'number-filter string) ")")))

(defun read-vent (stream)
  (numbers-from-string (read-line stream nil)))

(defun read-multiple (stream read-fun)
  (loop for item = (funcall read-fun stream)
        while item
        collect item))

(defun read-vents (stream)
  (read-multiple stream 'read-vent))

(defun load-lines (filename read-fun)
  (with-open-file (in filename :direction :input)
    (funcall read-fun in)))

(defun x1 (line)
  (first line))

(defun y1 (line)
  (second line))

(defun x2 (line)
  (third line))

(defun y2 (line)
  (fourth line))

(defun horizontal-or-vertical-p (line)
  (or (= (x1 line) (x2 line))
      (= (y1 line) (y2 line))))

(defun ortho-lines (lines)
  (remove-if-not 'horizontal-or-vertical-p lines))

(defun diff-step (p1 p2)
  (cond ((< (- p2 p1) 0) #'(lambda (el) (- el 1)))
        ((> (- p2 p1) 0) #'1+)
        (t #'(lambda (el) el))))

(defun line-points (line)
  (let ((x-diff (diff-step (x1 line) (x2 line)))
        (y-diff (diff-step (y1 line) (y2 line))))
    (append (loop for x = (x1 line) then (funcall x-diff x)
                  for y = (y1 line) then (funcall y-diff y)
                  until (and (= x (x2 line))
                             (= y (y2 line)))
                  collect `(,x ,y))
            (list (list (x2 line) (y2 line))))))

(defun common-points (line-pair)
  (intersection
   (line-points (car line-pair))
   (line-points (cadr line-pair))
   :test-not #'mismatch))

(defun all-pairs (lines)
  (loop for (x . y) on lines
        nconc (loop for yy in y
                    collect (list x yy))))

(defun find-intersections (lines)
  (mapcar 'common-points (all-pairs lines)))

(defun day5-1 ()
  (let* ((lines (load-lines "input.txt" 'read-vents))
         (ortho (ortho-lines lines))
         (clouds (find-intersections ortho))
         (big-vents (remove-duplicates (reduce #'append clouds) :test-not #'mismatch)))
    (length big-vents)))

(defun day5-2 ()
  (let* ((lines (load-lines "input.txt" 'read-vents))
         (clouds (find-intersections lines))
         (big-vents (remove-duplicates (reduce #'append clouds) :test-not #'mismatch)))
    (length big-vents)))
