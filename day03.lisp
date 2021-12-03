                                        ; Advent of Code 2021 day 3: https://adventofcode.com/2021/day/3

(defun load-lines (filename apply-fun)
  (with-open-file (in filename :direction :input)
    (loop for line = (read-line in nil)
          while line
          collect (funcall apply-fun line))))

(defun numbers-from-string (string)
  (loop for char across string
        collect (digit-char-p char)))

(defun count-ones (lines)
  (reduce
   #'(lambda (lhs rhs) (mapcar '+ lhs rhs)) lines
   :initial-value (make-list (length (car lines)) :initial-element 0)))

(defun one-or-zero (value total &key when-equal)
  (cond ((> value (- total value)) 1)
        ((< value (- total value)) 0)
        (t when-equal)))

(defun most-common (lines)
  (let ((total (length lines)))
    (mapcar #'(lambda (x) (one-or-zero x total :when-equal 1)) (count-ones lines))))

(defun binary-list-to-number (bin)
  (reduce #'(lambda (prev next) (+ next (ash prev 1))) bin))

(defun invert-binary (bin)
  (mapcar #'(lambda (x) (logxor x 1)) bin))

(defun day3-1 ()
  (let* ((lines (load-lines "input.txt" 'numbers-from-string))
         (by-freq (most-common lines))
         (gamma-rate (binary-list-to-number by-freq))
         (epsilon-rate (binary-list-to-number (invert-binary by-freq))))
    (* gamma-rate epsilon-rate)))

(defun least-common (lines)
  (invert-binary (most-common lines)))

(defun filter-down (lines pos frequency-fun)
  (let* ((freq-at-pos (nth pos (funcall frequency-fun lines)))
         (filtered (remove-if-not #'(lambda (bin) (= (nth pos bin) freq-at-pos)) lines))
         (new-length (length filtered)))
    (if (= new-length 1) (car filtered) (filter-down filtered (1+ pos) frequency-fun))))
    
(defun day3-2 ()
  (let* ((lines (load-lines "input.txt" 'numbers-from-string))
         (oxygen-rating (binary-list-to-number (filter-down lines 0 'most-common)))
         (co2-rating (binary-list-to-number (filter-down lines 0 'least-common))))
    (* oxygen-rating co2-rating)))
