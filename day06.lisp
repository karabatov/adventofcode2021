                                        ; Advent of Code 2021 day 6: https://adventofcode.com/2021/day/6

(defun load-numbers (filename)
  (with-open-file (stream filename :direction :input)
    (let* ((line (read-line stream nil))
           (cleaned (substitute #\Space #\, line))
           (as-vec (read-from-string (concatenate 'string "#(" cleaned ")")))
           (vec (make-array (length as-vec) :element-type 'fixnum :initial-contents as-vec :adjustable t :fill-pointer t)))
      vec)))

(defun simulate (school)
  (loop for idx from 0 below (fill-pointer school)
        do (let ((val (aref school idx)))
             (cond ((= val 0) (setf (aref school idx) 6)
                    (vector-push-extend 8 school))
                   (t (setf (aref school idx) (- val 1)))))))

(defun evolve (school days)
  (loop repeat days
        do (simulate school)))

(defun day6-1 ()
  (let ((school (load-numbers "input.txt")))
    (evolve school 80)
    (fill-pointer school)))

(defun cached-evolve-rec (cache start days)
  (defun evolve-internal (st d)
    (let ((args `(,st ,d)))
      (multiple-value-bind (found exists) (gethash args cache)
        (if exists found
            (setf (gethash args cache)
                  (if (= d 0) 1
                      (if (= st 0) (+ (evolve-internal 6 (- d 1)) (evolve-internal 8 (- d 1)))
                          (evolve-internal (- st 1) (- d 1)))))))))
  (evolve-internal start days))

(defun day6-2 ()
  (let* ((school (load-numbers "input.txt"))
         (days 256)
         (cache (make-hash-table :test #'equal))
         (total 0))
    (loop for idx from 0 below (fill-pointer school)
          do (setf total (+ total (cached-evolve-rec cache (aref school idx) days))))
    total))
