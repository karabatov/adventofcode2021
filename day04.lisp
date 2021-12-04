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

(defun card-column (n card)
  (mapcar #'(lambda (row) (nth n row)) card))

(defun card-columns (card)
  (loop for idx from 0 to (- (length card) 1)
        collect (card-column idx card)))

(defun winning-line-p (line drawn)
  (every #'(lambda (x) (member x drawn)) line))

(defun winning-card-p (card drawn)
  (flet ((winning-p (x) (winning-line-p x drawn)))
    (or (some #'winning-p card)
        (some #'winning-p (card-columns card)))))

(defun find-winning (cards drawn)
  (loop for card in cards
        when (winning-card-p card drawn) return card))

(defun draw (cards drawn remaining)
  (let ((winner? (find-winning cards drawn)))
    (if winner? (values winner? drawn remaining)
        (draw cards (cons (car remaining) drawn) (cdr remaining)))))

(defun score-card (card drawn)
  (let ((sums (mapcar #'(lambda (x) (reduce '+ (set-difference x drawn))) card)))
    (* (reduce '+ sums) (car drawn))))

(defun day4-1 ()
  (multiple-value-bind (numbers cards) (load-lines "input.txt" 'read-callouts 'read-bingo-cards)
    (multiple-value-bind (winner drawn) (draw cards '() numbers)
      (score-card winner drawn))))

(defun day4-2 ()
  (multiple-value-bind (numbers cards) (load-lines "input.txt" 'read-callouts 'read-bingo-cards)
    (let ((winner nil)
          (drawn '())
          (remaining numbers))
      (loop repeat (length cards)
            do (multiple-value-bind (w d r) (draw cards drawn remaining)
                 (setq winner w)
                 (setq drawn d)
                 (setq remaining r)
                 (setq cards (remove winner cards))))
      (score-card winner drawn))))
