;;; problem 11
;;; inefficient brute-force check of every combination
(defun container-with-most-water-inefficient (height)
  (flet ((calculate-area (left right)
           (* (min (aref height left)
                   (aref height right))
              (- right left))))
    (loop with length = (length height)
          for left from 0 upto (1- length)
          maximize (loop for right from (1- length) downto left
                         maximize (calculate-area left right)))))
;;; problem 11
(defun container-with-most-water (height)
  (let ((left-index 0)
        (right-index (1- (length height))))
    (labels ((left () (aref height left-index))
             (right () (aref height right-index))
             (calculate-area ()
               (* (min (left) (right))
                  (- right-index left-index))))
      (loop if (< (left) (right))
              do (incf left-index)
            else do (decf right-index)
            until (= left-index right-index)
            maximize (calculate-area)))))
;;; problem 12
(defun integer-to-roman-algorithm (num)
  (let ((roman-values (list 1000 900 500 400 100 90 50 40 10 9 5 4 1))
        (roman-strings (list "M" "CM" "D" "CD" "C" "XC" "L" "XL" "X" "IX" "V" "IV" "I")))
    (with-output-to-string (str)
      (loop for v in roman-values
            for s in roman-strings
            do (loop while (>= num v)
                     do (decf num v)
                        (princ s str))))))
;;; problem 12
(defun integer-to-roman (num)
  (format nil "~@R" num))
;;; problem 13
(defun roman-to-integer (s)
  (let ((table (make-hash-table)))
    (setf (gethash #\I table) 1
          (gethash #\V table) 5
          (gethash #\X table) 10
          (gethash #\L table) 50
          (gethash #\C table) 100
          (gethash #\D table) 500
          (gethash #\M table) 1000)
    (loop with final-index = (1- (length s))
          ;; initialize with the last digit, which is never subtracted
          with result = (gethash (schar s final-index) table)
          for index below final-index
          do (let* ((current (gethash (schar s index) table))
                    (next (gethash (schar s (1+ index)) table)))
               (if (< current next)
                   ;; subtract the current digit from the running total
                   ;; the next digit will be added in full, so it stays balanced
                   (decf result current)
                   (incf result current)))
          finally (return result))))
;;; problem 14
(defun longest-common-prefix (strs)
  (with-output-to-string (str)
    (loop named main
          for index below (reduce #'min strs :key #'length)
          do (loop with current = (schar (aref strs 0) index)
                   for string across strs
                   unless (char= (schar string index) current)
                     do (return-from main)
                   finally (princ current str)))))
;;; problem 15
(defun 3sum (nums)
  (loop with sorted-nums = (sort nums #'<)
        with total-length = (length sorted-nums)
        with results = '()
        for i upto (- total-length 2)
        for i-num = (aref sorted-nums i)
        ;; if the first element is > 0, a sum of 0 isn't possible (since it's sorted)
        unless (plusp i-num)
          ;; skip when sorted-nums[i] = sorted-nums [i-1] (duplicate)
          unless (and (plusp i) (= i-num (aref sorted-nums (1- i))))
            do (loop with j = (1+ i)
                     with k = (1- total-length)
                     for j-num = (aref sorted-nums j)
                     for k-num = (aref sorted-nums k)
                     while (< j k)
                     do (let ((x (+ i-num j-num k-num)))
                          (cond ((minusp x) (incf j))
                                ((plusp x) (decf k))
                                (t (push (vector i-num j-num k-num) results)
                                   (incf j)
                                   (decf k)
                                   ;; skip duplicates for j and then for k
                                   (loop while (and (< j k)
                                                    (= (aref sorted-nums j)
                                                       (aref sorted-nums (1- j))))
                                         do (incf j))
                                   (loop while (and (< j k)
                                                    (= (aref sorted-nums k)
                                                       (aref sorted-nums (1+ k))))
                                         do (decf k))))))
        finally (return results)))
;;; problem 16
(defun 3sum-closest (nums target)
  (loop with sorted-nums = (sort nums #'<)
        with total-length = (length sorted-nums)
        with result = (expt 2 32) ; arbitrary distant value
        for i upto (- total-length 2)
        for i-num = (aref sorted-nums i)
        do (loop with j = (1+ i)
                 with k = (1- total-length)
                 for j-num = (aref sorted-nums j)
                 for k-num = (aref sorted-nums k)
                 while (< j k)
                 do (let ((sum (+ i-num j-num k-num)))
                      (cond ((< sum target)
                             (incf j))
                            ((> sum target)
                             (decf k))
                            (t (return-from 3sum-closest sum)))
                      (unless (< (abs (- result target))
                                 (abs (- sum target)))
                        (setf result sum))))
        finally (return result)))