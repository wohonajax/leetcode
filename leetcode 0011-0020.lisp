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
;;; TODO: use math rather than just built-in functionality
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