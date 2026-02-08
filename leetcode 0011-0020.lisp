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
  (with-output-to-string (str)
    (let ((num-string (write-to-string num)))
      (cond ((or (char= (schar num-string 0) #\4)
                 (char= (schar num-string 0) #\9))
             )))))
;;; problem 12
(defun integer-to-roman (num)
  (format nil "~@R" num))