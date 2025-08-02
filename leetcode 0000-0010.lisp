;;; problem 1
(defun two-sum (nums target)
  (let ((length (length nums)))
    (dotimes (i length)
      (dotimes (j length)
        (unless (= i j)
          (when (= (+ (aref nums i) (aref nums j))
                   target)
            (return-from two-sum (vector i j))))))))
;;; problem 2
(defun add-two-numbers (l1 l2)
  (do ((x l1 (cdr x))
       (y l2 (cdr y))
       (digit-overflow-p nil)
       (result nil))
      ((and (endp x) (endp y))
       (nreverse (if digit-overflow-p
                     (cons 1 result)
                     result)))
    (flet ((dispatch-add (a b)
             (let* ((z (+ a b))
                    (new-overflow (> z 9)))
               (cond ((and digit-overflow-p new-overflow)
                      (push (- z 9) result)) ; 1 + z - 10
                     (digit-overflow-p
                      (setf digit-overflow-p nil)
                      (let ((temp (1+ z)))
                        (cond ((> temp 9)
                               (setf digit-overflow-p t)
                               (push (- temp 10) result))
                              (t (push temp result)))))
                     (new-overflow
                      (setf digit-overflow-p t)
                      (push (- z 10) result)) ; no digit-overflow-p
                     (t (setf digit-overflow-p nil)
                        (push z result))))))
      (cond ((and x y)
             (dispatch-add (car x) (car y)))
            (x (dispatch-add (car x) 0))
            (t (dispatch-add (car y) 0))))))
;;; problem 3
(defun longest-substring-without-repeating-characters-naive (s)
  (loop named main
        with total-length = (length s)
        for length = total-length then (1- length)
        do (loop for start upto (- total-length length)
                 for substring = (subseq s start (+ start length))
                 when (every (lambda (c)
                               (= 1 (count c substring :test #'char=)))
                             substring)
                   do (return-from main substring))))
;;; problem 3 but more efficient
;;; translated from https://github.com/kamyu104/LeetCode-Solutions, MIT license
(defun longest-substring-without-repeating-characters (s)
  (loop with result = 0
        with left-index = 0
        with table = (make-hash-table)
        for char across s
        for lookup = (gethash char table)
        for right-index below (length s)
        when lookup do (setf left-index (max left-index (1+ lookup)))
        do (setf (gethash char table) right-index
                 result (max result (1+ (- right-index left-index))))
        finally (return result)))
;;; problem 4
(defun median-of-two-sorted-arrays-using-merge (nums1 nums2)
  (let* ((new-vector (merge '(vector fixnum) nums1 nums2 #'<))
         (length (length new-vector))
         (halfway-point (floor length 2)))
    (if (oddp length)
        (aref new-vector halfway-point)
        (/ (+ (aref new-vector halfway-point)
              (aref new-vector (1- halfway-point)))
           2))))
;;; problem 4 more efficiently TODO: O(log(n+m)), without merging
(defun median-of-two-sorted-arrays (nums1 nums2)
  (let* ((length1 (length nums1))
         (length2 (length nums2))
         (total-length (+ length1 length2)))
    ))
;;; problem 5
(defun longest-palindromic-substring (s)
  (labels ((palindrome-p (substring)
             ;; for "abcdefg" check if a = g
             ;; if yes, check if "bcdef" is a palindrome
             (let* ((length (length str))
                    (final-index (1- length))
                    (two-p (= length 2))
                    (one-p (= length 1)))
               (cond (two-p (char= (schar str 0) (schar str 1)))
                     (one-p t)
                     ((char= (schar str 0) (schar str final-index))
                      (palindrome-p (subseq str 1 final-index)))
                     (t nil)))))
    ;; check every substring of a given length,
    ;; then every substring of length - 1, etc.
    (loop named main
          with total-length = (length s)
          for length = total-length then (1- length)
          do (loop for start upto (- total-length length)
                   for substring = (subseq s start (+ start length))
                   when (palindrome-p substring)
                     do (return-from main substring)))))
;;; problem 6
;;; modified from https://github.com/rrcgat/LeetCode, MIT license
;;; and from https://github.com/kamyu104/LeetCode-Solutions, MIT license
(defun zigzag-conversion (s num-rows)
  (when (= num-rows 1)
    (return-from zigzag-conversion s))
  (let* ((length (length s))
         (result (make-array length :element-type 'character :fill-pointer 0))
         (step (- (* 2 num-rows) 2))) ; skip this many characters per pass
    (loop for i below num-rows
          do (loop for j from i below length by step
                   do (vector-push (schar s j) result)
                      (let ((second-index (- (+ j step) (* 2 i))))
                        (and (< 0 i (1- num-rows))
                             (< second-index length)
                             (vector-push (schar s second-index) result)))))
    result))
;;; problem 7
(defun reverse-integer-hairy (x)
  (let ((negative-p (minusp x))
        (limit (expt 2 31)))
    (flet ((switch-digits (num)
             (nreverse
               (if negative-p
                   (subseq (format nil "~A" num) 1) ; drop the "-"
                   (format nil "~A" num)))))
      (let ((result
              (if negative-p
                  (- (parse-integer (switch-digits x)))
                  (parse-integer (switch-digits x)))))
        (cond ((< result (- limit))  0)
              ((> result (1- limit)) 0)
              (t result))))))
;;; problem 7 (modified from https://github.com/rrcgat/LeetCode, MIT license)
(defun reverse-integer (x)
  (let ((limit (expt 2 31))
          (result (if (plusp x)
                      (parse-integer (nreverse (write-to-string x)))
                      (- (parse-integer
                           (nreverse (write-to-string (abs x))))))))
      (cond ((< result (- limit))  0)
            ((> result (1- limit)) 0)
            (t result))))
;;; problem 8
(defun my-a-to-i (s)
  (let* ((result (parse-integer s :junk-allowed t))
         (limit (expt 2 31))
         (upper-limit (1- limit))
         (lower-limit (- limit)))
    (cond ((null result) 0)
          ((< result lower-limit) lower-limit)
          ((> result upper-limit) upper-limit)
          (t result))))
;;; problem 9 TODO: don't convert to string
(defun palindrome-number (x)
  (labels ((palindrome-p (str)
             (let* ((length (length str))
                    (final-index (1- length))
                    (two-p (= length 2))
                    (one-p (= length 1)))
               (cond (two-p (char= (schar str 0) (schar str 1)))
                     (one-p t)
                     ((char= (schar str 0) (schar str final-index))
                      (palindrome-p (subseq str 1 final-index)))
                     (t nil)))))
    (palindrome-p (write-to-string x))))
;;; problem 10
(defun regular-expression-matching (s p))