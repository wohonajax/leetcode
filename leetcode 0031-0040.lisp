;;; problem 31
(defun next-permutation (nums)
  )
;;; problem 32
(defun longest-valid-parentheses (s)
  (labels ((validp (string lefts rights)
             (cond ((> rights lefts) nil)
                   ((string= string "") t)
                   ((char= (schar string 0) #\()
                    (validp (subseq string 1) (1+ lefts) rights))
                   ((char= (schar string 0) #\))
                    (validp (subseq string 1) lefts (1+ rights))))))
    (loop with total-length = (length s)
          for window-length from total-length downto 1
          do (loop for start below (- total-length window-length)
                   when (validp (subseq s start (+ start window-length)) 0 0)
                     do (return-from longest-valid-parentheses window-length))
          finally (return 0))))