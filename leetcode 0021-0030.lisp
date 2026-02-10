;;; problem 21
(defun merge-two-sorted-lists-using-merge (list1 list2)
  (merge 'list list1 list2 #'<))
;;; problem 21
(defun merge-two-sorted-lists-using-insertion-sort (list1 list2)
  (labels ((insert (item list)
             (cond ((null list) (list item))
                   ((< item (first list)) (cons item list))
                   (t (cons (first list) (insert item (rest list)))))))
    (loop with result = list1
          for x in list2
          do (setf result (insert x result))
          finally (return result))))
;;; problem 21
(defun merge-two-sorted-lists (list1 list2)
  (cond ((null list1) list2)
        ((null list2) list1)
        ((< (first list1) (first list2))
         (cons (first list1) (merge-two-sorted-lists (rest list1) list2)))
        (t (cons (first list2) (merge-two-sorted-lists list1 (rest list2))))))
;;; problem 22
(defun generate-parentheses (n)
  (let ((results '()))
    (labels ((try (lefts rights string)
               (cond ((and (= lefts n) (= rights n)) (push string results))
                     ((> lefts n) nil)
                     ((> rights n) nil)
                     ((> rights lefts) nil)
                     (t (try (1+ lefts) rights (concatenate 'string string "("))
                        (try lefts (1+ rights) (concatenate 'string string ")"))))))
      (try 0 0 "")
      results)))
;;; problem 23
(defun merge-k-sorted-lists-using-merge (lists)
  (reduce (lambda (x y) (merge 'list x y #'<)) lists))
;;; problem 23
(defun merge-k-sorted-lists-using-merging (lists)
  (when (zerop (length lists))
    (return-from merge-k-sorted-lists-using-merging))
  (labels ((merge-lists (list1 list2)
             (cond ((null list1) list2)
                   ((null list2) list1)
                   ((< (first list1) (first list2))
                    (cons (first list1) (merge-lists (rest list1) list2)))
                   (t (cons (first list2) (merge-lists list1 (rest list2)))))))
    (loop with result = (aref lists 0)
          for i from 1 upto (1- (length lists))
          do (setf result (merge-lists result (aref lists i)))
          finally (return result))))
;;; problem 24
(defun swap-nodes-in-pairs (head)
  (loop for x on head by #'cddr
        when (second x)
          do (rotatef (first x) (second x)))
  head)
;;; problem 24
(defun swap-nodes-in-pairs-recursive (head)
  (labels ((swap (list)
             (cond ((null list) '())
                   ((null (rest list)) list)
                   (t (list* (second list) (first list) (swap (rest (rest list))))))))
    (swap head)))
;;; problem 25
;;; TODO: use constant space (in-place reversal, etc.)
(defun reverse-nodes-in-k-group (head k)
  (labels ((rev (list)
             (if (< (length list) k)
                 list
                 (concatenate 'list
                              (reverse (subseq list 0 k))
                              (rev (nthcdr k list))))))
    (rev head)))
;;; problem 26
(defun remove-duplicates-from-sorted-array (nums)
  (loop with current-index = 0
        for index below (length nums)
        unless (and (plusp index) (= (aref nums index) (aref nums (1- index))))
          do (setf (aref nums current-index) (aref nums index))
             (incf current-index)
        finally (return current-index)))
;;; problem 27
(defun remove-element (nums val)
  (loop with index = 0
        for elt across nums
        unless (= elt val)
          do (setf (aref nums index) elt)
             (incf index)
        finally (return index)))
;;; problem 28
(defun find-the-index-of-the-first-occurrence-in-a-string-using-search (haystack needle)
  (or (search needle haystack) -1))
;;; problem 28
(defun find-the-index-of-the-first-occurrence-in-a-string (haystack needle)
  (loop with haystack-length = (length haystack)
        with needle-length = (length needle)
        for current-index below (length haystack)
        when (> (+ needle-length current-index) haystack-length)
          return -1
        when (string= (subseq haystack current-index needle-length)
                      needle)
          return current-index
        finally (return -1)))
;;; problem 29
(defun divide-two-integers (dividend divisor)
  (loop with minusp = (or (and (plusp dividend) (minusp divisor))
                          (and (minusp dividend) (plusp divisor)))
        with quotient = 0
        with lower-bound = (- (expt 2 31))
        with upper-bound = (1- (expt 2 31))
        initially (setf dividend (abs dividend)
                        divisor (abs divisor))
        while (> dividend divisor)
        do (incf quotient)
           (decf dividend divisor)
        finally (return (cond ((< quotient lower-bound) lower-bound)
                              ((> quotient upper-bound) upper-bound)
                              (minusp (- quotient))
                              (t quotient)))))
;;; problem 30
(defun substring-with-concatenation-of-all-words (s words)
  (let ((total-length (length s))
        (number-of-words (length words))
        (word-length (length (aref words 0)))
        (word-counts (make-hash-table :test #'equal))
        (results (make-array 0 :fill-pointer 0)))
    (loop for word across words
          unless (gethash word word-counts)
            do (setf (gethash word word-counts) 0)
          do (incf (gethash word word-counts)))
    (loop for i below word-length
          for left-index = i
          for right-index = i
          for current-window-counts = (make-hash-table :test #'equal)
          do (loop for end-bound = (+ right-index word-length)
                   with current-word
                   while (<= end-bound total-length)
                   do (setf current-word (subseq s right-index (+ right-index word-length)))
                      (incf right-index word-length)
                   if (null (gethash current-word word-counts))
                     do (clrhash current-window-counts)
                        (setf left-index right-index)
                   else do (setf (gethash current-word current-window-counts)
                                 (1+ (gethash current-word current-window-counts 0)))
                           (loop for removed-word
                                   = (subseq s left-index (+ left-index word-length))
                                 while (> (gethash current-word current-window-counts)
                                          (gethash current-word word-counts))
                                 do (incf left-index word-length)
                                    (decf (gethash removed-word current-window-counts)))
                           (when (= (- right-index left-index) (* word-length number-of-words))
                             (vector-push-extend left-index results))))
    results))