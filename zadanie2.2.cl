;;;polynom pomocou reduce
(defun polynom (x list)
  (reduce #'(lambda (zvysok prvok) (+ (* zvysok x) prvok)) list))