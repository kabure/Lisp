
(defun poly (y &rest koeff)
  (polyy y koeff)
  )


(defun polyy (x koef)
   (if (not (null koef))
    (+ (* (expt x (- (list-length koef) 1)) (car koef)) (polyy x (cdr koef)))
  	'0
  ))