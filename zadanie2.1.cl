;;;generovanie postupnosti
(defun gen (pocet)
  (reverse (maplist #'length (make-list pocet :initial-element 1))))