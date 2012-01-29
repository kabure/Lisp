;;; Zadany lubovolny pocet mnozin - najst prvky ktore su v kazdej mnozine


(defun inter (zoznamy)
  (let ((vyskyty '()) 
        (pocet (length (rest zoznamy)))
        (poc-vys 0))
    (mapcar #'(lambda (x)
                (mapcar #'(lambda (zoznam)
                            (if (member x zoznam)
                                (progn
                                  (setf poc-vys (1+ poc-vys))
                                  (if (eql poc-vys pocet)
                                      (if (not (member x vyskyty))
                                          (setf vyskyty (append vyskyty (list x))))))))
                  (rest zoznamy))
                (setf poc-vys 0))
                (first zoznamy))
    vyskyty))