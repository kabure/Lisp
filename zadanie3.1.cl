;;;zadanie 3 generator
;;; funkciu volame pomocou generuj napr (generuj '((1 2) (a b) (3 4)))


(defvar *vysledok*)
(defvar *vysledok1*)
(defvar *temp*)
(defvar dlzka)
(defvar prvok)
(defvar pocitadlo 0)
(defvar poradie 0)

(defparameter *citac* '())

(defun kombinacia (zoznam1 zoznam2)
  (setf *vysledok* '())
  (setf *vysledok1* '()) 
  
  (dolist (y zoznam1)
   
    (if (= 0 pocitadlo) 
        (let ()          
          (setf *vysledok1* (mapcar #'(lambda (n) (list y n)) zoznam2))
          (setf *vysledok* (append *vysledok* *vysledok1*)))
      
        (let () 
          (setf *vysledok1* (mapcar #'(lambda (n) (append y (list n))) zoznam2))
          (setf *vysledok* (append *vysledok* *vysledok1*))
               ))
      )
  )


(defun vytvor-zoznamy (zoznam)
  (setf *temp* '())
  (setf pocitadlo 0)
  (setf *vysledok* (pop zoznam))
  (setf dlzka (length zoznam))
  (dotimes (x dlzka)
    (setf *temp* *vysledok*)
    (kombinacia *temp* (pop zoznam))
    (setf pocitadlo (1+ pocitadlo)))
   (setf *citac* *vysledok*)
  )

(defun generuj (zoznam)
  (if (not (null *citac*))
      (let ()
        (pop *citac*)
        )
    (let ()
      (vytvor-zoznamy zoznam)
      (pop *citac*)
      )
        )
        )