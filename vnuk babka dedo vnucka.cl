(defparameter *open* '())
(defparameter *close* '())


(defun vnuk-stav (stav)
  (first stav))
  
(defun babka-stav (stav)
  (second stav))
  
(defun dedo-stav (stav)
  (third stav))
  
(defun vnucka-stav (stav)
  (fourth stav))
  
(defun lampa-stav (stav)
  (fifth stav)) 
  
(defun vnuk-na-druhy-breh ()
  #'(lambda (stav)
    (if (equal 0 (lampa-stav stav))
      (setf stav (list 1 (second stav) (third stav) (fourth stav) 1))
      (setf stav (list 0 (second stav) (third stav) (fourth stav) 0)))))
      
(defun babka-na-druhy-breh ()
  #'(lambda (stav)
    (if (equal 0 (lampa-stav stav))
      (setf stav (list (first stav) 1 (third stav) (fourth stav) 1))
      (setf stav (list (first stav) 0 (third stav) (fourth stav) 0)))))  
      
(defun dedo-na-druhy-breh ()
  #'(lambda (stav)
    (if (equal 0 (lampa-stav stav))
      (setf stav (list (first stav) (second stav) 1 (fourth stav) 1))
      (setf stav (list (first stav) (second stav) 0 (fourth stav) 0)))))  
      
(defun vnucka-na-druhy-breh ()
  #'(lambda (stav)
    (if (equal 0 (lampa-stav stav))
      (setf stav (list (first stav) (second stav) (third stav) 1 1))
      (setf stav (list (first stav) (second stav) (third stav) 0 0)))))

(defun vnuk-babka-na-druhy-breh ()
  #'(lambda (stav)
    (if (equal 0 (lampa-stav stav))
      (setf stav (list 1 1 (third stav) (fourth stav) 1))
      (setf stav (list 0 0 (third stav) (fourth stav) 0)))))
      
(defun vnuk-dedo-na-druhy-breh ()
  #'(lambda (stav)
    (if (equal 0 (lampa-stav stav))
      (setf stav (list 1 (second stav) 1 (fourth stav) 1))
      (setf stav (list 0 (second stav) 0 (fourth stav) 0)))))
      
(defun vnuk-vnucka-na-druhy-breh ()
  #'(lambda (stav)
    (if (equal 0 (lampa-stav stav))
      (setf stav (list 1 (second stav) (third stav) 1 1))
      (setf stav (list 0 (second stav) (third stav) 0 0)))))
      
(defun babka-dedo-na-druhy-breh ()
  #'(lambda (stav)
    (if (equal 0 (lampa-stav stav))
      (setf stav (list (first stav) 1 1 (fourth stav) 1))
      (setf stav (list (first stav) 0 0 (fourth stav) 0))))) 
      
(defun babka-vnucka-na-druhy-breh ()
  #'(lambda (stav)
    (if (equal 0 (lampa-stav stav))
      (setf stav (list (first stav) 1 (third stav) 1 1))
      (setf stav (list (first stav) 0 (third stav) 0 0)))))
      
(defun dedo-vnucka-na-druhy-breh ()
  #'(lambda (stav)
    (if (equal 0 (lampa-stav stav))
      (setf stav (list (first stav) (second stav) 1 1 1))
      (setf stav (list (first stav) (second stav) 0 0 0)))))     
      
      
      
(defun find-operators (stav)
  (remove-if-not #'(lambda (x) x)
    (list
      (when (and (equal (lampa-stav stav) (vnuk-stav stav))
                  (equal (lampa-stav stav) (babka-stav stav))
                  (equal (lampa-stav stav) (dedo-stav stav))
                  (equal (lampa-stav stav) (vnucka-stav stav)))
                    (vnuk-babka-na-druhy-breh) (vnuk-dedo-na-druhy-breh) (vnuk-vnucka-na-druhy-breh)
                    (babka-dedo-na-druhy-breh) (babka-vnucka-na-druhy-breh) (dedo-vnucka-na-druhy-breh)
                    (vnuk-na-druhy-breh) (babka-na-druhy-breh) (dedo-na-druhy-breh) (vnucka-na-druhy-breh))
                    
      (when (and (equal (lampa-stav stav) (vnuk-stav stav))
                 (equal (lampa-stav stav) (babka-stav stav))
                 (not (equal (lampa-stav stav) (dedo-stav stav)))
                 (not (equal (lampa-stav stav) (vnucka-stav stav))))
                  (vnuk-babka-na-druhy-breh) (vnuk-na-druhy-breh) (babka-na-druhy-breh))
                  
      (when (and (equal (lampa-stav stav) (vnuk-stav stav))
                 (equal (lampa-stav stav) (dedo-stav stav))
                 (not (equal (lampa-stav stav) (babka-stav stav)))
                 (not (equal (lampa-stav stav) (vnucka-stav stav))))
                  (vnuk-dedo-na-druhy-breh) (vnuk-na-druhy-breh) (dedo-na-druhy-breh))
                  
      (when (and (equal (lampa-stav stav) (vnuk-stav stav))
                 (equal (lampa-stav stav) (vnucka-stav stav))
                 (not (equal (lampa-stav stav) (dedo-stav stav)))
                 (not (equal (lampa-stav stav) (babka-stav stav))))
                  (vnuk-vnucka-na-druhy-breh) (vnuk-na-druhy-breh) (vnucka-na-druhy-breh))
      
      (when (and (equal (lampa-stav stav) (babka-stav stav))
                 (equal (lampa-stav stav) (dedo-stav stav))
                 (not (equal (lampa-stav stav) (vnuk-stav stav)))
                 (not (equal (lampa-stav stav) (vnucka-stav stav))))
                  (babka-dedo-na-druhy-breh) (babka-na-druhy-breh) (dedo-na-druhy-breh)) 
                  
      (when (and (equal (lampa-stav stav) (babka-stav stav))
                 (equal (lampa-stav stav) (vnucka-stav stav))
                 (not (equal (lampa-stav stav) (vnuk-stav stav)))
                 (not (equal (lampa-stav stav) (dedo-stav stav))))
                  (babka-vnucka-na-druhy-breh) (babka-na-druhy-breh) (vnucka-na-druhy-breh))
                  
      (when (and (equal (lampa-stav stav) (dedo-stav stav))
                 (equal (lampa-stav stav) (vnucka-stav stav))
                 (not (equal (lampa-stav stav) (vnuk-stav stav)))
                 (not (equal (lampa-stav stav) (babka-stav stav))))
                  (dedo-vnucka-na-druhy-breh) (dedo-na-druhy-breh) (vnucka-na-druhy-breh))
                  
      (when (and (equal (lampa-stav stav) (vnuk-stav stav))
                 (not (equal (lampa-stav stav) (vnucka-stav stav)))
                 (not (equal (lampa-stav stav) (dedo-stav stav)))
                 (not (equal (lampa-stav stav) (babka-stav stav))))
                  (vnuk-na-druhy-breh))
                  
      (when (and (equal (lampa-stav stav) (vnucka-stav stav))
                 (not (equal (lampa-stav stav) (dedo-stav stav)))
                 (not (equal (lampa-stav stav) (vnuk-stav stav)))
                 (not (equal (lampa-stav stav) (babka-stav stav))))
                  (vnucka-na-druhy-breh))
                  
      (when (and (equal (lampa-stav stav) (dedo-stav stav))
                 (not (equal (lampa-stav stav) (vnucka-stav stav)))
                 (not (equal (lampa-stav stav) (vnuk-stav stav)))
                 (not (equal (lampa-stav stav) (babka-stav stav))))
                  (dedo-na-druhy-breh))
                  
      (when (and (equal (lampa-stav stav) (babka-stav stav))
                 (not (equal (lampa-stav stav) (vnucka-stav stav)))
                 (not (equal (lampa-stav stav) (vnuk-stav stav)))
                 (not (equal (lampa-stav stav) (dedo-stav stav))))
                  (babka-na-druhy-breh))                                                                                   
                  
      )))
      
(defun expanduj (stav)
  (mapcar #'(lambda (operator) (funcall operator stav)) (find-operators stav)))

(defun novy-uzol (stav &optional (uzol-id nil) (predok-id nil) (cena 0) (hlbka 0))
  (list :stav stav :uzol-id uzol-id :predok-id predok-id :cena cena :hlbka hlbka))

(defun uzol_stav (uzol) (getf uzol :stav))
(defun uzol_uzol-id (uzol) (getf uzol :uzol-id))
(defun uzol_predok-id (uzol) (getf uzol :predok-id))
(defun uzol_cena (uzol) (getf uzol :cena))
(defun uzol_hlbka (uzol) (getf uzol :hlbka))

(defun generuj-citac ()
  (let ((pamat 0))
    (lambda () (incf pamat))))

(defun rovnake-stavy-p (uzol1 uzol2)
  (equal (uzol_stav uzol1) (uzol_stav uzol2)))

(defun zisti-cenu (cena cena-stavu)
  (if (< cena cena-stavu)
    (setf cena cena-stavu)
     cena))

(defun cena-stavu (stav predok)
  (let* ((cenaa 0))
    (dotimes (x 4)
      (if (not (eql (nth x stav) (nth x (uzol_stav predok))))
        (progn (cond
          ((= x 0) (setf cenaa (zisti-cenu cenaa 1)))
          ((= x 1) (setf cenaa (zisti-cenu cenaa 10)))
          ((= x 2) (setf cenaa (zisti-cenu cenaa 5)))
          ((= x 3) (setf cenaa (zisti-cenu cenaa 2)))
          ))))
          cenaa))
          
(defun suma (stav predok)
  (+ (cena-stavu stav predok) (uzol_cena predok)))

(defun ocisluj (stavy citac predok)
  (let* ((predok-id (uzol_uzol-id predok))
         (aktual-hlbka (uzol_hlbka predok)))
    (mapcar #'(lambda (stav) (novy-uzol stav (funcall citac) predok-id
                                        (suma stav predok) (1+ aktual-hlbka)))
      stavy)))
 
(defun hladaj (ciel citac vypis)
  (unless (equal *open* '())
    (let* ((uzol-na-expanziu (pop *open*))
           (nove-stavy (unless (member uzol-na-expanziu *close* 
                                       :test #'rovnake-stavy-p)
                         (expanduj (uzol_stav uzol-na-expanziu)))))
      (push uzol-na-expanziu *close*)
      (setf *open* (append (ocisluj nove-stavy citac uzol-na-expanziu)
                           *open*))
      (print *open*)
      (informuj uzol-na-expanziu nove-stavy vypis)
      (setf *open* (sort *open* #'< :key #'uzol_cena))
      (if (member ciel nove-stavy :test #'equal)
          t
        (hladaj ciel citac vypis)))))



(defun ries (start ciel &key (vypis 0))
  (setf *open* (list (novy-uzol start 0)))
  (setf *close* '())
  (unless (equal start ciel) (hladaj ciel (generuj-citac) vypis))
  (if (eql *open* '()) (format t "neviem najst riesenie~%")
    (vytlac-cestu (novy-uzol ciel))))
    
    
(defun informuj (stav-na-expanziu nove-stavy mod)
  (when (> mod 0)
    (format t "~%exp: ~a~%" stav-na-expanziu)
    (format t "  gen: ~{ ~a ~}" nove-stavy))
  (when (> mod 1)
    (format t "~%  open:  ~{~a~^ ~a~^ ~a~^ ~a ~#[ ~:;~%         ~]~}" *open*)
    (format t "~%  close: ~{~a~^ ~a~^ ~a~^ ~a ~#[ ~:;~%         ~]~}" *close*)))
    
    
(defun zostav-cestu (cesta)
  (let ((krok (uzol_uzol-id (first cesta)))
        (predok-id (uzol_predok-id (first cesta))))
    (if (= 0 krok)
        cesta
      (zostav-cestu
       (cons (first (remove-if-not 
                     #'(lambda (x) (= (uzol_uzol-id x) predok-id)) *close*))
             cesta)))))

(defun vytlac-cestu (ciel)
  (let ((cesta (remove-if-not #'(lambda (x) (rovnake-stavy-p x ciel)) *open*)))
    (format t "~{  ~a ~%~}" (zostav-cestu cesta))))    
        