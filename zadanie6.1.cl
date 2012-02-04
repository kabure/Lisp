(defparameter *open* '())
(defparameter *close* '())
(defvar cenaa 0)

(defun nulty (stav)
  (nth 0 stav))

(defun prvy (stav)
  (nth 1 stav))

(defun druhy (stav)
  (nth 2 stav))

(defun treti (stav)
  (nth 3 stav))

(defun stvrty (stav)
  (nth 4 stav))

(defun piaty (stav)
  (nth 5 stav))

(defun siesty (stav)
  (nth 6 stav))

(defun siedmy (stav)
  (nth 7 stav))

(defun osmy (stav)
  (nth 8 stav))

(defun find-operators (stav)
  (remove-if-not #'(lambda (x) x)
                 
                 (let*((temp '()))
                   
                   (when (eql 0 (nulty stav))
                     (push #'(lambda (stav) 
                               (list (prvy stav) 0 (druhy stav) (treti stav) (stvrty stav) (piaty stav) (siesty stav) (siedmy stav) (osmy stav))) temp)
                     (push #'(lambda (stav)
                               (list (treti stav) (prvy stav) (druhy stav) 0 (stvrty stav) (piaty stav) (siesty stav) (siedmy stav) (osmy stav))) temp))
                   
                   (when (eql 0 (prvy stav))
                     (push #'(lambda (stav) 
                               (list 0 (nulty stav) (druhy stav) (treti stav) (stvrty stav) (piaty stav) (siesty stav) (siedmy stav) (osmy stav)))temp)
                     (push #'(lambda (stav)
                               (list (nulty stav) (druhy stav) 0 (treti stav) (stvrty stav) (piaty stav) (siesty stav) (siedmy stav) (osmy stav))) temp)
                     (push #'(lambda (stav)
                               (list (nulty stav) (stvrty stav) (druhy stav) (treti stav) 0 (piaty stav) (siesty stav) (siedmy stav) (osmy stav))) temp))
                   
                   (when (eql 0 (druhy stav))
                     (push #'(lambda (stav) 
                               (list (nulty stav) 0 (prvy stav) (treti stav) (stvrty stav) (piaty stav) (siesty stav) (siedmy stav) (osmy stav))) temp)
                     (push #'(lambda (stav)
                               (list (nulty stav) (prvy stav) (piaty stav) (treti stav) (stvrty stav) 0 (siesty stav) (siedmy stav) (osmy stav))) temp))
                   
                   (when (eql 0 (treti stav))
                     (push #'(lambda (stav) 
                               (list 0 (prvy stav) (druhy stav) (nulty stav) (stvrty stav) (piaty stav) (siesty stav) (siedmy stav) (osmy stav))) temp)
                     (push #'(lambda (stav)
                               (list (nulty stav) (prvy stav) (druhy stav) (stvrty stav) 0 (piaty stav) (siesty stav) (siedmy stav) (osmy stav))) temp)
                     (push #'(lambda (stav)
                               (list (nulty stav) (prvy stav) (druhy stav) (siesty stav) (stvrty stav) (piaty stav) 0 (siedmy stav) (osmy stav))) temp))
                   
                   (when (eql 0 (stvrty stav))
                     (push #'(lambda (stav) 
                               (list (nulty stav) 0 (druhy stav) (treti stav) (prvy stav) (piaty stav) (siesty stav) (siedmy stav) (osmy stav))) temp)
                     (push #'(lambda (stav)
                               (list (nulty stav) (prvy stav) (druhy stav) 0 (treti stav) (piaty stav) (siesty stav) (siedmy stav) (osmy stav))) temp)
                     (push #'(lambda (stav)
                               (list (nulty stav) (prvy stav) (druhy stav) (treti stav) (piaty stav) 0 (siesty stav) (siedmy stav) (osmy stav))) temp)
                     (push #'(lambda (stav)
                               (list (nulty stav) (prvy stav) (druhy stav) (treti stav) (siedmy stav) (piaty stav) (siesty stav) 0 (osmy stav))) temp))
                   
                   (when (eql 0 (piaty stav))
                     (push #'(lambda (stav) 
                               (list (nulty stav) (prvy stav) 0 (treti stav) (stvrty stav) (druhy stav) (siesty stav) (siedmy stav) (osmy stav))) temp)
                     (push #'(lambda (stav)
                               (list (nulty stav) (prvy stav) (druhy stav) (treti stav) 0 (stvrty stav) (siesty stav) (siedmy stav) (osmy stav))) temp)
                     (push #'(lambda (stav)
                               (list (nulty stav) (prvy stav) (druhy stav) (treti stav) (stvrty stav) (osmy stav) (siesty stav) (siedmy stav) 0)) temp))
                   
                   (when (eql 0 (siesty stav))
                     (push #'(lambda (stav) 
                               (list (nulty stav) (prvy stav) (druhy stav) 0 (stvrty stav) (piaty stav) (treti stav) (siedmy stav) (osmy stav))) temp)
                     (push #'(lambda (stav)
                               (list (nulty stav) (prvy stav) (druhy stav) (treti stav) (stvrty stav) (piaty stav) (siedmy stav) 0 (osmy stav))) temp))
                   
                   (when (eql 0 (siedmy stav))
                     (push #'(lambda (stav) 
                               (list (nulty stav) (prvy stav) (druhy stav) (treti stav) 0 (piaty stav) (siesty stav) (stvrty stav) (osmy stav))) temp)
                     (push #'(lambda (stav)
                               (list (nulty stav) (prvy stav) (druhy stav) (treti stav) (stvrty stav) (piaty stav) 0 (siesty stav) (osmy stav))) temp)
                     (push #'(lambda (stav)
                               (list (nulty stav) (prvy stav) (druhy stav) (treti stav) (stvrty stav) (piaty stav) (siesty stav) (osmy stav) 0)) temp))
                   
                   (when (eql 0 (osmy stav))
                     (push #'(lambda (stav) 
                               (list (nulty stav) (prvy stav) (druhy stav) (treti stav) (stvrty stav) 0 (siesty stav) (siedmy stav) (piaty stav))) temp)
                     (push #'(lambda (stav)
                               (list (nulty stav) (prvy stav) (druhy stav) (treti stav) (stvrty stav) (piaty stav) (siesty stav) 0 (siedmy stav))) temp))
                   temp
                   )))

(defun expanduj (stav)
  (mapcar #'(lambda (operator) (funcall operator stav)) (find-operators stav)))

(defun novy-uzol (stav &optional (uzol-id nil) (predok-id nil) cena (hlbka 0))
  (list :stav stav :uzol-id uzol-id :predok-id predok-id :cena cena :hlbka hlbka))

(defun uzol_stav (uzol) (getf uzol :stav))
(defun uzol_uzol-id (uzol) (getf uzol :uzol-id))
(defun uzol_predok-id (uzol) (getf uzol :predok-id))
(defun uzol_cena (uzol) (getf uzol :cena))
(defun uzol_hlbka (uzol) (getf uzol :hlbka))

(defun rovnake-stavy-p (uzol1 uzol2)
  (equal (uzol_stav uzol1) (uzol_stav uzol2)))

(defun generuj-citac ()
  (let ((pamat 0))
    (lambda () (incf pamat))))

(defun funkcia-h (stav ciel)
  (let* ((pocitadlo 0)
         (dlzka (length stav)))
    (dotimes (x dlzka)
      (when (not (eql (nth x ciel) (nth x stav)))
        (incf pocitadlo)))
    pocitadlo))

(defun funkcia-g (stav)
  (+ (uzol_hlbka stav) 1))


(defun cena-stavu (stav predok ciel)
  (+ (funkcia-h stav ciel) (funkcia-g predok)))

(defun ocisluj (stavy citac predok ciel)
  (let* ((predok-id (uzol_uzol-id predok))
         (aktual-hlbka (uzol_hlbka predok)))
    (mapcar #'(lambda (stav) (novy-uzol stav (funcall citac) predok-id
                                        (cena-stavu stav predok ciel) (1+ aktual-hlbka)))
      stavy)))

(defun informuj (stav-na-expanziu nove-stavy mod)
  (when (> mod 0)
    (format t "~%exp: ~a~%" stav-na-expanziu)
    (format t "  gen: ~{ ~a ~}" nove-stavy))
  (when (> mod 1)
    (format t "~%  open:  ~{~a~^ ~a~^ ~a~^ ~a ~#[ ~:;~%         ~]~}" *open*)
    (format t "~%  close: ~{~a~^ ~a~^ ~a~^ ~a ~#[ ~:;~%         ~]~}" *close*)))



(defun hladaj (ciel citac vypis)
  (unless (equal *open* '())
    (let* ((uzol-na-expanziu (pop *open*))
           (nove-stavy (unless (member uzol-na-expanziu *close* 
                                       :test #'rovnake-stavy-p)
                         (expanduj (uzol_stav uzol-na-expanziu)))))
      (push uzol-na-expanziu *close*)
      (setf *open* (append (ocisluj nove-stavy citac uzol-na-expanziu 
                                    ciel)
                           *open*))
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



