(defvar *mala-max* 3)
(defvar *velka-max* 4)

(defun obsah-malej (stav)
  (rest stav))

(defun obsah-velkej (stav)
  (first stav))

(defun op-vylej-velku ()
  #'(lambda (stav) (cons 0 (obsah-malej stav))))

(defun op-vylej-malu ()
  #'(lambda (stav) (cons (obsah-velkej stav) 0)))

(defun op-napln-velku ()
  #'(lambda (stav) (cons *velka-max* (obsah-malej stav))))

(defun op-napln-malu ()
  #'(lambda (stav) (cons (obsah-velkej stav) *mala-max*)))

(defun op-prelej-velku ()
  #'(lambda (stav)
      (if (< *mala-max* (+ (obsah-velkej stav) (obsah-malej stav)))
	  (cons (- (obsah-velkej stav)
		   (- *mala-max* (obsah-malej stav)))
		*mala-max*)
	  (cons 0 (+ (obsah-malej stav) (obsah-velkej stav))))))

(defun op-prelej-malu ()
  #'(lambda (stav)
      (if (< *velka-max* (+ (obsah-velkej stav) (obsah-malej stav)))
	  (cons *velka-max*
		(- (obsah-malej stav)
		   (- *velka-max* (obsah-velkej stav))))
	  (cons (+ (obsah-malej stav) (obsah-velkej stav)) 0))))

(defun find-operators (stav)
  (remove-if-not #'(lambda (x) x)
		 (list (when (< 0 (obsah-velkej stav))
			 (op-vylej-velku))
		       (when (< 0 (obsah-malej stav))
			 (op-vylej-malu))
		       (when (> *velka-max* (obsah-velkej stav))
			 (op-napln-velku))
		       (when (> *mala-max* (obsah-malej stav))
			 (op-napln-malu))
		       (when (and (< 0 (obsah-velkej stav)) 
				  (> *mala-max* (obsah-malej stav)))
			 (op-prelej-velku))
		       (when (and (< 0 (obsah-malej stav)) 
				  (> *velka-max* (obsah-velkej stav)))
			 (op-prelej-malu)))))

(defun expanduj (stav)
  (mapcar #'(lambda (operator) (funcall operator stav)) (find-operators stav)))

(defparameter *open* '())
(defparameter *close* '())

defun novy-uzol (stav &optional (uzol-id nil) (predok-id nil) cena)
  (list :stav stav :uzol-id uzol-id :predok-id predok-id :cena cena))

(defun najdi-prednajmensieho (zoznam minimum predosly key)
  (if (null zoznam)
      nil
      (let ((nove-minimum minimum) (nova-poloha))
	(when (> minimum (funcall key (car zoznam)))
	  (setf nove-minimum (funcall key (car zoznam)) nova-poloha predosly))
	(let ((nalez
	       (najdi-prednajmensieho (cdr zoznam) nove-minimum zoznam key)))
	  (if (null nalez) nova-poloha nalez))))))

(defun najmensi-dopredu (zoznam &key (key #'identity))
  (let ((najmensi zoznam))
    (unless (or (null zoznam) (null (cdr zoznam)))
      (let ((prednajmensi
	     (najdi-prednajmensieho (cdr zoznam)
				    (funcall key (car zoznam)) zoznam key)))
	(unless (null prednajmensi)
	  (setf najmensi (cdr prednajmensi))
	  (setf (cdr prednajmensi) (cdr najmensi))
	  (setf (cdr najmensi) zoznam))))
    najmensi
    ))

(defun funkcia-g (stav uzol)
  (let ((rodic (if (not (uzol>cena uzol)) 0 (uzol>cena uzol)))
	(rozdiel-v (- (car stav) (car (uzol>stav uzol))))
	(rozdiel-m (- (cdr stav) (cdr (uzol>stav uzol)))))
    (+ rodic
       (cond ((zerop rozdiel-v) (abs rozdiel-m))
	     ((zerop rozdiel-m) (abs rozdiel-v))
	     (t (max (abs rozdiel-v) (abs rozdiel-m)))))))

(defun funkcia-h (stav ciel)
  (abs (- (+ (car stav) (cdr stav)) 
	  (+ (car ciel) (cdr ciel)))))

defun uzol>stav (uzol) (getf uzol :stav))
(defun uzol>uzol-id (uzol) (getf uzol :uzol-id))
(defun uzol>predok-id (uzol) (getf uzol :predok-id))
(defun uzol>cena (uzol) (getf uzol :cena))

(defun rovnake-stavy-p (uzol1 uzol2)
  (equal (uzol>stav uzol1) (uzol>stav uzol2)))
  
(defun cena-stavu (stav uzol ciel metoda)
  (+ (if (or (eql metoda :uc) (eql metoda :a*))
	 (funkcia-g stav uzol) 0)
     (if (or (eql metoda :a*) (eql metoda :gs))
	 (funkcia-h stav ciel) 0)))

(defun informuj (stav-na-expanziu nove-stavy mod)
  (when (> mod 0)
    (format t "~%exp: ~a~%" stav-na-expanziu)
    (format t "  gen: ~{ ~a ~}" nove-stavy))
  (when (> mod 1)
    (format t "~%  open:  ~{~a~^ ~a~^ ~a~^ ~a ~#[ ~:;~%         ~]~}" *open*)
    (format t "~%  close: ~{~a~^ ~a~^ ~a~^ ~a ~#[ ~:;~%         ~]~}" *close*)))

(defun generuj-citac ()
  (let ((pamat 0))
    (lambda () (incf pamat))))

(defun ocisluj (stavy citac predok ciel metoda)
  (let ((predok-id (uzol>uzol-id predok)))
    (mapcar #'(lambda (stav) (novy-uzol stav (funcall citac) predok-id
					(cena-stavu stav predok ciel metoda)))
	    stavy)))


(defun hladaj (ciel citac vypis metoda)
  (unless (equal *open* '())
    (let* ((uzol-na-expanziu (pop *open*))
	   (nove-stavy (unless (member uzol-na-expanziu *close* 
				       :test #'rovnake-stavy-p)
			 (expanduj (uzol>stav uzol-na-expanziu)))))
      (push uzol-na-expanziu *close*)
      (setf *open* (append (ocisluj nove-stavy citac uzol-na-expanziu 
				    ciel metoda)
			   *open*))
      (informuj uzol-na-expanziu nove-stavy vypis)
      (setf *open* (najmensi-dopredu *open* :key #'uzol>cena))
      (if (member ciel nove-stavy :test #'equal)
	  t
	  (hladaj ciel citac vypis metoda)))))

(defun zostav-cestu (cesta)
  (let ((krok (uzol>uzol-id (first cesta)))
	(predok-id (uzol>predok-id (first cesta))))
    (if (= 0 krok)
	cesta
	(zostav-cestu
	 (cons (first (remove-if-not 
		       #'(lambda (x) (= (uzol>uzol-id x) predok-id)) *close*))
	       cesta)))))

(defun vytlac-cestu (ciel)
  (let ((cesta (remove-if-not #'(lambda (x) (rovnake-stavy-p x ciel)) *open*)))
    (format t "~{  ~a ~%~}" (zostav-cestu cesta))))

((defun ries (start ciel &key (vypis 0) (metoda :uc))
  (setf *open* (list (novy-uzol start 0)))
  (setf *close* '())
  (unless (equal start ciel) (hladaj ciel (generuj-citac) vypis metoda))
  (if (eql *open* '()) (format t "neviem najst riesenie~%")
      (vytlac-cestu (novy-uzol ciel))))
