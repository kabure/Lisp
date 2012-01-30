;;;; Verzia 4 ******************************************************

(defun expanduj (stav)
  (cond
    ((equal stav '(0 . 0)) '((4 . 0) (0 . 3)))
    ((equal stav '(0 . 1)) '((0 . 0) (4 . 1) (0 . 3) (1 . 0)))
    ((equal stav '(0 . 2)) '((0 . 0) (4 . 2) (0 . 3) (2 . 0)))
    ((equal stav '(0 . 3)) '((0 . 0) (4 . 3) (3 . 0)))
    ((equal stav '(1 . 0)) '((0 . 0) (4 . 0) (1 . 3) (0 . 1)))
    ((equal stav '(1 . 1)) '((0 . 1) (1 . 0) (4 . 1) (1 . 3) (0 . 2) (2 . 0)))
    ((equal stav '(1 . 2)) '((0 . 2) (1 . 0) (4 . 2) (1 . 3) (0 . 3) (3 . 0)))
    ((equal stav '(1 . 3)) '((0 . 3) (1 . 0) (4 . 3) (4 . 0)))
    ((equal stav '(2 . 0)) '((0 . 0) (4 . 0) (2 . 3) (0 . 2)))
    ((equal stav '(2 . 1)) '((0 . 1) (2 . 0) (4 . 1) (2 . 3) (0 . 3) (3 . 0)))
    ((equal stav '(2 . 2)) '((0 . 2) (2 . 0) (4 . 2) (2 . 3) (1 . 3) (4 . 0)))
    ((equal stav '(2 . 3)) '((0 . 3) (2 . 0) (4 . 3) (4 . 1)))
    ((equal stav '(3 . 0)) '((0 . 0) (4 . 0) (3 . 3) (0 . 3)))
    ((equal stav '(3 . 1)) '((0 . 1) (3 . 0) (4 . 1) (3 . 3) (1 . 3) (4 . 0)))
    ((equal stav '(3 . 2)) '((0 . 2) (3 . 0) (4 . 2) (3 . 3) (4 . 1) (2 . 3)))
    ((equal stav '(3 . 3)) '((0 . 3) (3 . 0) (4 . 3) (4 . 2)))
    ((equal stav '(4 . 0)) '((0 . 0) (4 . 3) (1 . 3)))
    ((equal stav '(4 . 1)) '((0 . 1) (4 . 0) (4 . 3) (2 . 3)))
    ((equal stav '(4 . 2)) '((0 . 2) (4 . 0) (4 . 3) (3 . 3)))
    ((equal stav '(4 . 3)) '((0 . 3) (4 . 0)))))

(defparameter *open* '())
(defparameter *close* '())
(defparameter *next-level* '())
(defvar *hlbka* 0)
(defvar citac)


(defun informuj (stav-na-expanziu nove-stavy mod)
  (when (> mod 0)
    (format t "~%exp: ~a~%" stav-na-expanziu)
    (format t "  gen: ~{ ~a ~}~%" nove-stavy))
  (when (> mod 1)
    (format t "~%  open:  ~{~a~^ ~a~^ ~a~^ ~a ~#[ ~:;~%         ~]~}" *open*)
    (format t "~%  close: ~{~a~^ ~a~^ ~a~^ ~a ~#[ ~:;~%         ~]~}" *close*)))

(defun rovnake-stavy-p (uzol1 uzol2)
  (and (= (car (cdr (cdr uzol1))) (car (cdr (cdr uzol2))))
       (= (cdr (cdr (cdr uzol1))) (cdr (cdr (cdr uzol2))))))

(defun generuj-citac ()
  (let ((pamat 0))
    (lambda () (incf pamat))))

(defun ocisluj (stavy citac predok)
  (let ((predok-id (caar (cdr predok)))
        (aktual-hlbka (car predok)))
       (mapcar #'(lambda (stav) (cons (1+ aktual-hlbka) (cons (cons (funcall citac) predok-id) stav)))
	    stavy)))

(defun vytlac-cestu (ciel)
  (let ((cesta (remove-if-not #'(lambda (x) (rovnake-stavy-p x ciel)) *open*)))
    (format t "~{  ~a ~%~}" (zostav-cestu cesta))))

(defun zostav-cestu (cesta)
  (let ((krok (caar (cdr (first cesta))))
	(predok-id (cdar (cdr (first cesta)))))
    (if (= 0 krok)
	cesta
	(zostav-cestu
	 (cons (first (remove-if-not
                #'(lambda (x) (= (caar (cdr x)) predok-id)) *close*))     
	       cesta)))))


(defun hladaj (ciel citac vypis metoda)
  
    (unless (equal *open* '())
    (if (= *hlbka* (caar *open*))
        (let* ((temp (pop *open*)))
              (push temp *next-level*)
          (hladaj ciel citac vypis metoda))
      
    (let* ((uzol-na-expanziu (pop *open*))
	   (nove-stavy (unless (member uzol-na-expanziu *close*
				       :test #'rovnake-stavy-p)
                  (expanduj (cdr (cdr uzol-na-expanziu))))))
         (push uzol-na-expanziu *close*)
      (ccase metoda
	(:bf (setf *open* (append *open*
				  (ocisluj nove-stavy citac uzol-na-expanziu))))
	(:df (setf *open* (append (ocisluj nove-stavy citac uzol-na-expanziu)
					*open*))))
      (informuj uzol-na-expanziu nove-stavy vypis)
      
      (if (member ciel nove-stavy :test #'equal)
          t
	  
        (hladaj ciel citac vypis metoda))
     
      )))
  (if (eql *open* '()) 
  (progn (if (eql *next-level* '())
    (format t "neviem najst riesenie~%") 
    (progn 
    (format t "neviem najst riesenie pri hlbke ~a~%" *hlbka*)
    (setf *hlbka* (+ 1 *hlbka*))
    (format t "hlbka prehladavania sa zvisila na ~a~%" *hlbka*)
    (setf *next-level* (reverse *next-level*))
      (setf *open* (append *next-level* *open*))
      (setf *next-level* '())
      (hladaj ciel citac vypis metoda)    
    ))                                                  
    )
    t)

  )

(defun ries (start ciel *hlbka* &key (vypis 1) (metoda :df))
  (decf *hlbka*)
  (setf *next-level* '())
  (setf *open* (list (cons 0(cons '(0 . 0) start))))
  (setf *close* '())
  (setf citac (generuj-citac))
  
  (unless (equal start ciel) (hladaj ciel citac vypis metoda))
  (if (eql *open* '()) (format t "neviem najst riesenie~%")
    (vytlac-cestu (cons t(cons t ciel))))
  ) 
