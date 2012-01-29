(defparameter *open* '())
(defparameter *close* '())

(defun baca_stav (stav)
  (first stav))

(defun baca-na-druhy-breh ()
  #'(lambda (stav) 
	(if (equal 0 (baca_stav stav))
		(setf stav (list 1 (second stav) (third stav) (fourth stav)))
		(setf stav (list 0 (second stav) (third stav) (fourth stav))))))

(defun koza_stav (stav)
  (second stav))


(defun koza-na-druhy-breh ()
  #'(lambda (stav) 
	(if (equal 0 (baca_stav stav))
		(setf stav (list 1 1 (third stav) (fourth stav)))
		(setf stav (list 0 0 (third stav) (fourth stav))))))	
  
(defun kapusta_stav (stav)
  (third stav))

(defun kapusta-na-druhy-breh ()
  #'(lambda (stav) 
	(if (equal 0 (baca_stav stav))
		(setf stav (list 1 (second stav) 1 (fourth stav)))
		(setf stav (list 0 (second stav) 0 (fourth stav))))))	

(defun vlk_stav (stav)
  (fourth stav))

(defun vlk-na-druhy-breh ()
  #'(lambda (stav) 
	(if (equal 0 (baca_stav stav))
		(setf stav (list 1 (second stav) (third stav) 1))
		(setf stav (list 0 (second stav) (third stav) 0)))))

(defun find-operators (stav)
  (remove-if-not #'(lambda (x) x)
		 (list 
			;;;4
			(when (and (equal (baca_stav stav) (kapusta_stav stav))
			 (not (equal (vlk_stav stav) (koza_stav stav)))
			 (equal (vlk_stav stav) (kapusta_stav stav))
			 )
			 (kapusta-na-druhy-breh))

			(when (and (equal (baca_stav stav) (kapusta_stav stav))
			 (not (equal (vlk_stav stav) (koza_stav stav)))
			 (equal (koza_stav stav) (kapusta_stav stav))
			 )
			 (kapusta-na-druhy-breh))
				;;;6
		       (when (and (equal (baca_stav stav) (vlk_stav stav)) 
				   (not (equal (koza_stav stav) (kapusta_stav stav)))
				   ;(not (equal (vlk_stav stav) (kapusta_stav stav)))
				   )
			     (vlk-na-druhy-breh))
				;;;1
		       (when (and(equal (koza_stav stav) (baca_stav stav))
                                 (equal (kapusta_stav stav)(vlk_stav stav)))
			     (koza-na-druhy-breh))
				 ;;;5
		       (when (and(equal (koza_stav stav) (baca_stav stav))
				(not(equal (kapusta_stav stav)(vlk_stav stav))))
			     (koza-na-druhy-breh))
				;;;2
			   (when (and (equal (baca_stav stav) (koza_stav stav)) 
			   	(equal (kapusta_stav stav) (vlk_stav stav))
				(not(equal (kapusta_stav stav)(koza_stav stav)))
				(not(equal (vlk_stav stav)(koza_stav stav)))
				)
			     (baca-na-druhy-breh))
				
		       (when (and (equal (baca_stav stav) (vlk_stav stav)) 
				   (not (equal (baca_stav stav) (koza_stav stav)))
				   (equal (baca_stav stav) (kapusta_stav stav))
				   (not(equal (kapusta_stav stav)(koza_stav stav)))
				   (not(equal (vlk_stav stav)(koza_stav stav)))
				   )
			     (baca-na-druhy-breh)))))

(defun hladaj (ciel vypis)
  (unless (equal *open* '())
    (let* ((stav-na-expanziu (pop *open*))
	   (nove-stavy (unless (member stav-na-expanziu *close* :test #'equal)
			 (expanduj stav-na-expanziu))))
      (push stav-na-expanziu *close*)
      (setf *open* (append *open* nove-stavy))
      (informuj stav-na-expanziu nove-stavy vypis)
      (if (member ciel nove-stavy :test #'equal)
	  t
	  (hladaj ciel vypis)))))

(defun ries (start ciel &optional (vypis 1))
  (setf *open* (list start))
  (setf *close* '())
  (unless (equal start ciel) (hladaj ciel vypis)))

(defun expanduj (stav)
  (mapcar #'(lambda (operator) (funcall operator stav)) (find-operators stav)))


(defun informuj (stav-na-expanziu nove-stavy mod)
  (when (> mod 0)
    (format t "~%exp: ~a~%" stav-na-expanziu)
    (format t "  gen: ~{ ~a ~}" nove-stavy))
  (when (> mod 1)
    (format t "~%  open:  ~{~a~^ ~a~^ ~a~^ ~a ~#[ ~:;~%         ~]~}" *open*)
    (format t "~%  close: ~{~a~^ ~a~^ ~a~^ ~a ~#[ ~:;~%         ~]~}" *close*)))
