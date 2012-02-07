;;;; AJL - eliza, v2
;;;  MM, 2009

;;; Zalozene na:
;;;   Code from Paradigms of Artificial Intelligence Programming
;;;   http://norvig.com/paip/
;;;   Copyright (c) 1991 Peter Norvig
;;;
;;;   File eliza1.lisp: Basic version of the Eliza program
;;;   File eliza.lisp: Advanced version of Eliza.
;;;   File auxfns.lisp: Auxiliary functions used by all other programs

;;;; Zadanie: Program pre rozhovor s pocitacom
;;;
;;;  Na zaklade pravidiel "vzor-reakcia" sa pre vstup od pouzivatela
;;;  vybera vhodna odpoved, ktora sa modifikuje na zaklade uzivatelovych
;;;  vyrokov.

;;;; Verzia 7 ******************************************************

(defconstant fail nil)

(defconstant no-bindings '((t . t)))

(defparameter *eliza-rules* nil)

(setf *eliza-rules*
  '(((Hello)
     (Hi. How do you do.)
     (Hello. What can I do for you ?))
    ((?a I am (?* b))
     (Why ?a ?) (Are you really ?b ?))
    ((I think (?* ?a))
     (Do you really think ?a ?))
    ((?3a bocik)
     (neverim ze ?3a ))
    ((Good bye)
     (good bye))
    ))

(defun pat-match (pattern input &optional (bindings no-bindings))
  "Match pattern against input in the context of the bindings"
  (cond ((eq bindings fail) fail)
        ((eql pattern input)
         bindings)
        ((segment-pattern-p pattern)
         (segment-match pattern input bindings))
        ((and (consp pattern) (consp input))
         (if (variable-p (first pattern))
               (let ((pocet-slov (length (string (first pattern)))))
                 (cond
                  ((eql pocet-slov 2) (pat-match (rest pattern) (rest input)
                                                 (pat-match (first pattern) (first input) bindings)))
                  ((eql pocet-slov 3)
                   (let ((dlzka (parse-integer
                                 (subseq (string (first pattern)) 1 2))))
                     (if (>= (length input) dlzka)
                         (pat-match 
                          (rest pattern) 
                          (subseq input dlzka)
                          (match-variable (first pattern) 
                                          (subseq input 0 dlzka) bindings)))))))
           
           (pat-match (rest pattern) (rest input)
                      (pat-match (first pattern) (first input) bindings))))
        (t fail)))

(defun segment-pattern-p (pattern)
  "Is this a segment matching pattern: ((?* var) . pat)"
  (and (consp pattern)
       (starts-with (first pattern) '?*)))

(defun starts-with (list x)
  "Is this a list whose first element is x?"
  (and (consp list) (eql (first list) x)))

(defun segment-match (pattern input bindings &optional (start 0))
  "Match the segment pattern ((?* var) . pat) against input."
  (let ((var (second (first pattern)))
        (pat (rest pattern)))
    (if (null pat)
        (match-variable var input bindings)
      ;; We assume that pat starts with a constant
      ;; In other words, a pattern can't have 2 consecutive vars
      (let ((pos (position (first pat) input
                           :start start :test #'equal)))
        (if (null pos)
            fail
          (let ((b2 (pat-match
                     pat (subseq input pos)
                     (match-variable var (subseq input 0 pos)
                                     bindings))))
            ;; If this match failed, try another longer one
            (if (eq b2 fail)
                (segment-match pattern input bindings (+ pos 1))
              b2)))))))

(defun match-variable (var input bindings)
  "Does VAR match input?  Uses (or updates) and returns bindings."
  (let ((binding (get-binding var bindings)))
    (cond ((not binding) (extend-bindings var input bindings))
          ((equal input (binding-val binding)) bindings)
          (t fail))))

(defun get-binding (var bindings)
  "Find a (variable . value) pair in a binding list."
  (assoc var bindings))

(defun binding-val (binding)
  "Get the value part of a single binding."
  (cdr binding))

(defun extend-bindings (var val bindings)
  "Add a (var . value) pair to a binding list."
  (cons (cons var val)
        ;; Once we add a "real" binding,
        ;; we can get rid of the dummy no-bindings
        (if (eq bindings no-bindings)
            nil
          bindings)))


(defun variable-p (x)
  "Is x a variable (a symbol beginning with `?')?"
  (and (symbolp x) (equal (elt (symbol-name x) 0) #\?)))


(defun use-eliza-rules (input)
  "Find some rule with which to transform the input."
  (some #'(lambda (rule)
            (let ((result (pat-match (rule-pattern rule) input)))
              (if (not (eq result fail))
                  (sublis (switch-viewpoint result)
                          (random-elt (rule-responses rule))))))
        *eliza-rules*))

(defun random-elt (choices)
  "Choose an element from a list at random."
  (elt choices (random (length choices))))

(defun switch-viewpoint (words)
  "Change I to you and vice versa, and so on."
  (sublis '((I . you) (you . I) (me . you) (am . are))
          words))


(defun read-line-no-punct ()
  "Read an input line, ignoring punctuation."
  (read-from-string
   (concatenate 'string "(" (substitute-if #\space #'punctuation-p
                                           (read-line))
     ")")))

(defun punctuation-p (char) (find char ".,;:`!?#-()\\\""))

(defun eliza ()
  "Respond to user input using pattern matching rules."
  (loop
    (print 'eliza>)
    (let* ((input (read-line-no-punct))
           (response (flatten (use-eliza-rules input))))
      (print-with-spaces response)
      (if (equal response '(good bye)) (RETURN)))))


(defun flatten (the-list)
  "Append together elements (or lists) in the list."
  (mappend #'mklist the-list))

(defun mklist (x)
  "Return x if it is a list, otherwise (x)."
  (if (listp x)
      x
    (list x)))

(defun mappend (fn the-list)
  "Apply fn to each element of list and append the results."
  (apply #'append (mapcar fn the-list)))



(defun print-with-spaces (list)
  (format t "~{~a ~}" list))


(defun rule-pattern (rule) (first rule))
(defun rule-responses (rule) (rest rule)) 
