;;;; Chapter 2

(defparameter *simple-grammar*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Noun))
    (verb-phrase ->(Verb noun-phrase))
    (Article -> the a)
    (Noun -> man ball woman table)
    (Verb -> hit took saw liked))

  "A grammar for a trivial subset of English.")

(defvar *grammar* *simple-grammar*)

(defun rule-lhs (rule)
  "The left-hand side of a rule."
  (first rule))

(defun rule-rhs (rule)
  "The right-hand side of a rule."
  (rest (rest rule)))

(defun rewrites (category)
  "Return a list of the possible rewrites for this category."
  (rule-rhs (assoc category *grammar*)))

(defun random-elt (choices)
  "Choose an element from a list at random."
  (elt choices (random (length choices))))

(defun mappend (f xs)
  "Apply function f to each element of list xs and append the results."
  (apply #'append (mapcar f xs)))

;; 2.1 Uses cond but avoids calling rewrites twice
(defun generate (phrase)
  "Generate a random sentence or phrase"
  (cond ((listp phrase) (mappend #'generate phrase))
        (t (let ((choices (rewrites phrase)))
             (cond ((null choices) (list phrase))
                   (t (generate (random-elt choices))))))))

;; 2.2 Version of generate that explicitly differentiates between terminal
;; symbols and nonterminal symbols

(defun non-terminal-p (category)
  "True if this is a category in the grammar."
  (not (null (rewrites category))))

(defun generate2 (phrase)
  "Generate a random sentence or phrase"
  (cond ((listp phrase) (mappend #'generate phrase))
        ((non-terminal-p phrase) (generate (random-elt (rewrites phrase))))
        (t (list phrase))))
