;;; Chapter 1 ;;;

;; Helpers
(defmacro assert-eq (test expected)
  `(assert (eql ,test ,expected)))

;; 1.1
(defparameter *name-suffixes*
  '(MD Jr.))

(defun last-name (name)
  "Returns the last name taking into consideration special suffixes as defined in *name-suffixes*."
  (let ((lname (car (last name))))
    (if (member lname *name-suffixes*)
        (last-name (butlast name))
        lname)))

(defun test-last-name ()
  (assert-eq (last-name '(Chathura Colombage MD)) 'COLOMBAGE))

;; 1.2

(defun power-naive (n m)
  "Naive version based on the definition of power."
  (cond
    ((= m 0) 1)
    (t (* n (power-naive n (- m 1))))))

(defun power (n m)
  "This version runs in logarithmic time."
  (cond
    ((= m 0) 1)
    ((evenp m) (let ((m/2 (/ m 2)))
                     (power (* n n) m/2)))
    (t (* n (power n (- m 1))))))

(defun test-power-naive () (assert-eq (power-naive 2 3) 8))
(defun test-power () (assert-eq (power 2 3) 8))

;; 1.3
(defun count-atoms (elem)
  "Counts the number of atoms in an expression. We avoid counting null at the
   end of a cons list by explicitly checking for that."
  (cond
    ((atom elem) 1)
    ((null (cdr elem)) (count-atoms (car elem)))
    (t (+ (count-atoms (car elem)) (count-atoms (cdr elem))))))

(defun test-count-atoms ()
  (assert-eq (count-atoms '(a (b) c)) 3))

;; 1.4
(defun count-anywhere (e exp)
  "Count number of times an expression occurs anywhere within another expression."
  (cond
    ((null exp) 0)
    ((eq e (car exp)) (+ 1 (count-anywhere e (cdr exp))))
    ((listp (car exp)) (+ (count-anywhere e (car exp)) (count-anywhere e (cdr exp))))
    (t (count-anywhere e (cdr exp)))))

(defun test-count-anywhere ()
  (assert-eq (count-anywhere 'a '(a ((a) b) a)) 3))

;; 1.5
(defun dot-product (xs ys)
  "Dot product."
  (reduce #'+ (mapcar #'* xs ys)))

(defun test-dot-product ()
  (assert-eq (dot-product '(10 20) '(3 4)) 110))
