(in-package :sat)

(defun binary-list (n)
  (assert (>= n 0))
  (multiple-value-bind (q r) (floor n 2)
    (if (zerop q)
        (list r)
        (nconc (binary-list q) (list r)))))

(defun get-unfiltered (n)
  (let ((exp (expt 2 n)))
    (loop for x from 0 to (decf exp 1)
       collect (binary-list x))))

(defun pad (1st k)
  (loop for x from 1 to k
     do (setf 1st (cons '0 1st)))
  1st)

(defun gen-table (n)
  (let ((rsl (get-unfiltered n)))
    (loop for x in rsl
       do (unless (<  n (length x))
	    (setf (nth (position x rsl :test #'equal) rsl)
		  (pad x (- n (length x))))))
    rsl))


(defun call-func (fn)
  (let ((bool 0))
    (loop for x in (gen-table (length (third (function-lambda-expression fn))))
       do (when (= 1 (apply fn x)) 
	    (progn (setf bool 1) (return))))
    bool))

(defun zero-zero (a b)
  (irreversible:and (irreversible:not a) (irreversible:not b)))
