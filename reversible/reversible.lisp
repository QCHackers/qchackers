(in-package :reversible)

(defun execute (fn args)
  (multiple-value-list
   (apply fn args)))

(defun toffoli (x y z)
  (execute
   #'(lambda (x y z)
       (values x y (irreversible:xor (irreversible:and x y) z)))
   (list x y z)))

(defun cnot (x y)
  (execute 
   #'(lambda (x y)
       (values x (irreversible:xor x y))) 
   (list x y)))

(defun rev-cnot (x y)
  (execute 
   #'(lambda (x y)
       (values (irreversible:xor y x) y)) 
   (list x y)))

(defun swap (x y)
  (apply #'cnot (apply #'rev-cnot (apply #'cnot (list x y)))))

(defun half-adder (x y z)
  (let* ((tf (toffoli x y z)) (subs (subseq tf 0 2)))
    (values (append (cnot (first subs) (second subs))(list (third tf))))))
