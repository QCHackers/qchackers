 (defun comp-cnot2 (x)
   "Compile CNOT for nonlinear qubit order"
   (cond
     (
       (< (cadr x) (car (last x)))
       (SWAP (+ 1 (cadr x)) (car (last x)))
       )
     (
      (> (cadr x) (car (last x)))
      (SWAP (+ 1 (car (last x))) (cadr x))
      )
     (
      (t) (comp-cnot x)
      )
     )
   )
