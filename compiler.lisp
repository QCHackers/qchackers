(defun comp-x (x)
  "Rotation around the X axis by pi radians"
  `((X ,x)))

(defun comp-y (x)
  "Rotation around the Y axis by pi radians"
  `((Y ,x)))

(defun comp-z (x)
  "Rotation around the Z axis by pi radians"
  `((Z ,x)))

(defun comp-h (x)
  "pi about X axis followed by pi/2 about Y axis"
  `((H ,x)))


(defun comp-measure (x)
  "Apply measurement to the wavefunction"
  `((MEASURE ,x)))

(defun gen (x file)
  "Writes the generated code to file"
  (let ((in file))
    (labels ((gen-rec (x)
	       (if (null x) '()
		   (progn (format in "~A~%" (car x))
			  (gen-rec (cdr x))))))
      (gen-rec x))))

(defun comp (x)
  "Compile expression to list of instructions"
  (cond
    ((null x) '())
    ((case (car x)
       (X (comp-x (cadr x)))
       (Y  (comp-y (cadr x)))
       (Z (comp-z (cadr x)))
       (H (comp-h (cadr x)))
       (pi/8 (comp-y (cadr x)))
       (CNOT (comp-cnot (cdr x)))
       (MEASURE (comp-measure (cadr x)))
       (t (error "GATE IS NOT IMPLEMENTED."))))
    
    (t  (comp (rest x)))))

(defun comp-swap (x)
  "Compile CNOT using CZ and H gates"
  `((SWAP ,(car x) ,(cadr x))))


(defun swap-cnot (x y)
	   (print x)
  (if (eq (- (cadr x) (car x)) 1) (append y `((CNOT ,(car x) ,(cadr x))) (append (reverse y) '()))
      (progn
	(print (comp-swap (cons (+ (car x) 1) (cons (cadr x)  '()))))
	(setf y (cons (comp-swap (cons (+ (car x) 1) (cons (cadr x)  '()))) y))
	     (swap-cnot (cons  (car x) (cons (- (cadr x) 1) '())) (car y))))
  )

(defun comp-cnot (x)
  "Compile CNOT using CZ and H gates"
  (if (eq (- (cadr x) (car x)) 1) `((CNOT ,(car x) ,(cadr x)))
      (swap-cnot x '())))

(defun parse (instr)
  "Parse Instruction into s-expression"
  (loop for i in instr collect (cdr i)))


(defun compiler (source destination)
  "Reads code file and compiles each expression into output file"
  (let ((instr '()))
    (with-open-file (in source :direction :input)
      (with-open-file (out destination :direction :output :if-exists :supersede)
	
	(when in
	  (loop for line = (read-line in nil)
	       
	     while line do (progn
			     (setf instr (lex-line line))
			     (setf instr (parse instr))
			     (gen (comp instr) out))))))))
