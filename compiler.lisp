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
  `((H ,x)))


(defun comp-measure (x)
  "Apply measurement to the wavefunction"
  `((MEASURE ,x)))

(defun gen (x file)
  "Writes the generated code to file"
  (let ((in file))
    (labels ((gen-rec (x)
	       (if (numberp x) (format in "QUBITS ~A~%" (incf x))
	       (if (null x) '()
		   (progn (format in "~A~%" (car x))
			  (gen-rec (cdr x)))))))
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


(defun comp-swap (x y complist)
  (let* ((code `(SWAP ,(cadr x) ,(car x))) (new-complist (cons code complist)))
    (if (equal (cdr code) y)
	(progn (setf (caar new-complist) 'CNOT) (append (reverse new-complist)  (cdr new-complist)))
	(progn (rotatef (car x) (cadr x)) (comp-swap (cdr x) y new-complist)))))

(defun create-seq-list (m n)
  (labels ((create (i)
	     (if (= i n)
		 (list i)
		 (cons i (create (1+ i))))))
    (create m)))


(defun rev-cnot (x)
	   "CNOT 2 1 = H 1, H2, CNOT 1 2, H 1, H2"
	   `(,@(comp-h (cadr x)) ,@(comp-h (car x)) ,@(comp-cnot (reverse x)) ,@(comp-h (cadr x)) ,@(comp-h (cadr x))))

(defun comp-cnot (x)
  "Compile CNOT using CZ and H gates"
  (if (> (car x) (cadr x)) (rev-cnot x)
  (comp-swap (reverse (create-seq-list  (car x) (cadr x))) x '())))

(defun parse (instr)
  "Parse Instruction into s-expression"
  (loop for i in instr collect (cdr i)))

(defun maximum (1st)
  (reduce #'max 1st))

(defun analysis (source)
  (let ((instr '()) (max 0))
    (with-open-file (in source :direction :input)	
	(when in
	  (loop for line = (read-line in nil)
	       
	     while line do (progn
			     (setf instr (lex-line line))
			     (setf instr (parse instr))
			     (if (> (maximum (cdr instr)) max) (setf max (maximum (cdr instr))) max) ))))
    max))
  
  
(defun compiler (source destination)
  "Reads code file and compiles each expression into output file"
  (let ((instr '()))
    (with-open-file (in source :direction :input)
      (with-open-file (out destination :direction :output :if-exists :supersede)
	
	(when in
	  (gen (analysis source) out)
	  
	  (loop for line = (read-line in nil)	       
	     while line do (progn
			     (setf instr (lex-line line))
			     (setf instr (parse instr))
			     (gen (comp instr) out))))))))
