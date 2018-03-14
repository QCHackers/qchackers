(defun comp-x (x)
  "Rotation around the X axis by pi radians"
  `((RX pi ,x)))

(defun comp-y (x)
  "Rotation around the Y axis by pi radians"
  `((RY pi ,x)))

(defun comp-z (x)
  "Rotation around the Z axis by pi radians"
  `((RZ pi ,x)))

(defun comp-h (x)
  "pi about X axis followed by pi/2 about Y axis"
  `((RX pi ,x) (RY pi/2 ,x)))

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
       (t "Doesnt exist")))
    
    (t  (comp (rest x)))))

(defun comp-cnot (x)
  "Compile CNOT using CZ and H gates"
  `(,@(comp  `(H ,(cadr x)))
      (CZ ,(car x) ,(cadr x))
      ,@(comp `(H ,(cadr x)))))

(defun compiler (code output)
  "Reads code file and compiles each expression into output file"
  (let ((in (open code :if-does-not-exist nil)) (out (open output :if-does-not-exist :create :direction :output :if-exists :supersede)))
    (when in
      (loop for line = (read in nil)
	   
	 while line do (gen (comp line) out)))
    (close in)
    (close out)))
