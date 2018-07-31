(in-package :gates)

(defmacro define-bool (name a &optional (b 0))
  (when (eq name 'NOT) 
    (setf name 'not!))
  `(labels ((not! (x &optional y)
	      (declare (ignore y))(not x)))
     (let ((result 0)(x ,a) (y ,b))
       (progn 		   
	 (progn (when (= x 0) (setf x NIL))
		(when (= y 0) (setf y NIL))
		(setf result (,name x y))) 
	 (if (eq result NIL) 0 1)))))


(in-package :irreversible)



(defmacro and-tmp (x y)
  `(bit-or (make-array ,x :element-type 'bit) (make-array ,y :element-type 'bit)))

(defmacro define-boolean (name a &optional (b 0))
  `(aref (,name (make-array 1 :element-type 'bit :initial-element ,a) 
		(make-array 1 :element-type 'bit :initial-element ,b)) 0))

(defun and (x y)
  (define-boolean bit-and x y))

(defmacro and-tmp (x y)
  `(bit-and (make-array ,x :element-type 'bit) (make-array ,y :element-type 'bit)))

(defun not (x)
  (define-boolean bit-not x))

(defun xor (x y) 
  (define-boolean bit-xor x y))

(defun or (x y)
  (not (xor x y)))




;;Functions
(defun f (x)
  (not x))

;;f is reversible
(f (f 1))


;;G is irreversible
(defun g (x)
  (or 1 (not x)))

(DEFUN CONSTANT (X)
  (AND 0 (NOT X)))

(DEFUN REV-CONSTANT (X Y)
  (VALUES X (XOR Y (CONSTANT X))))

(defmacro rev (name inputs ancilla fn)
  `(defun ,name ,(append inputs ancilla)
     (values ,@inputs (xor ,@(last ancilla) (apply ,fn (list ,@inputs))))))
